{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
}

{
  Модуль: uzeacadtable_model
  Назначение: Основная сущность GDBObjAcadTable — таблица AutoCAD
  из формата DXF. Хранит данные таблицы (точку вставки, размеры,
  тексты ячеек, стили, объединения) и делегирует логику
  специализированным модулям.
  Зависимости: uzeacadtable_types, uzeacadtable_styles,
               uzeacadtable_cell, uzeacadtable_merge,
               uzeacadtable_layout, uzeacadtable_stylemanager,
               uzeacadtable_dxf_read, uzeacadtable_dxf_write,
               uzeentcomplex, uzeentityfactory, uzeconsts и др.
}

unit uzeacadtable_model;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzgldrawcontext, uzedrawingdef, uzeentityfactory, uzeentcomplex,
  uzeentline, uzeentmtext, uzeentsubordinated, uzeentabstracttext,
  uzeentity, UGDBSelectedObjArray, uzglviewareadata, UGDBVisibleTreeArray,
  uzctnrVectorBytesStream, uzeTypes, uzeconsts,
  uzegeometry, uzegeometrytypes, uzeffdxfsupport, uzMVReader,
  uzbLogIntf, uzclog, SysUtils, Math, uzctnrvectordouble,
  uzestylestablesdxf, gzctnrVectorTypes, Types, uzestylestexts,
  uzeacadtable_types, uzeacadtable_styles,
  uzeacadtable_cell, uzeacadtable_merge,uzecamera,uzeSnap,
  uzeacadtable_layout, uzeacadtable_stylemanager,
  uzeacadtable_dxf_read, uzeacadtable_dxf_write;

const
  CAcadTableBreakHeightGripVertexBase = 100000;

type
  // Тип указателя на GDBObjAcadTable
  PGDBObjAcadTable = ^GDBObjAcadTable;

  // Данные одной части (фрагмента) разделённой по ширине таблицы.
  // AutoCAD сохраняет разбитую таблицу в DXF как несколько ACAD_TABLE
  // сущностей (ROUNDTRIP_2008). Первая часть — это сам объект, остальные
  // хранятся здесь как данные и отображаются в одном составном объекте
  // со смещением относительно точки вставки главной части (issue #1300).
  TAcadTablePart = record
    InsertPoint: TzePoint3d;
    // Логический индекс первой строки части в полной таблице. Нужен для
    // базового стиля Title/Header/Data, когда строки смещаются при удалении
    // повторяющихся верхних меток (issue #1311).
    RowBaseIndex: Integer;
    RowCount: Integer;
    ColCount: Integer;
    RowHeights: TZctnrVectorDouble;
    ColWidths: TZctnrVectorDouble;
    CellTexts: TTableTextArray;
    TableFlags: Integer;
    TableStyle: TTableStyle;
    // Хэндл DXF-стиля таблицы этой части (group code 342). Нужен, чтобы
    // применить тот же табличный стиль, что и к главной части: иначе у
    // продолжений остаётся стиль по умолчанию и текст рендерится высотой
    // CAcadTableDefaultTextHeight вместо высоты из DXF (issue #1300).
    TableStyleHandle: String;
    Rows: TTableRowArray;
    Cols: TTableColumnArray;
    Cells: TTableCellArray;
    Merges: TMergeRangeArray;
    BreakEnabled: Boolean;
    BreakDirection: TAcadTableBreakDirection;
    BreakRepeatTopLabels: Boolean;
    BreakRepeatBottomLabels: Boolean;
    BreakManualPosition: Boolean;
    BreakManualHeight: Boolean;
    BreakSpacing: Double;
    BreakHeight: Double;
    RawDXFEntity: String;
    RawDXFEntityValid: Boolean;
    // Имя сгенерированного перед сохранением анонимного блока с геометрией
    // этой части (issue #1381). Используется модельным путём записи DXF для
    // group code 2/343, чтобы AutoCAD отрисовал часть как отдельную таблицу.
    BlockName: String;
  end;

  TAcadTableBreakHeightArray = array of Double;
  TAcadTableBreakPositionArray = array of TzePoint3d;

  // Сущность ACAD_TABLE — таблица AutoCAD из формата DXF.
  // Хранит геометрию таблицы и текстовое содержимое ячеек.
  // При форматировании строит визуальное представление из линий и текста.
  GDBObjAcadTable = object(GDBObjComplex)
  private
    // Точка вставки таблицы
    FInsertPoint: TzePoint3d;
    // Количество строк
    FRowCount: Integer;
    // Количество столбцов
    FColCount: Integer;
    // Высоты строк (один элемент на строку)
    FRowHeights: TZctnrVectorDouble;
    // Ширины столбцов (один элемент на столбец)
    FColWidths: TZctnrVectorDouble;
    // Тексты ячеек: индекс = строка * FColCount + столбец
    FCellTexts: TTableTextArray;
    // Признак того, что геометрия уже была построена
    FGeometryBuilt: Boolean;
    // Если True — при рендеринге все строки используют базовый стиль Data
    // (issue #1357). В этой задаче программно созданная из редактора
    // электронных таблиц таблица состоит только из ячеек данных; отдельное
    // отображение строк Title/Header добавят будущие задачи.
    FForceDataStyleAllRows: Boolean;
    // Явно заданные типы строк (issue #1368). Один элемент на строку,
    // значение — индекс базового стиля строки (0=Title, 1=Header, 2=Data),
    // либо -1, если тип строки для этой строки не задан. Заполняется через
    // SetRowStyleTypes при экспорте из редактора электронных таблиц на
    // основании цвета заливки ячеек: строка получает тип Title/Header только
    // тогда, когда ВСЕ её ячейки относятся к соответствующему типу. Имеет
    // приоритет над FForceDataStyleAllRows и позиционным выбором стиля.
    FRowStyleTypes: array of Integer;
    // True, если типы строк заданы явно снаружи (SetRowStyleTypes из данных
    // DXF AcDbTableContent или из редактора электронных таблиц). Для старых
    // разорванных таблиц без объекта содержимого типы строк отсутствуют и
    // восстанавливаются по числу повторяющихся ведущих строк-меток частей
    // (issue #1373). Инференс не считается явной установкой, поэтому при
    // повторном определении он пересчитывается заново.
    FRowStyleTypesExplicit: Boolean;
    // Хэндл DXF-стиля таблицы (group code 342)
    FTableStyleHandle: String;
    // Флаги свойств таблицы (group code 90)
    FTableFlags: Integer;
    // Параметры разрыва таблицы
    FBreakEnabled: Boolean;
    FBreakDirection: TAcadTableBreakDirection;
    FBreakRepeatTopLabels: Boolean;
    FBreakRepeatBottomLabels: Boolean;
    FBreakManualPosition: Boolean;
    FBreakManualPositionExplicit: Boolean;
    FBreakManualHeight: Boolean;
    // True, когда признаки ручного позиционирования/высоты заданы явно
    // из roundtrip-данных DXF (BreakOption, issue #1339). В этом случае
    // эвристики DetectBreakManualPosition/DetectBreakManualHeight не должны
    // переопределять значения, полученные из файла.
    FBreakFlagsKnown: Boolean;
    FBreakSpacing: Double;
    // Высота разбиения (break height): после какой суммарной высоты строк
    // строки начинают переноситься в следующую часть разделённой таблицы.
    // Читается из XRECORD ACAD_ROUNDTRIP_2008_TABLE_ENTITY (issue #1307).
    FBreakHeight: Double;
    // Масштаб и поворот объекта (как у вставки блока). Хранятся отдельно,
    // потому что базовый CalcObjMatrixWithoutOwner восстанавливает ось OX
    // из OZ и теряет поворот в плоскости и масштаб. Благодаря этим полям
    // перенос, поворот и масштабирование таблицы отображаются корректно
    // (issue #1305).
    FScale: TzeVector3d;
    FRotate: Double;
    // Стиль таблицы
    FTableStyle: TTableStyle;
    // Строки, столбцы, ячейки и объединения
    FRows: TTableRowArray;
    FCols: TTableColumnArray;
    FCells: TTableCellArray;
    FMerges: TMergeRangeArray;
    // Части (фрагменты) разделённой по ширине таблицы, объединённые
    // в один объект AcadTable. Главная часть — это сам объект, а
    // продолжения хранятся здесь как данные и отображаются со смещением
    // относительно точки вставки главной части (issue #1300).
    FContinuationParts: array of TAcadTablePart;
    // Исходный текст DXF-entity для точного round-trip сохранения
    // неизменённых ACAD_TABLE (issue #1317).
    FRawDXFEntity: String;
    FRawDXFEntityValid: Boolean;
    // Имя сгенерированного перед сохранением анонимного блока с геометрией
    // главной части (issue #1381). См. EnsureSplitPartBlocks.
    FMainPartBlockName: String;

    // Обёртки для делегирования к модулю layout
    function GetRowHeightLocal(RowIndex: Integer): Double;
    function GetColWidthLocal(ColIndex: Integer): Double;
    function GetTotalHeight: Double;
    function GetTotalWidth: Double;
    function GetCellTextLocal(
      RowIdx, ColIdx: Integer): String;
    // Строит визуальное представление текущих полей таблицы в целевом
    // массиве ATarget со смещением (ABaseX, ABaseY) в OCS. AOwner —
    // владелец создаваемых подпримитивов (Self при рендеринге в
    // ConstObjArray, либо определение блока при генерации персональных
    // блоков частей, issue #1381). ARowBaseIndex задаёт логический индекс
    // первой строки для выбора базового стиля строки.
    procedure RenderCurrentTable(
      var ADrawing: TDrawingDef; var ADC: TDrawContext;
      var ATarget: GDBObjEntityTreeArray; AOwner: Pointer;
      ABaseX, ABaseY: Double; ARowBaseIndex: Integer);
    // Строит визуальное представление всей таблицы (главная часть и
    // все продолжения) в ConstObjArray
    procedure BuildVisualRepresentation(
      var ADrawing: TDrawingDef; var ADC: TDrawContext);
    // Обмен данными рендеринга между Self и частью-продолжением
    procedure SwapTableData(var APart: TAcadTablePart);
    // Глубокое копирование данных другой таблицы в часть-продолжение
    procedure CaptureTableDataToPart(
      var ASource: GDBObjAcadTable; var APart: TAcadTablePart);
    // Освобождение ресурсов части-продолжения
    procedure ClearPart(var APart: TAcadTablePart);
    // Глубокое копирование части-продолжения (для Clone)
    procedure CopyTablePart(
      const ASource: TAcadTablePart; var ADest: TAcadTablePart);
    // Вычисляет bounding box таблицы
    procedure getoutbound(var DC: TDrawContext);
    // Раскладывает objmatrix на точку вставки, базис и масштаб (issue #1305)
    procedure decomposite;
    // Поворачивает objmatrix вокруг оси Z на угол r (issue #1305)
    procedure setrot(r: Double);
    // Возвращает имя стиля таблицы
    function GetTableStyleName: String;
    // Возвращает количество частей-продолжений
    function GetContinuationPartCount: Integer;
    // Чтение/запись вычисляемого признака разрыва таблицы (issue #1305)
    function GetBreakEnabled: Boolean;
    procedure SetBreakEnabled(AValue: Boolean);
    // Чтение/запись направления разрыва таблицы (issue #1315)
    procedure SetBreakDirection(AValue: TAcadTableBreakDirection);
    // Чтение/запись ручного положения частей разорванной таблицы
    // (issue #1320).
    procedure SetBreakManualPosition(AValue: Boolean);
    // Чтение/запись ручной высоты разбиения частей таблицы (issue #1321).
    procedure SetBreakManualHeight(AValue: Boolean);
    // Чтение/запись интервала между частями и высоты разбиения (issue #1307)
    function GetBreakSpacing: Double;
    procedure SetBreakSpacing(AValue: Double);
    function GetBreakHeight: Double;
    procedure SetBreakHeight(AValue: Double);
    // Объединяет все части-продолжения в главную часть (строки сверху вниз).
    procedure MergeAllContinuationPartsIntoMain;
    // Пересегментирует объединённую главную часть по высоте разбиения,
    // вынося хвостовые строки в новые части-продолжения (issue #1307).
    procedure SplitMainTableByBreakHeight(AThreshold: Double);
    procedure SplitMainTableByBreakHeights(
      const ABreakHeights: TAcadTableBreakHeightArray);
    function BreakHeightForPart(
      const ABreakHeights: TAcadTableBreakHeightArray;
      APartNumber: Integer): Double;
    procedure CaptureContinuationInsertPoints(
      var APositions: TAcadTableBreakPositionArray);
    procedure RestoreManualContinuationInsertPoints(
      const APositions: TAcadTableBreakPositionArray;
      AWasExplicit: Boolean);
    procedure ResplitByCurrentBreakHeights;
    // Копирует диапазон строк [AStart..AEnd] исходной части в целевую часть.
    procedure SlicePartFromPart(const ASource: TAcadTablePart;
      AStart, AEnd: Integer; var ADest: TAcadTablePart);
    // Пересчитывает точки вставки частей-продолжений из текущего интервала
    // и направления разрыва (issue #1307).
    procedure RepositionContinuationParts;
    procedure SetBreakManualPositionForParts(AValue: Boolean);
    procedure SetBreakManualHeightForParts(AValue: Boolean);
    function HasManualContinuationPositions: Boolean;
    procedure DetectBreakManualPosition;
    procedure DetectBreakManualHeight;
    function ContinuationPartGripPointInWCS(APartIndex: Integer): TzePoint3d;
    function BreakHeightGripPointInWCS(APartNumber: Integer): TzePoint3d;
    function BreakHeightFromGripLocalOffset(
      APartNumber: Integer; const ALocalOffset: TzePoint3d): Double;
    function DecodeBreakHeightGripVertex(
      AVertexNum: Integer; out APartNumber: Integer): Boolean;
    // Число ведущих строк-меток (Title/Header) до первой строки Data.
    // При наличии явных типов строк они определяют зону повтора; иначе
    // используется legacy-позиция Title+Header (issue #1309, #1375).
    function ComputeTopLabelRowCount: Integer;
    // Фактическое число ведущих строк-меток, одинаково повторяющихся во всех
    // частях-продолжениях; 0, если повтора нет (по содержимому, issue #1309).
    function EffectiveRepeatTopRowCount: Integer;
    // Максимальное число ведущих строк, одинаково повторяющихся в начале
    // КАЖДОЙ части-продолжения, БЕЗ ограничения зоной меток
    // ComputeTopLabelRowCount. Служит для восстановления типов строк старых
    // разорванных таблиц (issue #1373): 0, если частей нет или повтора нет.
    function DetectRepeatedTopRowCountRaw: Integer;
    // Проверяет, повторяет ли часть-продолжение первые L строк-меток главной
    // части (тексты ячеек первых L строк совпадают) (issue #1309).
    function PartRepeatsTopLabels(
      const APart: TAcadTablePart; L: Integer): Boolean;
    // Обновляет логические индексы первых строк продолжений для выбора
    // базового стиля Title/Header/Data (issue #1311).
    procedure UpdateContinuationRowBaseIndexes;
    // Определяет значение BreakRepeatTopLabels по данным частей-продолжений:
    // если все части повторяют ведущие строки-метки главной части, значит
    // таблица была разорвана с повтором верхних меток (issue #1309).
    procedure DetectBreakRepeatTopLabels;
    // Чтение/запись признака повтора верхних меток (issue #1309).
    function GetBreakRepeatTopLabels: Boolean;
    procedure SetBreakRepeatTopLabels(AValue: Boolean);
    // Удаляет первые L строк-меток из каждой части-продолжения (issue #1309).
    procedure RemoveTopLabelsFromParts(L: Integer);
    // Добавляет первые L строк-меток главной части в начало каждой
    // части-продолжения, если их там ещё нет (issue #1309).
    procedure AddTopLabelsToParts(L: Integer);
    // Вставляет первые L строк-меток главной части в начало одной
    // части-продолжения (issue #1309).
    procedure PrependTopLabelsToPart(L: Integer; var APart: TAcadTablePart);
    // Подготовка данных для специализированного DXF writer.
    procedure FillDXFWritePartFromSelf(var APart: TAcadTableDXFWritePart);
    procedure FillDXFWritePartFromContinuation(
      const ASource: TAcadTablePart; var APart: TAcadTableDXFWritePart);
    procedure BuildDXFContinuationWriteParts(
      var AParts: TAcadTableDXFWritePartArray);
    procedure InvalidateRawDXFEntity;
    function CanSaveRawDXFEntity: Boolean;

  public
    constructor initnul(
      AOwner: PGDBObjGenericWithSubordinated);
    destructor done; virtual;

    // Генерирует перед сохранением DXF персональный анонимный блок с
    // геометрией каждой части таблицы (главной и продолжений) и запоминает
    // их имена в FMainPartBlockName / FContinuationParts[].BlockName. Нужно
    // модельному пути записи, чтобы AutoCAD отрисовал каждую часть
    // разорванной таблицы как самостоятельную таблицу (issue #1381).
    // Публичный, чтобы before-save обработчик и тесты могли его вызвать.
    procedure EnsureSplitPartBlocks(var ADrawing: TDrawingDef);

    procedure LoadFromDXF(var ARdr: TZMemReader;
      APtu: PExtensionData;
      var ADrawing: TDrawingDef;
      var AContext: TIODXFLoadContext); virtual;
    procedure SaveToDXF(var AOutStream: TZctnrVectorBytes;
      var ADrawing: TDrawingDef;
      var AIODXFContext: TIODXFSaveContext); virtual;
    procedure SaveToDXFFollow(
      var AOutStream: TZctnrVectorBytes;
      var ADrawing: TDrawingDef;
      var AIODXFContext: TIODXFSaveContext); virtual;
    procedure DXFOut(var AOutStream: TZctnrVectorBytes;
      var ADrawing: TDrawingDef;
      var AIODXFContext: TIODXFSaveContext); virtual;
    procedure BuildGeometry(
      var ADrawing: TDrawingDef); virtual;
    procedure FormatEntity(var ADrawing: TDrawingDef;
      var ADC: TDrawContext;
      AStage: TEFStages = EFAllStages); virtual;
    function IsStagedFormatEntity: Boolean; virtual;
    // Трансформация объекта: масштаб и поворот учитываются как у вставки
    // блока, чтобы перенос/поворот/масштабирование перестраивали таблицу
    // (issue #1305, часть 1).
    procedure CalcObjMatrix(pdrawing: PTDrawingDef = nil); virtual;
    procedure ReCalcFromObjMatrix; virtual;
    procedure rtsave(refp: Pointer); virtual;
    procedure TransformAt(p: PGDBObjEntity;
      t_matrix: PzeTypedMatrix4d); virtual;
    procedure addcontrolpoints(tdesc: Pointer); virtual;
    procedure remaponecontrolpoint(pdesc: pcontrolpointdesc;
      ProjectProc: GDBProjectProc); virtual;
    procedure rtmodifyonepoint(const rtmod: TRTModifyData); virtual;
    function Clone(AOwn: Pointer): PGDBObjEntity; virtual;
    function GetObjType: TObjID; virtual;
    function GetObjTypeName: String; virtual;
    function DXFDelayedBuildGeometry: Boolean; virtual;
    // Поглощает продолжение разделённой таблицы как ещё одну часть
    // этого же объекта AcadTable (issue #1300). Возвращает True, если
    // AOther поглощён и вызывающая сторона может его освободить.
    function TryMergeContinuation(
      AOther: PGDBObjEntity): Boolean; virtual;
    // Сохраняет параметры разбиения (интервал и высоту), прочитанные
    // загрузчиком DXF из XRECORD ACAD_ROUNDTRIP_2008_TABLE_ENTITY
    // (issue #1307). Возвращает True.
    function SetTableBreakData(
      ASpacing, ABreakHeight: Double): Boolean; virtual;
    // Задаёт явные признаки ручного позиционирования/высоты разбиения,
    // прочитанные из BreakOption (XRECORD ACAD_ROUNDTRIP_2008_TABLE_ENTITY,
    // issue #1339). Имеют приоритет над эвристиками Detect*.
    procedure SetBreakOptionFlags(
      AManualPosition, AManualHeight: Boolean); virtual;
    function SetTableStyleName(
      const AValue: String; var ADrawing: TDrawingDef): Boolean; virtual;
    procedure SetDXFRawEntityText(const ARawText: string); virtual;
    // Программно строит таблицу из текстов ячеек редактора электронных
    // таблиц (issue #1357). Все ячейки имеют тип cdtText и оформляются
    // стилем Data, объединения отсутствуют. ACellTexts индексируется как
    // строка*AColCount + столбец; недостающие тексты считаются пустыми.
    // Высоты строк и ширины столбцов берутся по умолчанию. Возвращает
    // True при успешном построении непустой таблицы.
    function BuildFromCellTexts(
      ARowCount, AColCount: Integer;
      const ACellTexts: TTableTextArray;
      const AInsertPoint: TzePoint3d): Boolean; virtual;
    // Расширенный вариант построения таблицы из текстов ячеек с явным
    // указанием ширин столбцов и высот строк в единицах чертежа (issue
    // #1359). AColWidths индексируется по столбцам, ARowHeights — по
    // строкам. Если размер отсутствует или не положителен, берётся
    // значение по умолчанию. BuildFromCellTexts делегирует сюда с
    // пустыми массивами размеров. Возвращает True при успехе.
    function BuildFromCellTextsWithSizes(
      ARowCount, AColCount: Integer;
      const ACellTexts: TTableTextArray;
      const AColWidths, ARowHeights: TTableSizeArray;
      const AInsertPoint: TzePoint3d): Boolean; virtual;
    // Полный вариант программного построения таблицы из текстов ячеек
    // с явными ширинами столбцов, высотами строк и выравниванием ячеек
    // (issue #1363). ACellAlignments индексируется как строка*AColCount +
    // столбец и содержит код выравнивания AutoCAD (group 170, 1..9); 0 или
    // отсутствующий элемент означает наследование выравнивания от стиля
    // таблицы. BuildFromCellTexts и BuildFromCellTextsWithSizes делегируют
    // сюда (с пустыми массивами размеров/выравниваний). Возвращает True при
    // успехе.
    function BuildFromCellTextsWithSizesAndAlignments(
      ARowCount, AColCount: Integer;
      const ACellTexts: TTableTextArray;
      const AColWidths, ARowHeights: TTableSizeArray;
      const ACellAlignments: TTableAlignmentArray;
      const AInsertPoint: TzePoint3d): Boolean; virtual;
    // Обновляет содержимое существующей таблицы, сохраняя её стиль,
    // трансформацию и свойства объекта. В отличие от BuildFrom* предназначен
    // для сохранения правок из uzvspreadsheet обратно в выбранную таблицу.
    function UpdateFromCellTextsWithSizesAndAlignments(
      ARowCount, AColCount: Integer;
      const ACellTexts: TTableTextArray;
      const AColWidths, ARowHeights: TTableSizeArray;
      const ACellAlignments: TTableAlignmentArray): Boolean; virtual;

    // Задаёт явные типы строк (issue #1368). ATypes индексируется по номеру
    // строки; значение — индекс базового стиля строки (0=Title, 1=Header,
    // 2=Data); значение < 0 означает «тип не задан» (используется
    // позиционный/принудительный выбор стиля). Вызывается при экспорте из
    // редактора электронных таблиц после построения геометрии, чтобы строки,
    // целиком состоящие из ячеек Title/Header, получили соответствующий стиль.
    procedure SetRowStyleTypes(const ATypes: array of Integer); virtual;
    procedure SetCellStyleTypes(const ATypes: array of Integer); virtual;
    // Возвращает эффективный индекс базового стиля для строки ARow
    // (0=Title, 1=Header, 2=Data). При отсутствии явных типов использует
    // ту же принудительную/позиционную логику, что и рендеринг таблицы;
    // для строки вне диапазона возвращает -1 (issue #1368/#1402).
    function RowStyleTypeAt(ARow: Integer): Integer;
    function CellStyleTypeAt(ARow, ACol: Integer): Integer;
    // Возвращает текст ячейки главной части таблицы. Для некорректных
    // индексов возвращает пустую строку (issue #1402).
    function CellTextAt(ARow, ACol: Integer): String;
    // Возвращает фактическую высоту строки / ширину столбца. Для
    // некорректного индекса возвращает 0 (issue #1402).
    function RowHeightAt(ARow: Integer): Double;
    function ColWidthAt(ACol: Integer): Double;

    // Публичные свойства для инспектора объектов
    property InsertPoint: TzePoint3d read FInsertPoint;
    property RowCount: Integer read FRowCount;
    property ColCount: Integer read FColCount;
    property Width: Double read GetTotalWidth;
    property Height: Double read GetTotalHeight;
    property TableStyleName: String read GetTableStyleName;
    // Если True — все строки таблицы отображаются стилем Data (issue #1357).
    property ForceDataStyleForAllRows: Boolean
      read FForceDataStyleAllRows write FForceDataStyleAllRows;
    // Признак разрыва вычисляется: учитывает как DXF-флаг, так и
    // поглощённые части-продолжения (issue #1305, часть 2a). Запись
    // в False объединяет разорванную таблицу сверху вниз (часть 2b).
    property BreakEnabled: Boolean read GetBreakEnabled write SetBreakEnabled;
    property BreakDirection: TAcadTableBreakDirection
      read FBreakDirection write SetBreakDirection;
    // Повтор верхних меток в каждой части разорванной таблицы (issue #1309).
    // Чтение возвращает определённое при загрузке значение; запись в False
    // удаляет повторяющиеся строки-метки из всех частей-продолжений, запись
    // в True — добавляет их обратно.
    property BreakRepeatTopLabels: Boolean
      read GetBreakRepeatTopLabels write SetBreakRepeatTopLabels;
    property BreakRepeatBottomLabels: Boolean
      read FBreakRepeatBottomLabels;
    property BreakManualPosition: Boolean
      read FBreakManualPosition write SetBreakManualPosition;
    property BreakManualHeight: Boolean
      read FBreakManualHeight write SetBreakManualHeight;
    // Интервал между частями разделённой таблицы (issue #1307). Чтение
    // возвращает значение из XRECORD; запись перестраивает расстояние
    // между всеми частями на чертеже.
    property BreakSpacing: Double
      read GetBreakSpacing write SetBreakSpacing;
    // Высота разбиения (issue #1307). Чтение возвращает значение из
    // XRECORD; запись пересчитывает число строк в каждой части
    // (автоматически определяя необходимое число частей).
    property BreakHeight: Double
      read GetBreakHeight write SetBreakHeight;
    // Количество поглощённых частей-продолжений (issue #1300).
    // Для неразделённой таблицы равно 0.
    property ContinuationPartCount: Integer read GetContinuationPartCount;
    // Число строк в части-продолжении по индексу (для инспекции/тестов,
    // issue #1309). Для некорректного индекса возвращает -1.
    function ContinuationPartRowCount(AIndex: Integer): Integer;
    // Текст ячейки части-продолжения (для тестов, issue #1309). Для
    // некорректных индексов возвращает пустую строку.
    function ContinuationPartCellText(
      APartIndex, ARow, ACol: Integer): string;
    // Фактический код выравнивания AutoCAD (1..9) ячейки главной части.
    // Явное значение group 170 имеет приоритет; при его отсутствии
    // возвращается выравнивание, разрешённое из стиля строки/ячейки.
    // Для некорректных индексов возвращает 0 (issue #1363, #1402).
    function CellAlignmentAt(ARow, ACol: Integer): Integer;
    // Повторно определяет признак повтора верхних меток по текущим данным
    // частей-продолжений и возвращает результат (issue #1309). Используется
    // для проверки детекции после программных изменений модели.
    function RecomputeBreakRepeatTopLabels: Boolean;
  end;

function AllocAcadTable: Pointer;
function AllocAndInitAcadTable(
  AOwner: PGDBObjGenericWithSubordinated): PGDBObjAcadTable;

implementation

uses
  // Модули, нужные только для генерации персональных блоков частей таблицы
  // перед сохранением DXF (issue #1381): реестр before-save callback'ов,
  // тип TSimpleDrawing, массив и определение блоков.
  uzedrawingsimple, uzeffdxfout, UGDBObjBlockdefArray, uzeblockdef;

const
  CAcadTableBreakPositionTolerance = 1e-6;
  CAcadTableBreakHeightTolerance = 1e-6;

function SameAcadTableBreakPoint(
  const ALeft, ARight: TzePoint3d): Boolean;
begin
  Result :=
    (Abs(ALeft.x - ARight.x) <= CAcadTableBreakPositionTolerance) and
    (Abs(ALeft.y - ARight.y) <= CAcadTableBreakPositionTolerance) and
    (Abs(ALeft.z - ARight.z) <= CAcadTableBreakPositionTolerance);
end;

function SameAcadTableBreakHeight(
  ALeft, ARight: Double): Boolean;
begin
  Result := Abs(ALeft - ARight) <= CAcadTableBreakHeightTolerance;
end;

function AcadTableBreakHeightHasValue(AValue: Double): Boolean;
begin
  Result := AValue >= CAcadTableBreakHeightTolerance;
end;

// --- Обёртки для делегирования ---

function GDBObjAcadTable.GetRowHeightLocal(
  RowIndex: Integer): Double;
begin
  Result := uzeacadtable_layout.GetRowHeight(
    RowIndex, FRowHeights);
end;

function GDBObjAcadTable.GetColWidthLocal(
  ColIndex: Integer): Double;
begin
  Result := uzeacadtable_layout.GetColWidth(
    ColIndex, FColWidths);
end;

function GDBObjAcadTable.GetTotalHeight: Double;
begin
  Result := uzeacadtable_layout.GetTotalHeight(
    FRowCount, FRowHeights);
end;

function GDBObjAcadTable.GetTotalWidth: Double;
begin
  Result := uzeacadtable_layout.GetTotalWidth(
    FColCount, FColWidths);
end;

function GDBObjAcadTable.GetTableStyleName: String;
begin
  Result := FTableStyle.Name;
end;

function GDBObjAcadTable.SetTableStyleName(
  const AValue: String; var ADrawing: TDrawingDef): Boolean;
var
  NewHandle: String;
  PartIdx: Integer;
  StyleName: String;
begin
  Result := False;
  StyleName := Trim(AValue);
  if StyleName = '' then
  begin
    programlog.LogOutStr(
      'AcadTable: model: SetTableStyleName — empty style name',
      LM_Info);
    Exit;
  end;

  if SameText(FTableStyle.Name, StyleName) then
  begin
    Result := True;
    Exit;
  end;

  if not uzeacadtable_stylemanager.ApplyDXFTableStyleByName(
    FTableStyle, StyleName, ADrawing, NewHandle) then
    Exit;

  FTableStyleHandle := NewHandle;
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    FContinuationParts[PartIdx].TableStyle := FTableStyle;
    FContinuationParts[PartIdx].TableStyleHandle := NewHandle;
  end;
  FGeometryBuilt := False;
  InvalidateRawDXFEntity;

  programlog.LogOutFormatStr(
    'AcadTable: model: SetTableStyleName="%s" handle=%s',
    [FTableStyle.Name, FTableStyleHandle], LM_Info);
  Result := True;
end;

function GDBObjAcadTable.BuildFromCellTexts(
  ARowCount, AColCount: Integer;
  const ACellTexts: TTableTextArray;
  const AInsertPoint: TzePoint3d): Boolean;
var
  EmptySizes: TTableSizeArray;
begin
  // Делегируем расширенному варианту без явных размеров — все ширины
  // столбцов и высоты строк будут взяты по умолчанию.
  System.SetLength(EmptySizes, 0);
  Result := BuildFromCellTextsWithSizes(ARowCount, AColCount, ACellTexts,
    EmptySizes, EmptySizes, AInsertPoint);
end;

{ Возвращает размер из массива по индексу, либо значение по умолчанию,
  если индекс вне диапазона или размер не положителен. }
function SizeOrDefault(const ASizes: TTableSizeArray;
  AIndex: Integer; ADefault: Double): Double;
begin
  if (AIndex >= 0) and (AIndex <= High(ASizes)) and (ASizes[AIndex] > 0) then
    Result := ASizes[AIndex]
  else
    Result := ADefault;
end;

function GDBObjAcadTable.BuildFromCellTextsWithSizes(
  ARowCount, AColCount: Integer;
  const ACellTexts: TTableTextArray;
  const AColWidths, ARowHeights: TTableSizeArray;
  const AInsertPoint: TzePoint3d): Boolean;
var
  EmptyAlignments: TTableAlignmentArray;
begin
  // Делегируем полному варианту без явного выравнивания — все ячейки
  // наследуют выравнивание от стиля таблицы (issue #1363).
  System.SetLength(EmptyAlignments, 0);
  Result := BuildFromCellTextsWithSizesAndAlignments(ARowCount, AColCount,
    ACellTexts, AColWidths, ARowHeights, EmptyAlignments, AInsertPoint);
end;

{ Возвращает код выравнивания из массива по индексу, либо 0 (наследование от
  стиля таблицы), если индекс вне диапазона. }
function AlignmentOrZero(const AAlignments: TTableAlignmentArray;
  AIndex: Integer): Integer;
begin
  if (AIndex >= 0) and (AIndex <= High(AAlignments)) then
    Result := AAlignments[AIndex]
  else
    Result := 0;
end;

function GDBObjAcadTable.BuildFromCellTextsWithSizesAndAlignments(
  ARowCount, AColCount: Integer;
  const ACellTexts: TTableTextArray;
  const AColWidths, ARowHeights: TTableSizeArray;
  const ACellAlignments: TTableAlignmentArray;
  const AInsertPoint: TzePoint3d): Boolean;
var
  RowIdx, ColIdx, CellIndex: Integer;
begin
  Result := False;

  programlog.LogOutFormatStr(
    'AcadTable: model: BuildFromCellTexts START rows=%d cols=%d texts=%d',
    [ARowCount, AColCount, Length(ACellTexts)], LM_Info);

  if (ARowCount <= 0) or (AColCount <= 0) then
  begin
    programlog.LogOutStr(
      'AcadTable: model: BuildFromCellTexts — пустые размеры таблицы',
      LM_Info);
    Exit;
  end;

  if ARowCount > CAcadTableMaxDimension then
    ARowCount := CAcadTableMaxDimension;
  if AColCount > CAcadTableMaxDimension then
    AColCount := CAcadTableMaxDimension;

  FInsertPoint := AInsertPoint;
  Local.P_insert := FInsertPoint;
  FRowCount := ARowCount;
  FColCount := AColCount;

  // Высоты строк и ширины столбцов: берём из переданных массивов,
  // при отсутствии значения — по умолчанию (issue #1359).
  for RowIdx := 0 to FRowCount - 1 do
    FRowHeights.PushBackData(
      SizeOrDefault(ARowHeights, RowIdx, CAcadTableDefaultRowHeight));
  for ColIdx := 0 to FColCount - 1 do
    FColWidths.PushBackData(
      SizeOrDefault(AColWidths, ColIdx, CAcadTableDefaultColWidth));

  // Тексты ячеек: индекс = строка*FColCount + столбец
  System.SetLength(FCellTexts, FRowCount * FColCount);
  for CellIndex := 0 to High(FCellTexts) do
    if CellIndex <= High(ACellTexts) then
      FCellTexts[CellIndex] := ACellTexts[CellIndex]
    else
      FCellTexts[CellIndex] := '';

  // Инициализируем табличный стиль значениями по умолчанию (стиль Standard
  // присваивается командой через SetTableStyleName)
  InitTableStyle(FTableStyle);

  // Все строки по умолчанию отображаются как данные. Конкретные типы строк
  // (Title/Header) задаются отдельно через SetRowStyleTypes при экспорте из
  // редактора электронных таблиц (issue #1368). Сбрасываем ранее заданные
  // типы строк, т.к. геометрия перестраивается с нуля.
  FForceDataStyleAllRows := True;
  System.SetLength(FRowStyleTypes, 0);
  FRowStyleTypesExplicit := False;

  // Инициализируем строки, столбцы и ячейки (все ячейки — текстовые)
  System.SetLength(FRows, FRowCount);
  for RowIdx := 0 to FRowCount - 1 do
  begin
    FRows[RowIdx].Height := GetRowHeightLocal(RowIdx);
    InitCellStyle(FRows[RowIdx].Style);
  end;

  System.SetLength(FCols, FColCount);
  for ColIdx := 0 to FColCount - 1 do
  begin
    FCols[ColIdx].Width := GetColWidthLocal(ColIdx);
    InitCellStyle(FCols[ColIdx].Style);
  end;

  System.SetLength(FCells, FRowCount, FColCount);
  for RowIdx := 0 to FRowCount - 1 do
    for ColIdx := 0 to FColCount - 1 do
    begin
      FCells[RowIdx][ColIdx].DataType := cdtText;
      FCells[RowIdx][ColIdx].Text := GetCellTextLocal(RowIdx, ColIdx);
      FCells[RowIdx][ColIdx].Value := 0;
      FCells[RowIdx][ColIdx].Formula := '';
      // Выравнивание ячейки из электронной таблицы (issue #1363): код
      // AutoCAD 1..9, либо 0 — наследование от стиля таблицы.
      FCells[RowIdx][ColIdx].CellAlignment :=
        AlignmentOrZero(ACellAlignments, RowIdx * FColCount + ColIdx);
      FCells[RowIdx][ColIdx].ColSpan := 1;
      FCells[RowIdx][ColIdx].RowSpan := 1;
      InitCellStyle(FCells[RowIdx][ColIdx].Style);
      FCells[RowIdx][ColIdx].StyleType := -1;
    end;

  // Программно созданная таблица не содержит объединений ячеек
  System.SetLength(FMerges, 0);

  FGeometryBuilt := False;
  InvalidateRawDXFEntity;

  programlog.LogOutFormatStr(
    'AcadTable: model: BuildFromCellTexts END rows=%d cols=%d cells=%d',
    [FRowCount, FColCount, Length(FCellTexts)], LM_Info);

  Result := True;
end;

function GDBObjAcadTable.UpdateFromCellTextsWithSizesAndAlignments(
  ARowCount, AColCount: Integer;
  const ACellTexts: TTableTextArray;
  const AColWidths, ARowHeights: TTableSizeArray;
  const ACellAlignments: TTableAlignmentArray): Boolean;
var
  SavedTableStyle: TTableStyle;
  SavedTableStyleHandle: String;
  SavedLocal: GDBObj2dprop;
  SavedObjMatrix: TzeTypedMatrix4d;
  SavedInsertInWCS: TzePoint3d;
  SavedScale: TzeVector3d;
  SavedRotate: Double;
begin
  SavedTableStyle := FTableStyle;
  SavedTableStyleHandle := FTableStyleHandle;
  SavedLocal := Local;
  SavedObjMatrix := objMatrix;
  SavedInsertInWCS := P_insert_in_WCS;
  SavedScale := FScale;
  SavedRotate := FRotate;

  // BuildFrom* is also used for newly allocated objects and therefore only
  // appends dimensions to the initialized vectors. For an existing table,
  // discard the old values before rebuilding so getters and geometry use the
  // dimensions saved by the spreadsheet (issue #1402).
  FRowHeights.Clear;
  FColWidths.Clear;

  Result := BuildFromCellTextsWithSizesAndAlignments(
    ARowCount, AColCount, ACellTexts, AColWidths, ARowHeights,
    ACellAlignments, FInsertPoint);
  if not Result then
    Exit;

  FTableStyle := SavedTableStyle;
  FTableStyleHandle := SavedTableStyleHandle;
  Local := SavedLocal;
  objMatrix := SavedObjMatrix;
  P_insert_in_WCS := SavedInsertInWCS;
  FScale := SavedScale;
  FRotate := SavedRotate;
end;

function GDBObjAcadTable.GetCellTextLocal(
  RowIdx, ColIdx: Integer): String;
var
  CellIndex: Integer;
begin
  Result := '';
  if FColCount <= 0 then Exit;
  CellIndex := RowIdx * FColCount + ColIdx;
  if (CellIndex >= 0) and
     (CellIndex <= High(FCellTexts)) then
    Result := FCellTexts[CellIndex];
end;

procedure GDBObjAcadTable.SetRowStyleTypes(const ATypes: array of Integer);
var
  Idx: Integer;
begin
  System.SetLength(FRowStyleTypes, Length(ATypes));
  for Idx := 0 to High(ATypes) do
    FRowStyleTypes[Idx] := ATypes[Idx];
  // Явная установка снаружи (issue #1373): отключает автоматическое
  // восстановление типов строк по повторяющимся меткам частей.
  FRowStyleTypesExplicit := True;
end;

procedure GDBObjAcadTable.SetCellStyleTypes(const ATypes: array of Integer);
var
  RowIdx, ColIdx, CellIdx, StyleType: Integer;
begin
  for RowIdx := 0 to FRowCount - 1 do
    for ColIdx := 0 to FColCount - 1 do
    begin
      CellIdx := RowIdx * FColCount + ColIdx;
      StyleType := -1;
      if CellIdx <= High(ATypes) then
        if (ATypes[CellIdx] >= 0) and (ATypes[CellIdx] <= 2) then
          StyleType := ATypes[CellIdx];
      FCells[RowIdx][ColIdx].StyleType := StyleType;
    end;
  InvalidateRawDXFEntity;
  FGeometryBuilt := False;
end;

function GDBObjAcadTable.RowStyleTypeAt(ARow: Integer): Integer;
begin
  if (ARow < 0) or (ARow >= FRowCount) then
    Result := -1
  else if ARow <= High(FRowStyleTypes) then
    Result := FRowStyleTypes[ARow]
  else if FForceDataStyleAllRows then
    Result := 2
  else
    Result := Min(ARow, 2);
end;

function GDBObjAcadTable.CellStyleTypeAt(ARow, ACol: Integer): Integer;
begin
  if (ARow < 0) or (ARow >= FRowCount) or
     (ACol < 0) or (ACol >= FColCount) then
    Result := -1
  else if (ARow <= High(FCells)) and
          (ACol <= High(FCells[ARow])) and
          (FCells[ARow][ACol].StyleType >= 0) then
    Result := FCells[ARow][ACol].StyleType
  else
    Result := RowStyleTypeAt(ARow);
end;

function GDBObjAcadTable.CellTextAt(ARow, ACol: Integer): String;
begin
  Result := GetCellTextLocal(ARow, ACol);
end;

function GDBObjAcadTable.RowHeightAt(ARow: Integer): Double;
begin
  Result := 0;
  if (ARow < 0) or (ARow >= FRowCount) then
    Exit;
  Result := GetRowHeightLocal(ARow);
end;

function GDBObjAcadTable.ColWidthAt(ACol: Integer): Double;
begin
  Result := 0;
  if (ACol < 0) or (ACol >= FColCount) then
    Exit;
  Result := GetColWidthLocal(ACol);
end;

// --- Конструктор и деструктор ---

constructor GDBObjAcadTable.initnul(
  AOwner: PGDBObjGenericWithSubordinated);
begin
  inherited initnul;
  FInsertPoint := cP3d__0__0__0;
  FRowCount := 0;
  FColCount := 0;
  FRowHeights.initnul;
  FColWidths.initnul;
  System.SetLength(FCellTexts, 0);
  FGeometryBuilt := False;
  FForceDataStyleAllRows := False;
  System.SetLength(FRowStyleTypes, 0);
  FRowStyleTypesExplicit := False;
  FTableStyleHandle := '';
  FTableFlags := 0;
  FBreakEnabled := False;
  FBreakDirection := atbdRight;
  FBreakRepeatTopLabels := False;
  FBreakRepeatBottomLabels := False;
  FBreakManualPosition := False;
  FBreakManualPositionExplicit := False;
  FBreakManualHeight := False;
  FBreakFlagsKnown := False;
  FBreakSpacing := 0;
  FBreakHeight := 0;
  // Трансформация по умолчанию: единичный масштаб, без поворота (issue #1305)
  FScale := cV3d__1__1__1;
  FRotate := 0;
  InitTableStyle(FTableStyle);
  System.SetLength(FRows, 0);
  System.SetLength(FCols, 0);
  System.SetLength(FCells, 0, 0);
  System.SetLength(FMerges, 0);
  System.SetLength(FContinuationParts, 0);
  FRawDXFEntity := '';
  FRawDXFEntityValid := False;
end;

destructor GDBObjAcadTable.done;
var
  PartIdx: Integer;
begin
  FRowHeights.done;
  FColWidths.done;
  System.SetLength(FCellTexts, 0);
  System.SetLength(FRows, 0);
  System.SetLength(FCols, 0);
  System.SetLength(FCells, 0, 0);
  System.SetLength(FMerges, 0);
  FRawDXFEntity := '';
  FRawDXFEntityValid := False;
  for PartIdx := 0 to High(FContinuationParts) do
    ClearPart(FContinuationParts[PartIdx]);
  System.SetLength(FContinuationParts, 0);
  inherited done;
end;

procedure GDBObjAcadTable.InvalidateRawDXFEntity;
var
  PartIdx: Integer;
begin
  FRawDXFEntity := '';
  FRawDXFEntityValid := False;
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    FContinuationParts[PartIdx].RawDXFEntity := '';
    FContinuationParts[PartIdx].RawDXFEntityValid := False;
  end;
end;

function GDBObjAcadTable.CanSaveRawDXFEntity: Boolean;
var
  PartIdx: Integer;
begin
  Result := FRawDXFEntityValid and (FRawDXFEntity <> '');
  if not Result then
    Exit;
  for PartIdx := 0 to High(FContinuationParts) do
    if (not FContinuationParts[PartIdx].RawDXFEntityValid) or
       (FContinuationParts[PartIdx].RawDXFEntity = '') then
      Exit(False);
end;

// --- Загрузка из DXF ---

procedure GDBObjAcadTable.LoadFromDXF(
  var ARdr: TZMemReader; APtu: PExtensionData;
  var ADrawing: TDrawingDef;
  var AContext: TIODXFLoadContext);
var
  GroupCode: Integer;
  SubclassMarker: String;
  DXFData: TAcadTableDXFData;
  RowIdx, ColIdx, CellIndex: Integer;
  MergeRange: TMergeRange;
begin
  programlog.LogOutStr(
    'AcadTable: model: LoadFromDXF START', LM_Info);

  GroupCode := ARdr.ParseInteger;

  // Обрабатываем общие коды (handle, слой, цвет и т.п.) до первого 100.
  // Важно: handle сущности (группа 5) и владелец (330) идут ДО маркера
  // подкласса AcDbEntity. Если их просто пропускать, PExtAttrib^.dwgHandle
  // остаётся равным 0, и логика отбрасывания продолжений разделённой
  // таблицы (TableContinuationHandles в uzeffdxf) не срабатывает — части
  // одной таблицы загружаются как отдельные ACAD_TABLE (issue #1300).
  while (GroupCode <> 0) and (GroupCode <> 100) do
  begin
    if not LoadFromDXFObjShared(
      ARdr, GroupCode, APtu, ADrawing, AContext) then
      ARdr.SkipString;
    GroupCode := ARdr.ParseInteger;
  end;

  // AcDbEntity
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
    GroupCode := ARdr.ParseInteger;
  end;

  // Пропускаем AcDbEntity до AcDbBlockReference
  while (GroupCode <> 0) and (GroupCode <> 100) do
  begin
    if not LoadFromDXFObjShared(
      ARdr, GroupCode, APtu, ADrawing, AContext) then
      ARdr.SkipString;
    GroupCode := ARdr.ParseInteger;
  end;

  // AcDbBlockReference
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
    GroupCode := ARdr.ParseInteger;
    while (GroupCode <> 0) and (GroupCode <> 100) do
    begin
      case GroupCode of
        10: FInsertPoint.x := ARdr.ParseDouble;
        20: FInsertPoint.y := ARdr.ParseDouble;
        30: FInsertPoint.z := ARdr.ParseDouble;
      else
        if not LoadFromDXFObjShared(
          ARdr, GroupCode, APtu, ADrawing, AContext) then
          ARdr.SkipString;
      end;
      GroupCode := ARdr.ParseInteger;
    end;
  end;

  // AcDbTable — читаем маркер подкласса
  if GroupCode = 100 then
  begin
    SubclassMarker := ARdr.ParseString;
  end;

  // Инициализируем структуру для чтения DXF-данных
  DXFData.InsertPoint := FInsertPoint;
  DXFData.RowCount := 0;
  DXFData.ColCount := 0;
  DXFData.RowHeights.initnul;
  DXFData.ColWidths.initnul;
  DXFData.TableStyleHandle := '';
  DXFData.TableFlags := 0;
  DXFData.BreakEnabled := False;
  DXFData.BreakDirection := atbdRight;
  DXFData.BreakRepeatTopLabels := False;
  DXFData.BreakRepeatBottomLabels := False;
  DXFData.BreakManualPosition := False;
  DXFData.BreakManualHeight := False;
  DXFData.BreakSpacing := 0;

  // Делегируем чтение данных таблицы модулю dxf_read
  ReadAcadTableFromDXF(ARdr, AContext, DXFData);

  // Копируем результат в поля объекта
  FInsertPoint := DXFData.InsertPoint;
  FRowCount := DXFData.RowCount;
  FColCount := DXFData.ColCount;
  FTableStyleHandle := DXFData.TableStyleHandle;
  FTableFlags := DXFData.TableFlags;
  FBreakEnabled := DXFData.BreakEnabled;
  FBreakDirection := DXFData.BreakDirection;
  FBreakRepeatTopLabels := DXFData.BreakRepeatTopLabels;
  FBreakRepeatBottomLabels := DXFData.BreakRepeatBottomLabels;
  FBreakManualPositionExplicit := DXFData.BreakManualPosition;
  FBreakManualPosition := FBreakManualPositionExplicit;
  FBreakManualHeight := DXFData.BreakManualHeight;
  FBreakSpacing := DXFData.BreakSpacing;

  // Копируем высоты и ширины
  for RowIdx := 0 to DXFData.RowHeights.Count - 1 do
    FRowHeights.PushBackData(
      DXFData.RowHeights.getData(RowIdx));
  for ColIdx := 0 to DXFData.ColWidths.Count - 1 do
    FColWidths.PushBackData(
      DXFData.ColWidths.getData(ColIdx));

  // Копируем тексты ячеек
  System.SetLength(FCellTexts, Length(DXFData.CellTexts));
  for CellIndex := 0 to High(DXFData.CellTexts) do
    FCellTexts[CellIndex] := DXFData.CellTexts[CellIndex];

  // Точка вставки для Local
  Local.P_insert := FInsertPoint;

  // Инициализируем табличный стиль
  InitTableStyle(FTableStyle);

  // Инициализируем строки, столбцы и ячейки
  if (FRowCount > 0) and (FColCount > 0) then
  begin
    System.SetLength(FRows, FRowCount);
    for RowIdx := 0 to FRowCount - 1 do
    begin
      FRows[RowIdx].Height := GetRowHeightLocal(RowIdx);
      InitCellStyle(FRows[RowIdx].Style);
    end;

    System.SetLength(FCols, FColCount);
    for ColIdx := 0 to FColCount - 1 do
    begin
      FCols[ColIdx].Width := GetColWidthLocal(ColIdx);
      InitCellStyle(FCols[ColIdx].Style);
    end;

    System.SetLength(FCells, FRowCount, FColCount);
    for RowIdx := 0 to FRowCount - 1 do
      for ColIdx := 0 to FColCount - 1 do
      begin
        FCells[RowIdx][ColIdx].DataType := cdtText;
        FCells[RowIdx][ColIdx].Text :=
          GetCellTextLocal(RowIdx, ColIdx);
        FCells[RowIdx][ColIdx].Value := 0;
        FCells[RowIdx][ColIdx].Formula := '';
        FCells[RowIdx][ColIdx].BlockName := '';
        FCells[RowIdx][ColIdx].CellAlignment := 0;
        FCells[RowIdx][ColIdx].ColSpan := 1;
        FCells[RowIdx][ColIdx].RowSpan := 1;
        InitCellStyle(FCells[RowIdx][ColIdx].Style);
        FCells[RowIdx][ColIdx].StyleType := -1;
        CellIndex := RowIdx * FColCount + ColIdx;
        if CellIndex < Length(DXFData.CellAlignments) then
          FCells[RowIdx][ColIdx].CellAlignment :=
            DXFData.CellAlignments[CellIndex];
        if CellIndex < Length(DXFData.CellColSpans) then
          FCells[RowIdx][ColIdx].ColSpan :=
            DXFData.CellColSpans[CellIndex];
        if CellIndex < Length(DXFData.CellRowSpans) then
          FCells[RowIdx][ColIdx].RowSpan :=
            DXFData.CellRowSpans[CellIndex];
      end;

    // Строим массив объединений FMerges
    System.SetLength(FMerges, 0);
    for RowIdx := 0 to FRowCount - 1 do
      for ColIdx := 0 to FColCount - 1 do
      begin
        CellIndex := RowIdx * FColCount + ColIdx;
        if (CellIndex < Length(DXFData.CellVirtualFlags))
           and DXFData.CellVirtualFlags[CellIndex] then
          Continue;
        if (FCells[RowIdx][ColIdx].ColSpan > 1) or
           (FCells[RowIdx][ColIdx].RowSpan > 1) then
        begin
          MergeRange.Row1 := RowIdx;
          MergeRange.Col1 := ColIdx;
          MergeRange.Row2 :=
            RowIdx + FCells[RowIdx][ColIdx].RowSpan - 1;
          MergeRange.Col2 :=
            ColIdx + FCells[RowIdx][ColIdx].ColSpan - 1;
          if MergeRange.Row2 >= FRowCount then
            MergeRange.Row2 := FRowCount - 1;
          if MergeRange.Col2 >= FColCount then
            MergeRange.Col2 := FColCount - 1;
          System.SetLength(FMerges, Length(FMerges) + 1);
          FMerges[High(FMerges)] := MergeRange;
          programlog.LogOutFormatStr(
            'AcadTable: model: LoadFromDXF Merge[%d] ' +
            '(%d,%d)-(%d,%d)',
            [High(FMerges), MergeRange.Row1,
             MergeRange.Col1, MergeRange.Row2,
             MergeRange.Col2], LM_Info);
        end;
      end;
  end;

  // Освобождаем временные данные
  DXFData.RowHeights.done;
  DXFData.ColWidths.done;

  programlog.LogOutFormatStr(
    'AcadTable: model: LoadFromDXF END rows=%d cols=%d ' +
    'cells=%d merges=%d',
    [FRowCount, FColCount, Length(FCellTexts),
     Length(FMerges)], LM_Info);
end;

// --- Построение визуального представления ---

procedure GDBObjAcadTable.RenderCurrentTable(
  var ADrawing: TDrawingDef; var ADC: TDrawContext;
  var ATarget: GDBObjEntityTreeArray; AOwner: Pointer;
  ABaseX, ABaseY: Double; ARowBaseIndex: Integer);
var
  RowIdx, ColIdx, SegmentIdx, SegmentCount: Integer;
  CurrentY, CurrentX: Double;
  TotalWidth, TotalHeight, SegmentHeight: Double;
  RowH, ColW: Double;
  PLine: PGDBObjLine;
  PMText: PGDBObjMText;
  CellStr: String;
  LineCount, TextCount: Integer;
  CellStyleLocal: TCellStyle;
  TextHeightLocal: Double;
  ResolvedTextStyle: PGDBTextStyle;
  RenderSegments: array[0..255] of TAcadTableRenderSegment;
  SegmentOffsetX, SegmentOffsetY: Double;
  MergeRootPt: TPoint;
  InlineBreakEnabled: Boolean;
  StyleRowIndex: Integer;
begin
  programlog.LogOutFormatStr(
    'AcadTable: model: RenderCurrentTable START ' +
    'rows=%d cols=%d baseX=%g baseY=%g',
    [FRowCount, FColCount, ABaseX, ABaseY], LM_Info);

  if (FRowCount <= 0) or (FColCount <= 0) then
  begin
    programlog.LogOutStr(
      'AcadTable: model: RenderCurrentTable — ' +
      'таблица пуста', LM_Info);
    Exit;
  end;

  TotalWidth := GetTotalWidth;
  TotalHeight := GetTotalHeight;
  LineCount := 0;
  TextCount := 0;

  // Если разбиение уже представлено частями-продолжениями или сохранённой
  // BreakHeight-моделью, не запускаем дополнительное inline-разбиение
  // текущего фрагмента при отрисовке (issue #1313).
  InlineBreakEnabled :=
    FBreakEnabled and (FBreakHeight <= 0) and
    (Length(FContinuationParts) = 0);

  uzeacadtable_layout.BuildRenderSegments(
    FRowCount, FRowHeights, FColWidths, FColCount,
    FTableFlags, InlineBreakEnabled, FBreakDirection,
    FBreakRepeatTopLabels, FBreakManualHeight,
    FBreakSpacing, RenderSegments, SegmentCount);
  if SegmentCount <= 0 then Exit;

  // --- Горизонтальные линии ---
  for SegmentIdx := 0 to SegmentCount - 1 do
  begin
    SegmentOffsetX := RenderSegments[SegmentIdx].OffsetX + ABaseX;
    SegmentOffsetY := RenderSegments[SegmentIdx].OffsetY + ABaseY;
    CurrentY := 0;
    for RowIdx := RenderSegments[SegmentIdx].StartRow to
                  RenderSegments[SegmentIdx].EndRow + 1 do
    begin
      if (RowIdx = RenderSegments[SegmentIdx].StartRow) or
         (RowIdx =
           RenderSegments[SegmentIdx].EndRow + 1) then
      begin
        pointer(PLine) :=
          ATarget.CreateInitObj(GDBLineID, AOwner);
        PLine^.CoordInOCS.lBegin.x := SegmentOffsetX;
        PLine^.CoordInOCS.lBegin.y :=
          SegmentOffsetY - CurrentY;
        PLine^.CoordInOCS.lBegin.z := 0;
        PLine^.CoordInOCS.lEnd.x :=
          SegmentOffsetX + TotalWidth;
        PLine^.CoordInOCS.lEnd.y :=
          SegmentOffsetY - CurrentY;
        PLine^.CoordInOCS.lEnd.z := 0;
        CopyVPto(PLine^);
        PLine^.FormatEntity(ADrawing, ADC);
        Inc(LineCount);
      end
      else
      begin
        CurrentX := 0;
        for ColIdx := 0 to FColCount - 1 do
        begin
          ColW := GetColWidthLocal(ColIdx);
          if uzeacadtable_merge.IsRowBorderVisible(
            RowIdx - 1, ColIdx, FMerges) then
          begin
            pointer(PLine) :=
              ATarget.CreateInitObj(GDBLineID, AOwner);
            PLine^.CoordInOCS.lBegin.x :=
              SegmentOffsetX + CurrentX;
            PLine^.CoordInOCS.lBegin.y :=
              SegmentOffsetY - CurrentY;
            PLine^.CoordInOCS.lBegin.z := 0;
            PLine^.CoordInOCS.lEnd.x :=
              SegmentOffsetX + CurrentX + ColW;
            PLine^.CoordInOCS.lEnd.y :=
              SegmentOffsetY - CurrentY;
            PLine^.CoordInOCS.lEnd.z := 0;
            CopyVPto(PLine^);
            PLine^.FormatEntity(ADrawing, ADC);
            Inc(LineCount);
          end;
          CurrentX := CurrentX + ColW;
        end;
      end;

      if RowIdx <= RenderSegments[SegmentIdx].EndRow then
        CurrentY :=
          CurrentY + GetRowHeightLocal(RowIdx);
    end;
  end;

  // --- Вертикальные линии ---
  LineCount := 0;
  for SegmentIdx := 0 to SegmentCount - 1 do
  begin
    SegmentOffsetX := RenderSegments[SegmentIdx].OffsetX + ABaseX;
    SegmentOffsetY := RenderSegments[SegmentIdx].OffsetY + ABaseY;
    SegmentHeight := 0;
    for RowIdx := RenderSegments[SegmentIdx].StartRow to
                  RenderSegments[SegmentIdx].EndRow do
      SegmentHeight :=
        SegmentHeight + GetRowHeightLocal(RowIdx);

    CurrentX := 0;
    for ColIdx := 0 to FColCount do
    begin
      if (ColIdx = 0) or (ColIdx = FColCount) then
      begin
        pointer(PLine) :=
          ATarget.CreateInitObj(GDBLineID, AOwner);
        PLine^.CoordInOCS.lBegin.x :=
          SegmentOffsetX + CurrentX;
        PLine^.CoordInOCS.lBegin.y := SegmentOffsetY;
        PLine^.CoordInOCS.lBegin.z := 0;
        PLine^.CoordInOCS.lEnd.x :=
          SegmentOffsetX + CurrentX;
        PLine^.CoordInOCS.lEnd.y :=
          SegmentOffsetY - SegmentHeight;
        PLine^.CoordInOCS.lEnd.z := 0;
        CopyVPto(PLine^);
        PLine^.FormatEntity(ADrawing, ADC);
        Inc(LineCount);
      end
      else
      begin
        CurrentY := 0;
        for RowIdx := RenderSegments[SegmentIdx].StartRow
          to RenderSegments[SegmentIdx].EndRow do
        begin
          RowH := GetRowHeightLocal(RowIdx);
          if uzeacadtable_merge.IsColBorderVisible(
            RowIdx, ColIdx - 1, FMerges) then
          begin
            pointer(PLine) :=
              ATarget.CreateInitObj(
                GDBLineID, AOwner);
            PLine^.CoordInOCS.lBegin.x :=
              SegmentOffsetX + CurrentX;
            PLine^.CoordInOCS.lBegin.y :=
              SegmentOffsetY - CurrentY;
            PLine^.CoordInOCS.lBegin.z := 0;
            PLine^.CoordInOCS.lEnd.x :=
              SegmentOffsetX + CurrentX;
            PLine^.CoordInOCS.lEnd.y :=
              SegmentOffsetY - (CurrentY + RowH);
            PLine^.CoordInOCS.lEnd.z := 0;
            CopyVPto(PLine^);
            PLine^.FormatEntity(ADrawing, ADC);
            Inc(LineCount);
          end;
          CurrentY := CurrentY + RowH;
        end;
      end;

      if ColIdx < FColCount then
        CurrentX := CurrentX + GetColWidthLocal(ColIdx);
    end;
  end;

  // --- Текст ячеек ---
  TextCount := 0;
  for SegmentIdx := 0 to SegmentCount - 1 do
  begin
    SegmentOffsetX := RenderSegments[SegmentIdx].OffsetX + ABaseX;
    SegmentOffsetY := RenderSegments[SegmentIdx].OffsetY + ABaseY;
    CurrentY := 0;
    for RowIdx := RenderSegments[SegmentIdx].StartRow to
                  RenderSegments[SegmentIdx].EndRow do
    begin
      RowH := GetRowHeightLocal(RowIdx);
      CurrentX := 0;

      for ColIdx := 0 to FColCount - 1 do
      begin
        ColW := GetColWidthLocal(ColIdx);

        if uzeacadtable_merge.IsCellMerged(
          RowIdx, ColIdx, FMerges) then
        begin
          MergeRootPt := uzeacadtable_merge.GetMergeRoot(
            RowIdx, ColIdx, FMerges);
          if not ((RowIdx = MergeRootPt.Y) and
                  (ColIdx = MergeRootPt.X)) then
          begin
            CurrentX := CurrentX + ColW;
            Continue;
          end;
        end;

        CellStr := '';
        if (Length(FCells) > RowIdx) and
           (Length(FCells[RowIdx]) > ColIdx) then
          CellStr := FCells[RowIdx][ColIdx].Text
        else
          CellStr := GetCellTextLocal(RowIdx, ColIdx);

        ColW := uzeacadtable_merge.GetMergedCellWidth(
          RowIdx, ColIdx, FMerges, GetColWidthLocal);
        RowH := uzeacadtable_merge.GetMergedCellHeight(
          RowIdx, ColIdx, FMerges, GetRowHeightLocal);

        if CellStr <> '' then
        begin
          // По умолчанию базовый стиль строки выбирается по её позиции
          // (0=Title, 1=Header, >=2=Data). Приоритеты:
          // 1) явно заданный или восстановленный тип строки (issue #1368,
          //    #1373) — но только для главной части и повторяющих метки
          //    частей-продолжений (ARowBaseIndex=0). У части-продолжения,
          //    которая НЕ повторяет метки (ARowBaseIndex>0), локальные номера
          //    строк не соответствуют типам строк главной части, поэтому там
          //    используется позиционный стиль от логической базы (issue #1311);
          // 2) FForceDataStyleAllRows (issue #1357) — все строки как данные;
          // 3) позиционный выбор стиля по номеру строки.
          if (ARowBaseIndex = 0) and
             (FCells[RowIdx][ColIdx].StyleType >= 0) then
            StyleRowIndex := FCells[RowIdx][ColIdx].StyleType
          else if RowStyleTypeAt(RowIdx) >= 0 then
            StyleRowIndex := RowStyleTypeAt(RowIdx)
          else if FForceDataStyleAllRows then
            StyleRowIndex := 2
          else
            StyleRowIndex := ARowBaseIndex + RowIdx;
          CellStyleLocal := uzeacadtable_cell.ResolveCellStyleForBaseRow(
            StyleRowIndex, RowIdx, ColIdx, FTableStyle,
            FRows, FCols, FCells,
            FRowCount, FColCount, FTableFlags);

          pointer(PMText) :=
            ATarget.CreateInitObj(GDBMTextID, AOwner);
          PMText^.Template := UTF8ToString(CellStr);
          ResolvedTextStyle :=
            uzeacadtable_stylemanager.ResolveTextStyle(
              CellStyleLocal.TextStyle, ADrawing);
          PMText^.TXTStyle := ResolvedTextStyle;

          // Nonzero STYLE group 40 is a fixed text height and must override
          // the table cell height stored in TABLESTYLE.
          if (ResolvedTextStyle <> nil) and
             (CellStyleLocal.TextStyle <> '') and
             SameText(ResolvedTextStyle^.Name, CellStyleLocal.TextStyle) and
             (ResolvedTextStyle^.prop.size > 0) then
            PMText^.textprop.size :=
              ResolvedTextStyle^.prop.size
          else if CellStyleLocal.TextHeight > 0 then
            PMText^.textprop.size :=
              CellStyleLocal.TextHeight
          else
            PMText^.textprop.size :=
              CAcadTableDefaultTextHeight;

          PMText^.linespacef := 1;
          PMText^.WrapMode := mwmByWordThenChar;
          PMText^.Width := ColW * 0.9;

          // Выравнивание justify
          case CellStyleLocal.VertAlign of
            vaTop:
              case CellStyleLocal.HorzAlign of
                haLeft: PMText^.textprop.justify := jstl;
                haCenter: PMText^.textprop.justify := jstc;
                haRight: PMText^.textprop.justify := jstr;
              else
                PMText^.textprop.justify := jstl;
              end;
            vaMiddle:
              case CellStyleLocal.HorzAlign of
                haLeft: PMText^.textprop.justify := jsml;
                haCenter: PMText^.textprop.justify := jsmc;
                haRight: PMText^.textprop.justify := jsmr;
              else
                PMText^.textprop.justify := jsml;
              end;
            vaBottom:
              case CellStyleLocal.HorzAlign of
                haLeft: PMText^.textprop.justify := jsbl;
                haCenter: PMText^.textprop.justify := jsbc;
                haRight: PMText^.textprop.justify := jsbr;
              else
                PMText^.textprop.justify := jsbl;
              end;
          else
            PMText^.textprop.justify := jstl;
          end;

          TextHeightLocal := PMText^.textprop.size;

          // Позиция X
          case CellStyleLocal.HorzAlign of
            haLeft:
              PMText^.Local.P_insert.x :=
                SegmentOffsetX + CurrentX +
                TextHeightLocal * 0.5;
            haCenter:
              PMText^.Local.P_insert.x :=
                SegmentOffsetX + CurrentX + ColW / 2;
            haRight:
              PMText^.Local.P_insert.x :=
                SegmentOffsetX + CurrentX +
                ColW - TextHeightLocal * 0.5;
          else
            PMText^.Local.P_insert.x :=
              SegmentOffsetX + CurrentX +
              TextHeightLocal * 0.5;
          end;

          // Позиция Y
          case CellStyleLocal.VertAlign of
            vaTop:
              PMText^.Local.P_insert.y :=
                SegmentOffsetY -
                (CurrentY + TextHeightLocal * 0.5);
            vaMiddle:
              PMText^.Local.P_insert.y :=
                SegmentOffsetY -
                (CurrentY + RowH / 2);
            vaBottom:
              PMText^.Local.P_insert.y :=
                SegmentOffsetY -
                (CurrentY + RowH -
                 TextHeightLocal * 0.5);
          else
            PMText^.Local.P_insert.y :=
              SegmentOffsetY -
              (CurrentY + RowH / 2);
          end;

          PMText^.Local.P_insert.z := 0;
          CopyVPto(PMText^);
          PMText^.FormatEntity(ADrawing, ADC);
          Inc(TextCount);
        end;

        CurrentX := CurrentX + GetColWidthLocal(ColIdx);
      end;

      CurrentY := CurrentY + GetRowHeightLocal(RowIdx);
    end;
  end;

  programlog.LogOutFormatStr(
    'AcadTable: model: RenderCurrentTable OK ' +
    'rows=%d cols=%d texts=%d TotalObj=%d',
    [FRowCount, FColCount, TextCount,
     ConstObjArray.Count], LM_Info);
end;

// Обмен данными рендеринга между Self и частью-продолжением.
// TZctnrVectorDouble и динамические массивы обмениваются целиком
// (3-сторонний обмен), что переносит владение буферами без копирования.
procedure GDBObjAcadTable.SwapTableData(var APart: TAcadTablePart);
var
  TmpVec: TZctnrVectorDouble;
  TmpInt: Integer;
  TmpStyle: TTableStyle;
  TmpFlags: Integer;
  TmpBreakEnabled, TmpRepeatTop, TmpRepeatBottom: Boolean;
  TmpManualPos, TmpManualHeight: Boolean;
  TmpDir: TAcadTableBreakDirection;
  TmpSpacing, TmpHeight: Double;
  TmpTexts: TTableTextArray;
  TmpRows: TTableRowArray;
  TmpCols: TTableColumnArray;
  TmpCells: TTableCellArray;
  TmpMerges: TMergeRangeArray;
  TmpRaw: String;
  TmpRawValid: Boolean;
begin
  TmpInt := FRowCount; FRowCount := APart.RowCount; APart.RowCount := TmpInt;
  TmpInt := FColCount; FColCount := APart.ColCount; APart.ColCount := TmpInt;

  TmpVec := FRowHeights; FRowHeights := APart.RowHeights; APart.RowHeights := TmpVec;
  TmpVec := FColWidths;  FColWidths := APart.ColWidths;   APart.ColWidths := TmpVec;

  TmpTexts := FCellTexts; FCellTexts := APart.CellTexts; APart.CellTexts := TmpTexts;

  TmpFlags := FTableFlags; FTableFlags := APart.TableFlags; APart.TableFlags := TmpFlags;
  TmpStyle := FTableStyle; FTableStyle := APart.TableStyle; APart.TableStyle := TmpStyle;

  TmpRows := FRows;   FRows := APart.Rows;     APart.Rows := TmpRows;
  TmpCols := FCols;   FCols := APart.Cols;     APart.Cols := TmpCols;
  TmpCells := FCells; FCells := APart.Cells;   APart.Cells := TmpCells;
  TmpMerges := FMerges; FMerges := APart.Merges; APart.Merges := TmpMerges;

  TmpBreakEnabled := FBreakEnabled; FBreakEnabled := APart.BreakEnabled; APart.BreakEnabled := TmpBreakEnabled;
  TmpDir := FBreakDirection; FBreakDirection := APart.BreakDirection; APart.BreakDirection := TmpDir;
  TmpRepeatTop := FBreakRepeatTopLabels; FBreakRepeatTopLabels := APart.BreakRepeatTopLabels; APart.BreakRepeatTopLabels := TmpRepeatTop;
  TmpRepeatBottom := FBreakRepeatBottomLabels; FBreakRepeatBottomLabels := APart.BreakRepeatBottomLabels; APart.BreakRepeatBottomLabels := TmpRepeatBottom;
  TmpManualPos := FBreakManualPosition; FBreakManualPosition := APart.BreakManualPosition; APart.BreakManualPosition := TmpManualPos;
  TmpManualHeight := FBreakManualHeight; FBreakManualHeight := APart.BreakManualHeight; APart.BreakManualHeight := TmpManualHeight;
  TmpSpacing := FBreakSpacing; FBreakSpacing := APart.BreakSpacing; APart.BreakSpacing := TmpSpacing;
  TmpHeight := FBreakHeight; FBreakHeight := APart.BreakHeight; APart.BreakHeight := TmpHeight;

  TmpRaw := FRawDXFEntity; FRawDXFEntity := APart.RawDXFEntity; APart.RawDXFEntity := TmpRaw;
  TmpRawValid := FRawDXFEntityValid; FRawDXFEntityValid := APart.RawDXFEntityValid; APart.RawDXFEntityValid := TmpRawValid;
end;

// Строит визуальное представление всей таблицы: сначала главная часть
// в нулевом смещении, затем каждое продолжение со смещением, равным
// разнице точек вставки (продолжение минус главная). Все части
// помещаются в ConstObjArray одного объекта AcadTable (issue #1300).
procedure GDBObjAcadTable.BuildVisualRepresentation(
  var ADrawing: TDrawingDef; var ADC: TDrawContext);
var
  PartIdx: Integer;
  BaseX, BaseY: Double;
begin
  programlog.LogOutFormatStr(
    'AcadTable: model: BuildVisualRepresentation START ' +
    'rows=%d cols=%d parts=%d',
    [FRowCount, FColCount, Length(FContinuationParts)], LM_Info);
  ConstObjArray.Free;

  // Главная часть в собственной системе координат
  RenderCurrentTable(ADrawing, ADC, ConstObjArray, @Self, 0, 0, 0);

  // Части-продолжения со смещением относительно главной точки вставки
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    BaseX := FContinuationParts[PartIdx].InsertPoint.x - FInsertPoint.x;
    BaseY := FContinuationParts[PartIdx].InsertPoint.y - FInsertPoint.y;
    SwapTableData(FContinuationParts[PartIdx]);
    // Часть-продолжение поглощается до того, как для неё вызывается
    // BuildGeometry, поэтому её FTableStyle остаётся стилем по умолчанию.
    // Применяем DXF-стиль части (тот же handle 342, что и у главной части),
    // иначе текст рендерится высотой CAcadTableDefaultTextHeight и
    // «разъезжается» относительно ячеек (issue #1300).
    uzeacadtable_stylemanager.ApplyDXFTableStyle(
      FTableStyle, FContinuationParts[PartIdx].TableStyleHandle, ADrawing);
    RenderCurrentTable(
      ADrawing, ADC, ConstObjArray, @Self, BaseX, BaseY,
      FContinuationParts[PartIdx].RowBaseIndex);
    SwapTableData(FContinuationParts[PartIdx]);
  end;

  programlog.LogOutFormatStr(
    'AcadTable: model: BuildVisualRepresentation OK ' +
    'parts=%d TotalObj=%d',
    [Length(FContinuationParts), ConstObjArray.Count], LM_Info);
end;

// Генерирует уникальное имя анонимного блока таблицы вида *T<N>, отсутствующее
// в BlockDefArray чертежа. Именно так AutoCAD называет блоки таблиц, поэтому
// сохраняем совместимую схему имён (issue #1381).
function GenerateUniqueTableBlockName(var ADrawing: TDrawingDef): String;
var
  N: Integer;
  Candidate: String;
  BlockArr: PGDBObjBlockdefArray;
begin
  BlockArr := PGDBObjBlockdefArray(ADrawing.GetBlockDefArraySimple);
  N := 1;
  while N < 1000000 do
  begin
    Candidate := '*T' + IntToStr(N);
    if BlockArr^.getindex(Candidate) < 0 then
      Exit(Candidate);
    Inc(N);
  end;
  raise Exception.Create(
    'GenerateUniqueTableBlockName: failed to generate unique name');
end;

// Генерирует персональный анонимный блок с геометрией каждой части таблицы
// (главной и всех продолжений) и запоминает имена блоков. Вызывается из
// before-save callback до записи секций BLOCKS/BLOCK_RECORD. AutoCAD
// отрисовывает proxy/roundtrip-таблицу по связанному через group code 343
// блоку, поэтому без такого блока части разорванной таблицы открываются
// пустыми/битыми. С персональным блоком на часть AutoCAD видит каждую часть
// как самостоятельную таблицу, а внешний вид совпадает с сохранённым в ZCAD
// (issue #1381).
procedure GDBObjAcadTable.EnsureSplitPartBlocks(var ADrawing: TDrawingDef);
var
  DC: TDrawContext;
  BlockArr: PGDBObjBlockdefArray;
  BlockDef: PGDBObjBlockdef;
  BlockName: String;
  PartIdx: Integer;
begin
  // RAW-путь пишет исходный DXF со своими блоками и ссылками 343 — тогда
  // отрисовка в AutoCAD уже корректна, генерировать блоки заново не нужно.
  if CanSaveRawDXFEntity then
    Exit;
  if (FRowCount <= 0) or (FColCount <= 0) then
    Exit;

  DC := ADrawing.CreateDrawingRC;

  // --- Главная часть: рендерим в собственной системе координат (0,0) ---
  BlockName := GenerateUniqueTableBlockName(ADrawing);
  BlockArr := PGDBObjBlockdefArray(ADrawing.GetBlockDefArraySimple);
  BlockDef := BlockArr^.create(BlockName);
  BlockDef^.Base := cP3d__0__0__0;
  // BlockDef валиден на протяжении всего вызова RenderCurrentTable: тот
  // создаёт только сущности (не блоки), поэтому BlockDefArray не растёт и
  // BlockDef не перемещается. Держать указатель через create() нельзя —
  // именно поэтому имя генерируется и блок создаётся отдельно на каждую часть.
  RenderCurrentTable(ADrawing, DC, BlockDef^.ObjArray, BlockDef, 0, 0, 0);
  FMainPartBlockName := BlockName;

  // --- Части-продолжения: каждая рендерится в (0,0) своего блока ---
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    SwapTableData(FContinuationParts[PartIdx]);
    // Продолжение поглощается со стилем по умолчанию — применяем DXF-стиль
    // части (тот же handle 342, что и у главной), иначе текст рендерится
    // высотой по умолчанию и «разъезжается» (как в BuildVisualRepresentation).
    uzeacadtable_stylemanager.ApplyDXFTableStyle(
      FTableStyle, FContinuationParts[PartIdx].TableStyleHandle, ADrawing);
    BlockName := GenerateUniqueTableBlockName(ADrawing);
    BlockArr := PGDBObjBlockdefArray(ADrawing.GetBlockDefArraySimple);
    BlockDef := BlockArr^.create(BlockName);
    BlockDef^.Base := cP3d__0__0__0;
    RenderCurrentTable(ADrawing, DC, BlockDef^.ObjArray, BlockDef, 0, 0,
      FContinuationParts[PartIdx].RowBaseIndex);
    SwapTableData(FContinuationParts[PartIdx]);
    FContinuationParts[PartIdx].BlockName := BlockName;
  end;
end;

// Глубокое копирование данных рендеринга исходной таблицы в
// часть-продолжение. Вызывается до освобождения исходного объекта,
// поэтому все буферы копируются, а не разделяются.
procedure GDBObjAcadTable.CaptureTableDataToPart(
  var ASource: GDBObjAcadTable; var APart: TAcadTablePart);
var
  Idx, Idx2: Integer;
begin
  APart.InsertPoint := ASource.FInsertPoint;
  APart.RowBaseIndex := 0;
  APart.RowCount := ASource.FRowCount;
  APart.ColCount := ASource.FColCount;
  APart.TableFlags := ASource.FTableFlags;
  APart.TableStyle := ASource.FTableStyle;
  APart.TableStyleHandle := ASource.FTableStyleHandle;
  APart.BreakEnabled := ASource.FBreakEnabled;
  APart.BreakDirection := ASource.FBreakDirection;
  APart.BreakRepeatTopLabels := ASource.FBreakRepeatTopLabels;
  APart.BreakRepeatBottomLabels := ASource.FBreakRepeatBottomLabels;
  APart.BreakManualPosition := ASource.FBreakManualPosition;
  APart.BreakManualHeight := ASource.FBreakManualHeight;
  APart.BreakSpacing := ASource.FBreakSpacing;
  APart.BreakHeight := ASource.FBreakHeight;
  APart.RawDXFEntity := ASource.FRawDXFEntity;
  APart.RawDXFEntityValid := ASource.FRawDXFEntityValid;

  APart.RowHeights.initnul;
  for Idx := 0 to ASource.FRowHeights.Count - 1 do
    APart.RowHeights.PushBackData(ASource.FRowHeights.getData(Idx));
  APart.ColWidths.initnul;
  for Idx := 0 to ASource.FColWidths.Count - 1 do
    APart.ColWidths.PushBackData(ASource.FColWidths.getData(Idx));

  System.SetLength(APart.CellTexts, Length(ASource.FCellTexts));
  for Idx := 0 to High(ASource.FCellTexts) do
    APart.CellTexts[Idx] := ASource.FCellTexts[Idx];

  System.SetLength(APart.Rows, Length(ASource.FRows));
  for Idx := 0 to High(ASource.FRows) do
    APart.Rows[Idx] := ASource.FRows[Idx];

  System.SetLength(APart.Cols, Length(ASource.FCols));
  for Idx := 0 to High(ASource.FCols) do
    APart.Cols[Idx] := ASource.FCols[Idx];

  System.SetLength(APart.Cells, Length(ASource.FCells));
  for Idx := 0 to High(ASource.FCells) do
  begin
    System.SetLength(APart.Cells[Idx], Length(ASource.FCells[Idx]));
    for Idx2 := 0 to High(ASource.FCells[Idx]) do
      APart.Cells[Idx][Idx2] := ASource.FCells[Idx][Idx2];
  end;

  System.SetLength(APart.Merges, Length(ASource.FMerges));
  for Idx := 0 to High(ASource.FMerges) do
    APart.Merges[Idx] := ASource.FMerges[Idx];
end;

// Глубокое копирование части-продолжения (часть -> часть), для Clone.
procedure GDBObjAcadTable.CopyTablePart(
  const ASource: TAcadTablePart; var ADest: TAcadTablePart);
var
  Idx, Idx2: Integer;
begin
  ADest.InsertPoint := ASource.InsertPoint;
  ADest.RowBaseIndex := ASource.RowBaseIndex;
  ADest.RowCount := ASource.RowCount;
  ADest.ColCount := ASource.ColCount;
  ADest.TableFlags := ASource.TableFlags;
  ADest.TableStyle := ASource.TableStyle;
  ADest.TableStyleHandle := ASource.TableStyleHandle;
  ADest.BreakEnabled := ASource.BreakEnabled;
  ADest.BreakDirection := ASource.BreakDirection;
  ADest.BreakRepeatTopLabels := ASource.BreakRepeatTopLabels;
  ADest.BreakRepeatBottomLabels := ASource.BreakRepeatBottomLabels;
  ADest.BreakManualPosition := ASource.BreakManualPosition;
  ADest.BreakManualHeight := ASource.BreakManualHeight;
  ADest.BreakSpacing := ASource.BreakSpacing;
  ADest.BreakHeight := ASource.BreakHeight;
  ADest.RawDXFEntity := ASource.RawDXFEntity;
  ADest.RawDXFEntityValid := ASource.RawDXFEntityValid;
  ADest.BlockName := ASource.BlockName;

  ADest.RowHeights.initnul;
  for Idx := 0 to ASource.RowHeights.Count - 1 do
    ADest.RowHeights.PushBackData(ASource.RowHeights.getData(Idx));
  ADest.ColWidths.initnul;
  for Idx := 0 to ASource.ColWidths.Count - 1 do
    ADest.ColWidths.PushBackData(ASource.ColWidths.getData(Idx));

  System.SetLength(ADest.CellTexts, Length(ASource.CellTexts));
  for Idx := 0 to High(ASource.CellTexts) do
    ADest.CellTexts[Idx] := ASource.CellTexts[Idx];

  System.SetLength(ADest.Rows, Length(ASource.Rows));
  for Idx := 0 to High(ASource.Rows) do
    ADest.Rows[Idx] := ASource.Rows[Idx];

  System.SetLength(ADest.Cols, Length(ASource.Cols));
  for Idx := 0 to High(ASource.Cols) do
    ADest.Cols[Idx] := ASource.Cols[Idx];

  System.SetLength(ADest.Cells, Length(ASource.Cells));
  for Idx := 0 to High(ASource.Cells) do
  begin
    System.SetLength(ADest.Cells[Idx], Length(ASource.Cells[Idx]));
    for Idx2 := 0 to High(ASource.Cells[Idx]) do
      ADest.Cells[Idx][Idx2] := ASource.Cells[Idx][Idx2];
  end;

  System.SetLength(ADest.Merges, Length(ASource.Merges));
  for Idx := 0 to High(ASource.Merges) do
    ADest.Merges[Idx] := ASource.Merges[Idx];
end;

// Освобождение ресурсов части-продолжения.
procedure GDBObjAcadTable.ClearPart(var APart: TAcadTablePart);
begin
  APart.TableStyleHandle := '';
  APart.RowBaseIndex := 0;
  APart.RawDXFEntity := '';
  APart.RawDXFEntityValid := False;
  APart.BlockName := '';
  APart.RowHeights.done;
  APart.ColWidths.done;
  System.SetLength(APart.CellTexts, 0);
  System.SetLength(APart.Rows, 0);
  System.SetLength(APart.Cols, 0);
  System.SetLength(APart.Cells, 0, 0);
  System.SetLength(APart.Merges, 0);
end;

function GDBObjAcadTable.GetContinuationPartCount: Integer;
begin
  Result := Length(FContinuationParts);
end;

function GDBObjAcadTable.ContinuationPartRowCount(AIndex: Integer): Integer;
begin
  if (AIndex >= 0) and (AIndex <= High(FContinuationParts)) then
    Result := FContinuationParts[AIndex].RowCount
  else
    Result := -1;
end;

function GDBObjAcadTable.RecomputeBreakRepeatTopLabels: Boolean;
begin
  DetectBreakRepeatTopLabels;
  Result := FBreakRepeatTopLabels;
end;

function GDBObjAcadTable.ContinuationPartCellText(
  APartIndex, ARow, ACol: Integer): string;
var
  Idx: Integer;
begin
  Result := '';
  if (APartIndex < 0) or (APartIndex > High(FContinuationParts)) then Exit;
  with FContinuationParts[APartIndex] do
  begin
    if (ARow < 0) or (ARow >= RowCount) then Exit;
    if (ACol < 0) or (ACol >= ColCount) then Exit;
    Idx := ARow * ColCount + ACol;
    if (Idx >= 0) and (Idx <= High(CellTexts)) then
      Result := CellTexts[Idx];
  end;
end;

function GDBObjAcadTable.CellAlignmentAt(ARow, ACol: Integer): Integer;
var
  StyleRowIndex: Integer;
  ResolvedStyle: TCellStyle;
begin
  Result := 0;
  if (ARow < 0) or (ARow >= FRowCount) then Exit;
  if (ACol < 0) or (ACol >= FColCount) then Exit;
  if (Length(FCells) > ARow) and (Length(FCells[ARow]) > ACol) then
  begin
    Result := FCells[ARow][ACol].CellAlignment;
    if Result > 0 then
      Exit;
  end;

  if (ARow <= High(FCells)) and (ACol <= High(FCells[ARow])) and
     (FCells[ARow][ACol].StyleType >= 0) then
    StyleRowIndex := FCells[ARow][ACol].StyleType
  else if RowStyleTypeAt(ARow) >= 0 then
    StyleRowIndex := RowStyleTypeAt(ARow)
  else if FForceDataStyleAllRows then
    StyleRowIndex := 2
  else
    StyleRowIndex := ARow;
  ResolvedStyle := uzeacadtable_cell.ResolveCellStyleForBaseRow(
    StyleRowIndex, ARow, ACol, FTableStyle, FRows, FCols, FCells,
    FRowCount, FColCount, FTableFlags);
  Result := Ord(ResolvedStyle.VertAlign) * 3 +
    Ord(ResolvedStyle.HorzAlign) + 1;
end;

// Признак разрыва вычисляется по двум источникам (issue #1305, часть 2a):
//  - FBreakEnabled: флаг разрыва из одиночного DXF ACAD_TABLE;
//  - поглощённые части-продолжения: таблица, разорванная на несколько
//    ACAD_TABLE (ROUNDTRIP_2008), хранится как несколько частей одного
//    объекта, и DXF-флаг разрыва у неё не выставлен. В этом случае факт
//    разрыва определяется наличием частей-продолжений.
function GDBObjAcadTable.GetBreakEnabled: Boolean;
begin
  Result := FBreakEnabled or (Length(FContinuationParts) > 0);
end;

// Изменение признака разрыва (issue #1305, часть 2b). Установка в False
// для разорванной таблицы объединяет все части-продолжения с главной
// частью, выстраивая строки сверху вниз в единую непрерывную таблицу.
procedure GDBObjAcadTable.SetBreakEnabled(AValue: Boolean);
var
  L: Integer;
begin
  if AValue then
  begin
    InvalidateRawDXFEntity;
    FBreakEnabled := True;
    if (Length(FContinuationParts) = 0) and (FBreakHeight > 0) then
      SplitMainTableByBreakHeight(FBreakHeight)
    else if FBreakManualPosition then
      SetBreakManualPositionForParts(True)
    else
      RepositionContinuationParts;
    FGeometryBuilt := False;
    programlog.LogOutFormatStr(
      'AcadTable: model: SetBreakEnabled(True) rows=%d parts=%d',
      [FRowCount, Length(FContinuationParts)], LM_Info);
    Exit;
  end;

  // Снять разрыв: сначала удаляем повторённые верхние метки из
  // частей-продолжений, затем объединяем логические строки в главную таблицу.
  L := 0;
  if FBreakRepeatTopLabels then
    L := EffectiveRepeatTopRowCount;
  if L > 0 then
    RemoveTopLabelsFromParts(L);
  InvalidateRawDXFEntity;
  FBreakEnabled := False;
  MergeAllContinuationPartsIntoMain;

  // Геометрию нужно перестроить
  FGeometryBuilt := False;

  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakEnabled(False) merged into ' +
    'single table rows=%d cols=%d', [FRowCount, FColCount], LM_Info);
end;

// Изменение направления разбиения (issue #1315). Для уже разорванной таблицы
// в автоматическом режиме смещает части-продолжения относительно главной
// части. В ручном режиме направление только сохраняется и не влияет на
// положение частей (issue #1328).
procedure GDBObjAcadTable.SetBreakDirection(AValue: TAcadTableBreakDirection);
var
  PartIdx: Integer;
begin
  if AValue = FBreakDirection then
    Exit;

  InvalidateRawDXFEntity;
  FBreakDirection := AValue;
  for PartIdx := 0 to High(FContinuationParts) do
    FContinuationParts[PartIdx].BreakDirection := AValue;
  if FBreakManualPosition then
    SetBreakManualPositionForParts(True)
  else
    RepositionContinuationParts;
  FGeometryBuilt := False;

  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakDirection=%d parts=%d',
    [Ord(FBreakDirection), Length(FContinuationParts)], LM_Info);
end;

// Объединяет все части-продолжения в главную часть: строки выстраиваются
// сверху вниз в единую непрерывную таблицу. Части-продолжения
// освобождаются. Не трогает FBreakEnabled и FGeometryBuilt — это делает
// вызывающая сторона (issue #1305 часть 2b, issue #1307).
procedure GDBObjAcadTable.MergeAllContinuationPartsIntoMain;
var
  PartIdx, RowIdx, ColIdx, SrcCount: Integer;
  RowOffset: Integer;
  MergeBase: Integer;
begin
  if Length(FContinuationParts) = 0 then
    Exit;

  for PartIdx := 0 to High(FContinuationParts) do
  begin
    RowOffset := FRowCount;

    // Высоты строк добавляемой части
    for RowIdx := 0 to FContinuationParts[PartIdx].RowHeights.Count - 1 do
      FRowHeights.PushBackData(
        FContinuationParts[PartIdx].RowHeights.getData(RowIdx));

    // Тексты ячеек (тот же порядок строк, та же ширина по столбцам)
    SrcCount := Length(FContinuationParts[PartIdx].CellTexts);
    if SrcCount > 0 then
    begin
      MergeBase := Length(FCellTexts);
      System.SetLength(FCellTexts, MergeBase + SrcCount);
      for RowIdx := 0 to SrcCount - 1 do
        FCellTexts[MergeBase + RowIdx] :=
          FContinuationParts[PartIdx].CellTexts[RowIdx];
    end;

    // Формат строк
    MergeBase := Length(FRows);
    System.SetLength(FRows,
      MergeBase + Length(FContinuationParts[PartIdx].Rows));
    for RowIdx := 0 to High(FContinuationParts[PartIdx].Rows) do
      FRows[MergeBase + RowIdx] :=
        FContinuationParts[PartIdx].Rows[RowIdx];

    // Ячейки (двумерный массив [строка][столбец])
    MergeBase := Length(FCells);
    System.SetLength(FCells,
      MergeBase + Length(FContinuationParts[PartIdx].Cells));
    for RowIdx := 0 to High(FContinuationParts[PartIdx].Cells) do
    begin
      System.SetLength(FCells[MergeBase + RowIdx],
        Length(FContinuationParts[PartIdx].Cells[RowIdx]));
      for ColIdx := 0 to
          High(FContinuationParts[PartIdx].Cells[RowIdx]) do
        FCells[MergeBase + RowIdx][ColIdx] :=
          FContinuationParts[PartIdx].Cells[RowIdx][ColIdx];
    end;

    // Объединения ячеек со сдвигом номеров строк
    MergeBase := Length(FMerges);
    System.SetLength(FMerges,
      MergeBase + Length(FContinuationParts[PartIdx].Merges));
    for RowIdx := 0 to High(FContinuationParts[PartIdx].Merges) do
    begin
      FMerges[MergeBase + RowIdx] :=
        FContinuationParts[PartIdx].Merges[RowIdx];
      Inc(FMerges[MergeBase + RowIdx].Row1, RowOffset);
      Inc(FMerges[MergeBase + RowIdx].Row2, RowOffset);
    end;

    // Учитываем добавленные строки
    Inc(FRowCount, FContinuationParts[PartIdx].RowCount);
  end;

  // Освобождаем части-продолжения — таблица снова непрерывна
  for PartIdx := 0 to High(FContinuationParts) do
    ClearPart(FContinuationParts[PartIdx]);
  System.SetLength(FContinuationParts, 0);
end;

// --- Параметры разбиения: интервал и высота (issue #1307) ---

function GDBObjAcadTable.GetBreakSpacing: Double;
begin
  Result := FBreakSpacing;
end;

// Изменение интервала между частями (issue #1307, часть 1). В автоматическом
// режиме пересчитывает точки вставки всех частей-продолжений. В ручном режиме
// интервал только сохраняется и не влияет на положение частей (issue #1328).
procedure GDBObjAcadTable.SetBreakSpacing(AValue: Double);
var
  PartIdx: Integer;
begin
  if AValue = FBreakSpacing then
    Exit;
  InvalidateRawDXFEntity;
  FBreakSpacing := AValue;
  for PartIdx := 0 to High(FContinuationParts) do
    FContinuationParts[PartIdx].BreakSpacing := AValue;
  if FBreakManualPosition then
    SetBreakManualPositionForParts(True)
  else
    RepositionContinuationParts;
  FGeometryBuilt := False;
  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakSpacing=%g parts=%d',
    [FBreakSpacing, Length(FContinuationParts)], LM_Info);
end;

function GDBObjAcadTable.GetBreakHeight: Double;
begin
  Result := FBreakHeight;
end;

// Изменение высоты разбиения (issue #1307, часть 2). Сначала объединяет
// все части в единую таблицу, затем заново разбивает строки по новой
// высоте — автоматически определяя необходимое число частей.
procedure GDBObjAcadTable.SetBreakHeight(AValue: Double);
var
  L: Integer;
  WasBreakEnabled: Boolean;
  WasManualPosition, WasManualPositionExplicit: Boolean;
  ManualPositions: TAcadTableBreakPositionArray;
begin
  if AValue = FBreakHeight then
    Exit;
  InvalidateRawDXFEntity;
  WasBreakEnabled := GetBreakEnabled;
  WasManualPosition := FBreakManualPosition and
    (Length(FContinuationParts) > 0);
  WasManualPositionExplicit := FBreakManualPositionExplicit;
  if WasManualPosition then
    CaptureContinuationInsertPoints(ManualPositions);
  FBreakHeight := AValue;
  // Перебор строк имеет смысл только при положительной высоте и
  // включённом разбиении. При BreakEnabled=False параметры разбиения
  // сохраняются, но не влияют на непрерывную таблицу (issue #1313).
  if (AValue > 0) and WasBreakEnabled then
  begin
    FBreakEnabled := True;
    if FBreakRepeatTopLabels then
    begin
      L := EffectiveRepeatTopRowCount;
      if L > 0 then
        RemoveTopLabelsFromParts(L);
    end;
    MergeAllContinuationPartsIntoMain;
    SplitMainTableByBreakHeight(AValue);
    if WasManualPosition then
      RestoreManualContinuationInsertPoints(
        ManualPositions, WasManualPositionExplicit);
  end;
  FGeometryBuilt := False;
  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakHeight=%g rows=%d parts=%d',
    [FBreakHeight, FRowCount, Length(FContinuationParts)], LM_Info);
end;

// Копирует диапазон строк [AStart..AEnd] исходной части в целевую часть.
// Столбцы, стиль и флаги копируются целиком; строки/высоты/тексты/ячейки/
// объединения вырезаются по диапазону. Точку вставки выставляет
// RepositionContinuationParts (issue #1307).
procedure GDBObjAcadTable.SlicePartFromPart(
  const ASource: TAcadTablePart;
  AStart, AEnd: Integer; var ADest: TAcadTablePart);
var
  RowIdx, ColIdx, ColCnt, SrcIdx, DstRows, TextBase: Integer;
begin
  DstRows := AEnd - AStart + 1;
  if DstRows < 0 then DstRows := 0;
  ColCnt := ASource.ColCount;

  ADest.InsertPoint := ASource.InsertPoint;
  ADest.RowBaseIndex := ASource.RowBaseIndex + AStart;
  ADest.RowCount := DstRows;
  ADest.ColCount := ColCnt;
  ADest.TableFlags := ASource.TableFlags;
  ADest.TableStyle := ASource.TableStyle;
  ADest.TableStyleHandle := ASource.TableStyleHandle;
  ADest.BreakEnabled := ASource.BreakEnabled;
  ADest.BreakDirection := ASource.BreakDirection;
  ADest.BreakRepeatTopLabels := ASource.BreakRepeatTopLabels;
  ADest.BreakRepeatBottomLabels := ASource.BreakRepeatBottomLabels;
  ADest.BreakManualPosition := ASource.BreakManualPosition;
  ADest.BreakManualHeight := ASource.BreakManualHeight;
  ADest.BreakSpacing := ASource.BreakSpacing;
  ADest.BreakHeight := ASource.BreakHeight;
  ADest.RawDXFEntity := '';
  ADest.RawDXFEntityValid := False;

  // Высоты строк диапазона
  ADest.RowHeights.initnul;
  for RowIdx := AStart to AEnd do
    if (RowIdx >= 0) and (RowIdx < ASource.RowHeights.Count) then
      ADest.RowHeights.PushBackData(ASource.RowHeights.getData(RowIdx));

  // Ширины столбцов — те же, что у источника
  ADest.ColWidths.initnul;
  for ColIdx := 0 to ASource.ColWidths.Count - 1 do
    ADest.ColWidths.PushBackData(ASource.ColWidths.getData(ColIdx));

  // Тексты ячеек (плоский массив, индекс = строка * ColCount + столбец)
  System.SetLength(ADest.CellTexts, DstRows * ColCnt);
  if ColCnt > 0 then
    for RowIdx := 0 to DstRows - 1 do
      for ColIdx := 0 to ColCnt - 1 do
      begin
        SrcIdx := (AStart + RowIdx) * ColCnt + ColIdx;
        TextBase := RowIdx * ColCnt + ColIdx;
        if (SrcIdx >= 0) and (SrcIdx <= High(ASource.CellTexts)) then
          ADest.CellTexts[TextBase] := ASource.CellTexts[SrcIdx]
        else
          ADest.CellTexts[TextBase] := '';
      end;

  // Формат строк
  System.SetLength(ADest.Rows, DstRows);
  for RowIdx := 0 to DstRows - 1 do
    if (AStart + RowIdx) <= High(ASource.Rows) then
      ADest.Rows[RowIdx] := ASource.Rows[AStart + RowIdx];

  // Столбцы — те же, что у источника
  System.SetLength(ADest.Cols, Length(ASource.Cols));
  for ColIdx := 0 to High(ASource.Cols) do
    ADest.Cols[ColIdx] := ASource.Cols[ColIdx];

  // Ячейки (двумерный массив [строка][столбец])
  System.SetLength(ADest.Cells, DstRows);
  for RowIdx := 0 to DstRows - 1 do
    if (AStart + RowIdx) <= High(ASource.Cells) then
    begin
      System.SetLength(ADest.Cells[RowIdx],
        Length(ASource.Cells[AStart + RowIdx]));
      for ColIdx := 0 to High(ASource.Cells[AStart + RowIdx]) do
        ADest.Cells[RowIdx][ColIdx] :=
          ASource.Cells[AStart + RowIdx][ColIdx];
    end;

  // Объединения ячеек, попадающие целиком в диапазон, со сдвигом строк
  System.SetLength(ADest.Merges, 0);
  for RowIdx := 0 to High(ASource.Merges) do
    if (ASource.Merges[RowIdx].Row1 >= AStart) and
       (ASource.Merges[RowIdx].Row2 <= AEnd) then
    begin
      ColIdx := Length(ADest.Merges);
      System.SetLength(ADest.Merges, ColIdx + 1);
      ADest.Merges[ColIdx] := ASource.Merges[RowIdx];
      Dec(ADest.Merges[ColIdx].Row1, AStart);
      Dec(ADest.Merges[ColIdx].Row2, AStart);
    end;
end;

// Пересегментирует объединённую главную часть по высоте разбиения:
// строки набираются в сегмент, пока их суммарная высота не превысит
// порог AThreshold; затем начинается новый сегмент. Первый сегмент
// остаётся в главной части, остальные выносятся в части-продолжения.
// Если BreakRepeatTopLabels=True, верхние метки не считаются отдельными
// строками логической таблицы в продолжениях: они добавляются поверх каждого
// продолжения и уменьшают доступную высоту сегмента (issue #1311).
// Число частей определяется автоматически (issue #1307).
procedure GDBObjAcadTable.SplitMainTableByBreakHeight(AThreshold: Double);
var
  BreakHeights: TAcadTableBreakHeightArray;
begin
  if AThreshold <= 0 then
    Exit;

  System.SetLength(BreakHeights, 1);
  BreakHeights[0] := AThreshold;
  SplitMainTableByBreakHeights(BreakHeights);
end;

function GDBObjAcadTable.BreakHeightForPart(
  const ABreakHeights: TAcadTableBreakHeightArray;
  APartNumber: Integer): Double;
begin
  Result := FBreakHeight;
  if (APartNumber >= 0) and
     (APartNumber < Length(ABreakHeights)) and
     AcadTableBreakHeightHasValue(ABreakHeights[APartNumber]) then
    Result := ABreakHeights[APartNumber];

  if not AcadTableBreakHeightHasValue(Result) then
    Result := CAcadTableBreakHeightTolerance;
end;

procedure GDBObjAcadTable.CaptureContinuationInsertPoints(
  var APositions: TAcadTableBreakPositionArray);
var
  PartIdx: Integer;
begin
  System.SetLength(APositions, Length(FContinuationParts));
  for PartIdx := 0 to High(FContinuationParts) do
    APositions[PartIdx] := FContinuationParts[PartIdx].InsertPoint;
end;

procedure GDBObjAcadTable.RestoreManualContinuationInsertPoints(
  const APositions: TAcadTableBreakPositionArray;
  AWasExplicit: Boolean);
var
  PartIdx, LastIdx: Integer;
begin
  if (Length(APositions) = 0) or (Length(FContinuationParts) = 0) then
    Exit;

  LastIdx := High(APositions);
  if LastIdx > High(FContinuationParts) then
    LastIdx := High(FContinuationParts);
  for PartIdx := 0 to LastIdx do
    FContinuationParts[PartIdx].InsertPoint := APositions[PartIdx];

  FBreakManualPosition := True;
  FBreakManualPositionExplicit := AWasExplicit;
  SetBreakManualPositionForParts(True);
end;

procedure GDBObjAcadTable.SplitMainTableByBreakHeights(
  const ABreakHeights: TAcadTableBreakHeightArray);
var
  FullData, TmpPart: TAcadTablePart;
  SegStart, SegEnd: array of Integer;
  SegCount, StartRow, EndRow, PartIdx, RepeatRows, RowIdx: Integer;
  CurHeight, NextHeight, RepeatHeight, PartBreakHeight: Double;
  ManualHeight: Boolean;
begin
  if FRowCount <= 0 then
    Exit;

  ManualHeight := FBreakManualHeight;

  // Снимок текущей (объединённой) главной части
  CaptureTableDataToPart(Self, FullData);
  try
    RepeatRows := 0;
    RepeatHeight := 0;
    if FBreakRepeatTopLabels then
    begin
      RepeatRows := ComputeTopLabelRowCount;
      if RepeatRows > FullData.RowCount then
        RepeatRows := FullData.RowCount;
      for RowIdx := 0 to RepeatRows - 1 do
        RepeatHeight := RepeatHeight +
          uzeacadtable_layout.GetRowHeight(RowIdx, FullData.RowHeights);
    end;

    // Вычисляем границы сегментов по высоте строк
    SegCount := 0;
    StartRow := 0;
    while StartRow < FullData.RowCount do
    begin
      EndRow := StartRow;
      CurHeight := 0;
      PartBreakHeight := BreakHeightForPart(ABreakHeights, SegCount);
      if (SegCount > 0) and (RepeatRows > 0) then
        CurHeight := RepeatHeight;
      while EndRow < FullData.RowCount do
      begin
        NextHeight := CurHeight +
          uzeacadtable_layout.GetRowHeight(EndRow, FullData.RowHeights);
        if (EndRow > StartRow) and
           (NextHeight > PartBreakHeight + 1e-9) then
          Break;
        CurHeight := NextHeight;
        Inc(EndRow);
      end;
      if EndRow = StartRow then
        Inc(EndRow);
      if (SegCount = 0) and (RepeatRows > 0) and
         (EndRow < RepeatRows) then
        EndRow := RepeatRows;

      System.SetLength(SegStart, SegCount + 1);
      System.SetLength(SegEnd, SegCount + 1);
      SegStart[SegCount] := StartRow;
      SegEnd[SegCount] := EndRow - 1;
      Inc(SegCount);
      StartRow := EndRow;
      if (RepeatRows > 0) and (StartRow < RepeatRows) then
        StartRow := RepeatRows;
    end;

    // Части-продолжения для сегментов 1..SegCount-1
    for PartIdx := 0 to High(FContinuationParts) do
      ClearPart(FContinuationParts[PartIdx]);
    System.SetLength(FContinuationParts, SegCount - 1);
    for PartIdx := 1 to SegCount - 1 do
    begin
      SlicePartFromPart(FullData, SegStart[PartIdx], SegEnd[PartIdx],
        FContinuationParts[PartIdx - 1]);
      FContinuationParts[PartIdx - 1].BreakHeight :=
        BreakHeightForPart(ABreakHeights, PartIdx);
      FContinuationParts[PartIdx - 1].BreakManualHeight := ManualHeight;
      if RepeatRows > 0 then
        PrependTopLabelsToPart(RepeatRows, FContinuationParts[PartIdx - 1]);
    end;

    // Сегмент 0 -> главная часть. Готовим временную часть и меняемся
    // данными с собой (точка вставки главной части сохраняется).
    SlicePartFromPart(FullData, SegStart[0], SegEnd[0], TmpPart);
    SwapTableData(TmpPart);
    FBreakHeight := BreakHeightForPart(ABreakHeights, 0);
    FBreakManualHeight := ManualHeight;
    ClearPart(TmpPart);

    RepositionContinuationParts;
  finally
    ClearPart(FullData);
  end;
end;

procedure GDBObjAcadTable.ResplitByCurrentBreakHeights;
var
  BreakHeights: TAcadTableBreakHeightArray;
  PartIdx, L: Integer;
  WasManualPosition, WasManualPositionExplicit: Boolean;
  ManualPositions: TAcadTableBreakPositionArray;
begin
  if not GetBreakEnabled then
    Exit;

  WasManualPosition := FBreakManualPosition and
    (Length(FContinuationParts) > 0);
  WasManualPositionExplicit := FBreakManualPositionExplicit;
  if WasManualPosition then
    CaptureContinuationInsertPoints(ManualPositions);

  System.SetLength(BreakHeights, Length(FContinuationParts) + 1);
  BreakHeights[0] := FBreakHeight;
  for PartIdx := 0 to High(FContinuationParts) do
    BreakHeights[PartIdx + 1] := FContinuationParts[PartIdx].BreakHeight;

  FBreakEnabled := True;
  FBreakManualHeight := True;
  L := 0;
  if FBreakRepeatTopLabels then
    L := EffectiveRepeatTopRowCount;
  if L > 0 then
    RemoveTopLabelsFromParts(L);
  MergeAllContinuationPartsIntoMain;
  SplitMainTableByBreakHeights(BreakHeights);
  if WasManualPosition then
    RestoreManualContinuationInsertPoints(
      ManualPositions, WasManualPositionExplicit);
  FBreakManualHeight := True;
  SetBreakManualHeightForParts(True);
end;

// Пересчитывает точки вставки частей-продолжений из текущего интервала
// (FBreakSpacing) и направления разрыва (FBreakDirection). Расстояние
// между соседними частями = (размер предыдущего сегмента вдоль оси
// разрыва) + интервал (issue #1307).
procedure GDBObjAcadTable.RepositionContinuationParts;
var
  PartIdx: Integer;
  Horizontal: Boolean;
  CumOffset, PrevExtent: Double;
begin
  if Length(FContinuationParts) = 0 then
    Exit;

  Horizontal := FBreakDirection in [atbdRight, atbdLeft];
  if Horizontal then
    PrevExtent := GetTotalWidth
  else
    PrevExtent := GetTotalHeight;

  CumOffset := 0;
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    CumOffset := CumOffset + PrevExtent + FBreakSpacing;

    FContinuationParts[PartIdx].InsertPoint := FInsertPoint;
    FContinuationParts[PartIdx].BreakDirection := FBreakDirection;
    FContinuationParts[PartIdx].BreakSpacing := FBreakSpacing;
    case FBreakDirection of
      atbdDown:
        FContinuationParts[PartIdx].InsertPoint.y :=
          FInsertPoint.y - CumOffset;
      atbdLeft:
        FContinuationParts[PartIdx].InsertPoint.x :=
          FInsertPoint.x - CumOffset;
    else // atbdRight
      FContinuationParts[PartIdx].InsertPoint.x :=
        FInsertPoint.x + CumOffset;
    end;
    FContinuationParts[PartIdx].BreakManualPosition := False;

    if Horizontal then
      PrevExtent := uzeacadtable_layout.GetTotalWidth(
        FContinuationParts[PartIdx].ColCount,
        FContinuationParts[PartIdx].ColWidths)
    else
      PrevExtent := uzeacadtable_layout.GetTotalHeight(
        FContinuationParts[PartIdx].RowCount,
        FContinuationParts[PartIdx].RowHeights);
  end;
  FBreakManualPosition := False;
  FBreakManualPositionExplicit := False;
end;

procedure GDBObjAcadTable.SetBreakManualPositionForParts(AValue: Boolean);
var
  PartIdx: Integer;
begin
  for PartIdx := 0 to High(FContinuationParts) do
    FContinuationParts[PartIdx].BreakManualPosition := AValue;
end;

procedure GDBObjAcadTable.SetBreakManualHeightForParts(AValue: Boolean);
var
  PartIdx: Integer;
begin
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    FContinuationParts[PartIdx].BreakManualHeight := AValue;
    if (not AValue) or
       (not AcadTableBreakHeightHasValue(
          FContinuationParts[PartIdx].BreakHeight)) then
      FContinuationParts[PartIdx].BreakHeight := FBreakHeight;
  end;
end;

procedure GDBObjAcadTable.SetBreakManualPosition(AValue: Boolean);
begin
  if AValue then
  begin
    if FBreakManualPosition and FBreakManualPositionExplicit then
      Exit;

    InvalidateRawDXFEntity;
    FBreakManualPosition := True;
    FBreakManualPositionExplicit := True;
    SetBreakManualPositionForParts(True);
    FGeometryBuilt := False;
    programlog.LogOutFormatStr(
      'AcadTable: model: SetBreakManualPosition=True parts=%d',
      [Length(FContinuationParts)], LM_Info);
    Exit;
  end;

  if (not FBreakManualPosition) and (not FBreakManualPositionExplicit) and
     (not HasManualContinuationPositions) then
    Exit;

  InvalidateRawDXFEntity;
  FBreakManualPosition := False;
  FBreakManualPositionExplicit := False;
  if Length(FContinuationParts) > 0 then
    RepositionContinuationParts
  else
    SetBreakManualPositionForParts(False);
  FGeometryBuilt := False;
  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakManualPosition=False parts=%d',
    [Length(FContinuationParts)], LM_Info);
end;

procedure GDBObjAcadTable.SetBreakManualHeight(AValue: Boolean);
begin
  if AValue = FBreakManualHeight then
    Exit;

  InvalidateRawDXFEntity;
  FBreakManualHeight := AValue;
  SetBreakManualHeightForParts(AValue);
  FGeometryBuilt := False;
  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakManualHeight=%d parts=%d',
    [Ord(FBreakManualHeight), Length(FContinuationParts)], LM_Info);
end;

function GDBObjAcadTable.HasManualContinuationPositions: Boolean;
var
  PartIdx: Integer;
  Horizontal: Boolean;
  CumOffset, PrevExtent: Double;
  ExpectedPoint: TzePoint3d;
begin
  Result := False;
  if Length(FContinuationParts) = 0 then
    Exit;

  Horizontal := FBreakDirection in [atbdRight, atbdLeft];
  if Horizontal then
    PrevExtent := GetTotalWidth
  else
    PrevExtent := GetTotalHeight;

  CumOffset := 0;
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    CumOffset := CumOffset + PrevExtent + FBreakSpacing;

    ExpectedPoint := FInsertPoint;
    case FBreakDirection of
      atbdDown:
        ExpectedPoint.y := FInsertPoint.y - CumOffset;
      atbdLeft:
        ExpectedPoint.x := FInsertPoint.x - CumOffset;
    else
      ExpectedPoint.x := FInsertPoint.x + CumOffset;
    end;

    if not SameAcadTableBreakPoint(
      FContinuationParts[PartIdx].InsertPoint, ExpectedPoint) then
      Exit(True);

    if Horizontal then
      PrevExtent := uzeacadtable_layout.GetTotalWidth(
        FContinuationParts[PartIdx].ColCount,
        FContinuationParts[PartIdx].ColWidths)
    else
      PrevExtent := uzeacadtable_layout.GetTotalHeight(
        FContinuationParts[PartIdx].RowCount,
        FContinuationParts[PartIdx].RowHeights);
  end;
end;

procedure GDBObjAcadTable.DetectBreakManualPosition;
var
  PartIdx: Integer;
  ManualPosition: Boolean;
begin
  { Явные флаги из roundtrip-данных DXF приоритетнее эвристики (issue #1339). }
  if FBreakFlagsKnown then
    Exit;
  if Length(FContinuationParts) = 0 then
    Exit;

  ManualPosition :=
    FBreakManualPositionExplicit or HasManualContinuationPositions;
  for PartIdx := 0 to High(FContinuationParts) do
    ManualPosition := ManualPosition or
      FContinuationParts[PartIdx].BreakManualPosition;

  FBreakManualPosition := ManualPosition;
end;

procedure GDBObjAcadTable.DetectBreakManualHeight;
var
  PartIdx: Integer;
  ReferenceHeight, PartHeight: Double;
  HasReference, ManualHeight: Boolean;
begin
  { Явные флаги из roundtrip-данных DXF приоритетнее эвристики (issue #1339). }
  if FBreakFlagsKnown then
    Exit;
  ManualHeight := FBreakManualHeight;
  HasReference := False;
  ReferenceHeight := 0;

  if AcadTableBreakHeightHasValue(FBreakHeight) then
  begin
    ReferenceHeight := FBreakHeight;
    HasReference := True;
  end;

  for PartIdx := 0 to High(FContinuationParts) do
  begin
    ManualHeight := ManualHeight or
      FContinuationParts[PartIdx].BreakManualHeight;
    PartHeight := FContinuationParts[PartIdx].BreakHeight;
    if not AcadTableBreakHeightHasValue(PartHeight) then
      Continue;

    if HasReference then
    begin
      if not SameAcadTableBreakHeight(PartHeight, ReferenceHeight) then
        ManualHeight := True;
    end
    else
    begin
      ReferenceHeight := PartHeight;
      HasReference := True;
    end;
  end;

  FBreakManualHeight := ManualHeight;
  if FBreakManualHeight then
    SetBreakManualHeightForParts(True);
end;

function GDBObjAcadTable.ContinuationPartGripPointInWCS(
  APartIndex: Integer): TzePoint3d;
var
  LocalOffset: TzeVector3d;
begin
  Result := P_insert_in_WCS;
  if (APartIndex < 0) or
     (APartIndex > High(FContinuationParts)) then
    Exit;

  LocalOffset := FContinuationParts[APartIndex].InsertPoint - FInsertPoint;
  Result := VectorTransform3D(LocalOffset, objMatrix).asPoint3d;
end;

function GDBObjAcadTable.BreakHeightGripPointInWCS(
  APartNumber: Integer): TzePoint3d;
var
  LocalOffset: TzePoint3d;
  PartIdx: Integer;
  PartWidth, PartHeight: Double;
begin
  Result := P_insert_in_WCS;
  if APartNumber = 0 then
  begin
    LocalOffset := TzePoint3d.make(
      GetTotalWidth / 2,
      -GetTotalHeight,
      0);
    Result := VectorTransform3D(LocalOffset, objMatrix);
    Exit;
  end;

  PartIdx := APartNumber - 1;
  if (PartIdx < 0) or (PartIdx > High(FContinuationParts)) then
    Exit;

  PartWidth := uzeacadtable_layout.GetTotalWidth(
    FContinuationParts[PartIdx].ColCount,
    FContinuationParts[PartIdx].ColWidths);
  PartHeight := uzeacadtable_layout.GetTotalHeight(
    FContinuationParts[PartIdx].RowCount,
    FContinuationParts[PartIdx].RowHeights);
  LocalOffset := (FContinuationParts[PartIdx].InsertPoint- FInsertPoint).asPoint3d;
  LocalOffset.x := LocalOffset.x + PartWidth / 2;
  LocalOffset.y := LocalOffset.y - PartHeight;
  Result := VectorTransform3D(LocalOffset, objMatrix);
end;

function GDBObjAcadTable.BreakHeightFromGripLocalOffset(
  APartNumber: Integer; const ALocalOffset: TzePoint3d): Double;
var
  PartIdx: Integer;
  PartInsertOffset: TzePoint3d;
begin
  if APartNumber = 0 then
    Result := -ALocalOffset.y
  else
  begin
    PartIdx := APartNumber - 1;
    if (PartIdx < 0) or (PartIdx > High(FContinuationParts)) then
      Result := FBreakHeight
    else
    begin
      PartInsertOffset := (FContinuationParts[PartIdx].InsertPoint - FInsertPoint).asPoint3d;
      Result := PartInsertOffset.y - ALocalOffset.y;
    end;
  end;

  if Result < CAcadTableBreakHeightTolerance then
    Result := CAcadTableBreakHeightTolerance;
end;

function GDBObjAcadTable.DecodeBreakHeightGripVertex(
  AVertexNum: Integer; out APartNumber: Integer): Boolean;
begin
  APartNumber := AVertexNum - CAcadTableBreakHeightGripVertexBase;
  Result :=
    (APartNumber >= 0) and
    (APartNumber <= Length(FContinuationParts));
end;

// --- Повтор верхних строк-меток в частях разорванной таблицы (issue #1309) ---

// Число ведущих строк-меток (Title/Header), которые могут повторяться в начале
// каждой части разорванной таблицы. Если доступны явные типы строк
// (SetRowStyleTypes: 0=Title, 1=Header, 2=Data), зона повтора идёт от начала
// таблицы до первой строки Data и может содержать любое количество строк
// Title/Header в любой последовательности (issue #1375).
//
// Прежняя реализация опиралась на биты подавления TableFlags (бит 2 — Title,
// бит 4 — Header), но реальные DXF (например, test/tablerazdel.dxf) приходят с
// TableFlags=22, где эти биты установлены, хотя строки Title и Header реально
// присутствуют и повторяются в каждой части. Поэтому признак повтора
// определяется не флагами, а сравнением содержимого строк частей с главной
// частью (см. EffectiveRepeatTopRowCount), а здесь возвращается лишь верхняя
// граница зоны меток. Для старых таблиц без явных типов строк сохраняется
// legacy-логика: строка 0 — Title, строка 1 — Header, строки >=2 — Data.
function GDBObjAcadTable.ComputeTopLabelRowCount: Integer;
var
  RowIdx, StyleType: Integer;
begin
  Result := 0;
  if FRowCount <= 0 then
    Exit;

  if Length(FRowStyleTypes) = 0 then
  begin
    if FRowCount > 0 then
      Inc(Result);
    if FRowCount > 1 then
      Inc(Result);
    Exit;
  end;

  for RowIdx := 0 to FRowCount - 1 do
  begin
    StyleType := RowStyleTypeAt(RowIdx);
    if StyleType < 0 then
    begin
      if RowIdx = 0 then
        StyleType := 0
      else if RowIdx = 1 then
        StyleType := 1
      else
        StyleType := 2;
    end;

    if StyleType = 2 then
      Break;
    if (StyleType = 0) or (StyleType = 1) then
      Inc(Result)
    else
      Break;
  end;
end;

// Фактическое число ведущих строк-меток, которые ОДИНАКОВО повторяются в начале
// КАЖДОЙ части-продолжения. Ищется наибольшее L в диапазоне
// [1..ComputeTopLabelRowCount], при котором все части повторяют первые L строк
// главной части. Возвращает 0, если частей нет или повтора нет. Подход опирается
// на содержимое (issue #1309), а не на флаги подавления заголовков.
function GDBObjAcadTable.EffectiveRepeatTopRowCount: Integer;
var
  L, MaxL, PartIdx: Integer;
  AllRepeat: Boolean;
begin
  Result := 0;
  if Length(FContinuationParts) = 0 then
    Exit;
  MaxL := ComputeTopLabelRowCount;
  for L := MaxL downto 1 do
  begin
    AllRepeat := True;
    for PartIdx := 0 to High(FContinuationParts) do
      if not PartRepeatsTopLabels(FContinuationParts[PartIdx], L) then
      begin
        AllRepeat := False;
        Break;
      end;
    if AllRepeat then
    begin
      Result := L;
      Exit;
    end;
  end;
end;

// Максимальное число ведущих строк, одинаково повторяющихся в начале КАЖДОЙ
// части-продолжения, БЕЗ ограничения зоной меток ComputeTopLabelRowCount.
// Поиск идёт от максимально возможной длины (не больше числа строк главной
// части и минимального числа строк среди частей) вниз до первого совпадения.
// Используется для восстановления типов строк старой разорванной таблицы,
// у которой нет объекта AcDbTableContent и, следовательно, явных типов строк
// (issue #1373): число повторяющихся ведущих строк = число строк-меток
// (Title + строки Header) перед первой строкой данных.
function GDBObjAcadTable.DetectRepeatedTopRowCountRaw: Integer;
var
  L, MaxL, PartIdx: Integer;
  AllRepeat: Boolean;
begin
  Result := 0;
  if Length(FContinuationParts) = 0 then
    Exit;
  MaxL := FRowCount;
  for PartIdx := 0 to High(FContinuationParts) do
    if FContinuationParts[PartIdx].RowCount < MaxL then
      MaxL := FContinuationParts[PartIdx].RowCount;
  for L := MaxL downto 1 do
  begin
    AllRepeat := True;
    for PartIdx := 0 to High(FContinuationParts) do
      if not PartRepeatsTopLabels(FContinuationParts[PartIdx], L) then
      begin
        AllRepeat := False;
        Break;
      end;
    if AllRepeat then
    begin
      Result := L;
      Exit;
    end;
  end;
end;

// Проверяет, повторяет ли часть-продолжение первые L строк-меток главной
// части: число столбцов должно совпадать, у части должно быть не меньше L
// строк, а тексты ячеек первых L строк должны быть идентичны главной части.
function GDBObjAcadTable.PartRepeatsTopLabels(
  const APart: TAcadTablePart; L: Integer): Boolean;
var
  RowIdx, ColIdx, MainIdx, PartTextIdx: Integer;
begin
  Result := False;
  if L <= 0 then Exit;
  if APart.ColCount <> FColCount then Exit;
  if APart.RowCount < L then Exit;
  for RowIdx := 0 to L - 1 do
    for ColIdx := 0 to FColCount - 1 do
    begin
      MainIdx := RowIdx * FColCount + ColIdx;
      PartTextIdx := RowIdx * APart.ColCount + ColIdx;
      if (MainIdx > High(FCellTexts)) or
         (PartTextIdx > High(APart.CellTexts)) then
        Exit;
      if FCellTexts[MainIdx] <> APart.CellTexts[PartTextIdx] then
        Exit;
    end;
  Result := True;
end;

// Обновляет логический индекс первой строки продолжений после чтения DXF или
// автоопределения RepeatTop. Если верхние метки повторяются, визуальные строки
// части начинаются с Title/Header и базовый стиль должен идти от нуля; иначе
// первая визуальная строка продолжения получает стиль по логической позиции.
procedure GDBObjAcadTable.UpdateContinuationRowBaseIndexes;
var
  PartIdx, BaseIdx, RepeatRows, LogicalRows: Integer;
begin
  BaseIdx := FRowCount;
  RepeatRows := 0;
  if FBreakRepeatTopLabels then
    RepeatRows := EffectiveRepeatTopRowCount;
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    if (RepeatRows > 0) and
       PartRepeatsTopLabels(FContinuationParts[PartIdx], RepeatRows) then
    begin
      FContinuationParts[PartIdx].RowBaseIndex := 0;
      LogicalRows := FContinuationParts[PartIdx].RowCount - RepeatRows;
    end
    else
    begin
      FContinuationParts[PartIdx].RowBaseIndex := BaseIdx;
      LogicalRows := FContinuationParts[PartIdx].RowCount;
    end;
    if LogicalRows > 0 then
      Inc(BaseIdx, LogicalRows);
  end;
end;

// Определяет признак повтора верхних меток по данным частей-продолжений.
// Без частей сохраняется значение, прочитанное из DXF. При наличии частей
// признак равен True тогда и только тогда, когда КАЖДАЯ часть повторяет
// ведущие строки-метки главной части (issue #1309). Решение принимается по
// содержимому строк, а не по флагам подавления заголовков.
procedure GDBObjAcadTable.DetectBreakRepeatTopLabels;
var
  RepeatCount, Idx: Integer;
begin
  if Length(FContinuationParts) = 0 then
    Exit;

  // Старые разорванные таблицы приходят из DXF без объекта AcDbTableContent,
  // поэтому явные типы строк отсутствуют, а legacy-логика ограничивает зону
  // меток парой Title+Header. Из-за этого вторая (и последующие) строки
  // заголовка ошибочно считались данными, зона повтора определялась неверно и
  // при изменении высоты разбиения такие строки дублировались (issue #1373).
  // Восстанавливаем типы ведущих строк по числу строк, реально повторяющихся
  // в начале КАЖДОЙ части: строка 0 — Title, строки 1..L-1 — Header, далее —
  // Data. Инференс выполняется только при отсутствии явных типов строк.
  if not FRowStyleTypesExplicit then
  begin
    RepeatCount := DetectRepeatedTopRowCountRaw;
    if (RepeatCount >= 2) and (RepeatCount < FRowCount) then
    begin
      System.SetLength(FRowStyleTypes, FRowCount);
      for Idx := 0 to FRowCount - 1 do
        if Idx = 0 then
          FRowStyleTypes[Idx] := 0
        else if Idx < RepeatCount then
          FRowStyleTypes[Idx] := 1
        else
          FRowStyleTypes[Idx] := 2;
    end;
  end;

  FBreakRepeatTopLabels := EffectiveRepeatTopRowCount > 0;
  UpdateContinuationRowBaseIndexes;
end;

function GDBObjAcadTable.GetBreakRepeatTopLabels: Boolean;
begin
  Result := FBreakRepeatTopLabels;
end;

// Изменение признака повтора верхних меток (issue #1309). Установка в False
// удаляет повторяющиеся ведущие строки-метки из всех частей-продолжений;
// установка в True — добавляет их обратно. В ручном режиме точки вставки
// частей сохраняются (issue #1328).
procedure GDBObjAcadTable.SetBreakRepeatTopLabels(AValue: Boolean);
var
  L: Integer;
  WasManualPosition, WasManualPositionExplicit: Boolean;
  ManualPositions: TAcadTableBreakPositionArray;
begin
  if AValue = FBreakRepeatTopLabels then
    Exit;
  InvalidateRawDXFEntity;
  L := 0;
  if Length(FContinuationParts) > 0 then
  begin
    WasManualPosition := FBreakManualPosition and
      (Length(FContinuationParts) > 0);
    WasManualPositionExplicit := FBreakManualPositionExplicit;
    if WasManualPosition then
      CaptureContinuationInsertPoints(ManualPositions);

    if AValue then
    begin
      // Добавляем ведущие строки-метки главной части в каждую часть.
      L := ComputeTopLabelRowCount;
      FBreakRepeatTopLabels := True;
      if FBreakHeight > 0 then
      begin
        MergeAllContinuationPartsIntoMain;
        SplitMainTableByBreakHeight(FBreakHeight);
      end
      else if L > 0 then
        AddTopLabelsToParts(L);
    end
    else
    begin
      // Удаляем ровно те ведущие строки, которые сейчас реально повторяются.
      L := EffectiveRepeatTopRowCount;
      if L > 0 then
        RemoveTopLabelsFromParts(L);
      FBreakRepeatTopLabels := False;
      if FBreakHeight > 0 then
      begin
        MergeAllContinuationPartsIntoMain;
        SplitMainTableByBreakHeight(FBreakHeight);
      end;
    end;
    if (FBreakHeight <= 0) and (L > 0) then
    begin
      if WasManualPosition then
        SetBreakManualPositionForParts(True)
      else
        RepositionContinuationParts;
    end;
    if WasManualPosition then
      RestoreManualContinuationInsertPoints(
        ManualPositions, WasManualPositionExplicit);
    FGeometryBuilt := False;
  end
  else
    FBreakRepeatTopLabels := AValue;
  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakRepeatTopLabels=%d L=%d parts=%d',
    [Ord(AValue), L, Length(FContinuationParts)], LM_Info);
end;

// Удаляет первые L строк-меток из каждой части-продолжения, которая их
// действительно повторяет. Часть пересобирается из своего диапазона строк
// [L..RowCount-1] через SlicePartFromPart.
procedure GDBObjAcadTable.RemoveTopLabelsFromParts(L: Integer);
var
  PartIdx, Last: Integer;
  Snap: TAcadTablePart;
begin
  if L <= 0 then Exit;
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    if not PartRepeatsTopLabels(FContinuationParts[PartIdx], L) then
      Continue;
    Last := FContinuationParts[PartIdx].RowCount - 1;
    // Независимый снимок части, затем пересоздание из диапазона без меток.
    CopyTablePart(FContinuationParts[PartIdx], Snap);
    ClearPart(FContinuationParts[PartIdx]);
    SlicePartFromPart(Snap, L, Last, FContinuationParts[PartIdx]);
    ClearPart(Snap);
  end;
end;

// Добавляет первые L строк-меток главной части в начало каждой
// части-продолжения, в которой их ещё нет.
procedure GDBObjAcadTable.AddTopLabelsToParts(L: Integer);
var
  PartIdx: Integer;
begin
  if L <= 0 then Exit;
  for PartIdx := 0 to High(FContinuationParts) do
    if not PartRepeatsTopLabels(FContinuationParts[PartIdx], L) then
      PrependTopLabelsToPart(L, FContinuationParts[PartIdx]);
end;

// Вставляет первые L строк-меток главной части в начало части-продолжения.
// Часть пересобирается: сверху — строки-метки главной части (0..L-1), затем
// собственные строки части. Высоты, тексты, формат строк, ячейки и
// объединения переносятся со сдвигом номеров строк на L.
procedure GDBObjAcadTable.PrependTopLabelsToPart(
  L: Integer; var APart: TAcadTablePart);
var
  Src: TAcadTablePart;
  ColCnt, DstRows, RowIdx, ColIdx, Idx, SrcRow: Integer;
begin
  if L <= 0 then Exit;
  ColCnt := FColCount;
  // Независимый снимок части без меток, затем пересоздание части с нуля.
  CopyTablePart(APart, Src);
  ClearPart(APart);

  DstRows := L + Src.RowCount;
  APart.InsertPoint := Src.InsertPoint;
  APart.RowBaseIndex := 0;
  APart.RowCount := DstRows;
  APart.ColCount := ColCnt;
  APart.TableFlags := Src.TableFlags;
  APart.TableStyle := Src.TableStyle;
  APart.TableStyleHandle := Src.TableStyleHandle;
  APart.BreakEnabled := Src.BreakEnabled;
  APart.BreakDirection := Src.BreakDirection;
  APart.BreakRepeatTopLabels := Src.BreakRepeatTopLabels;
  APart.BreakRepeatBottomLabels := Src.BreakRepeatBottomLabels;
  APart.BreakManualPosition := Src.BreakManualPosition;
  APart.BreakManualHeight := Src.BreakManualHeight;
  APart.BreakSpacing := Src.BreakSpacing;
  APart.BreakHeight := Src.BreakHeight;
  APart.RawDXFEntity := '';
  APart.RawDXFEntityValid := False;

  // Высоты строк: сверху L строк главной части, затем строки исходной части.
  APart.RowHeights.initnul;
  for RowIdx := 0 to L - 1 do
    APart.RowHeights.PushBackData(GetRowHeightLocal(RowIdx));
  for RowIdx := 0 to Src.RowHeights.Count - 1 do
    APart.RowHeights.PushBackData(Src.RowHeights.getData(RowIdx));

  // Ширины столбцов — те же, что у части.
  APart.ColWidths.initnul;
  for ColIdx := 0 to Src.ColWidths.Count - 1 do
    APart.ColWidths.PushBackData(Src.ColWidths.getData(ColIdx));

  // Тексты ячеек (плоский массив, индекс = строка * ColCnt + столбец).
  System.SetLength(APart.CellTexts, DstRows * ColCnt);
  if ColCnt > 0 then
    for RowIdx := 0 to DstRows - 1 do
      for ColIdx := 0 to ColCnt - 1 do
      begin
        Idx := RowIdx * ColCnt + ColIdx;
        if RowIdx < L then
        begin
          if (RowIdx * FColCount + ColIdx) <= High(FCellTexts) then
            APart.CellTexts[Idx] := FCellTexts[RowIdx * FColCount + ColIdx]
          else
            APart.CellTexts[Idx] := '';
        end
        else
        begin
          SrcRow := RowIdx - L;
          if (SrcRow * ColCnt + ColIdx) <= High(Src.CellTexts) then
            APart.CellTexts[Idx] := Src.CellTexts[SrcRow * ColCnt + ColIdx]
          else
            APart.CellTexts[Idx] := '';
        end;
      end;

  // Формат строк.
  System.SetLength(APart.Rows, DstRows);
  for RowIdx := 0 to L - 1 do
    if RowIdx <= High(FRows) then
      APart.Rows[RowIdx] := FRows[RowIdx];
  for RowIdx := 0 to Src.RowCount - 1 do
    if RowIdx <= High(Src.Rows) then
      APart.Rows[L + RowIdx] := Src.Rows[RowIdx];

  // Столбцы — те же, что у части.
  System.SetLength(APart.Cols, Length(Src.Cols));
  for ColIdx := 0 to High(Src.Cols) do
    APart.Cols[ColIdx] := Src.Cols[ColIdx];

  // Ячейки (двумерный массив [строка][столбец]).
  System.SetLength(APart.Cells, DstRows);
  for RowIdx := 0 to L - 1 do
    if RowIdx <= High(FCells) then
    begin
      System.SetLength(APart.Cells[RowIdx], Length(FCells[RowIdx]));
      for ColIdx := 0 to High(FCells[RowIdx]) do
        APart.Cells[RowIdx][ColIdx] := FCells[RowIdx][ColIdx];
    end;
  for RowIdx := 0 to Src.RowCount - 1 do
    if RowIdx <= High(Src.Cells) then
    begin
      System.SetLength(APart.Cells[L + RowIdx], Length(Src.Cells[RowIdx]));
      for ColIdx := 0 to High(Src.Cells[RowIdx]) do
        APart.Cells[L + RowIdx][ColIdx] := Src.Cells[RowIdx][ColIdx];
    end;

  // Объединения: сначала объединения главной части, попадающие в строки
  // [0..L-1], затем объединения исходной части со сдвигом строк на L.
  System.SetLength(APart.Merges, 0);
  for RowIdx := 0 to High(FMerges) do
    if (FMerges[RowIdx].Row1 >= 0) and (FMerges[RowIdx].Row2 <= L - 1) then
    begin
      Idx := Length(APart.Merges);
      System.SetLength(APart.Merges, Idx + 1);
      APart.Merges[Idx] := FMerges[RowIdx];
    end;
  for RowIdx := 0 to High(Src.Merges) do
  begin
    Idx := Length(APart.Merges);
    System.SetLength(APart.Merges, Idx + 1);
    APart.Merges[Idx] := Src.Merges[RowIdx];
    Inc(APart.Merges[Idx].Row1, L);
    Inc(APart.Merges[Idx].Row2, L);
  end;

  ClearPart(Src);
end;

// Поглощает продолжение разделённой таблицы как часть этого объекта.
function GDBObjAcadTable.TryMergeContinuation(
  AOther: PGDBObjEntity): Boolean;
var
  PartIdx: Integer;
begin
  Result := False;
  if AOther = nil then Exit;
  if AOther^.GetObjType <> GDBAcadTableID then Exit;

  PartIdx := Length(FContinuationParts);
  System.SetLength(FContinuationParts, PartIdx + 1);
  CaptureTableDataToPart(
    PGDBObjAcadTable(AOther)^, FContinuationParts[PartIdx]);
  // Геометрию нужно перестроить с учётом новой части
  FGeometryBuilt := False;

  // По данным частей определяем, повторяются ли верхние строки-метки
  // (issue #1309): если первые строки каждой части совпадают с верхними
  // строками-метками главной части, то таблица разорвана с RepeatTop=True.
  DetectBreakRepeatTopLabels;
  DetectBreakManualPosition;
  if AcadTableBreakHeightHasValue(FBreakHeight) and
     (not AcadTableBreakHeightHasValue(
        FContinuationParts[PartIdx].BreakHeight)) then
    FContinuationParts[PartIdx].BreakHeight := FBreakHeight;
  DetectBreakManualHeight;

  programlog.LogOutFormatStr(
    'AcadTable: model: TryMergeContinuation merged part %d ' +
    '(rows=%d cols=%d) repeattop=%d manualpos=%d manualheight=%d',
    [PartIdx, FContinuationParts[PartIdx].RowCount,
     FContinuationParts[PartIdx].ColCount,
     Ord(FBreakRepeatTopLabels), Ord(FBreakManualPosition),
     Ord(FBreakManualHeight)], LM_Info);
  Result := True;
end;

// Сохраняет параметры разбиения, прочитанные загрузчиком DXF из XRECORD
// ACAD_ROUNDTRIP_2008_TABLE_ENTITY (issue #1307). Значения только
// сохраняются — геометрия частей уже расставлена по точкам вставки из DXF,
// перестройка не требуется.
function GDBObjAcadTable.SetTableBreakData(
  ASpacing, ABreakHeight: Double): Boolean;
var
  PartIdx: Integer;
begin
  FBreakSpacing := ASpacing;
  FBreakHeight := ABreakHeight;
  for PartIdx := 0 to High(FContinuationParts) do
    if not AcadTableBreakHeightHasValue(
      FContinuationParts[PartIdx].BreakHeight) then
      FContinuationParts[PartIdx].BreakHeight := FBreakHeight;
  DetectBreakManualPosition;
  DetectBreakManualHeight;
  programlog.LogOutFormatStr(
    'AcadTable: model: SetTableBreakData spacing=%g breakheight=%g ' +
    'manualpos=%d manualheight=%d',
    [FBreakSpacing, FBreakHeight, Ord(FBreakManualPosition),
     Ord(FBreakManualHeight)], LM_Info);
  Result := True;
end;

procedure GDBObjAcadTable.SetBreakOptionFlags(
  AManualPosition, AManualHeight: Boolean);
begin
  FBreakFlagsKnown := True;
  FBreakManualPositionExplicit := AManualPosition;
  FBreakManualPosition := AManualPosition;
  FBreakManualHeight := AManualHeight;
  SetBreakManualPositionForParts(AManualPosition);
  SetBreakManualHeightForParts(AManualHeight);
  programlog.LogOutFormatStr(
    'AcadTable: model: SetBreakOptionFlags manualpos=%d manualheight=%d',
    [Ord(FBreakManualPosition), Ord(FBreakManualHeight)], LM_Info);
end;

procedure GDBObjAcadTable.SetDXFRawEntityText(const ARawText: string);
begin
  FRawDXFEntity := ARawText;
  FRawDXFEntityValid := ARawText <> '';
end;

// --- Трансформация объекта (issue #1305, часть 1) ---
// Таблица AcadTable реализует ту же модель трансформации, что и вставка
// блока (uzeentblockinsert): масштаб и поворот хранятся отдельными полями
// (FScale/FRotate) и восстанавливаются из objmatrix, чтобы перенос,
// поворот и масштабирование объекта корректно отображались.

// Раскладывает objmatrix на точку вставки, базис и масштаб.
procedure GDBObjAcadTable.decomposite;
var
  BX, BY, BZ: TzeVector3d;
  T: TzePoint3d;
  Mtr: TzeTypedMatrix4d;
begin
  Mtr := objMatrix;
  BX := Mtr.mtr.v[0].Slice;
  BY := Mtr.mtr.v[1].Slice;
  BZ := Mtr.mtr.v[2].Slice;
  T := Mtr.mtr.v[3].Slice.asPoint3d;
  Local := GetPointInOCSByBasis(BX, BY, BZ, T, FScale);
end;

// Поворачивает objmatrix вокруг оси Z на угол r.
procedure GDBObjAcadTable.setrot(r: Double);
var
  m1: TzeTypedMatrix4d;
begin
  m1 := CreateRotationMatrixZ(r);
  objMatrix := MatrixMultiply(m1, objMatrix);
end;

// Восстанавливает точку вставки, масштаб и угол поворота из objmatrix
// после трансформации (перенос/поворот/масштабирование).
procedure GDBObjAcadTable.ReCalcFromObjMatrix;
var
  ox, tv: TzeVector3d;
begin
  inherited;
  decomposite;
  ox := GetXfFromZ(Local.basis.oz);
  tv := Local.basis.ox;
  if FScale.x < -eps then
    tv := tv * -1;
  FRotate := scalardot(tv, ox);
  FRotate := arccos(FRotate);
  if scalardot(tv, VectorDot(Local.basis.oz,
       GetXfFromZ(Local.basis.oz))) < -eps then
    FRotate := 2 * pi - FRotate;
end;

// Строит objmatrix с учётом точки вставки, поворота и масштаба.
procedure GDBObjAcadTable.CalcObjMatrix(pdrawing: PTDrawingDef = nil);
var
  m1: TzeTypedMatrix4d;
begin
  inherited CalcObjMatrix(pdrawing);
  setrot(FRotate);
  m1 := CreateScaleMatrix(FScale);
  objMatrix := MatrixMultiply(m1, objMatrix);
end;

// Сохраняет масштаб и поворот в опорный объект (real-time трансформация).
procedure GDBObjAcadTable.rtsave(refp: Pointer);
begin
  inherited;
  PGDBObjAcadTable(refp)^.FRotate := FRotate;
  PGDBObjAcadTable(refp)^.FScale := FScale;
  PGDBObjAcadTable(refp)^.FBreakManualPositionExplicit :=
    FBreakManualPositionExplicit;
  PGDBObjAcadTable(refp)^.InvalidateRawDXFEntity;
end;

procedure GDBObjAcadTable.TransformAt(
  p: PGDBObjEntity; t_matrix: PzeTypedMatrix4d);
begin
  inherited TransformAt(p, t_matrix);
  InvalidateRawDXFEntity;
end;

procedure GDBObjAcadTable.addcontrolpoints(tdesc: Pointer);
var
  PDesc: controlpointdesc;
  PartIdx, ControlPointCount: Integer;
begin
  DetectBreakManualPosition;
  DetectBreakManualHeight;

  ControlPointCount := 1;
  if GetBreakEnabled then
  begin
    Inc(ControlPointCount);
    if FBreakManualHeight then
      Inc(ControlPointCount, Length(FContinuationParts));
  end;
  if FBreakManualPosition then
    Inc(ControlPointCount, Length(FContinuationParts));
  PSelectedObjDesc(tdesc)^.pcontrolpoint^.init(ControlPointCount);

  PDesc.selected := False;
  PDesc.PDrawable := nil;
  PDesc.attr := [];
  PDesc.dcoord := cP3d__0__0__0;
  PDesc.vn := 0;
  PDesc.pointtype := os_point;
  PDesc.worldcoord := self.P_insert_in_WCS;
  PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(PDesc);

  if GetBreakEnabled then
  begin
    PDesc.attr := [CPA_Strech];
    PDesc.vertexnum := CAcadTableBreakHeightGripVertexBase;
    PDesc.worldcoord := BreakHeightGripPointInWCS(0);
    PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(PDesc);

    if FBreakManualHeight then
      for PartIdx := 0 to High(FContinuationParts) do
      begin
        PDesc.vertexnum :=
          CAcadTableBreakHeightGripVertexBase + PartIdx + 1;
        PDesc.worldcoord := BreakHeightGripPointInWCS(PartIdx + 1);
        PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(PDesc);
      end;
  end;

  PDesc.attr := [CPA_Strech];
  if FBreakManualPosition then
    for PartIdx := 0 to High(FContinuationParts) do
    begin
      PDesc.vertexnum := PartIdx + 1;
      PDesc.worldcoord := ContinuationPartGripPointInWCS(PartIdx);
      PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(PDesc);
    end;
end;

procedure GDBObjAcadTable.remaponecontrolpoint(
  pdesc: pcontrolpointdesc; ProjectProc: GDBProjectProc);
var
  PartIdx, PartNumber: Integer;
  TV: TzePoint3d;
begin
  if pdesc^.pointtype = os_polymin then
  begin
    if DecodeBreakHeightGripVertex(pdesc^.vertexnum, PartNumber) then
    begin
      pdesc^.worldcoord := BreakHeightGripPointInWCS(PartNumber);
      ProjectProc(pdesc^.worldcoord, TV);
      pdesc^.dispcoord := TV.Slice.asPoint2i;
      Exit;
    end;

    PartIdx := pdesc^.vertexnum - 1;
    if (PartIdx >= 0) and (PartIdx <= High(FContinuationParts)) then
    begin
      pdesc^.worldcoord := ContinuationPartGripPointInWCS(PartIdx);
      ProjectProc(pdesc^.worldcoord, TV);
      pdesc^.dispcoord := TV.Slice.asPoint2i;
      Exit;
    end;
  end;

  inherited remaponecontrolpoint(pdesc, ProjectProc);
end;

procedure GDBObjAcadTable.rtmodifyonepoint(const rtmod: TRTModifyData);
var
  PartIdx, PartNumber: Integer;
  NewHeight: Double;
  NewGripPoint, LocalOffset: TzePoint3d;
  M: TzeTypedMatrix4d;
begin
  if rtmod.point.pointtype = os_polymin then
  begin
    if DecodeBreakHeightGripVertex(rtmod.point.vertexnum, PartNumber) then
    begin
      NewGripPoint := rtmod.point.worldcoord + rtmod.dist.asVector;
      M := objMatrix;
      MatrixInvert(M);
      LocalOffset := VectorTransform3D(NewGripPoint, M);
      NewHeight := BreakHeightFromGripLocalOffset(
        PartNumber, LocalOffset);

      if (PartNumber = 0) and (not FBreakManualHeight) then
      begin
        SetBreakHeight(NewHeight);
        Exit;
      end;

      InvalidateRawDXFEntity;
      if PartNumber = 0 then
        FBreakHeight := NewHeight
      else
      begin
        PartIdx := PartNumber - 1;
        if (PartIdx >= 0) and (PartIdx <= High(FContinuationParts)) then
          FContinuationParts[PartIdx].BreakHeight := NewHeight;
      end;
      FBreakManualHeight := True;
      ResplitByCurrentBreakHeights;
      FGeometryBuilt := False;
      Exit;
    end;

    PartIdx := rtmod.point.vertexnum - 1;
    if (PartIdx >= 0) and (PartIdx <= High(FContinuationParts)) then
    begin
      NewGripPoint := rtmod.point.worldcoord + rtmod.dist.asVector;
      M := objMatrix;
      MatrixInvert(M);
      LocalOffset := VectorTransform3D(NewGripPoint, M);
      FContinuationParts[PartIdx].InsertPoint :=
        FInsertPoint + LocalOffset.asVector;
      FBreakManualPosition := True;
      FBreakManualPositionExplicit := True;
      SetBreakManualPositionForParts(True);
      FGeometryBuilt := False;
      InvalidateRawDXFEntity;
      Exit;
    end;
  end;

  inherited rtmodifyonepoint(rtmod);
end;

// Вычисляет bounding box
procedure GDBObjAcadTable.getoutbound(var DC: TDrawContext);
var
  TotalWidthVal, TotalHeightVal: Double;
  MinX, MinY, MaxX, MaxY: Double;
  PartIdx: Integer;
  BaseX, BaseY, PartW, PartH: Double;
  PartMinX, PartMaxX, PartMinY, PartMaxY: Double;
begin
  // Если геометрия уже построена, берём bounding box из дочерних
  // объектов: их WCS-координаты учитывают перенос/поворот/масштаб-
  // ирование objmatrix, поэтому рамка следует за трансформацией
  // (issue #1305, часть 1).
  if ConstObjArray.Count > 0 then
  begin
    vp.BoundingBox := ConstObjArray.getoutbound(DC);
    Exit;
  end;

  if (FRowCount <= 0) or (FColCount <= 0) then
  begin
    vp.BoundingBox.LBN :=
      Local.P_insert + TzePoint3d.make(-0.01, -0.01, 0).asVector;
    vp.BoundingBox.RTF :=
      Local.P_insert + TzePoint3d.make(0.01, 0.01, 0).asVector;
    Exit;
  end;

  TotalWidthVal := GetTotalWidth;
  TotalHeightVal := GetTotalHeight;
  MinX := Local.P_insert.x;
  MaxX := Local.P_insert.x + TotalWidthVal;
  MinY := Local.P_insert.y - TotalHeightVal;
  MaxY := Local.P_insert.y;

  // Расширяем bounding box на все части-продолжения (issue #1300)
  for PartIdx := 0 to High(FContinuationParts) do
  begin
    BaseX := FContinuationParts[PartIdx].InsertPoint.x - FInsertPoint.x;
    BaseY := FContinuationParts[PartIdx].InsertPoint.y - FInsertPoint.y;
    PartW := uzeacadtable_layout.GetTotalWidth(
      FContinuationParts[PartIdx].ColCount,
      FContinuationParts[PartIdx].ColWidths);
    PartH := uzeacadtable_layout.GetTotalHeight(
      FContinuationParts[PartIdx].RowCount,
      FContinuationParts[PartIdx].RowHeights);
    PartMinX := Local.P_insert.x + BaseX;
    PartMaxX := PartMinX + PartW;
    PartMaxY := Local.P_insert.y + BaseY;
    PartMinY := PartMaxY - PartH;
    if PartMinX < MinX then MinX := PartMinX;
    if PartMaxX > MaxX then MaxX := PartMaxX;
    if PartMinY < MinY then MinY := PartMinY;
    if PartMaxY > MaxY then MaxY := PartMaxY;
  end;

  vp.BoundingBox.LBN :=
    TzePoint3d.make(MinX, MinY, Local.P_insert.z);
  vp.BoundingBox.RTF :=
    TzePoint3d.make(MaxX, MaxY, Local.P_insert.z);
end;

// --- Методы сущности ---

procedure GDBObjAcadTable.BuildGeometry(
  var ADrawing: TDrawingDef);
var
  DC: TDrawContext;
begin
  programlog.LogOutFormatStr(
    'AcadTable: model: BuildGeometry START built=%d ' +
    'rows=%d cols=%d',
    [Ord(FGeometryBuilt), FRowCount, FColCount], LM_Info);

  if not FGeometryBuilt then
  begin
    // Применяем DXF-стиль таблицы
    uzeacadtable_stylemanager.ApplyDXFTableStyle(
      FTableStyle, FTableStyleHandle, ADrawing);

    DC := ADrawing.CreateDrawingRC;
    BuildVisualRepresentation(ADrawing, DC);
    FGeometryBuilt := True;
    inherited BuildGeometry(ADrawing);
  end;

  programlog.LogOutStr(
    'AcadTable: model: BuildGeometry END', LM_Info);
end;

procedure GDBObjAcadTable.FormatEntity(
  var ADrawing: TDrawingDef; var ADC: TDrawContext;
  AStage: TEFStages = EFAllStages);
begin
  if EFCalcEntityCS in AStage then
  begin
    if Assigned(EntExtensions) then
      EntExtensions.RunOnBeforeEntityFormat(
        @Self, ADrawing, ADC);
    CalcObjMatrix;
  end;
  // Раскладка таблицы в OCS строится один раз и защищена флагом
  // FGeometryBuilt. Сами дочерние объекты (линии и тексты) каждый
  // кадр переформатируются ниже, чтобы перенос/поворот/масштаб-
  // ирование объекта отображались — дочерние объекты пересчитывают
  // свои WCS-координаты из objmatrix владельца (issue #1305, часть 1).
  BuildGeometry(ADrawing);
  ConstObjArray.FormatEntity(ADrawing, ADC, AStage);
  if EFCalcEntityCS in AStage then
  begin
    getoutbound(ADC);
    // Перестраиваем пространственное дерево по новым координатам
    // дочерних объектов, иначе перерисовки на месте не происходит.
    inherited BuildGeometry(ADrawing);
  end;
  CalcActualVisible(ADC.DrawingContext.VActuality);
  if EFDraw in AStage then
  begin
    if Assigned(EntExtensions) then
      EntExtensions.RunOnAfterEntityFormat(
        @Self, ADrawing, ADC);
  end;
end;

function GDBObjAcadTable.IsStagedFormatEntity: Boolean;
begin
  Result := True;
end;

function GDBObjAcadTable.DXFDelayedBuildGeometry: Boolean;
begin
  Result := True;
end;

procedure CopyVectorToDXFDoubleArray(
  const ASource: TZctnrVectorDouble;
  var ADest: TAcadTableDoubleArray);
var
  Idx: Integer;
begin
  System.SetLength(ADest, ASource.Count);
  for Idx := 0 to ASource.Count - 1 do
    ADest[Idx] := ASource.getData(Idx);
end;

procedure CopyCellTextArray(
  const ASource: TTableTextArray;
  var ADest: TTableTextArray);
var
  Idx: Integer;
begin
  System.SetLength(ADest, Length(ASource));
  for Idx := 0 to High(ASource) do
    ADest[Idx] := ASource[Idx];
end;

procedure CopyCellArray(
  const ASource: TTableCellArray;
  var ADest: TTableCellArray);
var
  RowIdx, ColIdx: Integer;
begin
  System.SetLength(ADest, Length(ASource));
  for RowIdx := 0 to High(ASource) do
  begin
    System.SetLength(ADest[RowIdx], Length(ASource[RowIdx]));
    for ColIdx := 0 to High(ASource[RowIdx]) do
      ADest[RowIdx][ColIdx] := ASource[RowIdx][ColIdx];
  end;
end;

procedure CopyMergeArray(
  const ASource: TMergeRangeArray;
  var ADest: TMergeRangeArray);
var
  Idx: Integer;
begin
  System.SetLength(ADest, Length(ASource));
  for Idx := 0 to High(ASource) do
    ADest[Idx] := ASource[Idx];
end;

procedure GDBObjAcadTable.FillDXFWritePartFromSelf(
  var APart: TAcadTableDXFWritePart);
begin
  APart.HandleKey := @Self;
  if vp.Layer <> nil then
    APart.LayerName := vp.Layer^.Name
  else
    APart.LayerName := '0';
  APart.Color := vp.Color;
  APart.LineWeight := vp.lineweight;
  if vp.LineType <> nil then
    APart.LineTypeName := vp.LineType^.Name
  else
    APart.LineTypeName := '';
  APart.LineTypeScale := vp.LineTypeScale;

  // FInsertPoint содержит исходную точку DXF/программного построения, а
  // интерактивный перенос обновляет Local.P_insert через objmatrix.
  // Сохраняем фактическое положение объекта после трансформаций (issue #1396).
  APart.InsertPoint := Local.P_insert;
  APart.Direction.x := Cos(FRotate);
  APart.Direction.y := Sin(FRotate);
  APart.Direction.z := 0;
  APart.RowCount := FRowCount;
  APart.ColCount := FColCount;
  CopyVectorToDXFDoubleArray(FRowHeights, APart.RowHeights);
  CopyVectorToDXFDoubleArray(FColWidths, APart.ColWidths);
  CopyCellTextArray(FCellTexts, APart.CellTexts);
  CopyCellArray(FCells, APart.Cells);
  CopyMergeArray(FMerges, APart.Merges);
  APart.TableStyleHandle := FTableStyleHandle;
  // Имя стиля нужно писателю DXF, чтобы получить актуальный (перенумерованный)
  // хэндл TABLESTYLE для ссылок 342/340 (issue #1409).
  APart.TableStyleName := FTableStyle.Name;
  APart.BlockName := FMainPartBlockName;
  APart.TableFlags := FTableFlags;
  APart.BreakEnabled := GetBreakEnabled;
  APart.BreakDirection := FBreakDirection;
  APart.BreakRepeatTopLabels := FBreakRepeatTopLabels;
  APart.BreakRepeatBottomLabels := FBreakRepeatBottomLabels;
  APart.BreakManualPosition := FBreakManualPosition;
  APart.BreakManualHeight := FBreakManualHeight;
  APart.BreakSpacing := FBreakSpacing;
  APart.BreakHeight := FBreakHeight;
end;

procedure GDBObjAcadTable.FillDXFWritePartFromContinuation(
  const ASource: TAcadTablePart; var APart: TAcadTableDXFWritePart);
begin
  FillDXFWritePartFromSelf(APart);
  APart.HandleKey := nil;
  APart.InsertPoint := ASource.InsertPoint;
  APart.RowCount := ASource.RowCount;
  APart.ColCount := ASource.ColCount;
  CopyVectorToDXFDoubleArray(ASource.RowHeights, APart.RowHeights);
  CopyVectorToDXFDoubleArray(ASource.ColWidths, APart.ColWidths);
  CopyCellTextArray(ASource.CellTexts, APart.CellTexts);
  CopyCellArray(ASource.Cells, APart.Cells);
  CopyMergeArray(ASource.Merges, APart.Merges);
  APart.TableStyleHandle := ASource.TableStyleHandle;
  if ASource.TableStyle.Name <> '' then
    APart.TableStyleName := ASource.TableStyle.Name;
  APart.BlockName := ASource.BlockName;
  APart.TableFlags := ASource.TableFlags;
  APart.BreakEnabled := ASource.BreakEnabled;
  APart.BreakDirection := ASource.BreakDirection;
  APart.BreakRepeatTopLabels := ASource.BreakRepeatTopLabels;
  APart.BreakRepeatBottomLabels := ASource.BreakRepeatBottomLabels;
  APart.BreakManualPosition := FBreakManualPosition;
  APart.BreakManualHeight := ASource.BreakManualHeight;
  APart.BreakSpacing := ASource.BreakSpacing;
  APart.BreakHeight := ASource.BreakHeight;
end;

procedure GDBObjAcadTable.BuildDXFContinuationWriteParts(
  var AParts: TAcadTableDXFWritePartArray);
var
  PartIdx: Integer;
begin
  DetectBreakManualPosition;
  DetectBreakManualHeight;
  System.SetLength(AParts, Length(FContinuationParts));
  for PartIdx := 0 to High(FContinuationParts) do
    FillDXFWritePartFromContinuation(
      FContinuationParts[PartIdx], AParts[PartIdx]);
end;

procedure GDBObjAcadTable.SaveToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext);
var
  DXFPart: TAcadTableDXFWritePart;
begin
  DetectBreakManualPosition;
  DetectBreakManualHeight;
  FillDXFWritePartFromSelf(DXFPart);
  uzeacadtable_dxf_write.WriteAcadTableToDXF(
    AOutStream, ADrawing, AIODXFContext, DXFPart, 0);
end;

procedure GDBObjAcadTable.SaveToDXFFollow(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext);
var
  MainPart: TAcadTableDXFWritePart;
  Parts: TAcadTableDXFWritePartArray;
begin
  DetectBreakManualPosition;
  DetectBreakManualHeight;
  FillDXFWritePartFromSelf(MainPart);
  BuildDXFContinuationWriteParts(Parts);
  uzeacadtable_dxf_write.WriteAcadTableContinuationPartsToDXF(
    AOutStream, ADrawing, AIODXFContext, MainPart, Parts);
end;

procedure GDBObjAcadTable.DXFOut(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext);
var
  RawParts: array of String;
  PartIdx: Integer;
begin
  if CanSaveRawDXFEntity then
  begin
    // Перед сохранением гарантируем, что имя стиля таблицы определено.
    // Для raw-таблиц (issue #1339) BuildGeometry мог ещё не выполняться
    // (например, при пакетном "Сохранить как" без отрисовки), поэтому
    // FTableStyle.Name остаётся пустым. Без имени невозможно перенумеровать
    // ссылку 342 на актуальный хэндл TABLESTYLE — и таблица сохраняется со
    // ссылкой на старый (чужой после перенумерации) хэндл.
    if (FTableStyle.Name = '') and (FTableStyleHandle <> '') then
      uzeacadtable_stylemanager.ApplyDXFTableStyle(
        FTableStyle, FTableStyleHandle, ADrawing);

    System.SetLength(RawParts, Length(FContinuationParts));
    for PartIdx := 0 to High(FContinuationParts) do
      RawParts[PartIdx] := FContinuationParts[PartIdx].RawDXFEntity;
    // Признаки ручного управления разрывами должны попасть в roundtrip-запись
    // (issue #1339), иначе при пересохранении BreakOption теряет манульные биты.
    DetectBreakManualPosition;
    DetectBreakManualHeight;
    if uzeacadtable_dxf_write.WriteRawAcadTablePartsToDXF(
      AOutStream, AIODXFContext, FRawDXFEntity, RawParts,
      FBreakSpacing, FBreakHeight,
      FBreakManualPosition, FBreakManualHeight, Self.TableStyleName) then
      Exit;
  end;

  inherited DXFOut(AOutStream, ADrawing, AIODXFContext);
end;

function GDBObjAcadTable.Clone(
  AOwn: Pointer): PGDBObjEntity;
var
  NewTable: PGDBObjAcadTable;
  Idx, Idx2: Integer;
begin
  GetMem(Pointer(NewTable), SizeOf(GDBObjAcadTable));
  NewTable^.initnul(AOwn);
  CopyVPto(NewTable^);
  CopyExtensionsTo(NewTable^);

  NewTable^.FInsertPoint := FInsertPoint;
  NewTable^.FRowCount := FRowCount;
  NewTable^.FColCount := FColCount;
  NewTable^.FForceDataStyleAllRows := FForceDataStyleAllRows;
  NewTable^.FTableStyleHandle := FTableStyleHandle;
  NewTable^.FTableFlags := FTableFlags;
  NewTable^.FBreakEnabled := FBreakEnabled;
  NewTable^.FBreakDirection := FBreakDirection;
  NewTable^.FBreakRepeatTopLabels := FBreakRepeatTopLabels;
  NewTable^.FBreakRepeatBottomLabels :=
    FBreakRepeatBottomLabels;
  NewTable^.FBreakManualPosition := FBreakManualPosition;
  NewTable^.FBreakManualPositionExplicit := FBreakManualPositionExplicit;
  NewTable^.FBreakManualHeight := FBreakManualHeight;
  NewTable^.FBreakFlagsKnown := FBreakFlagsKnown;
  NewTable^.FBreakSpacing := FBreakSpacing;
  NewTable^.FBreakHeight := FBreakHeight;
  // Трансформация объекта (issue #1305, часть 1)
  NewTable^.FScale := FScale;
  NewTable^.FRotate := FRotate;
  NewTable^.FRawDXFEntity := FRawDXFEntity;
  NewTable^.FRawDXFEntityValid := FRawDXFEntityValid;

  for Idx := 0 to FRowHeights.Count - 1 do
    NewTable^.FRowHeights.PushBackData(
      FRowHeights.getData(Idx));
  for Idx := 0 to FColWidths.Count - 1 do
    NewTable^.FColWidths.PushBackData(
      FColWidths.getData(Idx));

  System.SetLength(NewTable^.FCellTexts,
    Length(FCellTexts));
  for Idx := 0 to High(FCellTexts) do
    NewTable^.FCellTexts[Idx] := FCellTexts[Idx];

  System.SetLength(NewTable^.FRowStyleTypes, Length(FRowStyleTypes));
  for Idx := 0 to High(FRowStyleTypes) do
    NewTable^.FRowStyleTypes[Idx] := FRowStyleTypes[Idx];
  NewTable^.FRowStyleTypesExplicit := FRowStyleTypesExplicit;

  NewTable^.FTableStyle := FTableStyle;
  NewTable^.Local := Local;
  NewTable^.objMatrix := objMatrix;
  NewTable^.P_insert_in_WCS := P_insert_in_WCS;

  System.SetLength(NewTable^.FRows, Length(FRows));
  for Idx := 0 to High(FRows) do
    NewTable^.FRows[Idx] := FRows[Idx];

  System.SetLength(NewTable^.FCols, Length(FCols));
  for Idx := 0 to High(FCols) do
    NewTable^.FCols[Idx] := FCols[Idx];

  System.SetLength(NewTable^.FCells, Length(FCells));
  for Idx := 0 to High(FCells) do
  begin
    System.SetLength(NewTable^.FCells[Idx],
      Length(FCells[Idx]));
    for Idx2 := 0 to High(FCells[Idx]) do
      NewTable^.FCells[Idx][Idx2] :=
        FCells[Idx][Idx2];
  end;

  System.SetLength(NewTable^.FMerges, Length(FMerges));
  for Idx := 0 to High(FMerges) do
    NewTable^.FMerges[Idx] := FMerges[Idx];

  // Глубокое копирование частей-продолжений (issue #1300)
  System.SetLength(NewTable^.FContinuationParts,
    Length(FContinuationParts));
  for Idx := 0 to High(FContinuationParts) do
    NewTable^.CopyTablePart(
      FContinuationParts[Idx],
      NewTable^.FContinuationParts[Idx]);

  NewTable^.bp.ListPos.Owner := AOwn;
  Result := NewTable;
end;

function GDBObjAcadTable.GetObjType: TObjID;
begin
  Result := GDBAcadTableID;
end;

function GDBObjAcadTable.GetObjTypeName: String;
begin
  Result := ObjN_GDBObjAcadTable;
end;

// --- Функции выделения памяти ---

function AllocAcadTable: Pointer;
begin
  GetMem(Result, SizeOf(GDBObjAcadTable));
end;

function AllocAndInitAcadTable(
  AOwner: PGDBObjGenericWithSubordinated): PGDBObjAcadTable;
begin
  GetMem(Pointer(Result), SizeOf(GDBObjAcadTable));
  Result^.initnul(AOwner);
  Result^.bp.ListPos.Owner := AOwner;
end;

// Before-save callback: перед записью DXF генерирует персональные блоки для
// частей каждой таблицы, идущей модельным путём записи, чтобы AutoCAD
// отрисовал разорванную таблицу как несколько отдельных таблиц (issue #1381).
// Указатели на таблицы собираем заранее: EnsureSplitPartBlocks вызывает
// create() у BlockDefArray, что может привести к grow() и перевыделению
// массива блоков; сами AcadTable-объекты при этом не перемещаются.
procedure EnsureAcadTableSplitBlocksBeforeSave(var drawing: TSimpleDrawing);
var
  pArray: PGDBObjEntityTreeArray;
  Ent: PGDBObjEntity;
  I, N, Count: Integer;
  Tables: array of PGDBObjAcadTable;
begin
  if drawing.pObjRoot = nil then
    Exit;
  pArray := @drawing.pObjRoot^.ObjArray;
  N := pArray^.Count;
  SetLength(Tables, N);
  Count := 0;
  for I := 0 to N - 1 do
  begin
    Ent := PGDBObjEntity(pArray^.GetData(I));
    if (Ent <> nil) and (Ent^.GetObjType = GDBAcadTableID) then
    begin
      Tables[Count] := PGDBObjAcadTable(Ent);
      Inc(Count);
    end;
  end;
  for I := 0 to Count - 1 do
    Tables[I]^.EnsureSplitPartBlocks(drawing);
end;

initialization
  // Регистрация сущности ACAD_TABLE с привязкой к DXF-имени
  RegisterDXFEntity(
    GDBAcadTableID,
    'ACAD_TABLE',
    'AcadTable',
    @AllocAcadTable,
    @AllocAndInitAcadTable
  );

  // Регистрируем before-save callback генерации блоков частей таблицы
  // (issue #1381). Выполняется до записи секций BLOCKS/BLOCK_RECORD, поэтому
  // сгенерированные блоки попадают в файл и в карту «имя блока → хэндл».
  RegisterBeforeSaveDxfProc(@EnsureAcadTableSplitBlocksBeforeSave);

end.
