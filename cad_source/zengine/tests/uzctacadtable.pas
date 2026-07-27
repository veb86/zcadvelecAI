{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*****************************************************************************
}
unit uzctacadtable;
{$Codepage UTF8}
{$Mode delphi}{$H+}

interface

uses
  SysUtils,
  Math,
  Classes,
  fpcunit,
  testregistry,
  Interfaces,
  uzeffdxf,
  uzctnrVectorBytesStream,
  uzeffdxfsupport,
  uzedrawingsimple,
  uzeffmanager,
  uzgldrawcontext,
  uzeconsts,
  uzeTypes,
  gzctnrVectorTypes,
  uzegeometry,
  uzegeometrytypes,
  uzeentity,
  uzesnap,
  UGDBSelectedObjArray,
  UGDBControlPointArray,
  uzglviewareadata,
  uzeentmtext,
  uzeentline,
  uzeentgenericsubentry,
  uzeentsubordinated,
  uzeentcomplex,
  UGDBVisibleTreeArray,
  uzeenttext,
  uzeentblockinsert,
  uzeenttable,
  UGDBObjBlockdefArray,
  uzeblockdef,
  uzeacadtable_types,
  uzeacadtable_model,
  uzeacadtable_dxf_write;

type
  TAcadTableStyleTest = class(TTestCase)
  published
    procedure LoadsCellTextStylesFromDXFTableStyle;
    // issue #1330: fixed text height from the referenced text style should
    // be used for imported AcadTable cells.
    procedure LoadsFixedTextHeightFromDXFTextStyle;
    procedure LoadsBreakSettingsFromDXF;
    procedure LoadsBreakSettingsFromSecondSample;
    procedure RendersBrokenTableAsSeparatedFragments;
    procedure LoadsSplitTableAsSingleMergedObject;
    // issue #1381: ZCAD сохраняет части разорванной таблицы под приватным
    // маркером ZCAD_SPLIT_TABLE_ENTITY (AutoCAD его не пересобирает). Чтение
    // должно по этому маркеру восстанавливать единую разорванную таблицу, как
    // и при «родном» маркере AutoCAD.
    procedure ReadsSplitTableFromPrivateZcadMarker;
    procedure LoadsIssue1334BreakTableAsSingleMergedObject;
    procedure AppliesTableStyleToContinuationParts;
    // issue #1317: сохранение AcadTable в DXF должно писать структурированные
    // ACAD_TABLE-сущности, включая части-продолжения разделённой таблицы.
    procedure SavesSplitAcadTableToStructuredDXF;
    // issue #1381: при сохранении по модельному пути (raw-DXF инвалидирован
    // редактированием) для главной части и каждой части-продолжения должен
    // создаваться отдельный анонимный блок с геометрией этой части. Без него
    // AutoCAD рисует части пустыми/«разорванной» цельной таблицей.
    procedure GeneratesPerPartBlocksForModelPathSplitTable;
    // issue #1339: при сохранении сырой (raw) ACAD_TABLE ссылки на хэндлы
    // (330 владелец, 342 стиль таблицы, 343 анонимный блок) должны
    // перенумеровываться под актуальный файл, а блок расширенного словаря
    // (102/ACAD_XDICTIONARY) — удаляться, чтобы не оставлять висячих ссылок.
    procedure RemapsRawAcadTableHandlesOnSave;
    // issue #1339: сквозная проверка. Загружаем реальный файл с raw-таблицей,
    // у которой имя стиля ещё не разрешено (BuildGeometry не выполнялся), и
    // проверяем, что DXFOut сам разрешает имя стиля по хэндлу и перенумеровывает
    // ссылку 342 на актуальный хэндл TABLESTYLE, а не оставляет старый хэндл,
    // который после перенумерации указывал на чужой объект ("aits"/блок).
    procedure ResolvesRawTableStyleNameAndRemaps342OnDXFOut;
    // issue #1305, часть 1: трансформация (перенос) должна перестраивать
    // визуальное представление таблицы.
    procedure TransformationMovesRenderedTable;
    // issue #1332: временный клон для интерактивного редактирования должен
    // начинать отрисовку в текущем положении таблицы, а не в начале координат.
    procedure ClonedPreviewKeepsRenderedTableAtSourceLocation;
    // issue #1375: временный клон для интерактивного редактирования должен
    // сохранять явные типы строк Title/Header/Data, иначе предпросмотр ручки
    // высоты разбиения возвращается к legacy-паре Title+Header.
    procedure ClonedPreviewPreservesExplicitRowStyleTypes;
    // issue #1305, часть 2b: снятие признака разрыва объединяет
    // части-продолжения в единую непрерывную таблицу.
    procedure ClearingBreakMergesContinuationParts;
    // issue #1313: снятие разбиения должно удалить повторённые верхние
    // метки из частей-продолжений.
    procedure ClearingBreakRemovesRepeatedTopLabels;
    // issue #1307, часть 1: интервал между частями (Break spacing)
    // читается из round-trip данных DXF (≈0.99).
    procedure LoadsBreakSpacingFromDXF;
    // issue #1307, часть 2: высота разбиения (Break height) читается
    // из round-trip данных DXF (≈2.0486).
    procedure LoadsBreakHeightFromDXF;
    // issue #1307, часть 2: изменение высоты разбиения пересегментирует
    // таблицу с автоопределением числа частей.
    procedure ChangingBreakHeightResegmentsTable;
    // issue #1307, часть 1: изменение интервала смещает части-продолжения
    // (меняется ширина отрисованного представления).
    procedure ChangingBreakSpacingRepositionsParts;
    // issue #1315: изменение направления разбиения должно перемещать
    // части-продолжения влево или вниз от главной таблицы.
    procedure ChangingBreakDirectionRepositionsParts;
    // issue #1320: если части разорванной таблицы стоят не по BreakDirection
    // и BreakSpacing, то BreakManualPosition должен определяться как True.
    procedure DetectsBreakManualPositionFromPartOffsets;
    // issue #1320: при ручном положении разорванной таблицы каждая часть-
    // продолжение должна иметь собственную ручку положения.
    procedure ManualBreakPositionAddsContinuationPartGrips;
    // issue #1320: ручное положение должно включаться и отключаться через
    // свойство, используемое инспектором объектов.
    procedure TogglingBreakManualPositionUpdatesContinuationPartGrips;
    // issue #1328: при ручном положении изменение направления, интервала и
    // высоты разбиения не должно сбрасывать положение частей.
    procedure ManualBreakPositionSurvivesBreakLayoutPropertyChanges;
    // issue #1321: у разбитой таблицы всегда должна быть ручка высоты
    // разбиения внизу первой части.
    procedure BreakEnabledAddsFirstPartBreakHeightGrip;
    // issue #1321: ручная высота разбиения включает ручки у каждой части.
    procedure ManualBreakHeightAddsGripForEveryTablePart;
    // issue #1321: ручное управление высотой должно включаться и
    // отключаться через свойство инспектора объектов.
    procedure TogglingBreakManualHeightUpdatesPartGrips;
    // issue #1321: перетаскивание ручки высоты части-продолжения должно
    // перераспределять строки этой части, а не только менять сохранённое поле.
    procedure DraggingContinuationBreakHeightGripResegmentsPart;
    // issue #1309, часть 1: при загрузке разорванной таблицы признак повтора
    // верхних меток определяется по содержимому частей-продолжений.
    procedure DetectsBreakRepeatTopOnLoad;
    // issue #1309, часть 2: снятие признака повтора удаляет повторяющиеся
    // строки-метки из всех частей, а возврат — добавляет их обратно.
    procedure TogglingBreakRepeatTopAddsAndRemovesLabelRows;
    // issue #1375: повторяемая верхняя зона определяется всеми ведущими
    // строками Title/Header до первой Data, а не фиксированной парой
    // Title+Header.
    procedure BreakRepeatTopUsesAllLeadingTitleHeaderRows;
    // issue #1311: строка данных, смещённая на место удалённых верхних меток,
    // должна сохранять своё форматирование.
    procedure ClearingBreakRepeatTopKeepsDataRowFormatting;
    // issue #1309: неразорванная таблица не имеет повтора верхних меток.
    procedure NonBrokenTableHasNoBreakRepeatTop;
    // issue #1344: исходный хэндл сущности raw-таблицы (группа 5) может
    // совпасть с хэндлом, уже выданным анонимному блоку таблицы при записи
    // секции BLOCKS. При сохранении таблица и все её части-продолжения
    // должны получать свежие хэндлы из общего счётчика документа, а
    // round-trip XRECORD (360/361/330) — ссылаться на эти же новые хэндлы.
    procedure RenumbersRawAcadTableEntityHandlesToAvoidCollision;
    // issue #1357: программное построение ACAD_TABLE из текстов ячеек
    // (как при вставке из редактора электронных таблиц) должно задавать
    // размеры таблицы и отрисовывать текст каждой непустой ячейки.
    procedure BuildsAcadTableFromCellTexts;
    procedure ExposesAcadTableDataForSpreadsheetEditing;
    procedure UpdatesAcadTableWithoutMovingOrChangingStyle;
    // issue #1357: пустой диапазон не создаёт таблицу.
    procedure BuildFromCellTextsRejectsEmptyDimensions;
    // issue #1359: явные ширины столбцов и высоты строк должны переноситься
    // в таблицу ACAD_TABLE (суммарные размеры равны сумме переданных).
    procedure BuildFromCellTextsWithSizesAppliesColWidthsAndRowHeights;
    // issue #1359: отсутствующие или неположительные размеры заменяются
    // значениями по умолчанию.
    procedure BuildFromCellTextsWithSizesFallsBackToDefaults;
    // issue #1363: выравнивание ячеек (код AutoCAD 1..9) переносится в
    // таблицу ACAD_TABLE; отсутствующие коды дают 0 (наследование от стиля).
    procedure BuildFromCellTextsAppliesCellAlignments;
    // issue #1396: после интерактивного переноса программно созданной таблицы
    // DXF должен содержать фактическую точку вставки и явное выравнивание
    // ячеек в позиции group 170, совместимой с форматом AutoCAD.
    procedure SavesTransformedInsertPointAndCellAlignmentToDXF;
    // issue #1368: явные типы строк (SetRowStyleTypes) задают индекс базового
    // стиля строки (0=Title, 1=Header, 2=Data); RowStyleTypeAt возвращает
    // эффективный тип с учётом fallback рендера либо -1 вне диапазона, а
    // перестроение таблицы сбрасывает ранее заданные типы строк.
    procedure SetRowStyleTypesAssignsRowStyles;
    // issue #1409: mixed Title/Header/Data styles in one row must remain
    // independent instead of being collapsed to one row style.
    procedure SetCellStyleTypesKeepsMixedRowStyles;
    procedure RebuildResetsRowStyleTypes;
    // issue #1402: ZCAD model-path DXF не содержит AcDbTableContent, поэтому
    // редактор должен получать Title/Header/Data из legacy-позиционной логики.
    procedure LoadsLegacyRowStylesForSpreadsheetEditing;
    // issue #1373: таблица с несколькими строками-заголовками (tablebugheader.dxf:
    // строка 0 = Title, строки 1 и 2 = Header, остальные = Data) должна
    // загружаться с правильными типами строк, прочитанными из современного
    // объекта AcDbTableContent (TABLEROW_BEGIN group 90).
    procedure LoadsMultipleHeaderRowsFromDXF;
    // issue #1373: разорванная (составная) таблица без объекта AcDbTableContent
    // (tablebugheader3.dxf: строка 0 = Title, строки 1 и 2 = Header, остальные
    // = Data) хранит типы строк только косвенно — через строки-метки, которые
    // одинаково повторяются в начале каждой части-продолжения. При загрузке
    // типы ведущих строк должны восстанавливаться по числу повторяющихся меток,
    // а зона повтора должна включать обе строки Header, иначе вторая строка
    // Header считалась данными и дублировалась при изменении высоты разбиения.
    procedure LoadsMultipleHeaderRowsFromSplitDXF;
  end;

implementation

function FindFirstAcadTable(const ARoot: PGDBObjGenericSubEntry): PGDBObjAcadTable;
var
  IR: itrec;
  PEntity: PGDBObjEntity;
begin
  Result := nil;
  PEntity := ARoot^.ObjArray.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBAcadTableID then
      Exit(PGDBObjAcadTable(PEntity));
    PEntity := ARoot^.ObjArray.iterate(IR);
  end;
end;

function CountAcadTables(const ARoot: PGDBObjGenericSubEntry): Integer;
var
  IR: itrec;
  PEntity: PGDBObjEntity;
begin
  Result := 0;
  PEntity := ARoot^.ObjArray.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBAcadTableID then
      Inc(Result);
    PEntity := ARoot^.ObjArray.iterate(IR);
  end;
end;

function DxfStreamToText(var AStream: TZctnrVectorBytes): String;
var
  FileName: String;
  Lines: TStringList;
begin
  FileName := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'zcad_acadtable_save_test.dxf';
  if AStream.SaveToFile(FileName) < 0 then
    raise Exception.Create('Не удалось сохранить временный DXF-поток');

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    Result := Lines.Text;
  finally
    Lines.Free;
    DeleteFile(FileName);
  end;
end;

function CreateManualPositionAcadTableDXF: String;
const
  AutoPartX = '98.15509801618446';
  ManualPartX = '99.15509801618446';
var
  SourceName, DXFText: String;
  Lines: TStringList;
begin
  SourceName := ExpandFileName('../../../cad_source/test/tablerazdel.dxf');
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'zcad_acadtable_manual_position.dxf';

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(SourceName);
    DXFText := Lines.Text;
    if Pos(AutoPartX, DXFText) = 0 then
      raise Exception.Create(
        'Тестовый DXF должен содержать ожидаемую точку вставки первой части');
    DXFText := StringReplace(DXFText, AutoPartX, ManualPartX, []);
    Lines.Text := DXFText;
    Lines.SaveToFile(Result);
  finally
    Lines.Free;
  end;
end;

function CreateManualHeightAcadTableDXF: String;
const
  AcDbTableMarker = '100'#10'AcDbTable'#10;
  ManualHeightFlag = '100'#10'AcDbTable'#10'295'#10'     1'#10;
var
  SourceName, DXFText: String;
  Lines: TStringList;
begin
  SourceName := ExpandFileName('../../../cad_source/test/tablerazdel.dxf');
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'zcad_acadtable_manual_height.dxf';

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(SourceName);
    DXFText := Lines.Text;
    if Pos(AcDbTableMarker, DXFText) = 0 then
      raise Exception.Create(
        'Тестовый DXF должен содержать секцию AcDbTable');
    DXFText := StringReplace(
      DXFText, AcDbTableMarker, ManualHeightFlag, [rfReplaceAll]);
    Lines.Text := DXFText;
    Lines.SaveToFile(Result);
  finally
    Lines.Free;
  end;
end;

// issue #1381: создаёт копию tablerazdel.dxf, в которой «родной» маркер
// AutoCAD ACAD_ROUNDTRIP_2008_TABLE_ENTITY заменён на приватный маркер ZCAD
// ZCAD_SPLIT_TABLE_ENTITY — так файл выглядит после сохранения из ZCAD.
function CreateSplitMarkerAcadTableDXF: String;
var
  SourceName, DXFText: String;
  Lines: TStringList;
begin
  SourceName := ExpandFileName('../../../cad_source/test/tablerazdel.dxf');
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'zcad_acadtable_split_marker.dxf';

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(SourceName);
    DXFText := Lines.Text;
    if Pos(CAcadTableRoundtripMarkerName, DXFText) = 0 then
      raise Exception.Create(
        'Тестовый DXF должен содержать round-trip маркер AutoCAD');
    DXFText := StringReplace(DXFText,
      CAcadTableRoundtripMarkerName, CAcadTableSplitMarkerName, [rfReplaceAll]);
    Lines.Text := DXFText;
    Lines.SaveToFile(Result);
  finally
    Lines.Free;
  end;
end;

function CountAcadTableControlPoints(AAcadTable: PGDBObjAcadTable): Integer;
var
  Desc: SelectedObjDesc;
begin
  FillChar(Desc, SizeOf(Desc), 0);
  Desc.objaddr := PGDBObjEntity(AAcadTable);
  GetMem(Pointer(Desc.pcontrolpoint), SizeOf(GDBControlPointArray));
  try
    AAcadTable^.CalcObjMatrix;
    AAcadTable^.addcontrolpoints(@Desc);
    Result := Desc.pcontrolpoint^.Count;
  finally
    Desc.pcontrolpoint^.done;
    FreeMem(Pointer(Desc.pcontrolpoint));
  end;
end;

function CountAcadTableControlPointsInVertexRange(
  AAcadTable: PGDBObjAcadTable; AFirstVertex, ALastVertex: Integer): Integer;
var
  Desc: SelectedObjDesc;
  Point: pcontrolpointdesc;
  I: Integer;
begin
  Result := 0;
  FillChar(Desc, SizeOf(Desc), 0);
  Desc.objaddr := PGDBObjEntity(AAcadTable);
  GetMem(Pointer(Desc.pcontrolpoint), SizeOf(GDBControlPointArray));
  try
    AAcadTable^.CalcObjMatrix;
    AAcadTable^.addcontrolpoints(@Desc);
    if Desc.pcontrolpoint^.Count > 0 then
    begin
      Point := Desc.pcontrolpoint^.GetParrayAsPointer;
      for I := 0 to Desc.pcontrolpoint^.Count - 1 do
      begin
        if (Point^.pointtype = os_polymin) and
           (Point^.vertexnum >= AFirstVertex) and
           (Point^.vertexnum <= ALastVertex) then
          Inc(Result);
        Inc(Point);
      end;
    end;
  finally
    Desc.pcontrolpoint^.done;
    FreeMem(Pointer(Desc.pcontrolpoint));
  end;
end;

function CountAcadTablePositionControlPoints(
  AAcadTable: PGDBObjAcadTable): Integer;
begin
  Result := CountAcadTableControlPointsInVertexRange(
    AAcadTable, 1, AAcadTable^.ContinuationPartCount);
end;

function CountAcadTableBreakHeightControlPoints(
  AAcadTable: PGDBObjAcadTable): Integer;
begin
  Result := CountAcadTableControlPointsInVertexRange(
    AAcadTable,
    CAcadTableBreakHeightGripVertexBase,
    CAcadTableBreakHeightGripVertexBase +
      AAcadTable^.ContinuationPartCount);
end;

function FindAcadTableControlPoint(
  AAcadTable: PGDBObjAcadTable; AVertex: Integer;
  out APoint: controlpointdesc): Boolean;
var
  Desc: SelectedObjDesc;
  Point: pcontrolpointdesc;
  I: Integer;
begin
  Result := False;
  FillChar(APoint, SizeOf(APoint), 0);
  FillChar(Desc, SizeOf(Desc), 0);
  Desc.objaddr := PGDBObjEntity(AAcadTable);
  GetMem(Pointer(Desc.pcontrolpoint), SizeOf(GDBControlPointArray));
  try
    AAcadTable^.CalcObjMatrix;
    AAcadTable^.addcontrolpoints(@Desc);
    if Desc.pcontrolpoint^.Count > 0 then
    begin
      Point := Desc.pcontrolpoint^.GetParrayAsPointer;
      for I := 0 to Desc.pcontrolpoint^.Count - 1 do
      begin
        if (Point^.pointtype = os_polymin) and
           (Point^.vertexnum = AVertex) then
        begin
          APoint := Point^;
          Exit(True);
        end;
        Inc(Point);
      end;
    end;
  finally
    Desc.pcontrolpoint^.done;
    FreeMem(Pointer(Desc.pcontrolpoint));
  end;
end;

procedure CheckAcadTablePointEquals(
  const AExpected, AActual: TzePoint3d; const AMsg: String);
begin
  TAssert.CheckEquals(AExpected.x, AActual.x, 1e-6, AMsg + ' (X)');
  TAssert.CheckEquals(AExpected.y, AActual.y, 1e-6, AMsg + ' (Y)');
  TAssert.CheckEquals(AExpected.z, AActual.z, 1e-6, AMsg + ' (Z)');
end;

function CountDxfPairs(
  const ADXF, ACode, AValue: String): Integer;
var
  Lines: TStringList;
  I: Integer;
begin
  Result := 0;
  Lines := TStringList.Create;
  try
    Lines.Text := ADXF;
    I := 0;
    while I < Lines.Count - 1 do
    begin
      if (Trim(Lines[I]) = ACode) and
         (Trim(Lines[I + 1]) = AValue) then
        Inc(Result);
      Inc(I, 2);
    end;
  finally
    Lines.Free;
  end;
end;

function CountDxfCode(const ADXF, ACode: String): Integer;
var
  Lines: TStringList;
  I: Integer;
begin
  Result := 0;
  Lines := TStringList.Create;
  try
    Lines.Text := ADXF;
    I := 0;
    while I < Lines.Count - 1 do
    begin
      if Trim(Lines[I]) = ACode then
        Inc(Result);
      Inc(I, 2);
    end;
  finally
    Lines.Free;
  end;
end;

function HasDxfSequence(
  const ADXF: String; const ASequence: array of String): Boolean;
var
  Lines: TStringList;
  StartIdx, SeqIdx: Integer;
begin
  Result := False;
  if Length(ASequence) = 0 then
    Exit(True);

  Lines := TStringList.Create;
  try
    Lines.Text := ADXF;
    for StartIdx := 0 to Lines.Count - Length(ASequence) do
    begin
      SeqIdx := 0;
      while (SeqIdx <= High(ASequence)) and
        (Trim(Lines[StartIdx + SeqIdx]) = ASequence[SeqIdx]) do
        Inc(SeqIdx);
      if SeqIdx > High(ASequence) then
        Exit(True);
    end;
  finally
    Lines.Free;
  end;
end;

function LoadDrawingFromDXF(const AFileName: string; var ADrawing: TSimpleDrawing): Integer;
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
begin
  ADrawing.init(nil);
  DC := ADrawing.CreateDrawingRC;
  ZDC.CreateRec(ADrawing, ADrawing.pObjRoot^, TLOLoad, DC);
  AddFromDXF(AFileName, ZDC);
  Result := ADrawing.pObjRoot^.ObjArray.Count;
end;

// Геометрия AcadTable (линии и MText ячеек) генерируется в собственный
// ConstObjArray объекта, а не в корень чертежа. Построение отложенное
// (DXFDelayedBuildGeometry=True), поэтому тесты обязаны сначала вызвать
// BuildGeometry на таблице, а затем обходить именно её ConstObjArray.
procedure BuildTableGeometry(var ADrawing: TSimpleDrawing;
  ATable: PGDBObjAcadTable);
begin
  ATable^.BuildGeometry(ADrawing);
end;

procedure CollectMTextStyles(var AArr: GDBObjEntityTreeArray;
  AStyles: TStrings);
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PMText: PGDBObjMText;
begin
  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBMTextID then
    begin
      PMText := PGDBObjMText(PEntity);
      if (PMText^.TXTStyle <> nil) and (PMText^.TXTStyle^.Name <> '') then
        if AStyles.IndexOf(PMText^.TXTStyle^.Name) < 0 then
          AStyles.Add(PMText^.TXTStyle^.Name);
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

procedure CollectLineBounds(var AArr: GDBObjEntityTreeArray;
  out AMinX, AMaxX: Double; out AHasGap: Boolean);
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PLine: PGDBObjLine;
  MinSegX, MaxSegX: Double;
  Segments: array of record
    MinX: Double;
    MaxX: Double;
  end;
  SegCount, i, j: Integer;
  TmpMin, TmpMax: Double;
begin
  AMinX := 0;
  AMaxX := 0;
  AHasGap := False;
  SegCount := 0;

  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBLineID then
    begin
      PLine := PGDBObjLine(PEntity);
      MinSegX := Min(PLine^.CoordInWCS.lBegin.x, PLine^.CoordInWCS.lEnd.x);
      MaxSegX := Max(PLine^.CoordInWCS.lBegin.x, PLine^.CoordInWCS.lEnd.x);
      SetLength(Segments, SegCount + 1);
      Segments[SegCount].MinX := MinSegX;
      Segments[SegCount].MaxX := MaxSegX;
      Inc(SegCount);
    end;
    PEntity := AArr.iterate(IR);
  end;

  if SegCount = 0 then
    Exit;

  for i := 0 to SegCount - 2 do
    for j := i + 1 to SegCount - 1 do
      if Segments[i].MinX > Segments[j].MinX then
      begin
        TmpMin := Segments[i].MinX;
        TmpMax := Segments[i].MaxX;
        Segments[i].MinX := Segments[j].MinX;
        Segments[i].MaxX := Segments[j].MaxX;
        Segments[j].MinX := TmpMin;
        Segments[j].MaxX := TmpMax;
      end;

  AMinX := Segments[0].MinX;
  AMaxX := Segments[0].MaxX;
  for i := 1 to SegCount - 1 do
  begin
    if Segments[i].MinX > AMaxX + 1e-6 then
      AHasGap := True;
    if Segments[i].MaxX > AMaxX then
      AMaxX := Segments[i].MaxX;
  end;
end;

procedure CollectLineBounds2D(var AArr: GDBObjEntityTreeArray;
  out AMinX, AMaxX, AMinY, AMaxY: Double; out AHasLines: Boolean);
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PLine: PGDBObjLine;
  LineMinX, LineMaxX, LineMinY, LineMaxY: Double;
begin
  AMinX := 0;
  AMaxX := 0;
  AMinY := 0;
  AMaxY := 0;
  AHasLines := False;

  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBLineID then
    begin
      PLine := PGDBObjLine(PEntity);
      LineMinX := Min(PLine^.CoordInWCS.lBegin.x, PLine^.CoordInWCS.lEnd.x);
      LineMaxX := Max(PLine^.CoordInWCS.lBegin.x, PLine^.CoordInWCS.lEnd.x);
      LineMinY := Min(PLine^.CoordInWCS.lBegin.y, PLine^.CoordInWCS.lEnd.y);
      LineMaxY := Max(PLine^.CoordInWCS.lBegin.y, PLine^.CoordInWCS.lEnd.y);
      if not AHasLines then
      begin
        AMinX := LineMinX;
        AMaxX := LineMaxX;
        AMinY := LineMinY;
        AMaxY := LineMaxY;
        AHasLines := True;
      end
      else
      begin
        if LineMinX < AMinX then AMinX := LineMinX;
        if LineMaxX > AMaxX then AMaxX := LineMaxX;
        if LineMinY < AMinY then AMinY := LineMinY;
        if LineMaxY > AMaxY then AMaxY := LineMaxY;
      end;
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

// Собирает высоты текста (textprop.size) всех MText, отрисованных таблицей.
procedure CollectMTextSizes(var AArr: GDBObjEntityTreeArray;
  out AMinSize, AMaxSize: Double; out ACount: Integer);
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PMText: PGDBObjMText;
  Sz: Double;
begin
  AMinSize := 0;
  AMaxSize := 0;
  ACount := 0;
  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBMTextID then
    begin
      PMText := PGDBObjMText(PEntity);
      Sz := PMText^.textprop.size;
      if ACount = 0 then
      begin
        AMinSize := Sz;
        AMaxSize := Sz;
      end
      else
      begin
        if Sz < AMinSize then AMinSize := Sz;
        if Sz > AMaxSize then AMaxSize := Sz;
      end;
      Inc(ACount);
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

// Собирает диапазон высот MText для конкретного текстового стиля.
procedure CollectMTextSizeRangeByStyle(var AArr: GDBObjEntityTreeArray;
  const AStyleName: String; out AMinSize, AMaxSize: Double;
  out ACount: Integer);
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PMText: PGDBObjMText;
  Sz: Double;
begin
  AMinSize := 0;
  AMaxSize := 0;
  ACount := 0;
  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBMTextID then
    begin
      PMText := PGDBObjMText(PEntity);
      if (PMText^.TXTStyle <> nil) and
         SameText(PMText^.TXTStyle^.Name, AStyleName) then
      begin
        Sz := PMText^.textprop.size;
        if ACount = 0 then
        begin
          AMinSize := Sz;
          AMaxSize := Sz;
        end
        else
        begin
          if Sz < AMinSize then AMinSize := Sz;
          if Sz > AMaxSize then AMaxSize := Sz;
        end;
        Inc(ACount);
      end;
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

function FindMTextSizeByTemplate(var AArr: GDBObjEntityTreeArray;
  const ATemplate: String; out ASize: Double): Boolean;
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PMText: PGDBObjMText;
begin
  Result := False;
  ASize := 0;
  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBMTextID then
    begin
      PMText := PGDBObjMText(PEntity);
      if PMText^.Template = ATemplate then
      begin
        ASize := PMText^.textprop.size;
        Result := True;
        Exit;
      end;
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

function CountMTextByTemplate(var AArr: GDBObjEntityTreeArray;
  const ATemplate: String): Integer;
var
  IR: itrec;
  PEntity: PGDBObjEntity;
  PMText: PGDBObjMText;
begin
  Result := 0;
  PEntity := AArr.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBMTextID then
    begin
      PMText := PGDBObjMText(PEntity);
      if PMText^.Template = ATemplate then
        Inc(Result);
    end;
    PEntity := AArr.iterate(IR);
  end;
end;

procedure TAcadTableStyleTest.LoadsCellTextStylesFromDXFTableStyle;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  StyleNames: TStringList;
  EntityCount: Integer;
begin
  EntityCount := LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    Check(EntityCount > 0, 'DXF должен загрузить сущности');

    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    BuildTableGeometry(Drawing, AcadTable);

    StyleNames := TStringList.Create;
    try
      StyleNames.Sorted := False;
      StyleNames.Duplicates := dupIgnore;
      CollectMTextStyles(AcadTable^.ConstObjArray, StyleNames);

      Check(StyleNames.Count > 0, 'Ожидались текстовые сущности таблицы');
      Check(StyleNames.IndexOf('newtext') >= 0,
        'Стиль newtext должен применяться к части ячеек таблицы');
      Check(StyleNames.Count > 1,
        'Таблица должна использовать более одного текстового стиля');
    finally
      StyleNames.Free;
    end;
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.LoadsFixedTextHeightFromDXFTextStyle;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  MinTitleHeaderSize, MaxTitleHeaderSize: Double;
  MinDataSize, MaxDataSize: Double;
  TitleHeaderTextCount, DataTextCount: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tableheighttextbug.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    BuildTableGeometry(Drawing, AcadTable);

    CollectMTextSizeRangeByStyle(
      AcadTable^.ConstObjArray, '!!!VTableText_320',
      MinTitleHeaderSize, MaxTitleHeaderSize, TitleHeaderTextCount);
    CollectMTextSizeRangeByStyle(
      AcadTable^.ConstObjArray, '!!!VTableText_220',
      MinDataSize, MaxDataSize, DataTextCount);

    Check(TitleHeaderTextCount > 0,
      'Таблица должна отрисовать текст названия/шапки стилем !!!VTableText_320');
    Check(DataTextCount > 0,
      'Таблица должна отрисовать текст данных стилем !!!VTableText_220');
    CheckEquals(320.0, MinTitleHeaderSize, 1e-6,
      'Минимальная высота текста названия/шапки должна браться из стиля');
    CheckEquals(320.0, MaxTitleHeaderSize, 1e-6,
      'Максимальная высота текста названия/шапки должна браться из стиля');
    CheckEquals(220.0, MinDataSize, 1e-6,
      'Минимальная высота текста данных должна браться из стиля');
    CheckEquals(220.0, MaxDataSize, 1e-6,
      'Максимальная высота текста данных должна браться из стиля');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.LoadsBreakSettingsFromDXF;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    // tablerazdel.dxf — это таблица, физически разорванная на три части
    // (две части-продолжения). Признак разрыва вычисляемый: даже если
    // DXF-флаг разбиения у отдельных ACAD_TABLE снят, наличие частей-
    // продолжений означает, что таблица разорвана, поэтому
    // BreakEnabled = True (issue #1305, часть 2a). Раньше инспектор
    // ошибочно показывал здесь Break enabled = false.
    CheckTrue(AcadTable^.BreakEnabled,
      'Разорванная на части таблица должна показывать Break enabled = True');
    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Разорванная таблица должна хранить части-продолжения');
    // Подробные параметры разрыва (повтор меток, ручное положение,
    // интервал) у round-trip разорванной таблицы хранятся не в самой
    // сущности ACAD_TABLE, а в объектах TABLEBREAKDATA секции OBJECTS,
    // и их чтение выходит за рамки issue #1305 (части 2a/2b — это
    // корректное определение факта разрыва и его снятие).
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.LoadsBreakSettingsFromSecondSample;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel2.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    // tablerazdel2.dxf — одиночный неразорванный ACAD_TABLE: одна
    // сущность без частей-продолжений и без DXF-флага разрыва.
    // Вычисляемый признак разрыва не должен срабатывать ложно —
    // BreakEnabled = False (issue #1305, часть 2a: корректное
    // определение признаков разделения работает в обе стороны —
    // неразорванная таблица не должна показывать Break enabled = True).
    CheckEquals(0, AcadTable^.ContinuationPartCount,
      'Неразорванная таблица не должна иметь частей-продолжений');
    CheckFalse(AcadTable^.BreakEnabled,
      'Неразорванная таблица должна показывать Break enabled = False');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.RendersBrokenTableAsSeparatedFragments;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  MinX, MaxX: Double;
  HasGap: Boolean;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    BuildTableGeometry(Drawing, AcadTable);

    CollectLineBounds(AcadTable^.ConstObjArray, MinX, MaxX, HasGap);
    Check(HasGap,
      'После применения правил разбиения у AcadTable должны быть разрывы между фрагментами');
    Check(MaxX - MinX > 20.0,
      'Ширина визуализации должна включать несколько разнесённых фрагментов таблицы');
  finally
    Drawing.done;
  end;
end;

// Разделённая по ширине таблица (tablerazdel.dxf) сохранена в DXF как три
// отдельные ACAD_TABLE. Все три части должны быть объединены в один объект
// AcadTable и отображаться вместе (issue #1300).
procedure TAcadTableStyleTest.LoadsSplitTableAsSingleMergedObject;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  TableCount: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    TableCount := CountAcadTables(Drawing.pObjRoot);
    CheckEquals(1, TableCount,
      'Три части разделённой таблицы должны загружаться как один объект AcadTable');

    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'В главную таблицу должны быть поглощены две части-продолжения');
  finally
    Drawing.done;
  end;
end;

// issue #1381: файл, сохранённый ZCAD, помечает связку частей разорванной
// таблицы приватным маркером ZCAD_SPLIT_TABLE_ENTITY (AutoCAD его не
// распознаёт и не пересобирает части). При открытии в ZCAD этот маркер должен
// читаться так же, как «родной» round-trip маркер AutoCAD, и восстанавливать
// единую разорванную таблицу — иначе теряется функциональность.
procedure TAcadTableStyleTest.ReadsSplitTableFromPrivateZcadMarker;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  TableCount: Integer;
  SplitDXF: String;
begin
  SplitDXF := CreateSplitMarkerAcadTableDXF;
  try
    LoadDrawingFromDXF(SplitDXF, Drawing);
    try
      TableCount := CountAcadTables(Drawing.pObjRoot);
      CheckEquals(1, TableCount,
        'Части, связанные приватным маркером ZCAD, должны загружаться как один объект AcadTable');

      AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
      AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
      CheckEquals(2, AcadTable^.ContinuationPartCount,
        'По приватному маркеру ZCAD в главную таблицу должны быть поглощены две части-продолжения');
    finally
      Drawing.done;
    end;
  finally
    DeleteFile(SplitDXF);
  end;
end;

// Разорванная на 11 частей таблица (bugbreaktable.dxf) хранит список
// продолжений в OBJECTS/XRECORD. Эти части должны загружаться как один
// логический AcadTable (issue #1334).
procedure TAcadTableStyleTest.LoadsIssue1334BreakTableAsSingleMergedObject;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  TableCount: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/bugbreaktable.dxf'), Drawing);
  try
    TableCount := CountAcadTables(Drawing.pObjRoot);
    CheckEquals(1, TableCount,
      'Одиннадцать частей разорванной таблицы должны загружаться как один объект AcadTable');

    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(10, AcadTable^.ContinuationPartCount,
      'В главную таблицу должны быть поглощены десять частей-продолжений');
  finally
    Drawing.done;
  end;
end;

// Все три части разделённой таблицы (tablerazdel.dxf) используют один и тот же
// DXF-стиль таблицы (handle 342 = "87", высоты текста 0.18/0.25). Части-
// продолжения поглощаются до вызова BuildGeometry, поэтому раньше их стиль
// оставался по умолчанию и текст рендерился высотой CAcadTableDefaultTextHeight
// (2.5) — намного крупнее ячеек, из-за чего «разъезжался». После исправления
// табличный стиль применяется ко всем частям, и высота текста не превышает
// высоту из DXF (issue #1300).
procedure TAcadTableStyleTest.AppliesTableStyleToContinuationParts;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  MinSize, MaxSize: Double;
  TextCount: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    Check(AcadTable^.ContinuationPartCount > 0,
      'Таблица должна содержать части-продолжения');

    BuildTableGeometry(Drawing, AcadTable);
    CollectMTextSizes(AcadTable^.ConstObjArray, MinSize, MaxSize, TextCount);
    Check(TextCount > 0, 'Таблица должна отрисовать текст ячеек');
    // Высоты текста стиля 0.18/0.25 — задаём порог заметно ниже значения
    // по умолчанию 2.5, чтобы поймать неприменённый стиль у продолжений.
    Check(MaxSize < CAcadTableDefaultTextHeight - 1e-6,
      'Высота текста частей-продолжений должна браться из DXF-стиля, ' +
      'а не из значения по умолчанию');
  finally
    Drawing.done;
  end;
end;

// issue #1317. Разделённая таблица должна сохраняться обратно в DXF как
// структурированные ACAD_TABLE-сущности: главная часть, части-продолжения
// и XRECORD ACAD_ROUNDTRIP_2008_TABLE_ENTITY, по которому загрузчик снова
// объединяет продолжения с главной таблицей.
procedure TAcadTableStyleTest.SavesSplitAcadTableToStructuredDXF;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  OutStream: TZctnrVectorBytes;
  SaveContext: TIODXFSaveContext;
  DXFText: String;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'Тестовый DXF должен содержать две части-продолжения');

    OutStream.init(64 * 1024);
    SaveContext.InitRec;
    SaveContext.Header.Version := AC1021;
    SaveContext.Header.iVersion := 1021;
    try
      ResetAcadTableDXFWriteState;
      AcadTable^.DXFOut(OutStream, Drawing, SaveContext);
      WriteAcadTableRoundTripObjectsToDXF(
        OutStream, Drawing, SaveContext);
      DXFText := DxfStreamToText(OutStream);
    finally
      ResetAcadTableDXFWriteState;
      SaveContext.Done;
      OutStream.done;
    end;

    CheckEquals(3, CountDxfPairs(DXFText, '0', 'ACAD_TABLE'),
      'Главная таблица и две части-продолжения должны сохраниться как ACAD_TABLE');
    CheckEquals(3, CountDxfPairs(DXFText, '100', 'AcDbTable'),
      'Каждая сохранённая ACAD_TABLE должна содержать subclass AcDbTable');
    CheckTrue(CountDxfCode(DXFText, '310') > 0,
      'Сырые binary chunks ACAD_TABLE (310) должны сохраняться');
    CheckEquals(3, CountDxfCode(DXFText, '343'),
      'Каждая часть таблицы должна сохранить ссылку group 343');
    CheckTrue(
      HasDxfSequence(DXFText,
        ['301', 'CELL_VALUE',
         '93', '6',
         '90', '1',
         '91', '1']),
      'Числовые значения ячеек должны сохранять DXF-тип, а не превращаться в строки');
    CheckTrue(
      HasDxfSequence(DXFText,
        ['173', '1',
         '174', '0',
         '175', '1',
         '176', '1',
         '91', '0',
         '178', '0',
         '145', '0.0',
         '92', '0',
         '301', 'CELL_VALUE',
         '93', '7']),
      'Виртуальные ячейки объединений должны сохранять исходные флаги');
    CheckTrue(CountDxfPairs(DXFText, '301', 'CELL_VALUE') > 0,
      'Ячейки таблицы должны сохраняться как CELL_VALUE');
    CheckTrue(Pos('Zagolovok', DXFText) > 0,
      'DXF должен содержать текст ячейки из исходной таблицы');
    // issue #1381: запись больше НЕ использует распознаваемый AutoCAD маркер
    // ACAD_ROUNDTRIP_2008_TABLE_ENTITY (из-за него AutoCAD пересобирал части
    // в одну цельную таблицу). Вместо него пишется приватный маркер ZCAD,
    // который AutoCAD игнорирует и показывает части отдельными таблицами.
    CheckEquals(0,
      CountDxfPairs(
        DXFText, '102', 'ACAD_ROUNDTRIP_2008_TABLE_ENTITY'),
      'Распознаваемый AutoCAD round-trip маркер не должен записываться (issue #1381)');
    CheckEquals(1,
      CountDxfPairs(
        DXFText, '102', 'ZCAD_SPLIT_TABLE_ENTITY'),
      'Для разделённой таблицы должен сохраняться приватный split-XRECORD ZCAD');
  finally
    Drawing.done;
  end;
end;

// issue #1381. Разорванная таблица, отредактированная в ZCAD, теряет raw-DXF
// и сохраняется по модельному пути. Раньше он писал только имена *T1/*T2/*T3
// без реальных блоков, поэтому AutoCAD рисовал части пустыми или собирал их
// в одну цельную таблицу. Теперь before-save обработчик (EnsureSplitPartBlocks)
// генерирует для главной части и каждой части-продолжения отдельный анонимный
// блок с её геометрией (линии + текст ячеек); сущность ACAD_TABLE ссылается на
// него через group 2/343, и AutoCAD рисует каждую часть отдельной таблицей.
// Проверяем именно генерацию блоков на уровне модели (без полного savedxf20XX,
// требующего шаблон/LCL): сквозная запись проверяется harness'ом roundtrip1381.
procedure TAcadTableStyleTest.GeneratesPerPartBlocksForModelPathSplitTable;

  function CountLinesAndMText(var AArr: GDBObjEntityTreeArray): Integer;
  var
    IR: itrec;
    PEntity: PGDBObjEntity;
  begin
    Result := 0;
    PEntity := AArr.beginiterate(IR);
    while PEntity <> nil do
    begin
      if (PEntity^.GetObjType = GDBLineID) or
         (PEntity^.GetObjType = GDBMTextID) then
        Inc(Result);
      PEntity := AArr.iterate(IR);
    end;
  end;

var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  BlockArr: PGDBObjBlockdefArray;
  BlockDef: PGDBObjBlockdef;
  IR: itrec;
  NamesBefore: TStringList;
  PartCount, NewBlocks, NewBlocksWithGeometry: Integer;
  OrigDir, OtherDir: TAcadTableBreakDirection;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    PartCount := AcadTable^.ContinuationPartCount;
    CheckEquals(2, PartCount,
      'Тестовый DXF должен содержать две части-продолжения');

    // Эмулируем редактирование таблицы в ZCAD: любой публичный сеттер
    // (здесь — направление разбиения) инвалидирует raw-DXF, поэтому
    // сохранение идёт по модельному пути. Меняем направление и возвращаем
    // обратно, чтобы итоговая геометрия/раскладка не изменилась.
    OrigDir := AcadTable^.BreakDirection;
    if OrigDir = atbdRight then
      OtherDir := atbdLeft
    else
      OtherDir := atbdRight;
    AcadTable^.BreakDirection := OtherDir;
    AcadTable^.BreakDirection := OrigDir;

    BlockArr := PGDBObjBlockdefArray(Drawing.GetBlockDefArraySimple);
    NamesBefore := TStringList.Create;
    try
      BlockDef := BlockArr^.beginiterate(IR);
      while BlockDef <> nil do
      begin
        NamesBefore.Add(BlockDef^.Name);
        BlockDef := BlockArr^.iterate(IR);
      end;

      // Генерация персональных блоков частей — это то, что делает
      // before-save обработчик перед записью секции ENTITIES (issue #1381).
      AcadTable^.EnsureSplitPartBlocks(Drawing);

      NewBlocks := 0;
      NewBlocksWithGeometry := 0;
      BlockDef := BlockArr^.beginiterate(IR);
      while BlockDef <> nil do
      begin
        if NamesBefore.IndexOf(BlockDef^.Name) < 0 then
        begin
          Inc(NewBlocks);
          if CountLinesAndMText(BlockDef^.ObjArray) > 0 then
            Inc(NewBlocksWithGeometry);
        end;
        BlockDef := BlockArr^.iterate(IR);
      end;

      CheckEquals(PartCount + 1, NewBlocks,
        'Для главной части и каждой части-продолжения должен создаваться ' +
        'отдельный анонимный блок (issue #1381)');
      CheckEquals(PartCount + 1, NewBlocksWithGeometry,
        'Каждый персональный блок части должен содержать геометрию ' +
        '(линии/текст ячеек), иначе AutoCAD нарисует пустую таблицу');
    finally
      NamesBefore.Free;
    end;
  finally
    Drawing.done;
  end;
end;

// issue #1339. Сырая (raw) ACAD_TABLE из исходного файла сохраняла все
// исходные хэндлы дословно. После перенумерации файла шаблоном это давало
// 342 -> чужой стиль (грузился как "aits" вместо "Standard"), висячие 343/360
// и неверного владельца 330. Проверяем, что при записи ссылки переписываются
// под актуальную нумерацию, а блок расширенного словаря удаляется.
procedure TAcadTableStyleTest.RemapsRawAcadTableHandlesOnSave;
var
  Raw: TStringList;
  RawText, DXFText: String;
  OutStream: TZctnrVectorBytes;
  SaveContext: TIODXFSaveContext;
  Ok: Boolean;
begin
  Raw := TStringList.Create;
  try
    Raw.Add('0');   Raw.Add('ACAD_TABLE');
    Raw.Add('5');   Raw.Add('2FF');
    Raw.Add('102'); Raw.Add('{ACAD_XDICTIONARY');
    Raw.Add('360'); Raw.Add('357');
    Raw.Add('102'); Raw.Add('}');
    Raw.Add('330'); Raw.Add('1F');
    Raw.Add('100'); Raw.Add('AcDbEntity');
    Raw.Add('8');   Raw.Add('0');
    Raw.Add('100'); Raw.Add('AcDbBlockReference');
    Raw.Add('2');   Raw.Add('*T1');
    Raw.Add('100'); Raw.Add('AcDbTable');
    Raw.Add('342'); Raw.Add('87');
    Raw.Add('343'); Raw.Add('ED');
    Raw.Add('310'); Raw.Add('ABCDEF');
    RawText := Raw.Text;
  finally
    Raw.Free;
  end;

  OutStream.init(8 * 1024);
  SaveContext.InitRec;
  SaveContext.Header.Version := AC1021;
  SaveContext.Header.iVersion := 1021;
  try
    // Эмулируем состояние, которое savedxf20XX заполняет перед секцией
    // ENTITIES: новый хэндл владельца и карты имя->новый хэндл.
    SaveContext.AcadTableOwnerHandle := $123;
    SaveContext.TableStyleNameHandleMap.Add('Standard', '4A2');
    SaveContext.BlockNameHandleMap.Add('*T1', '5B3');

    ResetAcadTableDXFWriteState;
    Ok := WriteRawAcadTablePartsToDXF(
      OutStream, SaveContext, RawText, [], 0.0, 0.0, False, False, 'Standard');
    DXFText := DxfStreamToText(OutStream);
  finally
    ResetAcadTableDXFWriteState;
    SaveContext.Done;
    OutStream.done;
  end;

  CheckTrue(Ok, 'Сырая ACAD_TABLE должна успешно записаться');

  // Висячий расширенный словарь должен быть удалён целиком.
  CheckEquals(0, Pos('ACAD_XDICTIONARY', DXFText),
    'Блок 102/ACAD_XDICTIONARY должен удаляться при сохранении');
  CheckFalse(HasDxfSequence(DXFText, ['360', '357']),
    'Висячая ссылка 360 на словарь не должна сохраняться');

  // Собственный хэндл сущности (группа 5) перенумеровывается под актуальную
  // нумерацию файла (issue #1344). SaveContext.InitRec выставляет счётчик в $2,
  // поэтому первая (главная) raw-таблица получает хэндл 2.
  CheckTrue(HasDxfSequence(DXFText, ['5', '2']),
    'Собственный хэндл сущности (группа 5) должен перенумеровываться в свежий');
  CheckEquals(0, CountDxfPairs(DXFText, '5', '2FF'),
    'Старый хэндл сущности 2FF не должен оставаться (issue #1344)');

  // Ссылки перенумерованы под актуальный файл.
  CheckTrue(HasDxfSequence(DXFText, ['330', '123']),
    'Владелец (330) должен указывать на новый хэндл *Model_Space');
  CheckTrue(HasDxfSequence(DXFText, ['342', '4A2']),
    'Стиль таблицы (342) должен указывать на новый хэндл стиля');
  CheckTrue(HasDxfSequence(DXFText, ['343', '5B3']),
    'Анонимный блок (343) должен указывать на новый хэндл BLOCK_RECORD');

  // Старые (исходные) хэндлы ссылок не должны протекать в новый файл.
  CheckEquals(0, CountDxfPairs(DXFText, '330', '1F'),
    'Старый владелец 1F не должен оставаться');
  CheckEquals(0, CountDxfPairs(DXFText, '342', '87'),
    'Старый хэндл стиля 87 (грузится как "aits") не должен оставаться');
  CheckEquals(0, CountDxfPairs(DXFText, '343', 'ED'),
    'Старый хэндл блока ED не должен оставаться');

  // Бинарные данные ячеек должны сохраняться без изменений.
  CheckTrue(HasDxfSequence(DXFText, ['310', 'ABCDEF']),
    'Бинарные чанки ячеек (310) должны сохраняться');
end;

// issue #1339. Сквозная проверка корневой причины. В исходном файле таблица
// ссылается на стиль "Standard" (342 -> хэндл 87). Загруженная raw-таблица не
// разрешает имя стиля до BuildGeometry, поэтому FTableStyle.Name пуст. При
// пакетном "Сохранить как" (без отрисовки) DXFOut обязан сам разрешить имя
// стиля по хэндлу и перенумеровать 342 на актуальный хэндл TABLESTYLE. Раньше
// 342 оставался равен 87, который после перенумерации указывал на чужой объект
// (стиль грузился как "aits"). Проверяем имя стиля и перенумерацию 342.
procedure TAcadTableStyleTest.ResolvesRawTableStyleNameAndRemaps342OnDXFOut;
const
  StandardHandle = 'B8';
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  OutStream: TZctnrVectorBytes;
  SaveContext: TIODXFSaveContext;
  DXFText: String;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/acadtablerazdel2007_1.dxf'),
    Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    OutStream.init(64 * 1024);
    SaveContext.InitRec;
    SaveContext.Header.Version := AC1021;
    SaveContext.Header.iVersion := 1021;
    try
      // Эмулируем состояние, которое savedxf20XX (PreallocateTableStyleHandles)
      // заполняет перед секцией ENTITIES: владелец и карты имя->новый хэндл.
      SaveContext.AcadTableOwnerHandle := $1F;
      SaveContext.TableStyleNameHandleMap.Add('Standard', StandardHandle);
      SaveContext.BlockNameHandleMap.Add('*T1', 'A1');

      ResetAcadTableDXFWriteState;
      AcadTable^.DXFOut(OutStream, Drawing, SaveContext);
      DXFText := DxfStreamToText(OutStream);
    finally
      ResetAcadTableDXFWriteState;
      SaveContext.Done;
      OutStream.done;
    end;

    // DXFOut должен был разрешить имя стиля по хэндлу 87 -> "Standard".
    CheckEquals('Standard', AcadTable^.TableStyleName,
      'Имя стиля raw-таблицы должно разрешаться по хэндлу при сохранении');

    // Ссылка 342 должна указывать на актуальный хэндл TABLESTYLE.
    CheckTrue(HasDxfSequence(DXFText, ['342', StandardHandle]),
      'Стиль таблицы (342) должен перенумеровываться на хэндл стиля "Standard"');

    // Старый хэндл стиля 87 (после перенумерации — чужой объект) не должен течь.
    CheckEquals(0, CountDxfPairs(DXFText, '342', '87'),
      'Старый хэндл стиля 87 не должен оставаться в сохранённой таблице');
  finally
    Drawing.done;
  end;
end;

// issue #1305, часть 1. До исправления визуальное представление таблицы
// строилось один раз и было защищено флагом FGeometryBuilt, поэтому при
// переносе (ручкой переноса) сама таблица не перестраивалась — двигалась
// только ручка. Теперь дочерние объекты переформатируются каждый кадр и
// пересчитывают свои WCS-координаты из objmatrix владельца, поэтому
// перенос таблицы сдвигает все её линии на ту же величину.
procedure TAcadTableStyleTest.TransformationMovesRenderedTable;
const
  Shift = 100.0;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  DC: TDrawContext;
  MinX1, MaxX1, MinX2, MaxX2: Double;
  HasGap: Boolean;
  Matrix: TzeTypedMatrix4d;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    DC := Drawing.CreateDrawingRC;
    AcadTable^.FormatEntity(Drawing, DC);
    CollectLineBounds(AcadTable^.ConstObjArray, MinX1, MaxX1, HasGap);
    Check(MaxX1 > MinX1, 'Таблица должна отрисовать линии в WCS');

    // Переносим таблицу на Shift по оси X, как это делает ручка переноса.
    Matrix := CreateTranslationMatrix(Shift, 0, 0);
    AcadTable^.TransformAt(PGDBObjEntity(AcadTable), @Matrix);
    AcadTable^.FormatEntity(Drawing, DC);
    CollectLineBounds(AcadTable^.ConstObjArray, MinX2, MaxX2, HasGap);

    CheckEquals(MinX1 + Shift, MinX2, 1e-6,
      'После переноса таблицы её линии должны сдвинуться на ту же величину ' +
      '(issue #1305, часть 1)');
    CheckEquals(MaxX1 + Shift, MaxX2, 1e-6,
      'Правая граница перенесённой таблицы должна сдвинуться на ту же величину');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.ClonedPreviewKeepsRenderedTableAtSourceLocation;
var
  Drawing: TSimpleDrawing;
  AcadTable, PreviewTable: PGDBObjAcadTable;
  DC: TDrawContext;
  SrcMinX, SrcMaxX, SrcMinY, SrcMaxY: Double;
  CloneMinX, CloneMaxX, CloneMinY, CloneMaxY: Double;
  HasLines: Boolean;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    DC := Drawing.CreateDrawingRC;
    AcadTable^.FormatEntity(Drawing, DC);
    CollectLineBounds2D(AcadTable^.ConstObjArray,
      SrcMinX, SrcMaxX, SrcMinY, SrcMaxY, HasLines);
    CheckTrue(HasLines, 'Исходная таблица должна отрисовать линии в WCS');

    PreviewTable := PGDBObjAcadTable(AcadTable^.Clone(nil));
    AssertNotNull(
      'AcadTable.Clone должен вернуть временный объект предпросмотра',
      PreviewTable);
    try
      PreviewTable^.bp.ListPos.Owner := AcadTable^.bp.ListPos.Owner;
      PreviewTable^.FormatFast(Drawing, DC);
      PreviewTable^.BuildGeometry(Drawing);
      CollectLineBounds2D(PreviewTable^.ConstObjArray,
        CloneMinX, CloneMaxX, CloneMinY, CloneMaxY, HasLines);
      CheckTrue(HasLines,
        'Клон предпросмотра должен отрисовать линии в WCS');

      CheckEquals(SrcMinX, CloneMinX, 1e-6,
        'Клон предпросмотра не должен смещать таблицу в начало координат (MinX)');
      CheckEquals(SrcMaxX, CloneMaxX, 1e-6,
        'Клон предпросмотра должен сохранить правую границу таблицы');
      CheckEquals(SrcMinY, CloneMinY, 1e-6,
        'Клон предпросмотра должен сохранить нижнюю границу таблицы');
      CheckEquals(SrcMaxY, CloneMaxY, 1e-6,
        'Клон предпросмотра должен сохранить верхнюю границу таблицы');
    finally
      PreviewTable^.done;
      FreeMem(Pointer(PreviewTable));
    end;
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.ClonedPreviewPreservesExplicitRowStyleTypes;
var
  Drawing: TSimpleDrawing;
  AcadTable, PreviewTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablebugheader.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(1, AcadTable^.RowStyleTypeAt(2),
      'Исходная таблица должна иметь вторую строку Header');

    PreviewTable := PGDBObjAcadTable(AcadTable^.Clone(nil));
    AssertNotNull(
      'AcadTable.Clone должен вернуть временный объект предпросмотра',
      PreviewTable);
    try
      CheckEquals(1, PreviewTable^.RowStyleTypeAt(2),
        'Клон предпросмотра должен сохранить вторую строку Header');
      CheckEquals(2, PreviewTable^.RowStyleTypeAt(3),
        'Клон предпросмотра должен сохранить первую строку Data');
    finally
      PreviewTable^.done;
      FreeMem(Pointer(PreviewTable));
    end;
  finally
    Drawing.done;
  end;
end;

// issue #1305, часть 2b. Разорванная таблица (tablerazdel.dxf) загружается
// как один объект с двумя частями-продолжениями. Снятие признака разрыва
// (BreakEnabled := False) должно объединить все части в одну непрерывную
// таблицу: части-продолжения исчезают, а строки выстраиваются сверху вниз.
procedure TAcadTableStyleTest.ClearingBreakMergesContinuationParts;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  RowsBefore: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Разорванная таблица должна содержать части-продолжения');
    CheckTrue(AcadTable^.BreakEnabled,
      'До снятия разрыва BreakEnabled должен быть True');
    RowsBefore := AcadTable^.RowCount;

    // Снимаем признак разрыва — таблица должна перестроиться сверху вниз.
    AcadTable^.BreakEnabled := False;

    CheckEquals(0, AcadTable^.ContinuationPartCount,
      'После снятия разрыва части-продолжения должны быть объединены');
    CheckFalse(AcadTable^.BreakEnabled,
      'После объединения BreakEnabled должен быть False');
    CheckTrue(AcadTable^.RowCount > RowsBefore,
      'Объединённая таблица должна содержать строки всех частей ' +
      '(issue #1305, часть 2b)');
  finally
    Drawing.done;
  end;
end;

// issue #1313. Если разорванная таблица содержит повтор верхних меток
// (Title/Header) в частях-продолжениях, то при снятии BreakEnabled эти
// повторы должны исчезнуть: итоговая таблица становится непрерывной и
// содержит только один набор верхних меток. При повторном включении
// BreakEnabled сохранённые параметры разбиения должны примениться снова.
procedure TAcadTableStyleTest.ClearingBreakRemovesRepeatedTopLabels;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  ExpectedRows, PartIdx, RepeatRows, RowsAfterDisable: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckTrue(AcadTable^.BreakEnabled,
      'До снятия разбиения таблица должна быть разорванной');
    CheckTrue(AcadTable^.BreakRepeatTopLabels,
      'Тестовый DXF должен содержать повтор верхних меток');

    ExpectedRows := AcadTable^.RowCount;
    for PartIdx := 0 to AcadTable^.ContinuationPartCount - 1 do
    begin
      RepeatRows := 0;
      if AcadTable^.ContinuationPartCellText(PartIdx, 0, 0) = 'Zagolovok' then
        Inc(RepeatRows);
      if AcadTable^.ContinuationPartCellText(PartIdx, 1, 0) = 'A' then
        Inc(RepeatRows);
      Inc(ExpectedRows,
        AcadTable^.ContinuationPartRowCount(PartIdx) - RepeatRows);
    end;

    AcadTable^.BreakEnabled := False;
    CheckEquals(ExpectedRows, AcadTable^.RowCount,
      'Снятие разбиения должно объединить только логические строки, ' +
      'без повторённых верхних меток из частей-продолжений');

    BuildTableGeometry(Drawing, AcadTable);
    CheckEquals(1,
      CountMTextByTemplate(AcadTable^.ConstObjArray, 'Zagolovok'),
      'После снятия разбиения заголовок должен отрисоваться один раз');
    CheckEquals(1,
      CountMTextByTemplate(AcadTable^.ConstObjArray, 'A'),
      'После снятия разбиения шапка должна отрисоваться один раз');

    RowsAfterDisable := AcadTable^.RowCount;
    CheckTrue(AcadTable^.BreakHeight > 0,
      'Тестовый DXF должен содержать положительную высоту разбиения');
    AcadTable^.BreakHeight := AcadTable^.BreakHeight / 2;
    CheckEquals(0, AcadTable^.ContinuationPartCount,
      'При выключенном разбиении изменение высоты не должно разбивать таблицу');
    CheckEquals(RowsAfterDisable, AcadTable^.RowCount,
      'При выключенном разбиении высота разбиения должна игнорироваться');

    AcadTable^.BreakEnabled := True;
    CheckTrue(AcadTable^.BreakEnabled,
      'Повторное включение должно снова установить разбиение таблицы');
    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Повторное включение должно применить сохранённую высоту разбиения');
    CheckEquals('Zagolovok', AcadTable^.ContinuationPartCellText(0, 0, 0),
      'Повторное включение должно вернуть повтор заголовка в продолжение');
    CheckEquals('A', AcadTable^.ContinuationPartCellText(0, 1, 0),
      'Повторное включение должно вернуть повтор шапки в продолжение');
  finally
    Drawing.done;
  end;
end;

// issue #1307, часть 1. Интервал между частями разорванной таблицы
// (Break spacing) хранится в round-trip данных DXF (XRECORD
// ACAD_ROUNDTRIP_2008_TABLE_ENTITY, первое значение группы 40). Для
// tablerazdel.dxf он равен ≈0.99 и согласуется с геометрией: шаг между
// точками вставки частей (13.49) минус ширина таблицы (12.5) = 0.99.
// Раньше это свойство вычислялось неверно (issue #1307).
procedure TAcadTableStyleTest.LoadsBreakSpacingFromDXF;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckEquals(0.99, AcadTable^.BreakSpacing, 1e-4,
      'Интервал между частями (Break spacing) должен читаться из DXF ≈0.99');
  finally
    Drawing.done;
  end;
end;

// issue #1307, часть 2. Высота разбиения (Break height) — порог суммарной
// высоты строк, после которого строки переносятся в следующую часть. Она
// хранится только в round-trip данных DXF (второе значение группы 40
// XRECORD) и не выводится из геометрии. Для tablerazdel.dxf — ≈2.0486.
// Раньше это свойство полностью отсутствовало (issue #1307).
procedure TAcadTableStyleTest.LoadsBreakHeightFromDXF;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckEquals(2.048584359034151, AcadTable^.BreakHeight, 1e-4,
      'Высота разбиения (Break height) должна читаться из DXF ≈2.0486');
  finally
    Drawing.done;
  end;
end;

// issue #1307, часть 2. Изменение высоты разбиения должно пересегментировать
// таблицу: число частей определяется автоматически так, чтобы суммарная
// высота строк в каждой части не превышала заданного порога. tablerazdel.dxf
// загружается как [5,5,4] строк (главная часть + 2 продолжения = 14 строк).
//   • Очень большой порог → все строки помещаются в одну часть (0 продолжений,
//     главная часть содержит все 14 строк).
//   • Возврат к исходному порогу ≈2.0486 → снова 2 части-продолжения.
//   • Очень малый порог → больше частей, чем исходно.
procedure TAcadTableStyleTest.ChangingBreakHeightResegmentsTable;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  MainRowsBefore: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'Исходно таблица разбита на главную часть и 2 продолжения');
    MainRowsBefore := AcadTable^.RowCount;

    // Очень большой порог — все строки умещаются в одной части.
    AcadTable^.BreakHeight := 1000.0;
    CheckEquals(0, AcadTable^.ContinuationPartCount,
      'При большом пороге все строки должны помещаться в одну часть');
    CheckTrue(AcadTable^.RowCount > MainRowsBefore,
      'Объединённая главная часть должна содержать строки всех частей');

    // Возврат к исходному порогу — снова две части-продолжения.
    AcadTable^.BreakHeight := 2.048584359034151;
    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'При исходном пороге ≈2.0486 таблица снова разбивается на 3 части');

    // Очень малый порог — каждая часть вмещает меньше строк, частей больше.
    AcadTable^.BreakHeight := 0.5;
    CheckTrue(AcadTable^.ContinuationPartCount > 2,
      'При малом пороге число частей-продолжений должно вырасти');
  finally
    Drawing.done;
  end;
end;

// issue #1307, часть 1. Изменение интервала между частями должно смещать
// части-продолжения относительно главной части. Таблица разбита по ширине
// (части идут вправо), поэтому увеличение интервала раздвигает части и
// увеличивает общую ширину отрисованного представления.
procedure TAcadTableStyleTest.ChangingBreakSpacingRepositionsParts;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  MinX1, MaxX1, MinX2, MaxX2: Double;
  HasGap: Boolean;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Таблица должна содержать части-продолжения');

    BuildTableGeometry(Drawing, AcadTable);
    CollectLineBounds(AcadTable^.ConstObjArray, MinX1, MaxX1, HasGap);
    Check(MaxX1 > MinX1, 'Таблица должна отрисовать линии в WCS');

    // Увеличиваем интервал между частями — части должны раздвинуться.
    AcadTable^.BreakSpacing := 10.0;
    BuildTableGeometry(Drawing, AcadTable);
    CollectLineBounds(AcadTable^.ConstObjArray, MinX2, MaxX2, HasGap);

    CheckTrue(MaxX2 > MaxX1 + 1e-6,
      'Увеличение интервала должно раздвинуть части и увеличить общую ширину');
  finally
    Drawing.done;
  end;
end;

// issue #1315. Направление разбиения должно быть редактируемым и менять
// положение частей-продолжений: Left переносит их влево от главной таблицы,
// Down — ниже главной таблицы.
procedure TAcadTableStyleTest.ChangingBreakDirectionRepositionsParts;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  RightMinX, RightMaxX, RightMinY, RightMaxY: Double;
  LeftMinX, LeftMaxX, LeftMinY, LeftMaxY: Double;
  DownMinX, DownMaxX, DownMinY, DownMaxY: Double;
  HasLines: Boolean;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Таблица должна содержать части-продолжения');
    CheckEquals(Ord(atbdRight), Ord(AcadTable^.BreakDirection),
      'Исходное направление тестовой таблицы должно быть Right');

    BuildTableGeometry(Drawing, AcadTable);
    CollectLineBounds2D(AcadTable^.ConstObjArray,
      RightMinX, RightMaxX, RightMinY, RightMaxY, HasLines);
    CheckTrue(HasLines, 'Таблица должна отрисовать линии в WCS');

    AcadTable^.BreakDirection := atbdLeft;
    CheckEquals(Ord(atbdLeft), Ord(AcadTable^.BreakDirection),
      'Свойство BreakDirection должно принимать значение Left');
    BuildTableGeometry(Drawing, AcadTable);
    CollectLineBounds2D(AcadTable^.ConstObjArray,
      LeftMinX, LeftMaxX, LeftMinY, LeftMaxY, HasLines);
    CheckTrue(HasLines, 'Таблица с направлением Left должна отрисовать линии');
    CheckTrue(LeftMinX < RightMinX - 1e-6,
      'Left должен переносить части-продолжения влево от главной таблицы');
    CheckTrue(LeftMaxX < RightMaxX - 1e-6,
      'Left не должен оставлять части-продолжения справа от главной таблицы');

    AcadTable^.BreakDirection := atbdDown;
    CheckEquals(Ord(atbdDown), Ord(AcadTable^.BreakDirection),
      'Свойство BreakDirection должно принимать значение Down');
    BuildTableGeometry(Drawing, AcadTable);
    CollectLineBounds2D(AcadTable^.ConstObjArray,
      DownMinX, DownMaxX, DownMinY, DownMaxY, HasLines);
    CheckTrue(HasLines, 'Таблица с направлением Down должна отрисовать линии');
    CheckTrue(DownMinY < RightMinY - 1e-6,
      'Down должен переносить части-продолжения ниже главной таблицы');
    CheckTrue(DownMaxX < RightMaxX - 1e-6,
      'Down должен перестать раскладывать части вправо');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.DetectsBreakManualPositionFromPartOffsets;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  ManualDXF: String;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckFalse(AcadTable^.BreakManualPosition,
      'Исходная таблица разложена по BreakDirection/BreakSpacing и не должна ' +
      'считаться ручной');
  finally
    Drawing.done;
  end;

  ManualDXF := CreateManualPositionAcadTableDXF;
  try
    LoadDrawingFromDXF(ManualDXF, Drawing);
    try
      AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
      AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
      CheckEquals(2, AcadTable^.ContinuationPartCount,
        'Тестовый DXF должен содержать две части-продолжения');
      CheckEquals(0.99, AcadTable^.BreakSpacing, 1e-4,
        'Интервал BreakSpacing должен сохраниться из round-trip данных');
      CheckTrue(AcadTable^.BreakManualPosition,
        'Смещённая часть должна включить BreakManualPosition');
    finally
      Drawing.done;
    end;
  finally
    DeleteFile(ManualDXF);
  end;
end;

procedure TAcadTableStyleTest.ManualBreakPositionAddsContinuationPartGrips;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  ManualDXF: String;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(0, CountAcadTablePositionControlPoints(AcadTable),
      'Автоматически разложенная таблица не должна иметь ручек положения ' +
      'частей-продолжений');
  finally
    Drawing.done;
  end;

  ManualDXF := CreateManualPositionAcadTableDXF;
  try
    LoadDrawingFromDXF(ManualDXF, Drawing);
    try
      AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
      AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
      CheckTrue(AcadTable^.BreakManualPosition,
        'Тестовая таблица должна определиться как ручная');
      CheckEquals(AcadTable^.ContinuationPartCount,
        CountAcadTablePositionControlPoints(AcadTable),
        'Ручное положение должно добавить ручку для каждой части-продолжения');
    finally
      Drawing.done;
    end;
  finally
    DeleteFile(ManualDXF);
  end;
end;

procedure TAcadTableStyleTest.TogglingBreakManualPositionUpdatesContinuationPartGrips;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  ManualDXF: String;
begin
  ManualDXF := CreateManualPositionAcadTableDXF;
  try
    LoadDrawingFromDXF(ManualDXF, Drawing);
    try
      AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
      AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
      CheckTrue(AcadTable^.BreakManualPosition,
        'Тестовая таблица должна загрузиться в ручном режиме');
      CheckEquals(AcadTable^.ContinuationPartCount,
        CountAcadTablePositionControlPoints(AcadTable),
        'Ручной режим должен показывать ручки частей-продолжений');

      AcadTable^.BreakManualPosition := False;
      CheckFalse(AcadTable^.BreakManualPosition,
        'Отключение ручного режима должно вернуть автоматическое положение');
      CheckEquals(0, CountAcadTablePositionControlPoints(AcadTable),
        'Автоматическое положение должно скрыть ручки частей-продолжений');
      CheckFalse(AcadTable^.BreakManualPosition,
        'Повторное определение не должно снова включить ручной режим');

      AcadTable^.BreakManualPosition := True;
      CheckTrue(AcadTable^.BreakManualPosition,
        'Включение ручного режима должно сохраниться в модели');
      CheckEquals(AcadTable^.ContinuationPartCount,
        CountAcadTablePositionControlPoints(AcadTable),
        'Включение ручного режима должно вернуть ручки частей-продолжений');
    finally
      Drawing.done;
    end;
  finally
    DeleteFile(ManualDXF);
  end;
end;

procedure TAcadTableStyleTest.ManualBreakPositionSurvivesBreakLayoutPropertyChanges;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  ManualDXF: String;
  FirstGripBefore, SecondGripBefore: controlpointdesc;
  FirstGripAfter, SecondGripAfter: controlpointdesc;
begin
  ManualDXF := CreateManualPositionAcadTableDXF;
  try
    LoadDrawingFromDXF(ManualDXF, Drawing);
    try
      AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
      AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
      CheckTrue(AcadTable^.BreakManualPosition,
        'Тестовая таблица должна загрузиться в ручном режиме');
      CheckEquals(2, AcadTable^.ContinuationPartCount,
        'Тестовый DXF должен содержать две части-продолжения');
      CheckTrue(FindAcadTableControlPoint(AcadTable, 1, FirstGripBefore),
        'Ручка положения первой части-продолжения должна существовать');
      CheckTrue(FindAcadTableControlPoint(AcadTable, 2, SecondGripBefore),
        'Ручка положения второй части-продолжения должна существовать');

      AcadTable^.BreakDirection := atbdLeft;
      CheckTrue(AcadTable^.BreakManualPosition,
        'Изменение направления не должно сбрасывать ручное положение');
      CheckEquals(AcadTable^.ContinuationPartCount,
        CountAcadTablePositionControlPoints(AcadTable),
        'Ручки положения должны остаться видимыми после смены направления');
      CheckTrue(FindAcadTableControlPoint(AcadTable, 1, FirstGripAfter),
        'Ручка первой части должна сохраниться после смены направления');
      CheckTrue(FindAcadTableControlPoint(AcadTable, 2, SecondGripAfter),
        'Ручка второй части должна сохраниться после смены направления');
      CheckAcadTablePointEquals(
        FirstGripBefore.worldcoord, FirstGripAfter.worldcoord,
        'Смена направления не должна перемещать первую часть');
      CheckAcadTablePointEquals(
        SecondGripBefore.worldcoord, SecondGripAfter.worldcoord,
        'Смена направления не должна перемещать вторую часть');

      AcadTable^.BreakSpacing := AcadTable^.BreakSpacing + 10.0;
      CheckTrue(AcadTable^.BreakManualPosition,
        'Изменение интервала не должно сбрасывать ручное положение');
      CheckEquals(AcadTable^.ContinuationPartCount,
        CountAcadTablePositionControlPoints(AcadTable),
        'Ручки положения должны остаться видимыми после смены интервала');
      CheckTrue(FindAcadTableControlPoint(AcadTable, 1, FirstGripAfter),
        'Ручка первой части должна сохраниться после смены интервала');
      CheckTrue(FindAcadTableControlPoint(AcadTable, 2, SecondGripAfter),
        'Ручка второй части должна сохраниться после смены интервала');
      CheckAcadTablePointEquals(
        FirstGripBefore.worldcoord, FirstGripAfter.worldcoord,
        'Смена интервала не должна перемещать первую часть');
      CheckAcadTablePointEquals(
        SecondGripBefore.worldcoord, SecondGripAfter.worldcoord,
        'Смена интервала не должна перемещать вторую часть');

      AcadTable^.BreakHeight := AcadTable^.BreakHeight + 0.001;
      CheckTrue(AcadTable^.BreakManualPosition,
        'Изменение высоты не должно сбрасывать ручное положение');
      CheckEquals(AcadTable^.ContinuationPartCount,
        CountAcadTablePositionControlPoints(AcadTable),
        'Ручки положения должны остаться видимыми после смены высоты');
      CheckTrue(FindAcadTableControlPoint(AcadTable, 1, FirstGripAfter),
        'Ручка первой части должна сохраниться после смены высоты');
      CheckTrue(FindAcadTableControlPoint(AcadTable, 2, SecondGripAfter),
        'Ручка второй части должна сохраниться после смены высоты');
      CheckAcadTablePointEquals(
        FirstGripBefore.worldcoord, FirstGripAfter.worldcoord,
        'Смена высоты не должна перемещать первую часть');
      CheckAcadTablePointEquals(
        SecondGripBefore.worldcoord, SecondGripAfter.worldcoord,
        'Смена высоты не должна перемещать вторую часть');
    finally
      Drawing.done;
    end;
  finally
    DeleteFile(ManualDXF);
  end;
end;

procedure TAcadTableStyleTest.BreakEnabledAddsFirstPartBreakHeightGrip;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckTrue(AcadTable^.BreakEnabled,
      'Тестовый DXF должен содержать разбитую таблицу');
    CheckFalse(AcadTable^.BreakManualHeight,
      'Исходная таблица не должна загружаться с ручной высотой разбиения');
    CheckEquals(1, CountAcadTableBreakHeightControlPoints(AcadTable),
      'При BreakEnabled=True должна быть ручка высоты первой части');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.ManualBreakHeightAddsGripForEveryTablePart;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  ManualDXF: String;
begin
  ManualDXF := CreateManualHeightAcadTableDXF;
  try
    LoadDrawingFromDXF(ManualDXF, Drawing);
    try
      AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
      AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
      CheckEquals(2, AcadTable^.ContinuationPartCount,
        'Тестовый DXF должен содержать две части-продолжения');
      CheckTrue(AcadTable^.BreakManualHeight,
        'DXF с флагом 295=1 должен включить ручную высоту разбиения');
      CheckEquals(1 + AcadTable^.ContinuationPartCount,
        CountAcadTableBreakHeightControlPoints(AcadTable),
        'Ручная высота должна показать ручку у каждой части таблицы');
    finally
      Drawing.done;
    end;
  finally
    DeleteFile(ManualDXF);
  end;
end;

procedure TAcadTableStyleTest.TogglingBreakManualHeightUpdatesPartGrips;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckFalse(AcadTable^.BreakManualHeight,
      'Исходная таблица должна загрузиться с автоматической высотой');
    CheckEquals(1, CountAcadTableBreakHeightControlPoints(AcadTable),
      'Автоматическая высота должна показывать только ручку первой части');

    AcadTable^.BreakManualHeight := True;
    CheckTrue(AcadTable^.BreakManualHeight,
      'Включение ручной высоты должно сохраниться в модели');
    CheckEquals(1 + AcadTable^.ContinuationPartCount,
      CountAcadTableBreakHeightControlPoints(AcadTable),
      'Ручная высота должна включить ручки всех частей');

    AcadTable^.BreakManualHeight := False;
    CheckFalse(AcadTable^.BreakManualHeight,
      'Отключение ручной высоты должно сохраниться в модели');
    CheckEquals(1, CountAcadTableBreakHeightControlPoints(AcadTable),
      'Отключение ручной высоты должно оставить только ручку первой части');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.DraggingContinuationBreakHeightGripResegmentsPart;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  Grip: controlpointdesc;
  RTMod: TRTModifyData;
  PartRowsBefore: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'Тестовый DXF должен содержать две части-продолжения');

    AcadTable^.BreakManualHeight := True;
    CheckTrue(
      FindAcadTableControlPoint(
        AcadTable, CAcadTableBreakHeightGripVertexBase + 1, Grip),
      'Ручка высоты первой части-продолжения должна существовать');
    PartRowsBefore := AcadTable^.ContinuationPartRowCount(0);
    CheckTrue(PartRowsBefore > 0,
      'Первая часть-продолжение должна содержать строки до перетаскивания');

    FillChar(RTMod, SizeOf(RTMod), 0);
    RTMod.point := Grip;
    RTMod.dist := CreateVertex(0, AcadTable^.BreakHeight + 10.0, 0);
    RTMod.wc := VertexAdd(Grip.worldcoord, RTMod.dist);
    AcadTable^.rtmodifyonepoint(RTMod);

    CheckTrue(AcadTable^.BreakManualHeight,
      'Перетаскивание ручки продолжения должно оставить ручную высоту включённой');
    CheckTrue(AcadTable^.ContinuationPartRowCount(0) < PartRowsBefore,
      'Перетаскивание ручки высоты продолжения должно пересегментировать ' +
      'строки этой части');
  finally
    Drawing.done;
  end;
end;

// issue #1309, часть 1. Файл test/tablerazdel.dxf — разорванная таблица из
// трёх сегментов. Главная часть: строка 0 «Zagolovok» (Title), строка 1
// «A..E» (Header), далее строки данных. Каждый сегмент-продолжение начинается
// с тех же строк «Zagolovok» и «A..E», то есть таблица была разорвана с
// повтором верхних меток. Признак должен определяться по содержимому, а не по
// флагам подавления заголовков (реальный DXF приходит с TableFlags=22).
procedure TAcadTableStyleTest.DetectsBreakRepeatTopOnLoad;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'Таблица должна содержать две части-продолжения');
    CheckTrue(AcadTable^.BreakRepeatTopLabels,
      'Повтор верхних меток должен определяться как True');
    // Обе части-продолжения должны начинаться с повторённых строк-меток.
    CheckEquals('Zagolovok', AcadTable^.ContinuationPartCellText(0, 0, 0),
      'Часть 0, строка 0 должна повторять заголовок «Zagolovok»');
    CheckEquals('A', AcadTable^.ContinuationPartCellText(0, 1, 0),
      'Часть 0, строка 1 должна повторять шапку «A..E»');
    CheckEquals('Zagolovok', AcadTable^.ContinuationPartCellText(1, 0, 0),
      'Часть 1, строка 0 должна повторять заголовок «Zagolovok»');
    CheckEquals('A', AcadTable^.ContinuationPartCellText(1, 1, 0),
      'Часть 1, строка 1 должна повторять шапку «A..E»');
  finally
    Drawing.done;
  end;
end;

// issue #1309, часть 2 / issue #1311. Установка BreakRepeatTopLabels=False
// должна удалить повторяющиеся ведущие строки-метки (Title+Header, две строки)
// и пересегментировать строки по BreakHeight: освободившееся место заполняется
// строками из следующих частей, пустые части исчезают. Возврат в True добавляет
// метки обратно и снова пересегментирует таблицу.
procedure TAcadTableStyleTest.TogglingBreakRepeatTopAddsAndRemovesLabelRows;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  P0Before, P1Before: Integer;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    P0Before := AcadTable^.ContinuationPartRowCount(0);
    P1Before := AcadTable^.ContinuationPartRowCount(1);
    CheckTrue(AcadTable^.BreakRepeatTopLabels,
      'Исходно повтор верхних меток должен быть True');

    // Снимаем повтор: две строки-метки удаляются, строки следующих частей
    // подтягиваются в освободившееся место, пустая часть исчезает.
    AcadTable^.BreakRepeatTopLabels := False;
    CheckEquals(1, AcadTable^.ContinuationPartCount,
      'После снятия повтора строки должны перераспределиться без пустой части');
    CheckEquals(P0Before, AcadTable^.ContinuationPartRowCount(0),
      'Первая часть-продолжение должна заполнить освободившееся место');
    CheckEquals(-1, AcadTable^.ContinuationPartRowCount(1),
      'Пустая вторая часть-продолжение должна исчезнуть');
    CheckFalse(AcadTable^.RecomputeBreakRepeatTopLabels,
      'После удаления меток автоопределение должно давать False');
    // Первой строкой части 0 теперь должна стать строка данных «11».
    CheckEquals('11', AcadTable^.ContinuationPartCellText(0, 0, 0),
      'После снятия повтора часть 0 должна начинаться со строки данных');

    // Возвращаем повтор: строки-метки добавляются обратно.
    AcadTable^.BreakRepeatTopLabels := True;
    CheckEquals(2, AcadTable^.ContinuationPartCount,
      'После возврата повтора таблица снова должна разбиться на две части-продолжения');
    CheckEquals(P0Before, AcadTable^.ContinuationPartRowCount(0),
      'После возврата повтора число строк части 0 должно восстановиться');
    CheckEquals(P1Before, AcadTable^.ContinuationPartRowCount(1),
      'После возврата повтора число строк части 1 должно восстановиться');
    CheckTrue(AcadTable^.RecomputeBreakRepeatTopLabels,
      'После добавления меток автоопределение должно давать True');
    CheckEquals('Zagolovok', AcadTable^.ContinuationPartCellText(0, 0, 0),
      'После возврата повтора часть 0 снова начинается с «Zagolovok»');
    CheckEquals('A', AcadTable^.ContinuationPartCellText(0, 1, 0),
      'После возврата повтора часть 0 снова содержит шапку «A..E»');
  finally
    Drawing.done;
  end;
end;

// issue #1375. При повторе верхних меток зона повтора должна включать все
// ведущие строки со стилями Title/Header до первой строки Data. Таблица
// Title/Header/Header/Data раньше повторяла только первые две строки, потому
// что ComputeTopLabelRowCount жёстко ограничивал зону парой Title+Header.
procedure TAcadTableStyleTest.BreakRepeatTopUsesAllLeadingTitleHeaderRows;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  ColWidths, RowHeights: TTableSizeArray;
  Alignments: TTableAlignmentArray;
  InsertPt: TzePoint3d;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);

    System.SetLength(Texts, 6);
    Texts[0] := 'Title';
    Texts[1] := 'Header 1';
    Texts[2] := 'Header 2';
    Texts[3] := 'Data 1';
    Texts[4] := 'Data 2';
    Texts[5] := 'Data 3';
    System.SetLength(ColWidths, 0);
    System.SetLength(RowHeights, 0);
    System.SetLength(Alignments, 0);

    InsertPt := NulPoint;
    CheckTrue(Table^.BuildFromCellTextsWithSizesAndAlignments(6, 1, Texts,
      ColWidths, RowHeights, Alignments, InsertPt),
      'Построение тестовой таблицы должно завершиться успешно');

    Table^.SetRowStyleTypes([0, 1, 1, 2, 2, 2]);
    Table^.BreakRepeatTopLabels := True;
    Table^.BreakHeight := CAcadTableDefaultRowHeight * 4;
    Table^.BreakEnabled := True;

    CheckTrue(Table^.ContinuationPartCount > 0,
      'Малая высота разбиения должна создать часть-продолжение');
    CheckEquals('Title', Table^.ContinuationPartCellText(0, 0, 0),
      'Часть-продолжение должна повторять строку Title');
    CheckEquals('Header 1', Table^.ContinuationPartCellText(0, 1, 0),
      'Часть-продолжение должна повторять первую строку Header');
    CheckEquals('Header 2', Table^.ContinuationPartCellText(0, 2, 0),
      'Часть-продолжение должна повторять вторую строку Header');
    CheckEquals('Data 2', Table^.ContinuationPartCellText(0, 3, 0),
      'Первая строка данных продолжения должна идти после всех верхних меток');
  finally
    Drawing.done;
  end;
end;

// issue #1311. При снятии Repeat top labels строка данных «11» становится
// первой визуальной строкой части-продолжения. Её стиль должен остаться стилем
// данных, а не стать стилем Title/Header по новому локальному индексу строки.
procedure TAcadTableStyleTest.ClearingBreakRepeatTopKeepsDataRowFormatting;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
  SizeBefore, SizeAfter: Double;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    BuildTableGeometry(Drawing, AcadTable);
    CheckTrue(FindMTextSizeByTemplate(
      AcadTable^.ConstObjArray, '11', SizeBefore),
      'До снятия повтора должна отрисоваться строка данных 11');

    AcadTable^.BreakRepeatTopLabels := False;
    BuildTableGeometry(Drawing, AcadTable);
    CheckTrue(FindMTextSizeByTemplate(
      AcadTable^.ConstObjArray, '11', SizeAfter),
      'После снятия повтора должна отрисоваться строка данных 11');

    CheckEquals(SizeBefore, SizeAfter, 1e-6,
      'Смещение строки данных на место верхних меток не должно менять высоту текста');
  finally
    Drawing.done;
  end;
end;

// issue #1309. Неразорванная таблица (test/tablerazdel2.dxf) не имеет
// частей-продолжений, поэтому признак повтора верхних меток равен False.
procedure TAcadTableStyleTest.NonBrokenTableHasNoBreakRepeatTop;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablerazdel2.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(0, AcadTable^.ContinuationPartCount,
      'Неразорванная таблица не должна иметь частей-продолжений');
    CheckFalse(AcadTable^.BreakRepeatTopLabels,
      'Неразорванная таблица не имеет повтора верхних меток');
  finally
    Drawing.done;
  end;
end;

// issue #1344. Таблица AutoCAD хранит отрисованную графику в анонимном блоке
// (*T1, *T2, ...). При сохранении ZCAD заново и последовательно нумерует
// хэндлы секции BLOCKS, а raw-сущность ACAD_TABLE раньше сохраняла свой
// исходный хэндл (группа 5) дословно. Для разделённых таблиц с большим
// объёмом графики (файл №4: ручная высота разбиения) исходный хэндл таблицы
// совпадал с хэндлом, уже выданным MTEXT внутри анонимного блока. AutoCAD при
// открытии сообщал «Неверная метка <handle>: уже используется» и отказывался
// импортировать чертёж. Проверяем, что и главная таблица, и её части-
// продолжения получают свежие хэндлы из общего счётчика документа, исходные
// (потенциально конфликтующие) хэндлы не протекают в файл, а round-trip
// XRECORD (360/361/330) ссылается на эти же новые хэндлы.
procedure TAcadTableStyleTest.RenumbersRawAcadTableEntityHandlesToAvoidCollision;
var
  MainRaw, ContRaw: TStringList;
  MainText, ContText, DXFText: String;
  OutStream: TZctnrVectorBytes;
  SaveContext: TIODXFSaveContext;
  Drawing: TSimpleDrawing;
  Ok: Boolean;
begin
  MainRaw := TStringList.Create;
  ContRaw := TStringList.Create;
  try
    // Главная часть: исходный хэндл EB (в реальном файле №4 он совпадал с
    // хэндлом MTEXT внутри блока *T7).
    MainRaw.Add('0');   MainRaw.Add('ACAD_TABLE');
    MainRaw.Add('5');   MainRaw.Add('EB');
    MainRaw.Add('330'); MainRaw.Add('26');
    MainRaw.Add('100'); MainRaw.Add('AcDbEntity');
    MainRaw.Add('8');   MainRaw.Add('0');
    MainRaw.Add('100'); MainRaw.Add('AcDbBlockReference');
    MainRaw.Add('2');   MainRaw.Add('*T7');
    MainRaw.Add('100'); MainRaw.Add('AcDbTable');
    MainRaw.Add('342'); MainRaw.Add('FE');
    MainRaw.Add('343'); MainRaw.Add('32');
    MainText := MainRaw.Text;

    // Часть-продолжение: исходный хэндл 54C.
    ContRaw.Add('0');   ContRaw.Add('ACAD_TABLE');
    ContRaw.Add('5');   ContRaw.Add('54C');
    ContRaw.Add('330'); ContRaw.Add('26');
    ContRaw.Add('100'); ContRaw.Add('AcDbEntity');
    ContRaw.Add('8');   ContRaw.Add('0');
    ContRaw.Add('100'); ContRaw.Add('AcDbBlockReference');
    ContRaw.Add('2');   ContRaw.Add('*T8');
    ContRaw.Add('100'); ContRaw.Add('AcDbTable');
    ContRaw.Add('342'); ContRaw.Add('FE');
    ContRaw.Add('343'); ContRaw.Add('33');
    ContText := ContRaw.Text;
  finally
    MainRaw.Free;
    ContRaw.Free;
  end;

  OutStream.init(8 * 1024);
  SaveContext.InitRec;
  SaveContext.Header.Version := AC1021;
  SaveContext.Header.iVersion := 1021;
  try
    SaveContext.AcadTableOwnerHandle := $123;
    SaveContext.TableStyleNameHandleMap.Add('Standard', '4A2');
    // Эмулируем счётчик, который к началу секции ENTITIES уже прошёл BLOCKS:
    // следующий свободный хэндл — 100. Именно из него (а не из исходного EB)
    // должна нумероваться таблица.
    SaveContext.handle := $100;

    ResetAcadTableDXFWriteState;
    Ok := WriteRawAcadTablePartsToDXF(
      OutStream, SaveContext, MainText, [ContText],
      0.99, 2.05, False, True, 'Standard');
    WriteAcadTableRoundTripObjectsToDXF(OutStream, Drawing, SaveContext);
    DXFText := DxfStreamToText(OutStream);
  finally
    ResetAcadTableDXFWriteState;
    SaveContext.Done;
    OutStream.done;
  end;

  CheckTrue(Ok, 'Сырые части ACAD_TABLE должны успешно записаться');

  // Свежие хэндлы из счётчика: главная — 100, продолжение — 101.
  CheckTrue(HasDxfSequence(DXFText, ['5', '100']),
    'Главная таблица должна получить свежий хэндл 100');
  CheckTrue(HasDxfSequence(DXFText, ['5', '101']),
    'Часть-продолжение должна получить свежий хэндл 101');

  // Исходные (потенциально конфликтующие) хэндлы не должны протекать в файл.
  CheckEquals(0, CountDxfPairs(DXFText, '5', 'EB'),
    'Исходный хэндл главной таблицы EB не должен оставаться (issue #1344)');
  CheckEquals(0, CountDxfPairs(DXFText, '5', '54C'),
    'Исходный хэндл продолжения 54C не должен оставаться (issue #1344)');

  // Round-trip XRECORD должен ссылаться на новые хэндлы.
  CheckTrue(HasDxfSequence(DXFText, ['360', '100']),
    'Round-trip 360 должен указывать на новый хэндл главной таблицы');
  CheckTrue(HasDxfSequence(DXFText, ['361', '100']),
    'Round-trip 361 должен указывать на новый хэндл главной таблицы');
  CheckTrue(HasDxfSequence(DXFText, ['330', '101']),
    'Round-trip 330 должен указывать на новый хэндл продолжения');
  CheckEquals(0, CountDxfPairs(DXFText, '360', 'EB'),
    'Round-trip не должен ссылаться на исходный хэндл EB');
  CheckEquals(0, CountDxfPairs(DXFText, '330', '54C'),
    'Round-trip не должен ссылаться на исходный хэндл продолжения 54C');
end;

procedure TAcadTableStyleTest.BuildsAcadTableFromCellTexts;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  InsertPt: TzePoint3d;
  Ok: Boolean;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);

    // Заполненный диапазон 2 строки × 3 столбца. Часть ячеек пустые,
    // чтобы проверить, что отрисовывается только непустой текст.
    System.SetLength(Texts, 6);
    Texts[0] := 'A1'; Texts[1] := 'B1'; Texts[2] := 'C1';
    Texts[3] := 'A2'; Texts[4] := '';   Texts[5] := 'C2';

    InsertPt := NulPoint;
    Ok := Table^.BuildFromCellTexts(2, 3, Texts, InsertPt);
    CheckTrue(Ok, 'BuildFromCellTexts должен успешно построить таблицу');

    CheckEquals(2, Table^.RowCount,
      'Число строк должно соответствовать заполненному диапазону');
    CheckEquals(3, Table^.ColCount,
      'Число столбцов должно соответствовать заполненному диапазону');
    CheckTrue(Table^.ForceDataStyleForAllRows,
      'Все строки должны оформляться стилем Data (issue #1357)');

    // Строим геометрию и проверяем, что текст каждой непустой ячейки
    // отрисован ровно один раз, а пустая ячейка текста не создаёт.
    Table^.BuildGeometry(Drawing);
    CheckEquals(1, CountMTextByTemplate(Table^.ConstObjArray, 'A1'),
      'Ячейка A1 должна быть отрисована');
    CheckEquals(1, CountMTextByTemplate(Table^.ConstObjArray, 'C2'),
      'Ячейка C2 должна быть отрисована');
    CheckEquals(0, CountMTextByTemplate(Table^.ConstObjArray, ''),
      'Пустая ячейка не должна создавать текст');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.ExposesAcadTableDataForSpreadsheetEditing;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  ColWidths, RowHeights: TTableSizeArray;
  Alignments: TTableAlignmentArray;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);
    SetLength(Texts, 4);
    Texts[0] := 'Title'; Texts[1] := 'Value';
    Texts[2] := 'A'; Texts[3] := '1';
    SetLength(ColWidths, 2);
    ColWidths[0] := 25.0; ColWidths[1] := 35.0;
    SetLength(RowHeights, 2);
    RowHeights[0] := 12.0; RowHeights[1] := 18.0;
    SetLength(Alignments, 4);
    Alignments[0] := 1; Alignments[1] := 5;
    Alignments[2] := 7; Alignments[3] := 9;
    CheckTrue(Table^.BuildFromCellTextsWithSizesAndAlignments(
      2, 2, Texts, ColWidths, RowHeights, Alignments, NulPoint));

    CheckEquals('Value', Table^.CellTextAt(0, 1));
    CheckEquals(12.0, Table^.RowHeightAt(0), 1e-6);
    CheckEquals(35.0, Table^.ColWidthAt(1), 1e-6);
    CheckEquals(9, Table^.CellAlignmentAt(1, 1));
    CheckEquals('', Table^.CellTextAt(-1, 0));
    CheckEquals(0.0, Table^.RowHeightAt(10), 1e-6);
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.UpdatesAcadTableWithoutMovingOrChangingStyle;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  Sizes: TTableSizeArray;
  Alignments: TTableAlignmentArray;
  BeforeInsert: TzePoint3d;
  BeforeStyle: String;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);
    SetLength(Texts, 1);
    Texts[0] := 'Before';
    BeforeInsert := NulPoint;
    BeforeInsert.x := 10;
    BeforeInsert.y := 20;
    CheckTrue(Table^.BuildFromCellTexts(1, 1, Texts, BeforeInsert));
    BeforeInsert := Table^.InsertPoint;
    BeforeStyle := Table^.TableStyleName;

    Texts[0] := 'After';
    SetLength(Sizes, 1);
    Sizes[0] := 22.0;
    SetLength(Alignments, 1);
    Alignments[0] := 9;
    CheckTrue(Table^.UpdateFromCellTextsWithSizesAndAlignments(
      1, 1, Texts, Sizes, Sizes, Alignments));

    CheckEquals('After', Table^.CellTextAt(0, 0));
    CheckEquals(BeforeStyle, Table^.TableStyleName);
    CheckEquals(BeforeInsert.x, Table^.InsertPoint.x, 1e-6);
    CheckEquals(BeforeInsert.y, Table^.InsertPoint.y, 1e-6);
    CheckEquals(22.0, Table^.RowHeightAt(0), 1e-6,
      'При обновлении старая высота строки не должна оставаться первой');
    CheckEquals(22.0, Table^.ColWidthAt(0), 1e-6,
      'При обновлении старая ширина столбца не должна оставаться первой');
    CheckEquals(9, Table^.CellAlignmentAt(0, 0));
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.BuildFromCellTextsRejectsEmptyDimensions;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  InsertPt: TzePoint3d;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);

    System.SetLength(Texts, 0);
    InsertPt := NulPoint;
    CheckFalse(Table^.BuildFromCellTexts(0, 0, Texts, InsertPt),
      'Пустой диапазон не должен создавать таблицу');
    CheckEquals(0, Table^.RowCount,
      'У не построенной таблицы число строк должно остаться 0');
    CheckEquals(0, Table^.ColCount,
      'У не построенной таблицы число столбцов должно остаться 0');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.
  BuildFromCellTextsWithSizesAppliesColWidthsAndRowHeights;
const
  // Ожидаемая точность сравнения суммарных размеров таблицы
  CSizeDelta = 1e-6;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  ColWidths, RowHeights: TTableSizeArray;
  InsertPt: TzePoint3d;
  Ok: Boolean;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);

    System.SetLength(Texts, 6);
    Texts[0] := 'A1'; Texts[1] := 'B1'; Texts[2] := 'C1';
    Texts[3] := 'A2'; Texts[4] := 'B2'; Texts[5] := 'C2';

    // Явные ширины столбцов (3) и высоты строк (2)
    System.SetLength(ColWidths, 3);
    ColWidths[0] := 40; ColWidths[1] := 50; ColWidths[2] := 60;
    System.SetLength(RowHeights, 2);
    RowHeights[0] := 20; RowHeights[1] := 30;

    InsertPt := NulPoint;
    Ok := Table^.BuildFromCellTextsWithSizes(2, 3, Texts,
      ColWidths, RowHeights, InsertPt);
    CheckTrue(Ok, 'BuildFromCellTextsWithSizes должен построить таблицу');

    // Суммарная ширина = 40+50+60, суммарная высота = 20+30
    CheckEquals(150.0, Table^.Width, CSizeDelta,
      'Суммарная ширина должна равняться сумме ширин столбцов');
    CheckEquals(50.0, Table^.Height, CSizeDelta,
      'Суммарная высота должна равняться сумме высот строк');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.BuildFromCellTextsWithSizesFallsBackToDefaults;
const
  CSizeDelta = 1e-6;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  ColWidths, RowHeights: TTableSizeArray;
  InsertPt: TzePoint3d;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);

    System.SetLength(Texts, 3);
    Texts[0] := 'A1'; Texts[1] := 'B1'; Texts[2] := 'C1';

    // Один столбец задан, один нулевой (-> по умолчанию), один отсутствует.
    // Массив высот строк пуст -> высота по умолчанию.
    System.SetLength(ColWidths, 2);
    ColWidths[0] := 45; ColWidths[1] := 0;
    System.SetLength(RowHeights, 0);

    InsertPt := NulPoint;
    CheckTrue(Table^.BuildFromCellTextsWithSizes(1, 3, Texts,
      ColWidths, RowHeights, InsertPt),
      'BuildFromCellTextsWithSizes должен построить таблицу');

    // Ширина = 45 (задан) + 30 (нулевой -> по умолчанию)
    //         + 30 (отсутствует -> по умолчанию) = 105
    CheckEquals(45.0 + CAcadTableDefaultColWidth + CAcadTableDefaultColWidth,
      Table^.Width, CSizeDelta,
      'Отсутствующие/нулевые ширины должны заменяться значением по умолчанию');
    // Высота единственной строки = высота по умолчанию
    CheckEquals(CAcadTableDefaultRowHeight, Table^.Height, CSizeDelta,
      'Пустой массив высот должен давать высоту строки по умолчанию');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.BuildFromCellTextsAppliesCellAlignments;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  ColWidths, RowHeights: TTableSizeArray;
  Alignments: TTableAlignmentArray;
  InsertPt: TzePoint3d;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);

    System.SetLength(Texts, 6);
    Texts[0] := 'A1'; Texts[1] := 'B1'; Texts[2] := 'C1';
    Texts[3] := 'A2'; Texts[4] := 'B2'; Texts[5] := 'C2';

    System.SetLength(ColWidths, 0);
    System.SetLength(RowHeights, 0);

    // Коды AutoCAD: 1=TopLeft, 5=MiddleCenter, 9=BottomRight.
    // Последняя ячейка (индекс 5) не задана в массиве -> 0 (наследование).
    System.SetLength(Alignments, 5);
    Alignments[0] := 1; Alignments[1] := 5; Alignments[2] := 9;
    Alignments[3] := 4; Alignments[4] := 0;

    InsertPt := NulPoint;
    CheckTrue(Table^.BuildFromCellTextsWithSizesAndAlignments(2, 3, Texts,
      ColWidths, RowHeights, Alignments, InsertPt),
      'BuildFromCellTextsWithSizesAndAlignments должен построить таблицу');

    CheckEquals(1, Table^.CellAlignmentAt(0, 0),
      'Ячейка [0,0] должна получить выравнивание TopLeft (1)');
    CheckEquals(5, Table^.CellAlignmentAt(0, 1),
      'Ячейка [0,1] должна получить выравнивание MiddleCenter (5)');
    CheckEquals(9, Table^.CellAlignmentAt(0, 2),
      'Ячейка [0,2] должна получить выравнивание BottomRight (9)');
    CheckEquals(4, Table^.CellAlignmentAt(1, 0),
      'Ячейка [1,0] должна получить выравнивание MiddleLeft (4)');
    CheckEquals(0, Table^.CellAlignmentAt(1, 1),
      'Ячейка [1,1] с явным 0 должна наследовать выравнивание (0)');
    CheckEquals(0, Table^.CellAlignmentAt(1, 2),
      'Ячейка [1,2] вне массива должна наследовать выравнивание (0)');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.SavesTransformedInsertPointAndCellAlignmentToDXF;
const
  InsertX = 125.5;
  InsertY = -42.25;
  InsertZ = 7.0;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  ColWidths, RowHeights: TTableSizeArray;
  Alignments: TTableAlignmentArray;
  InsertPt: TzePoint3d;
  MoveMatrix: TzeTypedMatrix4d;
  OutStream: TZctnrVectorBytes;
  SaveContext: TIODXFSaveContext;
  DXFText: String;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);

    System.SetLength(Texts, 1);
    Texts[0] := 'Aligned';
    System.SetLength(ColWidths, 0);
    System.SetLength(RowHeights, 0);
    System.SetLength(Alignments, 1);
    Alignments[0] := 9;
    InsertPt := NulPoint;
    CheckTrue(Table^.BuildFromCellTextsWithSizesAndAlignments(
      1, 1, Texts, ColWidths, RowHeights, Alignments, InsertPt),
      'Таблица для проверки round-trip должна быть построена');

    // Повторяем финальный перенос из ConstructObjRoot при выборе точки.
    MoveMatrix := CreateTranslationMatrix(InsertX, InsertY, InsertZ);
    Table^.transform(MoveMatrix);

    OutStream.init(8 * 1024);
    SaveContext.InitRec;
    SaveContext.Header.Version := AC1021;
    SaveContext.Header.iVersion := 1021;
    try
      Table^.DXFOut(OutStream, Drawing, SaveContext);
      DXFText := DxfStreamToText(OutStream);
    finally
      SaveContext.Done;
      OutStream.done;
    end;

    CheckTrue(HasDxfSequence(DXFText, [
      '100', 'AcDbBlockReference', '2', '*T1',
      '10', '125.5', '20', '-42.25', '30', '7.0']),
      'DXF должен сохранять фактическую точку вставки после переноса');
    CheckTrue(HasDxfSequence(DXFText, [
      '171', '1', '172', '0', '173', '0', '174', '0',
      '175', '1', '176', '1', '91', '262145', '178', '0',
      '145', '0', '170', '9', '92', '0']),
      'Group 170 должен сохранять выравнивание в AutoCAD-совместимой позиции');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.SetRowStyleTypesAssignsRowStyles;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  ColWidths, RowHeights: TTableSizeArray;
  Alignments: TTableAlignmentArray;
  InsertPt: TzePoint3d;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);

    System.SetLength(Texts, 9);
    Texts[0] := 'A1'; Texts[1] := 'B1'; Texts[2] := 'C1';
    Texts[3] := 'A2'; Texts[4] := 'B2'; Texts[5] := 'C2';
    Texts[6] := 'A3'; Texts[7] := 'B3'; Texts[8] := 'C3';

    System.SetLength(ColWidths, 0);
    System.SetLength(RowHeights, 0);
    System.SetLength(Alignments, 0);

    InsertPt := NulPoint;
    CheckTrue(Table^.BuildFromCellTextsWithSizesAndAlignments(3, 3, Texts,
      ColWidths, RowHeights, Alignments, InsertPt),
      'BuildFromCellTextsWithSizesAndAlignments должен построить таблицу');

    // Программно созданная таблица до назначения типов состоит из Data.
    CheckEquals(2, Table^.RowStyleTypeAt(0),
      'До SetRowStyleTypes программная строка должна иметь тип Data');

    // Назначаем: строка 0 = Title (0), строка 1 = Header (1), строка 2 = Data (2).
    Table^.SetRowStyleTypes([0, 1, 2]);

    CheckEquals(0, Table^.RowStyleTypeAt(0),
      'Строка 0 должна получить тип Title (0)');
    CheckEquals(1, Table^.RowStyleTypeAt(1),
      'Строка 1 должна получить тип Header (1)');
    CheckEquals(2, Table^.RowStyleTypeAt(2),
      'Строка 2 должна получить тип Data (2)');
    CheckEquals(-1, Table^.RowStyleTypeAt(3),
      'Строка вне диапазона должна вернуть -1');
    CheckEquals(-1, Table^.RowStyleTypeAt(-1),
      'Отрицательный индекс строки должен вернуть -1');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.SetCellStyleTypesKeepsMixedRowStyles;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  ColWidths, RowHeights: TTableSizeArray;
  Alignments: TTableAlignmentArray;
  InsertPt: TzePoint3d;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);

    System.SetLength(Texts, 3);
    Texts[0] := 'Title';
    Texts[1] := 'Header';
    Texts[2] := 'Data';
    System.SetLength(ColWidths, 0);
    System.SetLength(RowHeights, 0);
    System.SetLength(Alignments, 0);
    InsertPt := NulPoint;
    CheckTrue(Table^.BuildFromCellTextsWithSizesAndAlignments(
      1, 3, Texts, ColWidths, RowHeights, Alignments, InsertPt),
      'Таблица со смешанными стилями должна быть построена');

    Table^.SetCellStyleTypes([0, 1, 2]);
    CheckEquals(0, Table^.CellStyleTypeAt(0, 0),
      'Первая ячейка должна сохранить Title');
    CheckEquals(1, Table^.CellStyleTypeAt(0, 1),
      'Вторая ячейка той же строки должна сохранить Header');
    CheckEquals(2, Table^.CellStyleTypeAt(0, 2),
      'Третья ячейка той же строки должна сохранить Data');
    CheckEquals(-1, Table^.CellStyleTypeAt(0, 3),
      'Ячейка вне диапазона должна вернуть -1');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.RebuildResetsRowStyleTypes;
var
  Drawing: TSimpleDrawing;
  Table: PGDBObjAcadTable;
  Texts: TTableTextArray;
  ColWidths, RowHeights: TTableSizeArray;
  Alignments: TTableAlignmentArray;
  InsertPt: TzePoint3d;
begin
  Drawing.init(nil);
  try
    Table := AllocAndInitAcadTable(
      PGDBObjGenericWithSubordinated(Drawing.pObjRoot));
    Table^.bp.ListPos.Owner := Drawing.pObjRoot;
    Drawing.pObjRoot^.ObjArray.AddPEntity(Table^);

    System.SetLength(Texts, 4);
    Texts[0] := 'A1'; Texts[1] := 'B1';
    Texts[2] := 'A2'; Texts[3] := 'B2';
    System.SetLength(ColWidths, 0);
    System.SetLength(RowHeights, 0);
    System.SetLength(Alignments, 0);

    InsertPt := NulPoint;
    CheckTrue(Table^.BuildFromCellTextsWithSizesAndAlignments(2, 2, Texts,
      ColWidths, RowHeights, Alignments, InsertPt),
      'Первичное построение таблицы должно завершиться успешно');

    Table^.SetRowStyleTypes([0, 1]);
    CheckEquals(0, Table^.RowStyleTypeAt(0),
      'Строка 0 должна иметь тип Title после назначения');

    // Перестроение таблицы должно сбросить ранее заданные типы строк.
    CheckTrue(Table^.BuildFromCellTextsWithSizesAndAlignments(2, 2, Texts,
      ColWidths, RowHeights, Alignments, InsertPt),
      'Повторное построение таблицы должно завершиться успешно');

    CheckEquals(2, Table^.RowStyleTypeAt(0),
      'После перестроения строка 0 должна вернуться к типу Data');
    CheckEquals(2, Table^.RowStyleTypeAt(1),
      'После перестроения строка 1 должна вернуться к типу Data');
  finally
    Drawing.done;
  end;
end;

procedure TAcadTableStyleTest.LoadsLegacyRowStylesForSpreadsheetEditing;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../experiments/issue1399/zcadtablevyrav.txt'),
    Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckEquals(4, AcadTable^.RowCount, 'Ожидались четыре строки');

    CheckEquals(0, AcadTable^.RowStyleTypeAt(0),
      'Первая legacy-строка должна открываться как Title');
    CheckEquals(1, AcadTable^.RowStyleTypeAt(1),
      'Вторая legacy-строка должна открываться как Header');
    CheckEquals(2, AcadTable^.RowStyleTypeAt(2),
      'Третья legacy-строка должна открываться как Data');
    CheckEquals(2, AcadTable^.RowStyleTypeAt(3),
      'Четвёртая legacy-строка должна открываться как Data');
  finally
    Drawing.done;
  end;
end;

// issue #1373. tablebugheader.dxf содержит одиночную таблицу 6×3, у которой
// строка 0 — стиль Title, строки 1 И 2 — стиль Header, остальные — Data.
// Позиционная логика legacy AcDbTable-парсера жёстко предполагает ровно одну
// строку Title и одну Header, поэтому раньше третья строка (индекс 2) ошибочно
// загружалась как Data. Числа строк-заголовков нет в legacy-потоке ячеек —
// оно хранится только в современном объекте AcDbTableContent, где каждый
// маркер TABLEROW_BEGIN несёт group 90 с типом строки (1=Title, 2=Header,
// 3=Data). Загрузчик читает эти типы pre-scan'ом и передаёт в таблицу через
// SetRowStyleTypes, так что RowStyleTypeAt отражает реальные типы строк.
procedure TAcadTableStyleTest.LoadsMultipleHeaderRowsFromDXF;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablebugheader.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);

    CheckEquals(0, AcadTable^.RowStyleTypeAt(0),
      'Строка 0 должна загружаться со стилем Title (0)');
    CheckEquals(1, AcadTable^.RowStyleTypeAt(1),
      'Строка 1 должна загружаться со стилем Header (1)');
    // Ключевая проверка issue #1373: вторая строка-заголовок раньше ошибочно
    // определялась как Data.
    CheckEquals(1, AcadTable^.RowStyleTypeAt(2),
      'Строка 2 (вторая строка-заголовок) должна загружаться со стилем Header (1)');
    CheckEquals(2, AcadTable^.RowStyleTypeAt(3),
      'Строка 3 должна загружаться со стилем Data (2)');
  finally
    Drawing.done;
  end;
end;

// issue #1373. Разорванная (составная) таблица tablebugheader3.dxf хранится как
// несколько сущностей ACAD_TABLE без современного объекта AcDbTableContent,
// поэтому явных типов строк в DXF нет. Строка 0 = Title, строки 1 и 2 = Header,
// остальные = Data. Раньше legacy-логика ограничивала зону меток парой
// Title+Header, из-за чего вторая строка-заголовок (строка 2) определялась как
// Data: при изменении высоты разбиения она не входила в повторяющиеся верхние
// метки и дублировалась. После исправления типы ведущих строк восстанавливаются
// по числу строк, одинаково повторяющихся в начале каждой части-продолжения, и
// строка 2 корректно получает стиль Header.
procedure TAcadTableStyleTest.LoadsMultipleHeaderRowsFromSplitDXF;
var
  Drawing: TSimpleDrawing;
  AcadTable: PGDBObjAcadTable;
begin
  LoadDrawingFromDXF(
    ExpandFileName('../../../cad_source/test/tablebugheader3.dxf'), Drawing);
  try
    AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
    AssertNotNull('Ожидалась сущность AcadTable', AcadTable);
    CheckTrue(AcadTable^.ContinuationPartCount > 0,
      'Разорванная таблица должна содержать части-продолжения');
    CheckTrue(AcadTable^.BreakRepeatTopLabels,
      'Повтор верхних меток должен определяться как True');

    // Восстановленные по повторяющимся меткам типы ведущих строк.
    CheckEquals(0, AcadTable^.RowStyleTypeAt(0),
      'Строка 0 должна восстанавливаться со стилем Title (0)');
    CheckEquals(1, AcadTable^.RowStyleTypeAt(1),
      'Строка 1 должна восстанавливаться со стилем Header (1)');
    // Ключевая проверка issue #1373: вторая строка-заголовок разорванной
    // таблицы раньше ошибочно определялась как Data и дублировалась при
    // изменении высоты разбиения.
    CheckEquals(1, AcadTable^.RowStyleTypeAt(2),
      'Строка 2 (вторая строка-заголовок) должна восстанавливаться со стилем Header (1)');
    CheckEquals(2, AcadTable^.RowStyleTypeAt(3),
      'Строка 3 должна восстанавливаться со стилем Data (2)');
  finally
    Drawing.done;
  end;
end;

begin
  RegisterTests([TAcadTableStyleTest]);
end.
