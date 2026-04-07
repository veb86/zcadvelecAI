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
  Модуль: uzestylesmleaderdxf
  Назначение: импорт и экспорт стилей мультивыносок (MLEADERSTYLE)
    из/в формат DXF.
  Реализован по аналогии с uzestylestablesdxf.pas для стилей таблиц.

  Стили мультивыносок в DXF хранятся в секции OBJECTS в виде
  объектов MLEADERSTYLE, связанных через словарь ACAD_MLEADERSTYLE.

  Модуль полностью самодостаточен: все необходимые типы данных
  определены внутри. Зависимости: UGDBNamedObjectsArray,
  gzctnrVectorTypes, uzeNamedObject, uzclog, sysutils, Classes
}
unit uzestylesmleaderdxf;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}
interface
uses
  UGDBNamedObjectsArray,
  uzeNamedObject,
  gzctnrVectorTypes,
  uzclog,
  uzcinterface,
  sysutils,
  Classes;

{ === Типы данных для DXF-стилей мультивыносок === }

type
  { Стиль мультивыноски для DXF-обмена — именованный объект.
    Содержит все параметры, необходимые для записи/чтения
    MLEADERSTYLE в DXF. Параметры соответствуют group codes
    из спецификации DXF для объекта AcDbMLeaderStyle. }
  TGDBDXFMLeaderStyle = object(GDBNamedObject)
    { Тип линии выноски: 0=прямая, 1=сплайн, 2=нет (код 170) }
    LeaderLineType: Integer;
    { Цвет линии выноски (код 91) }
    LeaderLineColor: Integer;
    { Тип стрелки выноски (код 171) }
    LeaderLineTypeId: Integer;
    { Ограничение первого сегмента (код 172) }
    FirstSegAngleConstraint: Integer;
    { Ограничение второго сегмента (код 173) }
    SecondSegAngleConstraint: Integer;
    { Тип содержимого: 0=нет, 1=блок, 2=мтекст (код 90) }
    ContentType: Integer;
    { Угол первого сегмента (код 40) }
    FirstSegAngle: Double;
    { Угол второго сегмента (код 41) }
    SecondSegAngle: Double;
    { Хэндл типа линии выноски (код 340, LTYPE) }
    LeaderLinetypeHandle: string;
    { Имя типа линии выноски (разрешённое из хэндла) }
    LeaderLinetypeName: string;
    { Тип присоединения текста слева (код 174) }
    TextAttachmentLeft: Integer;
    { Тип присоединения текста справа (код 178) }
    TextAttachmentRight: Integer;
    { Выравнивание текста по горизонтали (код 175) }
    TextAngleType: Integer;
    { Режим выравнивания (код 176) }
    TextAlignmentType: Integer;
    { Режим соединения (код 177) }
    TextAttachmentDirection: Integer;
    { Цвет линии соединения (код 92) }
    LeaderLineWeight: Integer;
    { Наличие площадки (код 290) }
    EnableDogleg: Boolean;
    { Расстояние площадки (код 42) }
    DoglegLength: Double;
    { Наличие рамки текста (код 291) }
    EnableLanding: Boolean;
    { Длина площадки (код 43) }
    LandingGap: Double;
    { Имя стиля мультивыноски (код 3) }
    Description: string;
    { Хэндл блока стрелки (код 341, BLOCK_RECORD) }
    ArrowHeadBlockHandle: string;
    { Имя блока стрелки (разрешённое из хэндла) }
    ArrowHeadBlockName: string;
    { Масштаб содержимого (код 44) }
    TextHeight: Double;
    { Имя текстового стиля по умолчанию (код 300) }
    DefaultTextContent: string;
    { Хэндл текстового стиля (код 342, STYLE) }
    TextStyleHandle: string;
    { Имя текстового стиля (разрешённое из хэндла) }
    TextStyleName: string;
    { Цвет текста мультивыноски (код 93) }
    TextColor: Integer;
    { Расстояние от площадки (код 45) }
    ArrowHeadSize: Double;
    { Наличие выравнивания текста сверху/снизу (код 292) }
    TextAlignAlwaysLeft: Boolean;
    { Выравнивание по направлению (код 297) }
    AlignSpace: Boolean;
    { Масштаб блока (код 46) }
    BlockContentScale: Double;
    { Хэндл блока содержимого (код 343, BLOCK_RECORD) }
    BlockContentHandle: string;
    { Имя блока содержимого (разрешённое из хэндла) }
    BlockContentName: string;
    { Цвет блока содержимого (код 94) }
    BlockContentColor: Integer;
    { Множитель масштаба (код 47) }
    BlockContentScaleX: Double;
    { Масштаб Y (код 49) }
    BlockContentScaleY: Double;
    { Общий масштаб (код 140) }
    OverallScale: Double;
    { Аннотативный (код 293) }
    Annotative: Boolean;
    { Расстояние от точки разрыва (код 141) }
    BreakGapSize: Double;
    { Текст по направлению (код 294) }
    TextDirectionNegative: Boolean;
    { Присоединение сверху/снизу (код 295) }
    IsBlockContent: Boolean;
    { Содержимое: мультитекст (код 296) }
    IsMTextContent: Boolean;
    { Масштаб блока Z (код 142) }
    BlockContentScaleZ: Double;
    { Флаг поворота блока (код 143) }
    BlockContentRotation: Double;
    { Хэндл расширенного словаря (блок 102/ACAD_XDICTIONARY) }
    XDictHandle: string;
    { Версия ACAD_MLEADERVER (xdata код 1070) }
    MLeaderVersion: Integer;
    constructor init(const StyleName: string);
    destructor Done; virtual;
  end;
  PTGDBDXFMLeaderStyle = ^TGDBDXFMLeaderStyle;

  { Массив стилей мультивыносок для DXF-обмена }
  GDBDXFMLeaderStyleArray = object(
    GDBNamedObjectsArray<PTGDBDXFMLeaderStyle,
                         TGDBDXFMLeaderStyle>)
    constructor init(InitialCapacity: Integer);
    constructor initnul;
    { Добавляет стиль или возвращает существующий }
    function AddStyle(
      const StyleName: string): PTGDBDXFMLeaderStyle;
  end;
  PGDBDXFMLeaderStyleArray = ^GDBDXFMLeaderStyleArray;

{ Загружает стили мультивыносок из секции OBJECTS DXF-файла.
  Находит словарь ACAD_MLEADERSTYLE и все объекты MLEADERSTYLE,
  восстанавливает имена стилей из словаря и заполняет таблицу.
  Для разрешения хэндлов ссылок (340-343) в имена объектов
  используется секция TABLES.
  Параметры:
    RawObjectsSection — полный текст секции OBJECTS
    RawTablesSection — полный текст секции TABLES
    MLeaderStyleTable — таблица стилей для заполнения }
procedure ReadMLeaderStylesFromDXFObjects(
  const RawObjectsSection: string;
  const RawTablesSection: string;
  var MLeaderStyleTable: GDBDXFMLeaderStyleArray);

{ Записывает стили мультивыносок в секцию OBJECTS DXF-файла.
  Существующие MLEADERSTYLE заменяются, новые добавляются.
  Параметры:
    MLeaderStyleTable — таблица стилей для записи
    RawObjectsSection — текст секции OBJECTS (in/out) }
procedure WriteMLeaderStylesToDXFObjects(
  var MLeaderStyleTable: GDBDXFMLeaderStyleArray;
  var RawObjectsSection: string);

implementation

{ === Конструктор и деструктор TGDBDXFMLeaderStyle === }

{ Инициализирует стиль мультивыноски значениями по умолчанию,
  соответствующими AutoCAD Standard MLEADERSTYLE }
constructor TGDBDXFMLeaderStyle.init(
  const StyleName: string);
begin
  inherited Init(StyleName);
  LeaderLineType := 2;
  LeaderLineColor := -1056964608;
  LeaderLineTypeId := 1;
  FirstSegAngleConstraint := 0;
  SecondSegAngleConstraint := 1;
  ContentType := 2;
  FirstSegAngle := 0.0;
  SecondSegAngle := 0.0;
  { Инициализируем строки через nil для raw memory }
  pointer(LeaderLinetypeHandle) := nil;
  pointer(LeaderLinetypeName) := nil;
  TextAttachmentLeft := 1;
  TextAttachmentRight := 1;
  TextAngleType := 1;
  TextAlignmentType := 0;
  TextAttachmentDirection := 0;
  LeaderLineWeight := -2;
  EnableDogleg := True;
  DoglegLength := 0.09;
  EnableLanding := True;
  LandingGap := 0.36;
  pointer(Description) := nil;
  pointer(ArrowHeadBlockHandle) := nil;
  pointer(ArrowHeadBlockName) := nil;
  TextHeight := 0.18;
  pointer(DefaultTextContent) := nil;
  pointer(TextStyleHandle) := nil;
  pointer(TextStyleName) := nil;
  TextColor := -1056964608;
  ArrowHeadSize := 0.18;
  TextAlignAlwaysLeft := False;
  AlignSpace := False;
  BlockContentScale := 0.18;
  pointer(BlockContentHandle) := nil;
  pointer(BlockContentName) := nil;
  BlockContentColor := -1056964608;
  BlockContentScaleX := 1.0;
  BlockContentScaleY := 1.0;
  OverallScale := 1.0;
  Annotative := True;
  BreakGapSize := 0.0;
  TextDirectionNegative := True;
  IsBlockContent := False;
  IsMTextContent := False;
  BlockContentScaleZ := 1.0;
  BlockContentRotation := 0.125;
  pointer(XDictHandle) := nil;
  MLeaderVersion := 2;
end;

{ Освобождает ресурсы строковых полей }
destructor TGDBDXFMLeaderStyle.Done;
begin
  inherited Done;
  LeaderLinetypeHandle := '';
  LeaderLinetypeName := '';
  Description := '';
  ArrowHeadBlockHandle := '';
  ArrowHeadBlockName := '';
  DefaultTextContent := '';
  TextStyleHandle := '';
  TextStyleName := '';
  BlockContentHandle := '';
  BlockContentName := '';
  XDictHandle := '';
end;

{ === Методы GDBDXFMLeaderStyleArray === }

constructor GDBDXFMLeaderStyleArray.init(
  InitialCapacity: Integer);
begin
  inherited init(InitialCapacity);
end;

constructor GDBDXFMLeaderStyleArray.initnul;
begin
  inherited initnul;
end;

{ Добавляет стиль с заданным именем или возвращает
  существующий }
function GDBDXFMLeaderStyleArray.AddStyle(
  const StyleName: string): PTGDBDXFMLeaderStyle;
var
  StylePtr: PTGDBDXFMLeaderStyle;
begin
  case AddItem(StyleName, pointer(StylePtr)) of
    IsFounded:
      { Стиль уже существует — возвращаем без изменений };
    IsCreated:
      StylePtr^.init(StyleName);
    IsError:
      StylePtr := nil;
  end;
  Result := StylePtr;
end;

{ === Вспомогательные функции разбора DXF === }

{ Разбивает текст DXF на строки.
  Чётные индексы — коды групп, нечётные — значения. }
function SplitDXFLines(const Text: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Text := Text;
end;

{ Возвращает числовой код группы из строки DXF,
  или -1 при ошибке }
function ParseGroupCode(const S: string): Integer;
begin
  if not TryStrToInt(Trim(S), Result) then
    Result := -1;
end;

{ Ищет словарь ACAD_MLEADERSTYLE и заполняет карту
  хэндл → имя стиля. OutDictHandle — хэндл словаря. }
procedure ExtractMLeaderStyleDictionary(
  Lines: TStringList;
  StyleNameByHandle: TStringList;
  out OutDictHandle: string);
var
  I, Code: Integer;
  Value, DictHandle, LastKey: string;
  InTargetDict: Boolean;
begin
  StyleNameByHandle.Clear;
  InTargetDict := False;
  DictHandle := '';
  LastKey := '';
  I := 0;

  { Шаг 1: ищем хэндл словаря ACAD_MLEADERSTYLE.
    Паттерн: ...3\nACAD_MLEADERSTYLE\n350\n<handle>... }
  while (I < Lines.Count - 1)
    and (DictHandle = '') do
  begin
    Code := ParseGroupCode(Lines[I]);
    Value := Trim(Lines[I + 1]);
    if (Code = 3)
      and (UpperCase(Value) = 'ACAD_MLEADERSTYLE') then
    begin
      if (I + 3 < Lines.Count)
        and (ParseGroupCode(Lines[I + 2]) = 350) then
        DictHandle := UpperCase(Trim(Lines[I + 3]));
    end;
    Inc(I, 2);
  end;

  OutDictHandle := DictHandle;

  if DictHandle = '' then
    Exit;

  { Шаг 2: ищем DICTIONARY с хэндлом = DictHandle }
  I := 0;
  while I < Lines.Count - 1 do
  begin
    Code := ParseGroupCode(Lines[I]);
    Value := Trim(Lines[I + 1]);

    if not InTargetDict then
    begin
      if (Code = 5)
        and (UpperCase(Value) = DictHandle) then
        InTargetDict := True;
    end
    else
    begin
      { Читаем записи словаря до следующего объекта }
      if Code = 0 then
        InTargetDict := False
      else if Code = 3 then
        LastKey := Value
      else if (Code = 350) and (LastKey <> '') then
      begin
        StyleNameByHandle.Values[
          UpperCase(Value)] := LastKey;
        LastKey := '';
      end;
    end;

    Inc(I, 2);
  end;
end;

{ Разбирает содержимое одного объекта MLEADERSTYLE.
  Заполняет поля Style по group codes из ObjectLines.
  Логирует все считанные параметры. }
procedure ParseMLeaderStyleObject(
  const StyleName: string;
  ObjectLines: TStringList;
  Style: PTGDBDXFMLeaderStyle);
var
  I, Code, IntVal: Integer;
  Value: string;
  InXDict: Boolean;
  InXData: Boolean;
begin
  InXDict := False;
  InXData := False;
  I := 0;

  while I < ObjectLines.Count - 1 do
  begin
    Code := ParseGroupCode(ObjectLines[I]);
    Value := Trim(ObjectLines[I + 1]);

    { Логируем каждый считанный параметр }
    programlog.LogOutFormatStr(
      'MLeaderStyle "%s": code=%d value=%s',
      [StyleName, Code, Value], LM_Info);

    { Обработка блока ACAD_XDICTIONARY }
    if (Code = 102)
      and (UpperCase(Copy(Value, 1, 17))
        = '{ACAD_XDICTIONARY') then
    begin
      InXDict := True;
      Inc(I, 2);
      Continue;
    end;
    if InXDict then
    begin
      if Code = 102 then
        InXDict := False
      else if Code = 360 then
        Style^.XDictHandle := Value;
      Inc(I, 2);
      Continue;
    end;

    { Обработка расширенных данных ACAD_MLEADERVER }
    if (Code = 1001)
      and (UpperCase(Value) = 'ACAD_MLEADERVER') then
    begin
      InXData := True;
      Inc(I, 2);
      Continue;
    end;
    if InXData then
    begin
      if Code = 1070 then
      begin
        if TryStrToInt(Value, IntVal) then
          Style^.MLeaderVersion := IntVal;
      end;
      { xdata завершается при следующем коде 0 или 1001 }
      if (Code = 0) or (Code = 1001) then
        InXData := False;
      Inc(I, 2);
      Continue;
    end;

    case Code of
      170:
        if TryStrToInt(Value, IntVal) then
          Style^.LeaderLineType := IntVal;
      171:
        if TryStrToInt(Value, IntVal) then
          Style^.LeaderLineTypeId := IntVal;
      172:
        if TryStrToInt(Value, IntVal) then
          Style^.FirstSegAngleConstraint := IntVal;
      173:
        if TryStrToInt(Value, IntVal) then
          Style^.SecondSegAngleConstraint := IntVal;
      174:
        if TryStrToInt(Value, IntVal) then
          Style^.TextAttachmentLeft := IntVal;
      175:
        if TryStrToInt(Value, IntVal) then
          Style^.TextAngleType := IntVal;
      176:
        if TryStrToInt(Value, IntVal) then
          Style^.TextAlignmentType := IntVal;
      177:
        if TryStrToInt(Value, IntVal) then
          Style^.TextAttachmentDirection := IntVal;
      178:
        if TryStrToInt(Value, IntVal) then
          Style^.TextAttachmentRight := IntVal;
      90:
        if TryStrToInt(Value, IntVal) then
          Style^.ContentType := IntVal;
      91:
        if TryStrToInt(Value, IntVal) then
          Style^.LeaderLineColor := IntVal;
      92:
        if TryStrToInt(Value, IntVal) then
          Style^.LeaderLineWeight := IntVal;
      93:
        if TryStrToInt(Value, IntVal) then
          Style^.TextColor := IntVal;
      94:
        if TryStrToInt(Value, IntVal) then
          Style^.BlockContentColor := IntVal;
      40:
        Style^.FirstSegAngle :=
          StrToFloatDef(Value, 0.0);
      41:
        Style^.SecondSegAngle :=
          StrToFloatDef(Value, 0.0);
      42:
        Style^.DoglegLength :=
          StrToFloatDef(Value, 0.09);
      43:
        Style^.LandingGap :=
          StrToFloatDef(Value, 0.36);
      44:
        Style^.TextHeight :=
          StrToFloatDef(Value, 0.18);
      45:
        Style^.ArrowHeadSize :=
          StrToFloatDef(Value, 0.18);
      46:
        Style^.BlockContentScale :=
          StrToFloatDef(Value, 0.18);
      47:
        Style^.BlockContentScaleX :=
          StrToFloatDef(Value, 1.0);
      49:
        Style^.BlockContentScaleY :=
          StrToFloatDef(Value, 1.0);
      140:
        Style^.OverallScale :=
          StrToFloatDef(Value, 1.0);
      141:
        Style^.BreakGapSize :=
          StrToFloatDef(Value, 0.0);
      142:
        Style^.BlockContentScaleZ :=
          StrToFloatDef(Value, 1.0);
      143:
        Style^.BlockContentRotation :=
          StrToFloatDef(Value, 0.125);
      290:
        if TryStrToInt(Value, IntVal) then
          Style^.EnableDogleg := IntVal <> 0;
      291:
        if TryStrToInt(Value, IntVal) then
          Style^.EnableLanding := IntVal <> 0;
      292:
        if TryStrToInt(Value, IntVal) then
          Style^.TextAlignAlwaysLeft := IntVal <> 0;
      293:
        if TryStrToInt(Value, IntVal) then
          Style^.Annotative := IntVal <> 0;
      294:
        if TryStrToInt(Value, IntVal) then
          Style^.TextDirectionNegative := IntVal <> 0;
      295:
        if TryStrToInt(Value, IntVal) then
          Style^.IsBlockContent := IntVal <> 0;
      296:
        if TryStrToInt(Value, IntVal) then
          Style^.IsMTextContent := IntVal <> 0;
      297:
        if TryStrToInt(Value, IntVal) then
          Style^.AlignSpace := IntVal <> 0;
      3:
        Style^.Description := Value;
      300:
        Style^.DefaultTextContent := Value;
      340:
        Style^.LeaderLinetypeHandle := Value;
      341:
        Style^.ArrowHeadBlockHandle := Value;
      342:
        Style^.TextStyleHandle := Value;
      343:
        Style^.BlockContentHandle := Value;
    end;

    Inc(I, 2);
  end;

  { Логируем завершение загрузки стиля }
  programlog.LogOutFormatStr(
    'MLeaderStyle "%s" loaded',
    [StyleName], LM_Info);
end;

{ Строит карту хэндл→имя из секции TABLES для объектов
  заданного типа (LTYPE, STYLE, BLOCK_RECORD).
  Используется для разрешения хэндлов ссылок в MLEADERSTYLE. }
procedure BuildHandleNameMap(
  const RawTablesSection: string;
  const ObjectType: string;
  HandleToName: TStringList);
var
  Lines: TStringList;
  I, Code: Integer;
  Value, CurHandle, CurName, CurType: string;
  InObject: Boolean;
begin
  if RawTablesSection = '' then
    Exit;

  Lines := SplitDXFLines(RawTablesSection);
  try
    InObject := False;
    CurHandle := '';
    CurName := '';
    CurType := '';
    I := 0;

    while I < Lines.Count - 1 do
    begin
      Code := ParseGroupCode(Lines[I]);
      Value := Trim(Lines[I + 1]);

      if Code = 0 then
      begin
        { Сохраняем предыдущий объект если он нужного типа }
        if InObject
          and (UpperCase(CurType) = UpperCase(ObjectType))
          and (CurHandle <> '')
          and (CurName <> '') then
        begin
          HandleToName.Values[
            UpperCase(CurHandle)] := CurName;
        end;
        CurHandle := '';
        CurName := '';
        CurType := Value;
        InObject := True;
      end
      else if InObject then
      begin
        if (Code = 5) and (CurHandle = '') then
          CurHandle := Value;
        if Code = 2 then
          CurName := Value;
      end;

      Inc(I, 2);
    end;

    { Последний объект }
    if InObject
      and (UpperCase(CurType) = UpperCase(ObjectType))
      and (CurHandle <> '')
      and (CurName <> '') then
    begin
      HandleToName.Values[
        UpperCase(CurHandle)] := CurName;
    end;
  finally
    Lines.Free;
  end;
end;

{ Разрешает хэндл ссылки в имя объекта используя карту.
  Возвращает имя или пустую строку если не найдено. }
function ResolveHandleToName(
  const Handle: string;
  HandleToName: TStringList): string;
begin
  if Handle = '' then
    Result := ''
  else
    Result := HandleToName.Values[UpperCase(Handle)];
end;

{ Код типа содержимого «блок» в MLEADERSTYLE (код 90).
  ZCAD не поддерживает мультивыноски с блочным содержимым }
const
  CONTENT_TYPE_BLOCK = 1;

{ Проверяет строки объекта MLEADERSTYLE на признаки
  блочного содержимого ДО добавления стиля в таблицу.
  Блочный стиль определяется по ContentType = 1 (код 90)
  или по наличию непустого хэндла блока (код 343) }
function HasBlockContentInLines(
  ObjectLines: TStringList): Boolean;
var
  J, CodeJ, IntVal: Integer;
  ValJ: string;
begin
  Result := False;
  J := 0;
  while J < ObjectLines.Count - 1 do
  begin
    CodeJ := ParseGroupCode(ObjectLines[J]);
    ValJ := Trim(ObjectLines[J + 1]);
    { Код 90 — тип содержимого: 1 означает блок }
    if (CodeJ = 90) and TryStrToInt(ValJ, IntVal) then
    begin
      if IntVal = CONTENT_TYPE_BLOCK then
      begin
        Result := True;
        Exit;
      end;
    end;
    { Код 343 — хэндл блока содержимого.
      Непустое значение означает блочный стиль
      (характерно для AutoCAD 2008) }
    if (CodeJ = 343) and (ValJ <> '') then
    begin
      Result := True;
      Exit;
    end;
    Inc(J, 2);
  end;
end;

{ Основная функция загрузки стилей мультивыносок
  из секции OBJECTS DXF-файла }
procedure ReadMLeaderStylesFromDXFObjects(
  const RawObjectsSection: string;
  const RawTablesSection: string;
  var MLeaderStyleTable: GDBDXFMLeaderStyleArray);
var
  Lines: TStringList;
  StyleNameByHandle: TStringList;
  LTypeHandleMap: TStringList;
  StyleHandleMap: TStringList;
  BlockRecHandleMap: TStringList;
  I, Code: Integer;
  Value, ObjHandle, StyleName, DictHandle: string;
  InMLeaderStyle: Boolean;
  ObjectLines: TStringList;
  Style: PTGDBDXFMLeaderStyle;
begin
  if RawObjectsSection = '' then
    Exit;

  programlog.LogOutFormatStr(
    'uzestylesmleaderdxf: начало загрузки стилей '
    + 'мультивыносок из секции OBJECTS',
    [], LM_Info);

  Lines := SplitDXFLines(RawObjectsSection);
  StyleNameByHandle := TStringList.Create;
  ObjectLines := TStringList.Create;
  LTypeHandleMap := TStringList.Create;
  StyleHandleMap := TStringList.Create;
  BlockRecHandleMap := TStringList.Create;
  try
    StyleNameByHandle.CaseSensitive := False;
    LTypeHandleMap.CaseSensitive := False;
    StyleHandleMap.CaseSensitive := False;
    BlockRecHandleMap.CaseSensitive := False;

    { Строим карты хэндл→имя из секции TABLES
      для разрешения ссылок MLEADERSTYLE }
    BuildHandleNameMap(
      RawTablesSection, 'LTYPE', LTypeHandleMap);
    BuildHandleNameMap(
      RawTablesSection, 'STYLE', StyleHandleMap);
    BuildHandleNameMap(
      RawTablesSection, 'BLOCK_RECORD',
      BlockRecHandleMap);

    programlog.LogOutFormatStr(
      'uzestylesmleaderdxf: карты из TABLES: '
      + 'LTYPE=%d, STYLE=%d, BLOCK_RECORD=%d',
      [LTypeHandleMap.Count,
       StyleHandleMap.Count,
       BlockRecHandleMap.Count], LM_Info);

    { Шаг 1: карта хэндл → имя из ACAD_MLEADERSTYLE }
    ExtractMLeaderStyleDictionary(
      Lines, StyleNameByHandle, DictHandle);

    programlog.LogOutFormatStr(
      'uzestylesmleaderdxf: найдено %d стилей '
      + 'в словаре ACAD_MLEADERSTYLE',
      [StyleNameByHandle.Count], LM_Info);

    { Шаг 2: обрабатываем объекты MLEADERSTYLE }
    InMLeaderStyle := False;
    ObjHandle := '';
    I := 0;

    while I < Lines.Count - 1 do
    begin
      Code := ParseGroupCode(Lines[I]);
      Value := Trim(Lines[I + 1]);

      if not InMLeaderStyle then
      begin
        if (Code = 0)
          and (UpperCase(Value) = 'MLEADERSTYLE') then
        begin
          InMLeaderStyle := True;
          ObjHandle := '';
          ObjectLines.Clear;
          Inc(I, 2);
          Continue;
        end;
      end
      else
      begin
        if Code = 0 then
        begin
          { Конец объекта MLEADERSTYLE }
          if ObjHandle <> '' then
          begin
            StyleName := StyleNameByHandle.Values[
              UpperCase(ObjHandle)];
            if StyleName <> '' then
            begin
              programlog.LogOutFormatStr(
                'uzestylesmleaderdxf: обработка '
                + 'стиля "%s" хэндл=%s',
                [StyleName, ObjHandle], LM_Info);
              { Проверяем блочный тип до добавления
                в таблицу — ZCAD не поддерживает
                мультивыноски с блочным содержимым }
              if HasBlockContentInLines(
                ObjectLines) then
              begin
                programlog.LogOutFormatStr(
                  'uzestylesmleaderdxf: стиль '
                  + '"%s" пропущен (блочное '
                  + 'содержимое не поддерживается)',
                  [StyleName], LM_Info);
                zcUI.TextMessage(
                  Format(
                    'Стиль мультивыноски "%s" '
                    + 'пропущен: ZCAD '
                    + 'не поддерживает тип '
                    + 'настройки текста '
                    + 'мультивыносок: block',
                    [StyleName]),
                  TMWOHistoryOut);
              end
              else
              begin
                Style :=
                  MLeaderStyleTable.AddStyle(
                    StyleName);
                if Style <> nil then
                begin
                  ParseMLeaderStyleObject(
                    StyleName, ObjectLines,
                    Style);
                  { Разрешаем хэндлы в имена }
                  Style^.LeaderLinetypeName :=
                    ResolveHandleToName(
                      Style^.LeaderLinetypeHandle,
                      LTypeHandleMap);
                  Style^.ArrowHeadBlockName :=
                    ResolveHandleToName(
                      Style^.ArrowHeadBlockHandle,
                      BlockRecHandleMap);
                  Style^.TextStyleName :=
                    ResolveHandleToName(
                      Style^.TextStyleHandle,
                      StyleHandleMap);
                  Style^.BlockContentName :=
                    ResolveHandleToName(
                      Style^.BlockContentHandle,
                      BlockRecHandleMap);
                  programlog.LogOutFormatStr(
                    'uzestylesmleaderdxf: '
                    + 'разрешены ссылки "%s": '
                    + 'ltype="%s" arrow="%s" '
                    + 'txtstyle="%s" block="%s"',
                    [StyleName,
                     Style^.LeaderLinetypeName,
                     Style^.ArrowHeadBlockName,
                     Style^.TextStyleName,
                     Style^.BlockContentName],
                    LM_Info);
                end;
              end;
            end
            else
              programlog.LogOutFormatStr(
                'uzestylesmleaderdxf: MLEADERSTYLE '
                + 'хэндл=%s не найден, пропускаем',
                [ObjHandle], LM_Info);
          end;
          InMLeaderStyle := False;
          ObjHandle := '';
          ObjectLines.Clear;
          Continue;
        end;

        { Фиксируем хэндл объекта }
        if (Code = 5) and (ObjHandle = '') then
          ObjHandle := UpperCase(Value);

        { Накапливаем строки объекта }
        ObjectLines.Add(Lines[I]);
        ObjectLines.Add(Lines[I + 1]);
      end;

      Inc(I, 2);
    end;

    { Обрабатываем последний объект }
    if InMLeaderStyle and (ObjHandle <> '') then
    begin
      StyleName := StyleNameByHandle.Values[
        UpperCase(ObjHandle)];
      if StyleName <> '' then
      begin
        { Проверяем блочный тип до добавления }
        if HasBlockContentInLines(
          ObjectLines) then
        begin
          programlog.LogOutFormatStr(
            'uzestylesmleaderdxf: стиль '
            + '"%s" пропущен (блочное '
            + 'содержимое не поддерживается)',
            [StyleName], LM_Info);
          zcUI.TextMessage(
            Format(
              'Стиль мультивыноски "%s" '
              + 'пропущен: ZCAD '
              + 'не поддерживает тип '
              + 'настройки текста '
              + 'мультивыносок: block',
              [StyleName]),
            TMWOHistoryOut);
        end
        else
        begin
          Style :=
            MLeaderStyleTable.AddStyle(StyleName);
          if Style <> nil then
          begin
            ParseMLeaderStyleObject(
              StyleName, ObjectLines, Style);
            Style^.LeaderLinetypeName :=
              ResolveHandleToName(
                Style^.LeaderLinetypeHandle,
                LTypeHandleMap);
            Style^.ArrowHeadBlockName :=
              ResolveHandleToName(
                Style^.ArrowHeadBlockHandle,
                BlockRecHandleMap);
            Style^.TextStyleName :=
              ResolveHandleToName(
                Style^.TextStyleHandle,
                StyleHandleMap);
            Style^.BlockContentName :=
              ResolveHandleToName(
                Style^.BlockContentHandle,
                BlockRecHandleMap);
          end;
        end;
      end;
    end;

    programlog.LogOutFormatStr(
      'uzestylesmleaderdxf: загрузка стилей '
      + 'мультивыносок завершена',
      [], LM_Info);
  finally
    Lines.Free;
    StyleNameByHandle.Free;
    ObjectLines.Free;
    LTypeHandleMap.Free;
    StyleHandleMap.Free;
    BlockRecHandleMap.Free;
  end;
end;

{ === Функции экспорта MLEADERSTYLE в DXF === }

{ Добавляет строки из многострочного текста в список.
  Убирает пустые строки с конца, чтобы не нарушить
  разбор пар (код, значение) в DXF. }
procedure AppendTextLinesToList(
  Lines: TStringList; const Text: string);
var
  TempList: TStringList;
  LastIndex, I: Integer;
begin
  TempList := TStringList.Create;
  try
    TempList.Text := Text;
    LastIndex := TempList.Count - 1;
    while (LastIndex >= 0)
      and (TempList[LastIndex] = '') do
      Dec(LastIndex);
    for I := 0 to LastIndex do
      Lines.Add(TempList[I]);
  finally
    TempList.Free;
  end;
end;

{ Создаёт текстовое представление объекта MLEADERSTYLE
  для секции OBJECTS.
  Handle — хэндл объекта (hex).
  OwnerHandle — хэндл словаря ACAD_MLEADERSTYLE.
  XDictHandle — хэндл расширенного словаря (или ''). }
function BuildMLeaderStyleObjectText(
  const StyleName: string;
  Style: PTGDBDXFMLeaderStyle;
  const Handle: string;
  const OwnerHandle: string;
  const XDictHandle: string): string;
var
  Lines: TStringList;
begin
  Lines := TStringList.Create;
  try
    Lines.Add('  0');
    Lines.Add('MLEADERSTYLE');
    Lines.Add('  5');
    Lines.Add(Handle);

    { Блок ACAD_XDICTIONARY }
    if XDictHandle <> '' then
    begin
      Lines.Add('102');
      Lines.Add('{ACAD_XDICTIONARY');
      Lines.Add('360');
      Lines.Add(XDictHandle);
      Lines.Add('102');
      Lines.Add('}');
    end;

    { Блок ACAD_REACTORS — связь с владельцем }
    if OwnerHandle <> '' then
    begin
      Lines.Add('102');
      Lines.Add('{ACAD_REACTORS');
      Lines.Add('330');
      Lines.Add(OwnerHandle);
      Lines.Add('102');
      Lines.Add('}');
      Lines.Add('330');
      Lines.Add(OwnerHandle);
    end;

    { Подкласс AcDbMLeaderStyle }
    Lines.Add('100');
    Lines.Add('AcDbMLeaderStyle');

    { Тип линии выноски (код 170) }
    Lines.Add('170');
    Lines.Add(IntToStr(Style^.LeaderLineType));

    { Тип стрелки (код 171) }
    Lines.Add('171');
    Lines.Add(IntToStr(Style^.LeaderLineTypeId));

    { Ограничение первого сегмента (код 172) }
    Lines.Add('172');
    Lines.Add(IntToStr(
      Style^.FirstSegAngleConstraint));

    { Тип содержимого (код 90) }
    Lines.Add(' 90');
    Lines.Add(IntToStr(Style^.ContentType));

    { Угол первого сегмента (код 40) }
    Lines.Add(' 40');
    Lines.Add(FloatToStr(Style^.FirstSegAngle));

    { Угол второго сегмента (код 41) }
    Lines.Add(' 41');
    Lines.Add(FloatToStr(Style^.SecondSegAngle));

    { Ограничение второго сегмента (код 173) }
    Lines.Add('173');
    Lines.Add(IntToStr(
      Style^.SecondSegAngleConstraint));

    { Цвет линии выноски (код 91) }
    Lines.Add(' 91');
    Lines.Add(IntToStr(Style^.LeaderLineColor));

    { Хэндл типа линии выноски (код 340) }
    Lines.Add('340');
    Lines.Add(Style^.LeaderLinetypeHandle);

    { Тип линии соединения (код 92) }
    Lines.Add(' 92');
    Lines.Add(IntToStr(Style^.LeaderLineWeight));

    { Наличие площадки (код 290) }
    Lines.Add('290');
    if Style^.EnableDogleg then
      Lines.Add('     1')
    else
      Lines.Add('     0');

    { Расстояние площадки (код 42) }
    Lines.Add(' 42');
    Lines.Add(FloatToStr(Style^.DoglegLength));

    { Наличие рамки (код 291) }
    Lines.Add('291');
    if Style^.EnableLanding then
      Lines.Add('     1')
    else
      Lines.Add('     0');

    { Длина площадки (код 43) }
    Lines.Add(' 43');
    Lines.Add(FloatToStr(Style^.LandingGap));

    { Описание стиля (код 3) }
    Lines.Add('  3');
    Lines.Add(Style^.Description);

    { Хэндл блока стрелки (код 341) — необязательный }
    if Style^.ArrowHeadBlockHandle <> '' then
    begin
      Lines.Add('341');
      Lines.Add(Style^.ArrowHeadBlockHandle);
    end;

    { Масштаб текста (код 44) }
    Lines.Add(' 44');
    Lines.Add(FloatToStr(Style^.TextHeight));

    { Текст по умолчанию (код 300) }
    Lines.Add('300');
    Lines.Add(Style^.DefaultTextContent);

    { Хэндл текстового стиля (код 342) }
    Lines.Add('342');
    Lines.Add(Style^.TextStyleHandle);

    { Присоединение текста слева (код 174) }
    Lines.Add('174');
    Lines.Add(IntToStr(Style^.TextAttachmentLeft));

    { Присоединение текста справа (код 178) }
    Lines.Add('178');
    Lines.Add(IntToStr(Style^.TextAttachmentRight));

    { Выравнивание текста (код 175) }
    Lines.Add('175');
    Lines.Add(IntToStr(Style^.TextAngleType));

    { Режим выравнивания (код 176) }
    Lines.Add('176');
    Lines.Add(IntToStr(Style^.TextAlignmentType));

    { Цвет текста (код 93) }
    Lines.Add(' 93');
    Lines.Add(IntToStr(Style^.TextColor));

    { Размер стрелки (код 45) }
    Lines.Add(' 45');
    Lines.Add(FloatToStr(Style^.ArrowHeadSize));

    { Выравнивание слева (код 292) }
    Lines.Add('292');
    if Style^.TextAlignAlwaysLeft then
      Lines.Add('     1')
    else
      Lines.Add('     0');

    { Выравнивание по направлению (код 297) }
    Lines.Add('297');
    if Style^.AlignSpace then
      Lines.Add('     1')
    else
      Lines.Add('     0');

    { Масштаб блока (код 46) }
    Lines.Add(' 46');
    Lines.Add(FloatToStr(Style^.BlockContentScale));

    { Хэндл блока содержимого (код 343) — необязательный }
    if Style^.BlockContentHandle <> '' then
    begin
      Lines.Add('343');
      Lines.Add(Style^.BlockContentHandle);
    end;

    { Цвет блока (код 94) }
    Lines.Add(' 94');
    Lines.Add(IntToStr(Style^.BlockContentColor));

    { Масштаб X (код 47) }
    Lines.Add(' 47');
    Lines.Add(FloatToStr(Style^.BlockContentScaleX));

    { Масштаб Y (код 49) }
    Lines.Add(' 49');
    Lines.Add(FloatToStr(Style^.BlockContentScaleY));

    { Общий масштаб (код 140) }
    Lines.Add('140');
    Lines.Add(FloatToStr(Style^.OverallScale));

    { Аннотативный (код 293) }
    Lines.Add('293');
    if Style^.Annotative then
      Lines.Add('     1')
    else
      Lines.Add('     0');

    { Расстояние разрыва (код 141) }
    Lines.Add('141');
    Lines.Add(FloatToStr(Style^.BreakGapSize));

    { Текст по направлению (код 294) }
    Lines.Add('294');
    if Style^.TextDirectionNegative then
      Lines.Add('     1')
    else
      Lines.Add('     0');

    { Направление присоединения (код 177) }
    Lines.Add('177');
    Lines.Add(IntToStr(
      Style^.TextAttachmentDirection));

    { Масштаб Z (код 142) }
    Lines.Add('142');
    Lines.Add(FloatToStr(Style^.BlockContentScaleZ));

    { Содержимое — блок (код 295) }
    Lines.Add('295');
    if Style^.IsBlockContent then
      Lines.Add('     1')
    else
      Lines.Add('     0');

    { Содержимое — мтекст (код 296) }
    Lines.Add('296');
    if Style^.IsMTextContent then
      Lines.Add('     1')
    else
      Lines.Add('     0');

    { Поворот блока (код 143) }
    Lines.Add('143');
    Lines.Add(FloatToStr(Style^.BlockContentRotation));

    { Расширенные данные: версия мультивыноски }
    Lines.Add('1001');
    Lines.Add('ACAD_MLEADERVER');
    Lines.Add('1070');
    Lines.Add(IntToStr(Style^.MLeaderVersion));

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

{ Добавляет запись стиля в словарь ACAD_MLEADERSTYLE.
  DictHandle — хэндл словаря.
  StyleName — имя стиля.
  StyleHandle — хэндл объекта MLEADERSTYLE. }
procedure AddStyleToDictionary(
  ResultLines: TStringList;
  const DictHandle, StyleName,
    StyleHandle: string);
var
  J, CodeJ: Integer;
  ValJ: string;
  InDict: Boolean;
begin
  InDict := False;
  J := 0;
  while J < ResultLines.Count - 1 do
  begin
    CodeJ := ParseGroupCode(ResultLines[J]);
    ValJ := Trim(ResultLines[J + 1]);
    if not InDict then
    begin
      if (CodeJ = 5)
        and (UpperCase(ValJ)
          = UpperCase(DictHandle)) then
        InDict := True;
    end
    else
    begin
      if CodeJ = 0 then
      begin
        { Вставляем перед следующим объектом }
        ResultLines.Insert(J, StyleHandle);
        ResultLines.Insert(J, '350');
        ResultLines.Insert(J, StyleName);
        ResultLines.Insert(J, '  3');
        Exit;
      end;
    end;
    Inc(J, 2);
  end;
end;

{ Записывает стили мультивыносок в секцию OBJECTS.
  Существующие MLEADERSTYLE заменяются с сохранением
  оригинальных хэндлов, новые добавляются перед ENDSEC. }
procedure WriteMLeaderStylesToDXFObjects(
  var MLeaderStyleTable: GDBDXFMLeaderStyleArray;
  var RawObjectsSection: string);
var
  Lines: TStringList;
  ResultLines: TStringList;
  StyleNameByHandle: TStringList;
  HandleByStyleName: TStringList;
  StyleIter: itrec;
  Style: PTGDBDXFMLeaderStyle;
  I, Code, HandleBase: Integer;
  Value, StyleName, ObjHandle: string;
  DictHandle, NewHandle: string;
  InMLeaderStyle: Boolean;
  WrittenStyleNames: TStringList;
  ObjectText: string;
begin
  if MLeaderStyleTable.count = 0 then
    Exit;

  programlog.LogOutFormatStr(
    'uzestylesmleaderdxf: начало записи %d стилей '
    + 'мультивыносок в секцию OBJECTS',
    [MLeaderStyleTable.count], LM_Info);

  Lines := SplitDXFLines(RawObjectsSection);
  ResultLines := TStringList.Create;
  WrittenStyleNames := TStringList.Create;
  StyleNameByHandle := TStringList.Create;
  HandleByStyleName := TStringList.Create;
  try
    StyleNameByHandle.CaseSensitive := False;
    HandleByStyleName.CaseSensitive := False;

    { Шаг 1: карта хэндл→имя и обратная }
    ExtractMLeaderStyleDictionary(
      Lines, StyleNameByHandle, DictHandle);

    for I := 0 to StyleNameByHandle.Count - 1 do
    begin
      Value := StyleNameByHandle.Names[I];
      StyleName :=
        StyleNameByHandle.ValueFromIndex[I];
      HandleByStyleName.Values[
        UpperCase(StyleName)] := Value;
    end;

    programlog.LogOutFormatStr(
      'uzestylesmleaderdxf: словарь '
      + 'ACAD_MLEADERSTYLE хэндл=%s, стилей=%d',
      [DictHandle, StyleNameByHandle.Count],
      LM_Info);

    HandleBase := $F100;
    InMLeaderStyle := False;
    ObjHandle := '';
    I := 0;

    while I < Lines.Count - 1 do
    begin
      Code := ParseGroupCode(Lines[I]);
      Value := Trim(Lines[I + 1]);

      if not InMLeaderStyle then
      begin
        if (Code = 0)
          and (UpperCase(Value)
            = 'MLEADERSTYLE') then
        begin
          InMLeaderStyle := True;
          ObjHandle := '';
          Inc(I, 2);
          Continue;
        end;

        { Перед ENDSEC вставляем новые стили }
        if (Code = 0)
          and (UpperCase(Value) = 'ENDSEC') then
        begin
          Style :=
            MLeaderStyleTable.beginiterate(StyleIter);
          while Style <> nil do
          begin
            StyleName := Style^.Name;
            if WrittenStyleNames.IndexOf(
              UpperCase(StyleName)) < 0 then
            begin
              NewHandle :=
                HandleByStyleName.Values[
                  UpperCase(StyleName)];
              if NewHandle = '' then
              begin
                NewHandle := UpperCase(
                  IntToHex(HandleBase, 0));
                Inc(HandleBase);
                if DictHandle <> '' then
                  AddStyleToDictionary(
                    ResultLines, DictHandle,
                    StyleName, NewHandle);
              end;
              ObjectText :=
                BuildMLeaderStyleObjectText(
                  StyleName, Style, NewHandle,
                  DictHandle, Style^.XDictHandle);
              AppendTextLinesToList(
                ResultLines, ObjectText);
              WrittenStyleNames.Add(
                UpperCase(StyleName));
              programlog.LogOutFormatStr(
                'uzestylesmleaderdxf: записан '
                + 'новый стиль "%s" хэндл=%s',
                [StyleName, NewHandle], LM_Info);
            end;
            Style :=
              MLeaderStyleTable.iterate(StyleIter);
          end;
        end;

        ResultLines.Add(Lines[I]);
        ResultLines.Add(Lines[I + 1]);
      end
      else
      begin
        { Внутри пропускаемого MLEADERSTYLE }
        if (Code = 5) and (ObjHandle = '') then
          ObjHandle := UpperCase(Value);

        if Code = 0 then
        begin
          { Пишем обновлённую версию }
          if ObjHandle <> '' then
          begin
            StyleName :=
              StyleNameByHandle.Values[ObjHandle];
            if StyleName = '' then
              programlog.LogOutFormatStr(
                'uzestylesmleaderdxf: MLEADERSTYLE'
                + ' хэндл=%s не в словаре',
                [ObjHandle], LM_Info)
            else if WrittenStyleNames.IndexOf(
              UpperCase(StyleName)) < 0 then
            begin
              Style := PTGDBDXFMLeaderStyle(
                MLeaderStyleTable.getAddres(
                  StyleName));
              if Style <> nil then
              begin
                ObjectText :=
                  BuildMLeaderStyleObjectText(
                    StyleName, Style, ObjHandle,
                    DictHandle,
                    Style^.XDictHandle);
                AppendTextLinesToList(
                  ResultLines, ObjectText);
                WrittenStyleNames.Add(
                  UpperCase(StyleName));
                programlog.LogOutFormatStr(
                  'uzestylesmleaderdxf: '
                  + 'перезаписан стиль "%s" '
                  + 'хэндл=%s',
                  [StyleName, ObjHandle],
                  LM_Info);
              end;
            end;
          end;
          InMLeaderStyle := False;
          ObjHandle := '';
          Continue;
        end;
      end;

      Inc(I, 2);
    end;

    RawObjectsSection := ResultLines.Text;

    programlog.LogOutFormatStr(
      'uzestylesmleaderdxf: запись стилей '
      + 'мультивыносок завершена',
      [], LM_Info);
  finally
    Lines.Free;
    ResultLines.Free;
    WrittenStyleNames.Free;
    StyleNameByHandle.Free;
    HandleByStyleName.Free;
  end;
end;

begin
end.
