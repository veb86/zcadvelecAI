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
  Модуль: uzestylestablesdxf
  Назначение: импорт и экспорт стилей таблиц (TABLESTYLE) из/в формат DXF.
  Реализован по аналогии с uzestylesdim.pas для размерных стилей.

  Стили таблиц в DXF хранятся в секции OBJECTS в виде объектов TABLESTYLE,
  связанных через словарь ACAD_TABLESTYLE. В отличие от DIMSTYLE (который
  находится в секции TABLES), TABLESTYLE является объектом секции OBJECTS.

  Модуль полностью самодостаточен: все необходимые типы данных определены
  внутри. Не зависит от uzestylestables.

  Зависимости: UGDBNamedObjectsArray, gzctnrVector, uzeNamedObject,
               gzctnrVectorTypes, uzclog, sysutils, Classes
}
unit uzestylestablesdxf;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}
interface
uses
  UGDBNamedObjectsArray,
  gzctnrVector,
  uzeNamedObject,
  gzctnrVectorTypes,
  uzclog,
  sysutils,
  Classes;

{ === Типы данных для DXF-стилей таблиц === }

type
  { Стиль одной строки ячейки таблицы (для DXF-обмена).
    Содержит только value types — безопасно для GZVector (raw memory). }
  TGDBDXFTableCellStyle = record
    { Высота текста строки (группа DXF 140) }
    TextHeight: Double;
    { Выравнивание текста в ячейке (группа DXF 170) }
    Alignment: Integer;
    { Цвет текста (группа DXF 62) }
    TextColor: Integer;
    { Цвет фона ячейки (группа DXF 63) }
    BackgroundColor: Integer;
    { Признак включения цвета фона (группа DXF 283) }
    BackgroundColorEnabled: Boolean;
  end;
  PTGDBDXFTableCellStyle = ^TGDBDXFTableCellStyle;

  GDBDXFCellFormatArray = GZVector<TGDBDXFTableCellStyle>;

  { Стиль таблицы для DXF-обмена — именованный объект.
    Содержит все параметры, необходимые для записи/чтения TABLESTYLE в DXF.
    Намеренно не зависит от uzestylestables. }
  TGDBDXFTableStyle = object(GDBNamedObject)
    { Массив стилей ячеек: 0=title, 1=header, 2=data }
    CellFormats: GDBDXFCellFormatArray;
    { Признак подавления строки заголовка (группа DXF 280) }
    TitleSuppressed: Boolean;
    { Признак подавления строки имён колонок (группа DXF 281) }
    ColumnHeadingSuppressed: Boolean;
    { Имена текстовых стилей для трёх типов строк: title, header, data
      (группа DXF 7). Массив строк хранится отдельно от record-а ячейки,
      так как string в GZVector небезопасен (raw memory). }
    CellTextStyleName: array[0..2] of string;
    { Хэндл расширенного словаря объекта (блок 102/ACAD_XDICTIONARY).
      Сохраняется при чтении и восстанавливается при записи, чтобы AutoCAD
      не считал файл повреждённым из-за отсутствия XDICTIONARY-ссылок. }
    XDictHandle: string;
    constructor init(const StyleName: string);
    destructor Done; virtual;
  end;
  PTGDBDXFTableStyle = ^TGDBDXFTableStyle;

  { Массив стилей таблиц для DXF-обмена }
  GDBDXFTableStyleArray = object(GDBNamedObjectsArray<PTGDBDXFTableStyle,
                                                      TGDBDXFTableStyle>)
    constructor init(InitialCapacity: Integer);
    constructor initnul;
    { Добавляет стиль с заданным именем или возвращает существующий }
    function AddStyle(const StyleName: string): PTGDBDXFTableStyle;
  end;
  PGDBDXFTableStyleArray = ^GDBDXFTableStyleArray;

{ Загружает стили таблиц из сырого текста секции OBJECTS DXF-файла.
  Находит словарь ACAD_TABLESTYLE и все объекты TABLESTYLE,
  восстанавливает имена стилей из словаря и заполняет таблицу стилей.
  Параметры:
    RawObjectsSection — полный текст секции OBJECTS (включая 0/SECTION..0/ENDSEC)
    TableStyleTable   — таблица стилей для заполнения }
procedure ReadTableStylesFromDXFObjects(
  const RawObjectsSection: string;
  var TableStyleTable: GDBDXFTableStyleArray);

{ Записывает стили таблиц из таблицы стилей в сырой текст секции OBJECTS.
  Если секция OBJECTS уже содержит объекты TABLESTYLE — они заменяются.
  Если нет — добавляются перед ENDSEC.
  Параметры:
    TableStyleTable   — таблица стилей для записи
    RawObjectsSection — текст секции OBJECTS для обновления (in/out) }
procedure WriteTableStylesToDXFObjects(
  var TableStyleTable: GDBDXFTableStyleArray;
  var RawObjectsSection: string);

implementation

{ === Конструктор и деструктор TGDBDXFTableStyle === }

constructor TGDBDXFTableStyle.init(const StyleName: string);
var
  I: Integer;
begin
  inherited Init(StyleName);
  CellFormats.Init(3);
  TitleSuppressed := False;
  ColumnHeadingSuppressed := False;
  { Инициализируем указатели строк через nil для корректной работы с AnsiString }
  for I := 0 to 2 do
    pointer(CellTextStyleName[I]) := nil;
  pointer(XDictHandle) := nil;
end;

destructor TGDBDXFTableStyle.Done;
var
  I: Integer;
begin
  inherited Done;
  CellFormats.Done;
  { Явно освобождаем строки — необходимо при использовании object с GZVector }
  for I := 0 to 2 do
    CellTextStyleName[I] := '';
  XDictHandle := '';
end;

{ === Методы GDBDXFTableStyleArray === }

constructor GDBDXFTableStyleArray.init(InitialCapacity: Integer);
begin
  inherited init(InitialCapacity);
end;

constructor GDBDXFTableStyleArray.initnul;
begin
  inherited initnul;
end;

function GDBDXFTableStyleArray.AddStyle(const StyleName: string): PTGDBDXFTableStyle;
var
  StylePtr: PTGDBDXFTableStyle;
begin
  case AddItem(StyleName, pointer(StylePtr)) of
    IsFounded:
      { Стиль уже существует — возвращаем указатель без изменений };
    IsCreated:
      { Новый стиль — инициализируем }
      StylePtr^.init(StyleName);
    IsError:
      { Ошибка добавления — возвращаем nil }
      StylePtr := nil;
  end;
  Result := StylePtr;
end;

{ === Вспомогательные функции разбора DXF === }

{ Разбивает текст DXF на строки и возвращает список.
  Чётные индексы — коды групп, нечётные — значения. }
function SplitDXFLines(const Text: string): TStringList;
begin
  Result := TStringList.Create;
  Result.Text := Text;
end;

{ Возвращает числовой код группы из строки DXF, или -1 при ошибке. }
function ParseGroupCode(const S: string): Integer;
begin
  if not TryStrToInt(Trim(S), Result) then
    Result := -1;
end;

{ Ищет в Lines словарь ACAD_TABLESTYLE и заполняет StyleNameByHandle.
  Ключ — хэндл объекта TABLESTYLE (в верхнем регистре),
  значение — имя стиля из словаря.
  OutDictHandle — хэндл самого словаря ACAD_TABLESTYLE (или '' если не найден). }
procedure ExtractTableStyleDictionary(
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

  { Шаг 1: ищем хэндл словаря ACAD_TABLESTYLE.
    Паттерн в DXF: ...3\nACAD_TABLESTYLE\n350\n<handle>... }
  while (I < Lines.Count - 1) and (DictHandle = '') do
  begin
    Code := ParseGroupCode(Lines[I]);
    Value := Trim(Lines[I + 1]);
    if (Code = 3) and (UpperCase(Value) = 'ACAD_TABLESTYLE') then
    begin
      if (I + 3 < Lines.Count) and (ParseGroupCode(Lines[I + 2]) = 350) then
        DictHandle := UpperCase(Trim(Lines[I + 3]));
    end;
    Inc(I, 2);
  end;

  OutDictHandle := DictHandle;

  if DictHandle = '' then
    Exit;

  { Шаг 2: ищем объект DICTIONARY с хэндлом = DictHandle и читаем его }
  I := 0;
  while I < Lines.Count - 1 do
  begin
    Code := ParseGroupCode(Lines[I]);
    Value := Trim(Lines[I + 1]);

    if not InTargetDict then
    begin
      { Ищем начало нужного словаря по хэндлу }
      if (Code = 5) and (UpperCase(Value) = DictHandle) then
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
        { Хэндл → имя стиля }
        StyleNameByHandle.Values[UpperCase(Value)] := LastKey;
        LastKey := '';
      end;
    end;

    Inc(I, 2);
  end;
end;

{ Разбирает содержимое одного объекта TABLESTYLE и заполняет поля Style.
  ObjectLines — строки DXF объекта (от кода 5/handle до начала следующего).
  Логирует все считанные параметры согласно требованиям ТЗ. }
procedure ParseTableStyleObject(
  const StyleName: string;
  ObjectLines: TStringList;
  Style: PTGDBDXFTableStyle);
var
  I, Code, IntVal: Integer;
  Value: string;
  CellStyle: TGDBDXFTableCellStyle;
  { Индекс текущего блока строки таблицы: 0=title, 1=header, 2=data }
  CellIdx: Integer;
  { Признак нахождения внутри блока ACAD_XDICTIONARY }
  InXDict: Boolean;
begin
  CellIdx := -1;
  InXDict := False;
  FillChar(CellStyle, SizeOf(CellStyle), 0);
  I := 0;

  while I < ObjectLines.Count - 1 do
  begin
    Code := ParseGroupCode(ObjectLines[I]);
    Value := Trim(ObjectLines[I + 1]);

    { Логируем каждый считанный параметр }
    programlog.LogOutFormatStr(
      'TableStyle "%s": code=%d value=%s',
      [StyleName, Code, Value], LM_Info);

    { Обрабатываем блок ACAD_XDICTIONARY: сохраняем хэндл расширенного словаря }
    if (Code = 102) and (UpperCase(Copy(Value, 1, 17)) = '{ACAD_XDICTIONARY') then
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

    case Code of
      3:
        { Описание стиля — не имя, имя берётся из словаря ACAD_TABLESTYLE }
        ;
      70, 71:
        { Флаги стиля таблицы — зарезервировано для будущего использования }
        ;
      40:
        { Горизонтальный отступ ячейки — не используется в текущей версии }
        ;
      41:
        { Вертикальный отступ ячейки — не используется в текущей версии }
        ;
      280:
        begin
          { Признак подавления строки заголовка таблицы }
          if TryStrToInt(Value, IntVal) then
            Style^.TitleSuppressed := IntVal <> 0;
        end;
      281:
        begin
          { Признак подавления строки имён колонок }
          if TryStrToInt(Value, IntVal) then
            Style^.ColumnHeadingSuppressed := IntVal <> 0;
        end;
      7:
        begin
          { Начало нового блока стиля строки таблицы (группа 7 = имя текст. стиля).
            Каждое новое значение группы 7 начинает следующий блок (title/header/data). }
          if CellIdx >= 0 then
            Style^.CellFormats.PushBackData(CellStyle);
          FillChar(CellStyle, SizeOf(CellStyle), 0);
          Inc(CellIdx);
          { Сохраняем имя текстового стиля для данного блока }
          if CellIdx <= 2 then
            Style^.CellTextStyleName[CellIdx] := Value;
        end;
      140:
        { Высота текста строки }
        if CellIdx >= 0 then
          CellStyle.TextHeight := StrToFloatDef(Value, 2.5);
      170:
        { Выравнивание текста в ячейке }
        if CellIdx >= 0 then
          if TryStrToInt(Value, IntVal) then
            CellStyle.Alignment := IntVal;
      62:
        { Цвет текста ячейки }
        if CellIdx >= 0 then
          if TryStrToInt(Value, IntVal) then
            CellStyle.TextColor := IntVal;
      63:
        { Цвет фона ячейки }
        if CellIdx >= 0 then
          if TryStrToInt(Value, IntVal) then
            CellStyle.BackgroundColor := IntVal;
      283:
        { Признак использования цвета фона }
        if CellIdx >= 0 then
          if TryStrToInt(Value, IntVal) then
            CellStyle.BackgroundColorEnabled := IntVal <> 0;
    end;

    Inc(I, 2);
  end;

  { Сохраняем последний блок стиля строки }
  if CellIdx >= 0 then
    Style^.CellFormats.PushBackData(CellStyle);

  { Логируем завершение загрузки стиля }
  programlog.LogOutFormatStr(
    'uzestylestablesdxf: стиль "%s" загружен',
    [StyleName], LM_Info);
end;

{ Основная функция загрузки стилей таблиц из секции OBJECTS DXF-файла. }
procedure ReadTableStylesFromDXFObjects(
  const RawObjectsSection: string;
  var TableStyleTable: GDBDXFTableStyleArray);
var
  Lines: TStringList;
  StyleNameByHandle: TStringList;
  I, Code: Integer;
  Value, ObjHandle, StyleName, DictHandle: string;
  InTableStyle: Boolean;
  ObjectLines: TStringList;
  Style: PTGDBDXFTableStyle;
begin
  if RawObjectsSection = '' then
    Exit;

  programlog.LogOutFormatStr(
    'uzestylestablesdxf: начало загрузки стилей таблиц из секции OBJECTS',
    [], LM_Info);

  Lines := SplitDXFLines(RawObjectsSection);
  StyleNameByHandle := TStringList.Create;
  ObjectLines := TStringList.Create;
  try
    StyleNameByHandle.CaseSensitive := False;

    { Шаг 1: строим карту хэндл → имя стиля из словаря ACAD_TABLESTYLE }
    ExtractTableStyleDictionary(Lines, StyleNameByHandle, DictHandle);

    programlog.LogOutFormatStr(
      'uzestylestablesdxf: найдено %d стилей в словаре ACAD_TABLESTYLE',
      [StyleNameByHandle.Count], LM_Info);

    { Шаг 2: проходим по секции OBJECTS и обрабатываем объекты TABLESTYLE }
    InTableStyle := False;
    ObjHandle := '';
    I := 0;

    while I < Lines.Count - 1 do
    begin
      Code := ParseGroupCode(Lines[I]);
      Value := Trim(Lines[I + 1]);

      if not InTableStyle then
      begin
        if (Code = 0) and (UpperCase(Value) = 'TABLESTYLE') then
        begin
          { Начало нового объекта TABLESTYLE }
          InTableStyle := True;
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
          { Конец объекта TABLESTYLE — ищем имя по хэндлу и разбираем }
          if ObjHandle <> '' then
          begin
            StyleName := StyleNameByHandle.Values[UpperCase(ObjHandle)];
            if StyleName <> '' then
            begin
              programlog.LogOutFormatStr(
                'uzestylestablesdxf: обработка стиля "%s" хэндл=%s',
                [StyleName, ObjHandle], LM_Info);
              Style := TableStyleTable.AddStyle(StyleName);
              if Style <> nil then
                ParseTableStyleObject(StyleName, ObjectLines, Style);
            end
            else
              programlog.LogOutFormatStr(
                'uzestylestablesdxf: TABLESTYLE хэндл=%s не найден в словаре, пропускаем',
                [ObjHandle], LM_Info);
          end;
          { Сбрасываем состояние, текущую строку обработаем в следующей итерации }
          InTableStyle := False;
          ObjHandle := '';
          ObjectLines.Clear;
          Continue;
        end;

        { Фиксируем хэндл объекта (первое вхождение кода 5) }
        if (Code = 5) and (ObjHandle = '') then
          ObjHandle := UpperCase(Value);

        { Накапливаем строки объекта для последующего разбора }
        ObjectLines.Add(Lines[I]);
        ObjectLines.Add(Lines[I + 1]);
      end;

      Inc(I, 2);
    end;

    { Обрабатываем последний объект, если он не завершился до конца файла }
    if InTableStyle and (ObjHandle <> '') then
    begin
      StyleName := StyleNameByHandle.Values[UpperCase(ObjHandle)];
      if StyleName <> '' then
      begin
        Style := TableStyleTable.AddStyle(StyleName);
        if Style <> nil then
          ParseTableStyleObject(StyleName, ObjectLines, Style);
      end;
    end;

    programlog.LogOutFormatStr(
      'uzestylestablesdxf: загрузка стилей таблиц завершена',
      [], LM_Info);
  finally
    Lines.Free;
    StyleNameByHandle.Free;
    ObjectLines.Free;
  end;
end;

{ Добавляет строки из многострочного текста Text в список Lines.
  Необходимо для вставки результата BuildTableStyleObjectText (который использует
  Lines.Text, оканчивающийся на #10) в ResultLines по одной строке, чтобы избежать
  создания двойных переносов строк при последующем вызове ResultLines.Text. }
procedure AppendTextLinesToList(Lines: TStringList; const Text: string);
var
  TempList: TStringList;
  LastIndex, I: Integer;
begin
  TempList := TStringList.Create;
  try
    TempList.Text := Text;
    { TStringList.Text добавляет завершающий #10, что создаёт пустой последний элемент.
      Пропускаем все пустые элементы с конца, чтобы не вставлять пустые строки в DXF,
      которые нарушают разбор пар (код_группы, значение) и вызывают PARSEINTEGER ошибку. }
    LastIndex := TempList.Count - 1;
    while (LastIndex >= 0) and (TempList[LastIndex] = '') do
      Dec(LastIndex);
    for I := 0 to LastIndex do
      Lines.Add(TempList[I]);
  finally
    TempList.Free;
  end;
end;

{ Добавляет в Lines строки одного блока ячейки стиля таблицы (title/header/data).
  TextStyleName — имя текстового стиля для данного блока (группа 7).
  Если TextStyleName пустой — используется 'Standard'. }
procedure AppendCellStyleLines(Lines: TStringList;
  const CS: TGDBDXFTableCellStyle; const TextStyleName: string);
begin
  Lines.Add('  7');
  if TextStyleName <> '' then
    Lines.Add(TextStyleName)
  else
    Lines.Add('Standard');

  Lines.Add('140');
  Lines.Add(FloatToStr(CS.TextHeight));

  Lines.Add('170');
  Lines.Add(IntToStr(CS.Alignment));

  Lines.Add(' 62');
  Lines.Add(IntToStr(CS.TextColor));

  Lines.Add(' 63');
  Lines.Add(IntToStr(CS.BackgroundColor));

  Lines.Add('283');
  if CS.BackgroundColorEnabled then
    Lines.Add('     1')
  else
    Lines.Add('     0');

  Lines.Add(' 90');
  Lines.Add('      512');
  Lines.Add(' 91');
  Lines.Add('        0');

  Lines.Add('  1');
  Lines.Add('');

  { Границы ячейки: коды 274-279 (типы линий), 284-289, 64-69 (цвета).
    Значение -2 означает "наследовать от родителя / по умолчанию". }
  Lines.Add('274');   Lines.Add('    -2');
  Lines.Add('284');   Lines.Add('     1');
  Lines.Add(' 64');   Lines.Add('     0');
  Lines.Add('275');   Lines.Add('    -2');
  Lines.Add('285');   Lines.Add('     1');
  Lines.Add(' 65');   Lines.Add('     0');
  Lines.Add('276');   Lines.Add('    -2');
  Lines.Add('286');   Lines.Add('     1');
  Lines.Add(' 66');   Lines.Add('     0');
  Lines.Add('277');   Lines.Add('    -2');
  Lines.Add('287');   Lines.Add('     1');
  Lines.Add(' 67');   Lines.Add('     0');
  Lines.Add('278');   Lines.Add('    -2');
  Lines.Add('288');   Lines.Add('     1');
  Lines.Add(' 68');   Lines.Add('     0');
  Lines.Add('279');   Lines.Add('    -2');
  Lines.Add('289');   Lines.Add('     1');
  Lines.Add(' 69');   Lines.Add('     0');
end;

{ Создаёт текстовое представление объекта TABLESTYLE для секции OBJECTS.
  Handle      — хэндл объекта в 16-ричном формате (например '299').
  OwnerHandle — хэндл владельца (словарь ACAD_TABLESTYLE), или '' если неизвестен.
  XDictHandle — хэндл расширенного словаря объекта (блок 102/ACAD_XDICTIONARY),
                или '' если блок отсутствует. }
function BuildTableStyleObjectText(
  const StyleName: string;
  Style: PTGDBDXFTableStyle;
  const Handle: string;
  const OwnerHandle: string;
  const XDictHandle: string): string;
var
  Lines: TStringList;
  DefaultCS: TGDBDXFTableCellStyle;
  PCellStyle: PTGDBDXFTableCellStyle;
  Iter: itrec;
  { Значения выравнивания по умолчанию для строк: title(0), header(1), data(2) }
  DefaultAlignments: array[0..2] of Integer;
  CellIdx: Integer;
begin
  { Выравнивание по умолчанию: title=TopLeft(2), header=MiddleCenter(5), data=MiddleCenter(5) }
  DefaultAlignments[0] := 2;
  DefaultAlignments[1] := 5;
  DefaultAlignments[2] := 5;

  Lines := TStringList.Create;
  try
    Lines.Add('  0');
    Lines.Add('TABLESTYLE');
    Lines.Add('  5');
    Lines.Add(Handle);
    { Блок 102/ACAD_XDICTIONARY — ссылка на расширенный словарь объекта.
      Должна предшествовать блоку ACAD_REACTORS. AutoCAD требует этот блок,
      если он присутствовал в исходном файле, иначе считает объект повреждённым. }
    if XDictHandle <> '' then
    begin
      Lines.Add('102');
      Lines.Add('{ACAD_XDICTIONARY');
      Lines.Add('360');
      Lines.Add(XDictHandle);
      Lines.Add('102');
      Lines.Add('}');
    end;
    { Группа 102/330 — реакторы: объект принадлежит словарю ACAD_TABLESTYLE.
      Это обязательная связь, без которой AutoCAD считает файл повреждённым. }
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
    { TABLESTYLE не использует подкласс AcDbObject.
      AutoCAD напрямую ожидает AcDbTableStyle после блоков REACTORS/XDICTIONARY. }
    Lines.Add('100');
    Lines.Add('AcDbTableStyle');
    Lines.Add('  3');
    { Группа 3 — описание стиля (не имя, имя хранится в словаре ACAD_TABLESTYLE) }
    Lines.Add(StyleName);
    Lines.Add(' 70');
    Lines.Add('     0');
    Lines.Add(' 71');
    Lines.Add('     0');
    Lines.Add(' 40');
    Lines.Add('1.5');
    Lines.Add(' 41');
    Lines.Add('1.5');
    Lines.Add('280');
    if Style^.TitleSuppressed then
      Lines.Add('     1')
    else
      Lines.Add('     0');
    Lines.Add('281');
    if Style^.ColumnHeadingSuppressed then
      Lines.Add('     1')
    else
      Lines.Add('     0');

    { Записываем блоки ячеек из CellFormats, при нехватке — используем значения по умолчанию }
    CellIdx := 0;
    PCellStyle := Style^.CellFormats.beginiterate(Iter);
    while (PCellStyle <> nil) and (CellIdx < 3) do
    begin
      { Имя текстового стиля хранится в CellTextStyleName (отдельно от record ячейки) }
      AppendCellStyleLines(Lines, PCellStyle^, Style^.CellTextStyleName[CellIdx]);
      Inc(CellIdx);
      PCellStyle := Style^.CellFormats.iterate(Iter);
    end;

    { Добиваем до трёх блоков значениями по умолчанию }
    while CellIdx < 3 do
    begin
      FillChar(DefaultCS, SizeOf(DefaultCS), 0);
      DefaultCS.TextHeight := 2.5;
      DefaultCS.Alignment := DefaultAlignments[CellIdx];
      DefaultCS.BackgroundColor := 7;
      AppendCellStyleLines(Lines, DefaultCS, 'Standard');
      Inc(CellIdx);
    end;

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

{ Обновляет словарь ACAD_TABLESTYLE в ResultLines, добавляя запись для нового стиля.
  DictHandle  — хэндл словаря ACAD_TABLESTYLE.
  StyleName   — имя нового стиля.
  StyleHandle — хэндл нового объекта TABLESTYLE. }
procedure AddStyleToDictionary(ResultLines: TStringList;
  const DictHandle, StyleName, StyleHandle: string);
var
  J, CodeJ: Integer;
  ValJ: string;
  InDict: Boolean;
begin
  { Ищем DICTIONARY с хэндлом DictHandle и вставляем перед его завершением. }
  InDict := False;
  J := 0;
  while J < ResultLines.Count - 1 do
  begin
    CodeJ := ParseGroupCode(ResultLines[J]);
    ValJ := Trim(ResultLines[J + 1]);
    if not InDict then
    begin
      if (CodeJ = 5) and (UpperCase(ValJ) = UpperCase(DictHandle)) then
        InDict := True;
    end
    else
    begin
      if CodeJ = 0 then
      begin
        { Вставляем новую запись перед следующим объектом }
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

{ Записывает стили таблиц из TableStyleTable в секцию OBJECTS.
  Существующие объекты TABLESTYLE заменяются с сохранением оригинальных хэндлов,
  что предотвращает разрыв ссылок из словаря ACAD_TABLESTYLE.
  Новые стили добавляются перед ENDSEC с обновлением словаря. }
procedure WriteTableStylesToDXFObjects(
  var TableStyleTable: GDBDXFTableStyleArray;
  var RawObjectsSection: string);
var
  Lines: TStringList;
  ResultLines: TStringList;
  StyleNameByHandle: TStringList;
  HandleByStyleName: TStringList;
  StyleIter: itrec;
  Style: PTGDBDXFTableStyle;
  I, Code, HandleBase: Integer;
  Value, StyleName, ObjHandle, DictHandle, NewHandle: string;
  InTableStyle: Boolean;
  WrittenStyleNames: TStringList;
  ObjectText: string;
begin
  if TableStyleTable.count = 0 then
    Exit;

  programlog.LogOutFormatStr(
    'uzestylestablesdxf: начало записи %d стилей таблиц в секцию OBJECTS',
    [TableStyleTable.count], LM_Info);

  Lines := SplitDXFLines(RawObjectsSection);
  ResultLines := TStringList.Create;
  WrittenStyleNames := TStringList.Create;
  StyleNameByHandle := TStringList.Create;
  HandleByStyleName := TStringList.Create;
  try
    StyleNameByHandle.CaseSensitive := False;
    HandleByStyleName.CaseSensitive := False;

    { Шаг 1: читаем карту хэндл→имя из словаря ACAD_TABLESTYLE и строим обратную.
      Это нужно, чтобы при перезаписи использовать оригинальные хэндлы:
      словарь ссылается на них, и замена нарушает целостность файла. }
    ExtractTableStyleDictionary(Lines, StyleNameByHandle, DictHandle);

    for I := 0 to StyleNameByHandle.Count - 1 do
    begin
      Value := StyleNameByHandle.Names[I];
      StyleName := StyleNameByHandle.ValueFromIndex[I];
      HandleByStyleName.Values[UpperCase(StyleName)] := Value;
    end;

    programlog.LogOutFormatStr(
      'uzestylestablesdxf: словарь ACAD_TABLESTYLE хэндл=%s, стилей=%d',
      [DictHandle, StyleNameByHandle.Count], LM_Info);

    { Хэндлы для новых стилей (отсутствующих в словаре) — берём из
      диапазона, который не конфликтует со стандартными хэндлами. }
    HandleBase := $F000;
    InTableStyle := False;
    ObjHandle := '';
    I := 0;

    while I < Lines.Count - 1 do
    begin
      Code := ParseGroupCode(Lines[I]);
      Value := Trim(Lines[I + 1]);

      if not InTableStyle then
      begin
        if (Code = 0) and (UpperCase(Value) = 'TABLESTYLE') then
        begin
          { Начало существующего объекта TABLESTYLE.
            Не копируем — заменим обновлённой версией при обнаружении хэндла. }
          InTableStyle := True;
          ObjHandle := '';
          Inc(I, 2);
          Continue;
        end;

        { Перед ENDSEC вставляем стили, которые ещё не были записаны
          (например, добавленные программно, без записи в файле). }
        if (Code = 0) and (UpperCase(Value) = 'ENDSEC') then
        begin
          Style := TableStyleTable.beginiterate(StyleIter);
          while Style <> nil do
          begin
            StyleName := Style^.Name;
            if WrittenStyleNames.IndexOf(UpperCase(StyleName)) < 0 then
            begin
              NewHandle := HandleByStyleName.Values[UpperCase(StyleName)];
              if NewHandle = '' then
              begin
                { Новый стиль — назначаем уникальный хэндл }
                NewHandle := UpperCase(IntToHex(HandleBase, 0));
                Inc(HandleBase);
                { Добавляем запись в словарь ACAD_TABLESTYLE }
                if DictHandle <> '' then
                  AddStyleToDictionary(ResultLines, DictHandle, StyleName, NewHandle);
              end;
              ObjectText := BuildTableStyleObjectText(
                StyleName, Style, NewHandle, DictHandle, Style^.XDictHandle);
              AppendTextLinesToList(ResultLines, ObjectText);
              WrittenStyleNames.Add(UpperCase(StyleName));
              programlog.LogOutFormatStr(
                'uzestylestablesdxf: записан новый стиль "%s" хэндл=%s',
                [StyleName, NewHandle], LM_Info);
            end;
            Style := TableStyleTable.iterate(StyleIter);
          end;
        end;

        ResultLines.Add(Lines[I]);
        ResultLines.Add(Lines[I + 1]);
      end
      else
      begin
        { Внутри пропускаемого объекта TABLESTYLE — собираем хэндл. }
        if (Code = 5) and (ObjHandle = '') then
          ObjHandle := UpperCase(Value);

        if Code = 0 then
        begin
          { Конец существующего TABLESTYLE — пишем его обновлённую версию. }
          if ObjHandle <> '' then
          begin
            StyleName := StyleNameByHandle.Values[ObjHandle];
            if StyleName = '' then
              programlog.LogOutFormatStr(
                'uzestylestablesdxf: TABLESTYLE хэндл=%s не в словаре, пропускаем',
                [ObjHandle], LM_Info)
            else if WrittenStyleNames.IndexOf(UpperCase(StyleName)) < 0 then
            begin
              Style := PTGDBDXFTableStyle(TableStyleTable.getAddres(StyleName));
              if Style <> nil then
              begin
                { Используем оригинальный хэндл — словарь уже ссылается на него }
                ObjectText := BuildTableStyleObjectText(
                  StyleName, Style, ObjHandle, DictHandle, Style^.XDictHandle);
                AppendTextLinesToList(ResultLines, ObjectText);
                WrittenStyleNames.Add(UpperCase(StyleName));
                programlog.LogOutFormatStr(
                  'uzestylestablesdxf: перезаписан стиль "%s" хэндл=%s',
                  [StyleName, ObjHandle], LM_Info);
              end;
            end;
          end;
          InTableStyle := False;
          ObjHandle := '';
          { Текущую строку (начало следующего объекта) обработаем снова }
          Continue;
        end;
      end;

      Inc(I, 2);
    end;

    RawObjectsSection := ResultLines.Text;

    programlog.LogOutFormatStr(
      'uzestylestablesdxf: запись стилей таблиц завершена',
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
