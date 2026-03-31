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

  Зависимости: uzestylestables, uzclog, sysutils, Classes
}
unit uzestylestablesdxf;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}
interface
uses
  uzestylestables,
  UGDBNamedObjectsArray,
  gzctnrVectorTypes,
  uzclog,
  sysutils,
  Classes;

{ Загружает стили таблиц из сырого текста секции OBJECTS DXF-файла.
  Находит словарь ACAD_TABLESTYLE и все объекты TABLESTYLE,
  восстанавливает имена стилей из словаря и заполняет таблицу стилей.
  Параметры:
    RawObjectsSection — полный текст секции OBJECTS (включая 0/SECTION..0/ENDSEC)
    TableStyleTable   — таблица стилей для заполнения }
procedure ReadTableStylesFromDXFObjects(
  const RawObjectsSection: string;
  var TableStyleTable: GDBTableStyleArray);

{ Записывает стили таблиц из таблицы стилей в сырой текст секции OBJECTS.
  Если секция OBJECTS уже содержит объекты TABLESTYLE — они заменяются.
  Если нет — добавляются перед ENDSEC.
  Параметры:
    TableStyleTable   — таблица стилей для записи
    RawObjectsSection — текст секции OBJECTS для обновления (in/out) }
procedure WriteTableStylesToDXFObjects(
  var TableStyleTable: GDBTableStyleArray;
  var RawObjectsSection: string);

implementation

{ Разбивает текст DXF на строки и возвращает список.
  Каждая строка — отдельный элемент: чётные индексы — коды групп,
  нечётные — значения. }
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
  значение — имя стиля из словаря. }
procedure ExtractTableStyleDictionary(
  Lines: TStringList;
  StyleNameByHandle: TStringList);
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
      begin
        InTargetDict := False;
      end
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
  Style: PTGDBTableStyle);
var
  I, Code, IntVal: Integer;
  Value: string;
  CellStyle: TGDBTableCellStyle;
  InCellStyle: Boolean;
begin
  InCellStyle := False;
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
          { Начало нового блока стиля строки таблицы (group 7 = имя текстового стиля).
            При встрече второго блока сохраняем предыдущий. }
          if InCellStyle then
            Style^.tblformat.PushBackData(CellStyle);
          FillChar(CellStyle, SizeOf(CellStyle), 0);
          CellStyle.TextStyleName := Value;
          InCellStyle := True;
        end;
      140:
        { Высота текста строки }
        if InCellStyle then
          CellStyle.TextHeight := StrToFloatDef(Value, 2.5);
      170:
        { Выравнивание текста в ячейке }
        if InCellStyle then
          if TryStrToInt(Value, IntVal) then
            CellStyle.Alignment := IntVal;
      62:
        { Цвет текста ячейки }
        if InCellStyle then
          if TryStrToInt(Value, IntVal) then
            CellStyle.TextColor := IntVal;
      63:
        { Цвет фона ячейки }
        if InCellStyle then
          if TryStrToInt(Value, IntVal) then
            CellStyle.BackgroundColor := IntVal;
      283:
        { Признак использования цвета фона }
        if InCellStyle then
          if TryStrToInt(Value, IntVal) then
            CellStyle.BackgroundColorEnabled := IntVal <> 0;
    end;

    Inc(I, 2);
  end;

  { Сохраняем последний блок стиля строки }
  if InCellStyle then
    Style^.tblformat.PushBackData(CellStyle);

  { Логируем завершение загрузки стиля }
  programlog.LogOutFormatStr(
    'TableStyle "%s" loaded',
    [StyleName], LM_Info);
end;

{ Основная функция загрузки стилей таблиц из секции OBJECTS DXF-файла. }
procedure ReadTableStylesFromDXFObjects(
  const RawObjectsSection: string;
  var TableStyleTable: GDBTableStyleArray);
var
  Lines: TStringList;
  StyleNameByHandle: TStringList;
  I, Code: Integer;
  Value, ObjHandle, StyleName: string;
  InTableStyle: Boolean;
  ObjectLines: TStringList;
  Style: PTGDBTableStyle;
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
    ExtractTableStyleDictionary(Lines, StyleNameByHandle);

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

{ Добавляет в Lines строки одного блока ячейки стиля таблицы (title/header/data).
  Если CS.TextStyleName пустой — используется 'Standard'. }
procedure AppendCellStyleLines(Lines: TStringList; const CS: TGDBTableCellStyle);
begin
  Lines.Add('  7');
  if CS.TextStyleName <> '' then
    Lines.Add(CS.TextStyleName)
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
  Handle — хэндл в 16-ричном формате (например 'F001'). }
function BuildTableStyleObjectText(
  const StyleName: string;
  Style: PTGDBTableStyle;
  const Handle: string): string;
var
  Lines: TStringList;
  CS: TGDBTableCellStyle;
  PCellStyle: PTGDBTableCellStyle;
  Iter: itrec;
  { Значения ячеек по умолчанию для строк: title(0), header(1), data(2) }
  DefaultAlignments: array[0..2] of Integer;
  CellIdx: Integer;
begin
  DefaultAlignments[0] := 2;  { TopLeft }
  DefaultAlignments[1] := 5;  { MiddleCenter }
  DefaultAlignments[2] := 5;  { MiddleCenter }

  Lines := TStringList.Create;
  try
    Lines.Add('  0');
    Lines.Add('TABLESTYLE');
    Lines.Add('  5');
    Lines.Add(Handle);
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

    { Записываем блоки ячеек из tblformat, при нехватке — используем значения по умолчанию }
    CellIdx := 0;
    PCellStyle := Style^.tblformat.beginiterate(Iter);
    while (PCellStyle <> nil) and (CellIdx < 3) do
    begin
      AppendCellStyleLines(Lines, PCellStyle^);
      Inc(CellIdx);
      PCellStyle := Style^.tblformat.iterate(Iter);
    end;

    { Добиваем до трёх блоков значениями по умолчанию }
    while CellIdx < 3 do
    begin
      FillChar(CS, SizeOf(CS), 0);
      CS.TextStyleName := 'Standard';
      CS.TextHeight := 2.5;
      CS.Alignment := DefaultAlignments[CellIdx];
      CS.BackgroundColor := 7;
      AppendCellStyleLines(Lines, CS);
      Inc(CellIdx);
    end;

    Result := Lines.Text;
  finally
    Lines.Free;
  end;
end;

{ Записывает стили таблиц из TableStyleTable в секцию OBJECTS.
  Существующие объекты TABLESTYLE заменяются, новые добавляются перед ENDSEC. }
procedure WriteTableStylesToDXFObjects(
  var TableStyleTable: GDBTableStyleArray;
  var RawObjectsSection: string);
var
  Lines: TStringList;
  ResultLines: TStringList;
  StyleIter: itrec;
  Style: PTGDBTableStyle;
  I, Code, HandleBase: Integer;
  Value, StyleName: string;
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
  try
    InTableStyle := False;
    { Хэндлы для новых объектов берём из диапазона, который не конфликтует
      со стандартными хэндлами DXF (начинаем с большого значения) }
    HandleBase := $F000;
    I := 0;

    while I < Lines.Count - 1 do
    begin
      Code := ParseGroupCode(Lines[I]);
      Value := Trim(Lines[I + 1]);

      if not InTableStyle then
      begin
        if (Code = 0) and (UpperCase(Value) = 'TABLESTYLE') then
        begin
          { Пропускаем существующие объекты TABLESTYLE — они будут заменены }
          InTableStyle := True;
          Inc(I, 2);
          Continue;
        end;

        { Непосредственно перед ENDSEC вставляем все стили из таблицы }
        if (Code = 0) and (UpperCase(Value) = 'ENDSEC') then
        begin
          Style := TableStyleTable.beginiterate(StyleIter);
          while Style <> nil do
          begin
            StyleName := Style^.Name;
            if WrittenStyleNames.IndexOf(UpperCase(StyleName)) < 0 then
            begin
              ObjectText := BuildTableStyleObjectText(
                StyleName, Style,
                UpperCase(IntToHex(HandleBase, 0)));
              ResultLines.Add(ObjectText);
              WrittenStyleNames.Add(UpperCase(StyleName));

              programlog.LogOutFormatStr(
                'uzestylestablesdxf: записан стиль "%s" хэндл=%s',
                [StyleName, UpperCase(IntToHex(HandleBase, 0))], LM_Info);

              Inc(HandleBase);
            end;
            Style := TableStyleTable.iterate(StyleIter);
          end;
        end;

        ResultLines.Add(Lines[I]);
        ResultLines.Add(Lines[I + 1]);
      end
      else
      begin
        { Внутри пропускаемого TABLESTYLE — ждём конца объекта }
        if Code = 0 then
        begin
          InTableStyle := False;
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
  end;
end;

begin
end.
