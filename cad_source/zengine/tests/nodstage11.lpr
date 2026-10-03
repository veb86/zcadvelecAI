program nodstage11;

// issue #1465 (замечание от 2026-10-03): файл AutoCAD
// cad_source/testacadtable/acadtable2007.dxf, пересохранённый ZCAD
// (cad_source/testacadtable/ZCADTABLE2007.dxf), открывался в AutoCAD с
// неверными ширинами колонок и высотами строк.
//
// Причина: CELLSTYLEMAP стиля таблицы писался с жёсткими полями ячеек 1.5
// вместо прочитанных 0.06. AutoCAD 2008+ раскладывает таблицу именно по
// полям CELLSTYLEMAP и расширял колонки 2.5 и строки 0.36 под поля 3.0.
// Вторая ошибка — контрольная сумма ячейки ACAD_ROUNDTRIP_2008_CELL_CHECKSUM:
// AutoCAD считает сумму «код символа × позиция», ZCAD писал простую сумму.
//
// Для каждого образца тест читает чертёж, сохраняет его в DXF 2007 дважды
// (raw — с исходными сущностями, edit — модельным путём записи) и сверяет
// сохранённый файл с исходным:
//  1. поля ячеек CELLSTYLEMAP каждого стиля таблицы (по имени стиля и
//     идентификатору стиля ячейки 1=_TITLE, 2=_HEADER, 3=_DATA);
//  2. контрольные суммы ячеек: для каждого текста ячейки — то же значение,
//     что записал AutoCAD; в исходном файле AutoCAD все суммы обязаны
//     совпасть с формулой «код × позиция»;
//  3. ширины колонок и высоты строк TABLECONTENT.
// Кроме того, ZCADTABLE2007.dxf (запись ZCAD до исправления) после
// пересохранения должен совпасть по этим пунктам с acadtable2007.dxf:
// карта CELLSTYLEMAP, не согласованная с группами 40/41 стиля, не
// используется, суммы ячеек пересчитываются.
//
// Использование:
//   nodstage11 [<корень репозитория>]

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager,
  uzgldrawcontext, uzeTypes, uzeconsts, gzctnrVectorTypes, uzeentity,
  uzeffdxfobjects, uzeffdxfnod, uzeacadtable_types, uzeacadtable_model;

const
  TemplateFile =
    'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/' +
    'savetemplate2007.dxf';
  { Эталон AutoCAD из замечания к issue #1465 и образцы с разными полями
    у разных стилей ячеек (0.5/1.0, 1.0/2.0, 1.5, интервалы 4.5) }
  Samples: array[0..2] of string = (
    'cad_source/testacadtable/acadtable2007',
    'cad_source/test/bugbreaktable',
    'cad_source/test/tableheighttextbug');
  { acadtable2007, сохранённый ZCAD до исправления (из замечания) }
  OldZCADSample = 'cad_source/testacadtable/ZCADTABLE2007';
  { Ключ карты стилей ячеек в расширенном словаре TABLESTYLE }
  CellStyleMapKey = 'ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP';
  ChecksumKey = 'ACAD_ROUNDTRIP_2008_CELL_CHECKSUM';

var
  Root: string;
  Failed: Integer;
  DXFFmt: TFormatSettings;

procedure Fail(const S: string);
begin
  writeln('FAIL: ', S);
  Inc(Failed);
end;

procedure Ok(const S: string);
begin
  writeln('ok:   ', S);
end;

procedure CheckStr(const AExpected, AActual, S: string);
begin
  if AExpected = AActual then
    Ok(S + ' = ' + AActual)
  else
    Fail(Format('%s: expected "%s", got "%s"', [S, AExpected, AActual]));
end;

procedure LoadDrawing(const AFileName: string; var ADrawing: TSimpleDrawing);
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
begin
  ADrawing.init(nil);
  DC := ADrawing.CreateDrawingRC;
  ZDC.CreateRec(ADrawing, ADrawing.pObjRoot^, TLOLoad, DC);
  AddFromDXF(AFileName, ZDC);
end;

{ TSimpleDrawing.done не освобождает строковые поля (см. nodstage2). }
procedure DoneDrawing(var ADrawing: TSimpleDrawing);
begin
  ADrawing.done;
  ADrawing.RawClassesSection := '';
  ADrawing.RawObjectsSection := '';
end;

{ Число DXF в каноническом виде: 0.06, 0.060000 и 6.0E-2 совпадают }
function N(const AValue: string): string;
begin
  Result := FormatFloat('0.######',
    StrToFloatDef(Trim(AValue), -1, DXFFmt), DXFFmt);
end;

{ Пары «код/значение» секции OBJECTS: AList[2*I] — код, AList[2*I+1] —
  значение. Возвращает текст секции (для модели NOD). }
function LoadObjectsPairs(const AFileName: string; AList: TStrings): string;
var
  L: TStringList;
  I: Integer;
  InObjects: Boolean;
begin
  Result := '';
  AList.Clear;
  InObjects := False;
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    I := 0;
    while I + 1 < L.Count do begin
      if not InObjects then
        InObjects := (Trim(L[I]) = '2') and (Trim(L[I + 1]) = 'OBJECTS');
      if InObjects then begin
        Result := Result + L[I] + LineEnding + L[I + 1] + LineEnding;
        AList.Add(Trim(L[I]));
        AList.Add(L[I + 1]);
        if (Trim(L[I]) = '0') and (Trim(L[I + 1]) = 'ENDSEC') then
          Exit;
      end;
      Inc(I, 2);
    end;
  finally
    L.Free;
  end;
end;

{ Поля ячеек одного CELLSTYLEMAP: строки «<id>:верх/право/низ/лево/
  интервал/интервал» в порядке записи }
procedure DumpCellStyleMap(AMap: TZDXFRawObject; const AStyle: string;
  AOut: TStrings);
var
  I: Integer;
  V, Margins: string;
  InMargin, InCellStyle: Boolean;
begin
  Margins := '';
  InMargin := False;
  InCellStyle := False;
  for I := 0 to AMap.PairCount - 1 do begin
    V := Trim(AMap.Pairs[I].Value);
    case AMap.Pairs[I].Code of
      1:
        if V = 'CELLMARGIN_BEGIN' then begin
          InMargin := True;
          Margins := '';
        end else if V = 'CELLSTYLE_BEGIN' then
          InCellStyle := True;
      309:
        if V = 'CELLMARGIN_END' then
          InMargin := False
        else if V = 'CELLSTYLE_END' then
          InCellStyle := False;
      40:
        if InMargin then begin
          if Margins <> '' then
            Margins := Margins + '/';
          Margins := Margins + N(V);
        end;
      90:
        if InCellStyle then begin
          { Сравниваются стили ячеек, которые ZCAD хранит: 1..3 }
          if (V = '1') or (V = '2') or (V = '3') then
            AOut.Add(AStyle + ' cellstyle ' + V + ': ' + Margins);
          InCellStyle := False;
        end;
    end;
  end;
end;

{ Поля ячеек всех стилей таблиц файла: словарь ACAD_TABLESTYLE → TABLESTYLE
  → расширенный словарь → CELLSTYLEMAP }
procedure DumpMargins(const AObjects: string; AOut: TStrings);
var
  Model: TZNODModel;
  Dict, XDict: TZDXFDictionary;
  Style, Map: TZDXFRawObject;
  I, J: Integer;
begin
  AOut.Clear;
  Model := TZNODModel.Create;
  try
    if not Model.LoadFromText(AObjects) then begin
      Fail('LoadFromText: ' + Model.ParseError);
      Exit;
    end;
    Dict := Model.ResolveDictionary('ACAD_TABLESTYLE');
    if Dict = nil then
      Exit;
    for I := 0 to Dict.Count - 1 do begin
      Style := Model.FindObject(Dict[I].TargetHandle);
      if (Style = nil) or (Style.XDictHandle = 0) then
        Continue;
      XDict := Model.FindDictionary(Style.XDictHandle);
      if XDict = nil then
        Continue;
      for J := 0 to XDict.Count - 1 do
        if XDict[J].Key = CellStyleMapKey then begin
          Map := Model.FindObject(XDict[J].TargetHandle);
          if Map <> nil then
            DumpCellStyleMap(Map, Dict[I].Key, AOut);
        end;
    end;
  finally
    Model.Free;
  end;
  TStringList(AOut).Sort;
end;

{ Контрольная сумма AutoCAD: сумма «код символа × позиция (с 1)» }
function WeightedChecksum(const AText: string): Int64;
var
  U: UnicodeString;
  I: Integer;
begin
  Result := 0;
  U := UTF8Decode(AText);
  for I := 1 to Length(U) do
    Result := Result + Int64(Ord(U[I])) * I;
end;

{ Контрольные суммы ячеек: «текст=сумма». Сумма (140 после ключа
  ACAD_ROUNDTRIP_2008_CELL_CHECKSUM) стоит в DATAMAP ячейки перед её
  содержимым; текст — первая непустая группа 302 значения ячейки. }
procedure DumpChecksums(APairs: TStrings; AOut: TStrings);
var
  I, J: Integer;
  Code, V, Sum, Text: string;
  HaveCell: Boolean;

  procedure Flush;
  begin
    if HaveCell then
      AOut.Add(Text + '=' + Sum);
    HaveCell := False;
  end;

begin
  AOut.Clear;
  HaveCell := False;
  I := 0;
  while I + 1 < APairs.Count do begin
    Code := APairs[I];
    V := APairs[I + 1];
    if (Code = '300') and (V = ChecksumKey) then begin
      Flush;
      HaveCell := True;
      Text := '';
      Sum := '';
      J := I + 2;
      while (J + 1 < APairs.Count) and (J < I + 24) do begin
        if APairs[J] = '140' then begin
          Sum := N(APairs[J + 1]);
          Break;
        end;
        Inc(J, 2);
      end;
    end else if (Code = '0') then
      Flush
    else if HaveCell and (Text = '') and (Code = '302')
      and (Trim(V) <> '') and (V <> 'GRIDFORMAT') and (V <> 'CONTENT')
      and (V <> 'CONTENTFORMAT') then
      Text := V;
    Inc(I, 2);
  end;
  Flush;
  TStringList(AOut).Sort;
end;

{ Ширины колонок и высоты строк TABLECONTENT (группа 40 блоков
  TABLECOLUMN_BEGIN/TABLEROW_BEGIN) }
procedure DumpSizes(APairs: TStrings; AOut: TStrings);
var
  I: Integer;
  Kind: string;
begin
  AOut.Clear;
  Kind := '';
  I := 0;
  while I + 1 < APairs.Count do begin
    if APairs[I] = '1' then begin
      if APairs[I + 1] = 'TABLECOLUMN_BEGIN' then
        Kind := 'col'
      else if APairs[I + 1] = 'TABLEROW_BEGIN' then
        Kind := 'row';
    end else if (APairs[I] = '309') and ((APairs[I + 1] = 'TABLECOLUMN_END')
      or (APairs[I + 1] = 'TABLEROW_END')) then
      Kind := ''
    else if (Kind <> '') and (APairs[I] = '40') then begin
      AOut.Add(Kind + ' ' + N(APairs[I + 1]));
      Kind := '';
    end;
    Inc(I, 2);
  end;
  TStringList(AOut).Sort;
end;

procedure CompareLists(AExpected, AActual: TStrings; const AName: string);
var
  I: Integer;
begin
  if AExpected.Count = 0 then begin
    Fail(AName + ': nothing to compare in the source file');
    Exit;
  end;
  if AExpected.Text = AActual.Text then begin
    Ok(Format('%s: %d values equal', [AName, AExpected.Count]));
    Exit;
  end;
  for I := 0 to AExpected.Count - 1 do
    if (I >= AActual.Count) or (AExpected[I] <> AActual[I]) then begin
      if I < AActual.Count then
        Fail(Format('%s: expected "%s", got "%s"',
          [AName, AExpected[I], AActual[I]]))
      else
        Fail(Format('%s: missing "%s"', [AName, AExpected[I]]));
      Exit;
    end;
  Fail(Format('%s: extra "%s"', [AName, AActual[AExpected.Count]]));
end;

{ В исходном файле AutoCAD каждая контрольная сумма — «код × позиция» }
procedure CheckSourceChecksums(AChecksums: TStrings; const AName: string);
var
  I, P, Bad: Integer;
  S: string;
begin
  Bad := 0;
  for I := 0 to AChecksums.Count - 1 do begin
    S := AChecksums[I];
    P := LastDelimiter('=', S);
    if N(Copy(S, P + 1, MaxInt)) <>
       IntToStr(WeightedChecksum(Copy(S, 1, P - 1))) then begin
      Inc(Bad);
      if Bad = 1 then
        Fail(AName + ': AutoCAD checksum differs from formula: ' + S);
    end;
  end;
  if Bad = 0 then
    Ok(Format('%s: %d AutoCAD checksums match code*position',
      [AName, AChecksums.Count]));
end;

{ Правка таблицы в ZCAD (как в nodstage10save): смена направления разрыва
  туда и обратно сбрасывает исходные сущности. }
procedure TouchTable(T: PGDBObjAcadTable);
var
  Dir: TAcadTableBreakDirection;
begin
  Dir := T^.BreakDirection;
  if Dir = atbdRight then
    T^.BreakDirection := atbdLeft
  else
    T^.BreakDirection := atbdRight;
  T^.BreakDirection := Dir;
end;

procedure RoundTripSample(const ASample: string; AEdit: Boolean;
  ASrcMargins, ASrcChecksums, ASrcSizes: TStrings);
var
  Drawing: TSimpleDrawing;
  Pairs, Margins, Checksums, Sizes: TStringList;
  Name, OutFile, Objects: string;
  E: PGDBObjEntity;
  IR: itrec;
begin
  Name := ExtractFileName(ASample) + BoolToStr(AEdit, ' (edit)', ' (raw)');
  OutFile := GetTempDir(False) + 'nodstage11_' + ExtractFileName(ASample) +
    BoolToStr(AEdit, '_edit', '_raw') + '.dxf';
  Pairs := TStringList.Create;
  Margins := TStringList.Create;
  Checksums := TStringList.Create;
  Sizes := TStringList.Create;
  try
    LoadDrawing(Root + ASample + '.dxf', Drawing);
    if AEdit then begin
      { Правка в ZCAD сбрасывает исходные сущности таблиц — запись идёт
        модельным путём }
      E := Drawing.pObjRoot^.ObjArray.beginiterate(IR);
      while E <> nil do begin
        if E^.GetObjType = GDBAcadTableID then
          TouchTable(PGDBObjAcadTable(E));
        E := Drawing.pObjRoot^.ObjArray.iterate(IR);
      end;
    end;
    if not savedxf20XX(OutFile, Root + TemplateFile, Drawing, ZCDxf2007) then
      Fail(Name + ': savedxf20XX');
    DoneDrawing(Drawing);
    Objects := LoadObjectsPairs(OutFile, Pairs);
    DumpMargins(Objects, Margins);
    DumpChecksums(Pairs, Checksums);
    DumpSizes(Pairs, Sizes);
    CompareLists(ASrcMargins, Margins, Name + ': CELLSTYLEMAP margins');
    CompareLists(ASrcChecksums, Checksums, Name + ': cell checksums');
    CompareLists(ASrcSizes, Sizes, Name + ': column widths/row heights');
  finally
    Pairs.Free;
    Margins.Free;
    Checksums.Free;
    Sizes.Free;
  end;
end;

{ ASample сохраняется ZCAD и сверяется с файлом AutoCAD AReference }
procedure TestSample(const ASample, AReference: string);
var
  Pairs, Margins, Checksums, Sizes: TStringList;
  Objects: string;
begin
  writeln('--- ', ASample);
  Pairs := TStringList.Create;
  Margins := TStringList.Create;
  Checksums := TStringList.Create;
  Sizes := TStringList.Create;
  try
    Objects := LoadObjectsPairs(Root + AReference + '.dxf', Pairs);
    DumpMargins(Objects, Margins);
    DumpChecksums(Pairs, Checksums);
    DumpSizes(Pairs, Sizes);
    CheckSourceChecksums(Checksums, ExtractFileName(AReference));
    RoundTripSample(ASample, False, Margins, Checksums, Sizes);
    RoundTripSample(ASample, True, Margins, Checksums, Sizes);
  finally
    Pairs.Free;
    Margins.Free;
    Checksums.Free;
    Sizes.Free;
  end;
  Flush(Output);
end;

{ Явные значения из замечания к issue #1465 }
procedure TestKnownValues;
begin
  CheckStr('2192', IntToStr(WeightedChecksum('Title-1')),
    'checksum "Title-1"');
  CheckStr('2861', IntToStr(WeightedChecksum('Header-1')),
    'checksum "Header-1"');
  CheckStr('1092', IntToStr(WeightedChecksum('ф')), 'checksum "ф"');
end;

var
  I: Integer;
begin
  DXFFmt := DefaultFormatSettings;
  DXFFmt.DecimalSeparator := '.';
  Root := '';
  if ParamCount > 0 then
    Root := IncludeTrailingPathDelimiter(ParamStr(1));
  Failed := 0;
  try
    TestKnownValues;
    for I := Low(Samples) to High(Samples) do
      TestSample(Samples[I], Samples[I]);
    { Файл, записанный ZCAD до исправления (поля 1.5 в CELLSTYLEMAP при
      группах 40/41 = 0.06, простые суммы ячеек): после пересохранения —
      как исходный файл AutoCAD }
    TestSample(OldZCADSample, Samples[0]);
  except
    on E: Exception do
      Fail('nodstage11: ' + E.ClassName + ': ' + E.Message);
  end;
  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
