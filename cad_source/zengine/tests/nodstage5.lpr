program nodstage5;

// issue #1452: этап 5 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (загрузка ACAD_TABLESTYLE NOD-обработчиком, удаление разбора сырого
// текста OBJECTS). Тест проверяет:
//
//  1. Обработчик ACAD_TABLESTYLE зарегистрирован с LoadProc и
//     EnsureDefaultsProc (MinVersion AC1015).
//  2. Стили, загруженные из NOD (LoadTableStylesFromNOD), совпадают с
//     эталонами data/nod/stage5/*.txt. Эталоны реальных чертежей получены
//     прежним разбором сырого текста (ReadTableStylesFromDXFObjects) —
//     новый загрузчик ему эквивалентен; nod_not_first и nod_hardowner_360
//     прежний разбор читал неверно (NOD не первый, 360 вместо 350).
//  3. Забранные хэндлы: каждый TABLESTYLE, его расширенный словарь и
//     CELLSTYLEMAP расширенного словаря, и только они.
//  4. Синтетическая ветка: запись не на TABLESTYLE, на несуществующий
//     объект и без имени пропускаются; существующий в таблице стиль
//     (вставка/слияние) не меняется; повтор имени — берётся первый.
//  5. Стиль Standard по умолчанию (EnsureDefaultsProc): для R12 и для
//     файла без ACAD_TABLESTYLE — один Standard со значениями
//     savetemplate2007; EnsureDefaultTableStyle не трогает непустую таблицу.
//  6. GetStyleByHandle: ссылка 342 (+testtable.dxf, BA) → vebtable.
//  7. tablestyleetalon: загрузка → сохранение 2000/2007 → загрузка даёт
//     те же стили (с точностью до хэндлов); класс CELLSTYLEMAP записан
//     один раз, группа 91 — только в AC1021; TABLESTYLE по числу стилей.
//
// Использование:
//   nodstage5 [<корень репозитория>]

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  gzctnrVectorTypes,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager, uzeffdxfsupport,
  uzgldrawcontext, uzeconsts, uzeTypes,
  uzeffdxfobjects, uzeffdxfnod, uzeffdxfnodregistry,
  uzestylestablesdxf, uzestylestablesdxfnod,
  // Регистрация загрузчика сущности ACAD_TABLE (как в nodstage0/4)
  uzeacadtable_types, uzeacadtable_model, uzeacadtable_dxf_write;

const
  TemplatesDir = 'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/';
  Stage5Dir = 'cad_source/zengine/tests/data/nod/stage5/';
  NodDataDir = 'cad_source/zengine/tests/data/nod/';
  EtalonFile = 'cad_source/test/tablestyleetalon.dxf';
  TableFile = 'cad_source/test/+testtable.dxf';

  TableStyleKey = 'ACAD_TABLESTYLE';
  CellStyleMapKey = 'ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP';

type
  TLoaderCase = record
    FileName: string;
    Golden: string;
  end;

const
  LoaderCases: array[0..7] of TLoaderCase = (
    (FileName: 'cad_source/test/tablestyleetalon.dxf'; Golden: 'tablestyleetalon.txt'),
    (FileName: 'cad_source/test/+testtable.dxf'; Golden: '+testtable.txt'),
    (FileName: 'cad_source/testtable.dxf'; Golden: 'testtable.txt'),
    (FileName: 'cad_source/test/tableheighttextbug.dxf'; Golden: 'tableheighttextbug.txt'),
    (FileName: 'cad_source/test/bugbreaktable.dxf'; Golden: 'bugbreaktable.txt'),
    (FileName: TemplatesDir + 'savetemplate2007.dxf'; Golden: 'savetemplate2007.txt'),
    (FileName: NodDataDir + 'nod_not_first.dxf'; Golden: 'nod_not_first.txt'),
    (FileName: NodDataDir + 'nod_hardowner_360.dxf'; Golden: 'nod_hardowner_360.txt')
  );

  { Синтетическая секция OBJECTS: NOD C, ACAD_TABLESTYLE → 20;
    словарь 20: Good → 21 (TABLESTYLE), NotStyle → 22 (XRECORD),
    Missing → 99 (нет объекта), '' → 23 (TABLESTYLE без имени),
    Good → 24 (повтор имени), Kept → 25 (TABLESTYLE). У 21 расширенный
    словарь 26: CELLSTYLEMAP 27 и XRECORD 28 (не переносится). }
  SyntheticObjects =
    '0'#10'SECTION'#10'2'#10'OBJECTS'#10 +
    '0'#10'DICTIONARY'#10'5'#10'C'#10'330'#10'0'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'ACAD_TABLESTYLE'#10'350'#10'20'#10 +
    '0'#10'DICTIONARY'#10'5'#10'20'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'Good'#10'350'#10'21'#10 +
    '3'#10'NotStyle'#10'350'#10'22'#10 +
    '3'#10'Missing'#10'350'#10'99'#10 +
    '3'#10#10'350'#10'23'#10 +
    '3'#10'Good'#10'350'#10'24'#10 +
    '3'#10'Kept'#10'350'#10'25'#10 +
    '0'#10'TABLESTYLE'#10'5'#10'21'#10'102'#10'{ACAD_XDICTIONARY'#10'360'#10'26'#10'102'#10'}'#10 +
    '330'#10'20'#10'100'#10'AcDbTableStyle'#10'3'#10'Good'#10'70'#10'1'#10'71'#10'2'#10 +
    '40'#10'0.25'#10'41'#10'0.5'#10'280'#10'1'#10'281'#10'0'#10 +
    '7'#10'TS1'#10'140'#10'1.5'#10'170'#10'1'#10'62'#10'3'#10'63'#10'4'#10'283'#10'1'#10 +
    '7'#10'TS2'#10'140'#10'2.5'#10'170'#10'5'#10'62'#10'0'#10'63'#10'7'#10'283'#10'0'#10 +
    '0'#10'XRECORD'#10'5'#10'22'#10'330'#10'20'#10'100'#10'AcDbXrecord'#10 +
    '0'#10'TABLESTYLE'#10'5'#10'23'#10'330'#10'20'#10'100'#10'AcDbTableStyle'#10 +
    '0'#10'TABLESTYLE'#10'5'#10'24'#10'330'#10'20'#10'100'#10'AcDbTableStyle'#10'40'#10'9'#10 +
    '0'#10'TABLESTYLE'#10'5'#10'25'#10'330'#10'20'#10'100'#10'AcDbTableStyle'#10'40'#10'9'#10 +
    '0'#10'DICTIONARY'#10'5'#10'26'#10'330'#10'21'#10'100'#10'AcDbDictionary'#10'280'#10'1'#10 +
    '3'#10 + CellStyleMapKey + #10'360'#10'27'#10 +
    '3'#10'OTHER'#10'360'#10'28'#10 +
    '0'#10'CELLSTYLEMAP'#10'5'#10'27'#10'330'#10'26'#10'100'#10'AcDbCellStyleMap'#10 +
    '0'#10'XRECORD'#10'5'#10'28'#10'330'#10'26'#10'100'#10'AcDbXrecord'#10 +
    '0'#10'ENDSEC'#10;

var
  Root: string;
  Failed: Integer;
  FS: TFormatSettings;

procedure Fail(const S: string);
begin
  writeln('FAIL: ', S);
  Inc(Failed);
end;

procedure Ok(const S: string);
begin
  writeln('ok:   ', S);
end;

procedure Check(ACondition: Boolean; const S: string);
begin
  if ACondition then
    Ok(S)
  else
    Fail(S);
end;

procedure CheckEquals(const AExpected, AActual, S: string);
begin
  if AExpected = AActual then
    Ok(S + ' = ' + AActual)
  else
    Fail(Format('%s: expected "%s", got "%s"', [S, AExpected, AActual]));
end;

procedure CheckInt(AExpected, AActual: Int64; const S: string);
begin
  CheckEquals(IntToStr(AExpected), IntToStr(AActual), S);
end;

{ Многострочное сравнение: при расхождении — обе версии целиком }
procedure CheckText(const AExpected, AActual, S: string);
begin
  if AExpected = AActual then
    Ok(S)
  else begin
    Fail(S + ': text differs');
    writeln('--- expected:'#10, AExpected, '--- actual:'#10, AActual, '---');
  end;
end;

type
  TTestProc = procedure;

{ Запускает проверку; исключение считается провалом проверки. }
procedure Run(const AName: string; AProc: TTestProc);
begin
  writeln('--- ', AName);
  try
    AProc();
  except
    on E: Exception do
      Fail(AName + ': ' + E.ClassName + ': ' + E.Message);
  end;
  Flush(Output);
end;

{ ---------- Вспомогательные ---------- }

function F(V: Double): string;
begin
  Result := FloatToStr(V, FS);
end;

{ Текстовый дамп стилей (формат эталонов data/nod/stage5). AMaskHandles —
  хэндлы заменяются на '*' (сравнение после пересохранения). }
function DumpTableStyles(var T: GDBDXFTableStyleArray;
  AMaskHandles: Boolean = False): string;
var
  S: PTGDBDXFTableStyle;
  C: PTGDBDXFTableCellStyle;
  It, It2: itrec;
  I: Integer;
  H, X: string;
begin
  Result := '';
  S := T.beginiterate(It);
  while S <> nil do begin
    H := S^.DXFHandle;
    X := S^.XDictHandle;
    if AMaskHandles then begin
      H := '*';
      X := '*';
    end;
    Result := Result + Format('%s handle=%s xdict=%s 70=%d 71=%d 40=%s 41=%s 280=%d 281=%d cells=%d',
      [S^.Name, H, X, S^.Flags70, S^.Flags71,
       F(S^.HorzCellMargin), F(S^.VertCellMargin), Ord(S^.TitleSuppressed),
       Ord(S^.ColumnHeadingSuppressed), S^.CellFormats.Count]) + #10;
    I := 0;
    C := S^.CellFormats.beginiterate(It2);
    while C <> nil do begin
      if I <= 2 then
        Result := Result + Format('  cell%d 7=%s', [I, S^.CellTextStyleName[I]])
      else
        Result := Result + Format('  cell%d', [I]);
      Result := Result + Format(' 140=%s 170=%d 62=%d 63=%d 283=%d',
        [F(C^.TextHeight), C^.Alignment, C^.TextColor, C^.BackgroundColor,
         Ord(C^.BackgroundColorEnabled)]) + #10;
      Inc(I);
      C := S^.CellFormats.iterate(It2);
    end;
    S := T.iterate(It);
  end;
end;

{ Эталон дампа; AMaskHandles — хэндлы заменяются на '*' }
function ReadGolden(const AName: string; AMaskHandles: Boolean = False): string;
var
  L: TStringList;
  I, P, E: Integer;
  Line, Key: string;
begin
  Result := '';
  L := TStringList.Create;
  try
    L.LoadFromFile(Root + Stage5Dir + AName);
    for I := 0 to L.Count - 1 do begin
      Line := TrimRight(L[I]);
      if AMaskHandles then
        for Key in ['handle=', 'xdict='] do begin
          P := Pos(Key, Line);
          if P > 0 then begin
            E := P + Length(Key);
            while (E <= Length(Line)) and (Line[E] <> ' ') do
              Inc(E);
            Line := Copy(Line, 1, P + Length(Key) - 1) + '*' + Copy(Line, E, MaxInt);
          end;
        end;
      Result := Result + Line + #10;
    end;
  finally
    L.Free;
  end;
end;

procedure LoadDrawing(const AFileName: string; var ADrawing: TSimpleDrawing);
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
begin
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

type
  TDXFPairs = record
    Codes: array of Integer;
    Values: array of string;
    Count: Integer;
  end;

{ Все пары файла (код, значение без пробелов по краям) }
function ReadPairs(const AFileName: string): TDXFPairs;
var
  L: TStringList;
  I, Code: Integer;
begin
  Result.Count := 0;
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    SetLength(Result.Codes, L.Count div 2);
    SetLength(Result.Values, L.Count div 2);
    I := 0;
    while I + 1 < L.Count do begin
      if not TryStrToInt(Trim(L[I]), Code) then
        raise Exception.CreateFmt('%s: line %d: bad group code "%s"',
          [AFileName, I + 1, L[I]]);
      Result.Codes[Result.Count] := Code;
      Result.Values[Result.Count] := Trim(L[I + 1]);
      Inc(Result.Count);
      Inc(I, 2);
    end;
  finally
    L.Free;
  end;
end;

{ Индекс пары 2/<AName> после 0/SECTION (-1 — секции нет) }
function SectionStart(const P: TDXFPairs; const AName: string): Integer;
var
  I: Integer;
begin
  for I := 1 to P.Count - 1 do
    if (P.Codes[I] = 2) and (P.Values[I] = AName) and
       (P.Codes[I - 1] = 0) and (P.Values[I - 1] = 'SECTION') then
      Exit(I);
  Result := -1;
end;

{ Индекс пары 0/ENDSEC, закрывающей секцию, начатую в AStart }
function SectionEnd(const P: TDXFPairs; AStart: Integer): Integer;
var
  I: Integer;
begin
  for I := AStart to P.Count - 1 do
    if (P.Codes[I] = 0) and (P.Values[I] = 'ENDSEC') then
      Exit(I);
  Result := -1;
end;

function PairsText(const P: TDXFPairs; AFrom, ATo: Integer): string;
var
  SB: TStringList;
  I: Integer;
begin
  SB := TStringList.Create;
  try
    for I := AFrom to ATo do begin
      SB.Add(IntToStr(P.Codes[I]));
      SB.Add(P.Values[I]);
    end;
    Result := SB.Text;
  finally
    SB.Free;
  end;
end;

{ Модель секции OBJECTS файла }
function ObjectsModelOfFile(const AFileName: string): TZNODModel;
var
  P: TDXFPairs;
  S, E: Integer;
begin
  P := ReadPairs(AFileName);
  Result := TZNODModel.Create;
  S := SectionStart(P, 'OBJECTS');
  if S < 0 then
    Exit;
  E := SectionEnd(P, S);
  if not Result.LoadFromText(PairsText(P, S - 1, E)) then
    Fail(AFileName + ': OBJECTS parse error: ' + Result.ParseError);
end;

function HandleOf(const AStr: string): TDWGHandle;
begin
  if not TryDXFStrToHandle(AStr, Result) then
    Result := 0;
end;

{ ---------- 1. Регистрация ---------- }

procedure TestHandlerRegistered;
var
  H: TZNODHandler;
begin
  Check(FindNODHandler(TableStyleKey, H), 'handler ' + TableStyleKey + ' is registered');
  CheckEquals('AC1015', ACDWGVerName(H.MinVersion), 'handler: MinVersion');
  Check(Assigned(H.LoadProc), 'handler: LoadProc');
  Check(Assigned(H.EnsureDefaultsProc), 'handler: EnsureDefaultsProc');
end;

{ ---------- 2. Загрузчик и эталоны ---------- }

procedure TestLoaderGolden;
var
  I: Integer;
  Model: TZNODModel;
  Styles: GDBDXFTableStyleArray;
  N: Integer;
begin
  for I := Low(LoaderCases) to High(LoaderCases) do begin
    Model := ObjectsModelOfFile(Root + LoaderCases[I].FileName);
    Styles.init(10);
    try
      N := LoadTableStylesFromNOD(Model, Styles);
      CheckInt(Styles.Count, N, LoaderCases[I].Golden + ': loaded count');
      CheckText(ReadGolden(LoaderCases[I].Golden), DumpTableStyles(Styles),
        LoaderCases[I].Golden + ': styles equal golden');
    finally
      Styles.done;
      Model.Free;
    end;
  end;
end;

{ ---------- 3. Забранные хэндлы ---------- }

{ Число хэндлов, которые должен забрать загрузчик для стиля, и проверка,
  что они забраны }
function CheckStyleClaims(AModel: TZNODModel; AStyle: PTGDBDXFTableStyle;
  const ACase: string): Integer;
var
  XDict: TZDXFDictionary;
  I: Integer;
  Obj: TZDXFRawObject;
begin
  Result := 1;
  Check(AModel.IsHandleClaimed(HandleOf(AStyle^.DXFHandle)),
    Format('%s: TABLESTYLE %s (%s) claimed', [ACase, AStyle^.Name, AStyle^.DXFHandle]));
  if AStyle^.XDictHandle = '' then
    Exit;
  Inc(Result);
  Check(AModel.IsHandleClaimed(HandleOf(AStyle^.XDictHandle)),
    Format('%s: xdict %s claimed', [ACase, AStyle^.XDictHandle]));
  XDict := AModel.FindDictionary(HandleOf(AStyle^.XDictHandle));
  if XDict = nil then
    Exit;
  for I := 0 to XDict.Count - 1 do begin
    Obj := AModel.FindObject(XDict[I].TargetHandle);
    if (Obj <> nil) and SameText(Obj.ObjType, 'CELLSTYLEMAP') then begin
      Inc(Result);
      Check(AModel.IsHandleClaimed(Obj.Handle),
        Format('%s: CELLSTYLEMAP %s claimed', [ACase, Obj.HandleStr]));
    end;
  end;
end;

procedure TestClaimedHandles;
var
  I, Expected: Integer;
  Model: TZNODModel;
  Styles: GDBDXFTableStyleArray;
  S: PTGDBDXFTableStyle;
  It: itrec;
begin
  for I := Low(LoaderCases) to High(LoaderCases) do begin
    Model := ObjectsModelOfFile(Root + LoaderCases[I].FileName);
    Styles.init(10);
    try
      LoadTableStylesFromNOD(Model, Styles);
      Expected := 0;
      S := Styles.beginiterate(It);
      while S <> nil do begin
        Inc(Expected, CheckStyleClaims(Model, S, LoaderCases[I].Golden));
        S := Styles.iterate(It);
      end;
      CheckInt(Expected, Model.ClaimedHandleCount,
        LoaderCases[I].Golden + ': only style objects are claimed');
    finally
      Styles.done;
      Model.Free;
    end;
  end;
end;

{ ---------- 4. Синтетическая ветка ---------- }

procedure TestSyntheticBranch;
var
  Model: TZNODModel;
  Styles: GDBDXFTableStyleArray;
  Kept: PTGDBDXFTableStyle;
  N: Integer;
begin
  Model := TZNODModel.Create;
  Styles.init(10);
  try
    Check(Model.LoadFromText(SyntheticObjects), 'synthetic: OBJECTS parsed');
    { Стиль Kept уже есть в чертеже (вставка/слияние) }
    Kept := Styles.AddStyle('Kept');
    Kept^.HorzCellMargin := 7;
    N := LoadTableStylesFromNOD(Model, Styles);
    CheckInt(1, N, 'synthetic: loaded styles (Good only)');
    CheckText(
      'Kept handle= xdict= 70=0 71=0 40=7 41=1.5 280=0 281=0 cells=0'#10 +
      'Good handle=21 xdict=26 70=1 71=2 40=0.25 41=0.5 280=1 281=0 cells=2'#10 +
      '  cell0 7=TS1 140=1.5 170=1 62=3 63=4 283=1'#10 +
      '  cell1 7=TS2 140=2.5 170=5 62=0 63=7 283=0'#10,
      DumpTableStyles(Styles), 'synthetic: styles');
    Check(Model.IsHandleClaimed($21) and Model.IsHandleClaimed($26) and
      Model.IsHandleClaimed($27), 'synthetic: 21, 26, 27 claimed');
    { Повтор имени и существующий стиль — объекты ветки всё равно забраны
      (не переносятся как неизвестные) }
    Check(Model.IsHandleClaimed($24) and Model.IsHandleClaimed($25),
      'synthetic: duplicate 24 and kept 25 claimed');
    Check(not Model.IsHandleClaimed($22) and not Model.IsHandleClaimed($23) and
      not Model.IsHandleClaimed($28),
      'synthetic: XRECORD 22, unnamed 23, xdict entry 28 not claimed');
    CheckInt(5, Model.ClaimedHandleCount, 'synthetic: claimed count (21, 24, 25, 26, 27)');
    CheckEquals('Good', Styles.GetStyleByHandle('21')^.Name, 'synthetic: GetStyleByHandle(21)');
    Check(Styles.GetStyleByHandle('24') = nil, 'synthetic: GetStyleByHandle(24) = nil');
    CheckInt(0, LoadTableStylesFromNOD(nil, Styles), 'LoadTableStylesFromNOD(nil)');
  finally
    Styles.done;
    Model.Free;
  end;
end;

{ ---------- 5. Standard по умолчанию ---------- }

procedure TestDefaultStandard;
const
  Files: array[0..1] of string = ('nod_load_r12.dxf', 'nod_no_tablestyle.dxf');
var
  I: Integer;
  Drawing: TSimpleDrawing;
  Styles: GDBDXFTableStyleArray;
  Expected: string;
begin
  { Значения Standard — как в TABLESTYLE Standard шаблона savetemplate2007 }
  Expected := ReadGolden('savetemplate2007.txt', True);
  for I := Low(Files) to High(Files) do begin
    Drawing.init(nil);
    try
      LoadDrawing(Root + NodDataDir + Files[I], Drawing);
      CheckText(Expected, DumpTableStyles(Drawing.DXFTableStyleTable, True),
        Files[I] + ': default Standard');
    finally
      DoneDrawing(Drawing);
    end;
  end;

  Styles.init(10);
  try
    Check(EnsureDefaultTableStyle(Styles) <> nil, 'EnsureDefaultTableStyle: empty table → Standard');
    Check(EnsureDefaultTableStyle(Styles) = nil, 'EnsureDefaultTableStyle: non-empty table → nil');
    CheckInt(1, Styles.Count, 'EnsureDefaultTableStyle: one style');
  finally
    Styles.done;
  end;
end;

{ ---------- 6. Ссылка 342 ---------- }

procedure TestStyleByHandle;
var
  Drawing: TSimpleDrawing;
  S: PTGDBDXFTableStyle;
begin
  Drawing.init(nil);
  try
    LoadDrawing(Root + TableFile, Drawing);
    CheckInt(3, Drawing.DXFTableStyleTable.Count, '+testtable: loaded styles');
    S := Drawing.DXFTableStyleTable.GetStyleByHandle('ba');
    Check(S <> nil, '+testtable: GetStyleByHandle(ba) found');
    if S <> nil then
      CheckEquals('vebtable', S^.Name, '+testtable: 342 BA');
    Check(Drawing.DXFTableStyleTable.GetStyleByHandle('') = nil, 'GetStyleByHandle('''') = nil');
  finally
    DoneDrawing(Drawing);
  end;
end;

{ ---------- 7. Сохранение и повторная загрузка ---------- }

{ Число записей CLASS с именем AName в секции CLASSES и число из них с
  группой 91 }
procedure CountClass(const P: TDXFPairs; const AName: string;
  out ACount, AWith91: Integer);
var
  S, E, I, J: Integer;
begin
  ACount := 0;
  AWith91 := 0;
  S := SectionStart(P, 'CLASSES');
  if S < 0 then
    Exit;
  E := SectionEnd(P, S);
  for I := S to E - 1 do
    if (P.Codes[I] = 0) and (P.Values[I] = 'CLASS') and
       (P.Codes[I + 1] = 1) and (P.Values[I + 1] = AName) then begin
      Inc(ACount);
      J := I + 1;
      while (J < E) and (P.Codes[J] <> 0) do begin
        if P.Codes[J] = 91 then begin
          Inc(AWith91);
          Break;
        end;
        Inc(J);
      end;
    end;
end;

function CountObjects(const P: TDXFPairs; const AType: string): Integer;
var
  S, E, I: Integer;
begin
  Result := 0;
  S := SectionStart(P, 'OBJECTS');
  if S < 0 then
    Exit;
  E := SectionEnd(P, S);
  for I := S to E do
    if (P.Codes[I] = 0) and (P.Values[I] = AType) then
      Inc(Result);
end;

procedure TestSaveReload;
const
  Templates: array[0..1] of string = ('savetemplate2000.dxf', 'savetemplate2007.dxf');
  Vers: array[0..1] of TZCDxfVersion = (ZCDxf2000, ZCDxf2007);
  With91: array[0..1] of Integer = (0, 1);
var
  I, Cnt, Cnt91, NStyles: Integer;
  Drawing: TSimpleDrawing;
  OutFile, Expected: string;
  P: TDXFPairs;
begin
  Expected := ReadGolden('tablestyleetalon.txt', True);
  for I := 0 to 1 do begin
    OutFile := GetTempDir(False) + 'nodstage5_etalon_' + Templates[I];
    Drawing.init(nil);
    try
      LoadDrawing(Root + EtalonFile, Drawing);
      NStyles := Drawing.DXFTableStyleTable.Count;
      if not savedxf20XX(OutFile, Root + TemplatesDir + Templates[I], Drawing, Vers[I]) then
        Fail('savedxf20XX failed for ' + OutFile);
    finally
      DoneDrawing(Drawing);
    end;

    P := ReadPairs(OutFile);
    CountClass(P, 'CELLSTYLEMAP', Cnt, Cnt91);
    CheckInt(1, Cnt, Templates[I] + ': CELLSTYLEMAP class written once');
    CheckInt(With91[I], Cnt91, Templates[I] + ': CELLSTYLEMAP class with group 91');
    CheckInt(NStyles, CountObjects(P, 'TABLESTYLE'), Templates[I] + ': TABLESTYLE objects');
    CheckInt(NStyles, CountObjects(P, 'CELLSTYLEMAP'), Templates[I] + ': CELLSTYLEMAP objects');

    Drawing.init(nil);
    try
      LoadDrawing(OutFile, Drawing);
      CheckText(Expected, DumpTableStyles(Drawing.DXFTableStyleTable, True),
        Templates[I] + ': reloaded styles equal etalon');
    finally
      DoneDrawing(Drawing);
    end;
  end;
end;

begin
  Root := '';
  if ParamCount > 0 then
    Root := IncludeTrailingPathDelimiter(ParamStr(1));
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Failed := 0;
  Run('TestHandlerRegistered', @TestHandlerRegistered);
  Run('TestLoaderGolden', @TestLoaderGolden);
  Run('TestClaimedHandles', @TestClaimedHandles);
  Run('TestSyntheticBranch', @TestSyntheticBranch);
  Run('TestDefaultStandard', @TestDefaultStandard);
  Run('TestStyleByHandle', @TestStyleByHandle);
  Run('TestSaveReload', @TestSaveReload);

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
