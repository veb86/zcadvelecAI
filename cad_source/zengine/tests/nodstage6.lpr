program nodstage6;

// issue #1454: этап 6 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (стили мультивыносок ACAD_MLEADERSTYLE через NOD-обработчик). Тест
// проверяет:
//
//  1. Обработчик ACAD_MLEADERSTYLE зарегистрирован: MinVersion AC1021,
//     все процедуры, XDataAppName ACAD_MLEADERVER, NeedsSymbolTables.
//  2. Стили образцов AutoCAD (cad_source/test/*mleader*.dxf), загруженные
//     в чертёж, совпадают с эталонами data/nod/stage6/*.txt; ссылки
//     340/341/342/343 переведены в имена записей LTYPE/BLOCK_RECORD/STYLE.
//  3. Забранные хэндлы: объекты MLEADERSTYLE (и их расширенные словари),
//     и только они (загрузка по модели OBJECTS + индекс TABLES).
//  4. Синтетическая ветка: запись не на MLEADERSTYLE, на несуществующий
//     объект и без имени пропускаются; существующий стиль не меняется;
//     повтор имени — берётся первый; неизвестные группы тела — в
//     ExtraPairs, ссылки — нет; xdata других приложений не переносятся;
//     ссылка 343 на запись не той таблицы — имя не разрешается.
//  5. Стиль Standard по умолчанию (EnsureDefaultsProc): для R12 и для
//     файлов без ACAD_MLEADERSTYLE — один Standard с параметрами стиля
//     Standard AutoCAD (+mleader2008.dxf, D8); сохранённый в DXF 2007 он
//     ссылается на LTYPE ByBlock и STYLE Standard (tablestyleetalon.dxf).
//  6. Round-trip: образец → сохранение DXF 2007 → загрузка даёт те же
//     стили (с точностью до хэндлов); класс MLEADERSTYLE записан один
//     раз (91 = числу стилей), APPID ACAD_MLEADERVER — один раз; ветка
//     в NOD, словарь-ветка принадлежит NOD; ссылки 340/341/342/343
//     указывают на записи нужных таблиц сохранённого файла.
//  7. DXF 2000 (MinVersion): ни ветки, ни объектов, ни класса, ни APPID;
//     пустой чертёж (без стилей) в DXF 2007 — ни ветки, ни класса (APPID
//     ACAD_MLEADERVER есть — регистрируется по версии, как у AutoCAD).
//
// Использование:
//   nodstage6 [<корень репозитория>] [--update]
//   --update — перезаписать эталоны data/nod/stage6/*.txt

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  gzctnrVectorTypes,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager, uzeffdxfsupport,
  uzgldrawcontext, uzeconsts, uzeTypes,
  uzeffdxfobjects, uzeffdxfnod, uzeffdxfnodregistry,
  uzestylesmleaderdxf, uzestylesmleaderdxfnod,
  // Регистрация загрузчика сущности ACAD_TABLE (как в nodstage0/4/5)
  uzeacadtable_types, uzeacadtable_model, uzeacadtable_dxf_write;

const
  TemplatesDir = 'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/';
  Stage6Dir = 'cad_source/zengine/tests/data/nod/stage6/';
  NodDataDir = 'cad_source/zengine/tests/data/nod/';
  Template2007 = 'savetemplate2007.dxf';
  Template2000 = 'savetemplate2000.dxf';
  { Чертёж AutoCAD 2007 без ACAD_MLEADERSTYLE (есть LTYPE ByBlock и STYLE
    Standard — ссылки стиля по умолчанию разрешаются при сохранении) }
  EtalonFile = 'cad_source/test/tablestyleetalon.dxf';

type
  TLoaderCase = record
    FileName: string;
    Golden: string;
  end;

const
  LoaderCases: array[0..3] of TLoaderCase = (
    (FileName: 'cad_source/test/+mleader2008.dxf'; Golden: '+mleader2008.txt'),
    (FileName: 'cad_source/test/mleaderblock.dxf'; Golden: 'mleaderblock.txt'),
    (FileName: 'cad_source/test/mleader2007notwork.dxf'; Golden: 'mleader2007notwork.txt'),
    (FileName: 'cad_source/test/mleader2000notwork.dxf'; Golden: 'mleader2000notwork.txt')
  );

  { Синтетическая секция TABLES: LTYPE 14 ByBlock, STYLE 11 Standard,
    BLOCK_RECORD 30 _Dot }
  SyntheticTables =
    '0'#10'SECTION'#10'2'#10'TABLES'#10 +
    '0'#10'TABLE'#10'2'#10'LTYPE'#10'5'#10'5'#10 +
    '0'#10'LTYPE'#10'5'#10'14'#10'330'#10'5'#10'100'#10'AcDbSymbolTableRecord'#10 +
    '100'#10'AcDbLinetypeTableRecord'#10'2'#10'ByBlock'#10'70'#10'0'#10 +
    '0'#10'ENDTAB'#10 +
    '0'#10'TABLE'#10'2'#10'STYLE'#10'5'#10'3'#10 +
    '0'#10'STYLE'#10'5'#10'11'#10'330'#10'3'#10'100'#10'AcDbSymbolTableRecord'#10 +
    '100'#10'AcDbTextStyleTableRecord'#10'2'#10'Standard'#10'70'#10'0'#10 +
    '0'#10'ENDTAB'#10 +
    '0'#10'TABLE'#10'2'#10'BLOCK_RECORD'#10'5'#10'1'#10 +
    '0'#10'BLOCK_RECORD'#10'5'#10'30'#10'330'#10'1'#10'100'#10'AcDbSymbolTableRecord'#10 +
    '100'#10'AcDbBlockTableRecord'#10'2'#10'_Dot'#10 +
    '0'#10'ENDTAB'#10 +
    '0'#10'ENDSEC'#10;

  { Синтетическая секция OBJECTS: NOD C, ACAD_MLEADERSTYLE → 20;
    словарь 20: Good → 21 (MLEADERSTYLE), NotStyle → 22 (XRECORD),
    Missing → 99 (нет объекта), '' → 23 (MLEADERSTYLE без имени),
    Good → 24 (повтор имени), Kept → 25 (MLEADERSTYLE). У 21 расширенный
    словарь 26 с записью XRECORD 28 (не переносится), неизвестная группа
    271, ссылка 390 (не переносится), xdata OTHERAPP (не переносится) и
    ACAD_MLEADERVER 1070 3; 343 указывает на STYLE 11 (не BLOCK_RECORD). }
  SyntheticObjects =
    '0'#10'SECTION'#10'2'#10'OBJECTS'#10 +
    '0'#10'DICTIONARY'#10'5'#10'C'#10'330'#10'0'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'ACAD_MLEADERSTYLE'#10'350'#10'20'#10 +
    '0'#10'DICTIONARY'#10'5'#10'20'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'Good'#10'350'#10'21'#10 +
    '3'#10'NotStyle'#10'350'#10'22'#10 +
    '3'#10'Missing'#10'350'#10'99'#10 +
    '3'#10#10'350'#10'23'#10 +
    '3'#10'Good'#10'350'#10'24'#10 +
    '3'#10'Kept'#10'350'#10'25'#10 +
    '0'#10'MLEADERSTYLE'#10'5'#10'21'#10 +
    '102'#10'{ACAD_XDICTIONARY'#10'360'#10'26'#10'102'#10'}'#10 +
    '102'#10'{ACAD_REACTORS'#10'330'#10'20'#10'102'#10'}'#10 +
    '330'#10'20'#10'100'#10'AcDbMLeaderStyle'#10 +
    '170'#10'1'#10'171'#10'3'#10'340'#10'14'#10'290'#10'0'#10'42'#10'1.5'#10 +
    '3'#10'  Good text'#10'341'#10'30'#10'44'#10'2.5'#10'300'#10'Note'#10 +
    '342'#10'11'#10'343'#10'11'#10'293'#10'0'#10 +
    '271'#10'7'#10'390'#10'99'#10 +
    '1001'#10'OTHERAPP'#10'1000'#10'x'#10 +
    '1001'#10'ACAD_MLEADERVER'#10'1070'#10'3'#10 +
    '0'#10'XRECORD'#10'5'#10'22'#10'330'#10'20'#10'100'#10'AcDbXrecord'#10 +
    '0'#10'MLEADERSTYLE'#10'5'#10'23'#10'330'#10'20'#10'100'#10'AcDbMLeaderStyle'#10 +
    '0'#10'MLEADERSTYLE'#10'5'#10'24'#10'330'#10'20'#10'100'#10'AcDbMLeaderStyle'#10'44'#10'9'#10 +
    '0'#10'MLEADERSTYLE'#10'5'#10'25'#10'330'#10'20'#10'100'#10'AcDbMLeaderStyle'#10'44'#10'9'#10 +
    '0'#10'DICTIONARY'#10'5'#10'26'#10'330'#10'21'#10'100'#10'AcDbDictionary'#10'280'#10'1'#10 +
    '3'#10'OTHER'#10'360'#10'28'#10 +
    '0'#10'XRECORD'#10'5'#10'28'#10'330'#10'26'#10'100'#10'AcDbXrecord'#10 +
    '0'#10'ENDSEC'#10;

var
  Root: string;
  Update: Boolean;
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

function B(V: Boolean): string;
begin
  Result := IntToStr(Ord(V));
end;

{ Ссылка на запись таблицы: <хэндл>:<имя>; ANamesOnly — только имя }
function Ref(const AHandle, AName: string; ANamesOnly: Boolean): string;
begin
  if ANamesOnly then
    Result := AName
  else
    Result := AHandle + ':' + AName;
end;

{ Текстовый дамп стилей (формат эталонов data/nod/stage6). ANamesOnly —
  без хэндлов (сравнение после пересохранения: хэндлы ссылок меняются,
  расширенный словарь стиля не переносится). }
function DumpMLeaderStyle(S: PTGDBDXFMLeaderStyle; ANamesOnly: Boolean): string;
var
  I: Integer;
  Extra: string;
begin
  Result := S^.Name;
  if not ANamesOnly then
    Result := Result + ' xdict=' + S^.XDictHandle;
  Result := Result + Format(' ver=%d', [S^.MLeaderVersion]) + #10;
  Result := Result + Format(
    '  leader 170=%d 171=%d 172=%d 173=%d 40=%s 41=%s 91=%d 92=%d 340=%s',
    [S^.LeaderLineType, S^.LeaderLineTypeId, S^.FirstSegAngleConstraint,
     S^.SecondSegAngleConstraint, F(S^.FirstSegAngle), F(S^.SecondSegAngle),
     S^.LeaderLineColor, S^.LeaderLineWeight,
     Ref(S^.LeaderLinetypeHandle, S^.LeaderLinetypeName, ANamesOnly)]) + #10;
  Result := Result + Format(
    '  landing 290=%s 42=%s 291=%s 43=%s 341=%s 45=%s',
    [B(S^.EnableDogleg), F(S^.DoglegLength), B(S^.EnableLanding),
     F(S^.LandingGap), Ref(S^.ArrowHeadBlockHandle, S^.ArrowHeadBlockName, ANamesOnly),
     F(S^.ArrowHeadSize)]) + #10;
  Result := Result + Format(
    '  text 90=%d 3="%s" 300="%s" 342=%s 44=%s 93=%d 174=%d 178=%d 175=%d 176=%d 177=%d 292=%s 297=%s 294=%s',
    [S^.ContentType, S^.Description, S^.DefaultTextContent,
     Ref(S^.TextStyleHandle, S^.TextStyleName, ANamesOnly), F(S^.TextHeight),
     S^.TextColor, S^.TextAttachmentLeft, S^.TextAttachmentRight,
     S^.TextAngleType, S^.TextAlignmentType, S^.TextAttachmentDirection,
     B(S^.TextAlignAlwaysLeft), B(S^.AlignSpace), B(S^.TextDirectionNegative)]) + #10;
  Result := Result + Format(
    '  block 46=%s 343=%s 94=%d 47=%s 49=%s 142=%s 143=%s 295=%s 296=%s',
    [F(S^.BlockContentScale),
     Ref(S^.BlockContentHandle, S^.BlockContentName, ANamesOnly),
     S^.BlockContentColor, F(S^.BlockContentScaleX), F(S^.BlockContentScaleY),
     F(S^.BlockContentScaleZ), F(S^.BlockContentRotation),
     B(S^.IsBlockContent), B(S^.IsMTextContent)]) + #10;
  Result := Result + Format('  scale 140=%s 293=%s 141=%s',
    [F(S^.OverallScale), B(S^.Annotative), F(S^.BreakGapSize)]) + #10;
  if Length(S^.ExtraPairs) > 0 then begin
    Extra := '  extra';
    for I := 0 to High(S^.ExtraPairs) do
      Extra := Extra + Format(' %d="%s"', [S^.ExtraPairs[I].Code, S^.ExtraPairs[I].Value]);
    Result := Result + Extra + #10;
  end;
end;

function DumpMLeaderStyles(var T: GDBDXFMLeaderStyleArray;
  ANamesOnly: Boolean = False): string;
var
  S: PTGDBDXFMLeaderStyle;
  It: itrec;
begin
  Result := '';
  S := T.beginiterate(It);
  while S <> nil do begin
    Result := Result + DumpMLeaderStyle(S, ANamesOnly);
    S := T.iterate(It);
  end;
end;

{ Эталон дампа }
function ReadGolden(const AName: string): string;
var
  L: TStringList;
  I: Integer;
begin
  Result := '';
  L := TStringList.Create;
  try
    L.LoadFromFile(Root + Stage6Dir + AName);
    for I := 0 to L.Count - 1 do
      Result := Result + TrimRight(L[I]) + #10;
  finally
    L.Free;
  end;
end;

{ --update: записывает эталон }
procedure WriteGolden(const AName, AText: string);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.LineBreak := #10;
    L.Text := AText;
    L.SaveToFile(Root + Stage6Dir + AName);
    writeln('updated: ', Stage6Dir + AName);
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

{ Текст секции AName файла ('' — секции нет) }
function SectionText(const P: TDXFPairs; const AName: string): string;
var
  S: Integer;
begin
  Result := '';
  S := SectionStart(P, AName);
  if S >= 0 then
    Result := PairsText(P, S - 1, SectionEnd(P, S));
end;

{ Модель секции OBJECTS файла с индексом символьных таблиц TABLES
  (образцы — DXF 2007+, имена в UTF-8, кроме mleader2000notwork, где
  имена ASCII) }
function ModelOfFile(const AFileName: string): TZNODModel;
var
  P: TDXFPairs;
begin
  P := ReadPairs(AFileName);
  Result := TZNODModel.Create;
  if not Result.LoadFromText(SectionText(P, 'OBJECTS')) then
    Fail(AFileName + ': OBJECTS parse error: ' + Result.ParseError);
  if not Result.LoadSymbolTablesFromText(SectionText(P, 'TABLES')) then
    Fail(AFileName + ': TABLES parse error');
end;

function HandleOf(const AStr: string): TDWGHandle;
begin
  if not TryDXFStrToHandle(AStr, Result) then
    Result := 0;
end;

{ Первое значение группы ACode объекта (с позиции AFrom до следующей
  группы 0); '' — нет }
function GroupValue(const P: TDXFPairs; AFrom, ACode: Integer): string;
var
  I: Integer;
begin
  Result := '';
  I := AFrom + 1;
  while (I < P.Count) and (P.Codes[I] <> 0) do begin
    if P.Codes[I] = ACode then
      Exit(P.Values[I]);
    Inc(I);
  end;
end;

{ ---------- 1. Регистрация ---------- }

procedure TestHandlerRegistered;
var
  H: TZNODHandler;
begin
  Check(FindNODHandler(CNODMLeaderStyleKey, H), 'handler ' + CNODMLeaderStyleKey + ' is registered');
  CheckEquals('MLEADERSTYLE', H.ObjectType, 'handler: ObjectType');
  CheckEquals('AC1021', ACDWGVerName(H.MinVersion), 'handler: MinVersion');
  CheckEquals('Standard', H.DefaultName, 'handler: DefaultName');
  CheckEquals('ACAD_MLEADERVER', H.XDataAppName, 'handler: XDataAppName');
  Check(H.NeedsSymbolTables, 'handler: NeedsSymbolTables');
  Check(Assigned(H.LoadProc), 'handler: LoadProc');
  Check(Assigned(H.ReserveHandlesProc), 'handler: ReserveHandlesProc');
  Check(Assigned(H.SaveProc), 'handler: SaveProc');
  Check(Assigned(H.ClassesProc), 'handler: ClassesProc');
  Check(Assigned(H.FindObjectHandleProc), 'handler: FindObjectHandleProc');
  Check(Assigned(H.EnsureDefaultsProc), 'handler: EnsureDefaultsProc');
end;

{ ---------- 2. Загрузка образцов и эталоны ---------- }

procedure TestLoaderGolden;
var
  I: Integer;
  Drawing: TSimpleDrawing;
  Dump: string;
begin
  for I := Low(LoaderCases) to High(LoaderCases) do begin
    Drawing.init(nil);
    try
      LoadDrawing(Root + LoaderCases[I].FileName, Drawing);
      Dump := DumpMLeaderStyles(Drawing.DXFMLeaderStyleTable);
      if Update then
        WriteGolden(LoaderCases[I].Golden, Dump);
      CheckText(ReadGolden(LoaderCases[I].Golden), Dump,
        LoaderCases[I].Golden + ': styles equal golden');
      Check(Drawing.GetDXFMLeaderStyleTable = @Drawing.DXFMLeaderStyleTable,
        LoaderCases[I].Golden + ': GetDXFMLeaderStyleTable');
    finally
      DoneDrawing(Drawing);
    end;
  end;
end;

{ Имена записей, на которые ссылаются стили +mleader2008 и mleaderblock }
procedure TestResolvedNames;
var
  Drawing: TSimpleDrawing;

  procedure CheckStyle(const AStyle, ALType, AArrow, ATextStyle, ABlock: string);
  var
    S: PTGDBDXFMLeaderStyle;
  begin
    S := Drawing.DXFMLeaderStyleTable.getAddres(AStyle);
    Check(S <> nil, AStyle + ': loaded');
    if S = nil then
      Exit;
    CheckEquals(ALType, S^.LeaderLinetypeName, AStyle + ': 340 LTYPE');
    CheckEquals(AArrow, S^.ArrowHeadBlockName, AStyle + ': 341 BLOCK_RECORD');
    CheckEquals(ATextStyle, S^.TextStyleName, AStyle + ': 342 STYLE');
    CheckEquals(ABlock, S^.BlockContentName, AStyle + ': 343 BLOCK_RECORD');
  end;

begin
  Drawing.init(nil);
  try
    LoadDrawing(Root + LoaderCases[0].FileName, Drawing);
    CheckInt(5, Drawing.DXFMLeaderStyleTable.Count, '+mleader2008: styles');
    CheckStyle('ai', 'ByBlock', '_Dot', 'Standard', '');
    CheckStyle('Annotative', 'ByBlock', '', 'Standard', '');
    CheckStyle('spline', 'ByBlock', '_BoxBlank', 'Standard', '_TagSlot');
    CheckStyle('Standard', 'ByBlock', '', 'Standard', '');
    CheckStyle('veb', 'ByBlock', '', 'Standard', '');
  finally
    DoneDrawing(Drawing);
  end;
  Drawing.init(nil);
  try
    LoadDrawing(Root + LoaderCases[1].FileName, Drawing);
    CheckInt(2, Drawing.DXFMLeaderStyleTable.Count, 'mleaderblock: styles');
    CheckStyle('Standard', 'ByBlock', '', 'Standard', '_DetailCallout');
  finally
    DoneDrawing(Drawing);
  end;
end;

{ ---------- 3. Забранные хэндлы ---------- }

procedure TestClaimedHandles;
var
  I, J, Expected: Integer;
  Model: TZNODModel;
  Styles: GDBDXFMLeaderStyleArray;
  Dict: TZDXFDictionary;
  Obj: TZDXFRawObject;
  N: Integer;
begin
  for I := Low(LoaderCases) to High(LoaderCases) do begin
    Model := ModelOfFile(Root + LoaderCases[I].FileName);
    Styles.init(10);
    try
      Check(Model.SymbolRecordCount > 0, LoaderCases[I].Golden + ': TABLES indexed');
      Check(NODLoadNeedsSymbolTables(Model),
        LoaderCases[I].Golden + ': NODLoadNeedsSymbolTables');
      N := LoadMLeaderStylesFromNOD(Model, Styles);
      CheckInt(Styles.Count, N, LoaderCases[I].Golden + ': loaded count');
      { Та же загрузка, что и в чертеже (эталон с разрешёнными именами) }
      CheckText(ReadGolden(LoaderCases[I].Golden), DumpMLeaderStyles(Styles),
        LoaderCases[I].Golden + ': model load equals golden');
      Dict := Model.ResolveDictionary(CNODMLeaderStyleKey);
      Check(Dict <> nil, LoaderCases[I].Golden + ': branch found');
      if Dict = nil then
        Continue;
      Expected := 0;
      for J := 0 to Dict.Count - 1 do begin
        Obj := Model.FindObject(Dict[J].TargetHandle);
        if (Obj = nil) or not SameText(Obj.ObjType, 'MLEADERSTYLE') then
          Continue;
        Inc(Expected);
        Check(Model.IsHandleClaimed(Obj.Handle),
          Format('%s: MLEADERSTYLE %s claimed', [LoaderCases[I].Golden, Obj.HandleStr]));
        if (Obj.XDictHandle <> 0) and (Model.FindDictionary(Obj.XDictHandle) <> nil) then begin
          Inc(Expected);
          Check(Model.IsHandleClaimed(Obj.XDictHandle),
            Format('%s: xdict %s claimed', [LoaderCases[I].Golden, DXFHandleToStr(Obj.XDictHandle)]));
        end;
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
  Styles: GDBDXFMLeaderStyleArray;
  Kept: PTGDBDXFMLeaderStyle;
  N: Integer;
begin
  Model := TZNODModel.Create;
  Styles.init(10);
  try
    Check(Model.LoadFromText(SyntheticObjects), 'synthetic: OBJECTS parsed');
    Check(Model.LoadSymbolTablesFromText(SyntheticTables), 'synthetic: TABLES parsed');
    CheckInt(3, Model.SymbolRecordCount, 'synthetic: symbol records');
    { Стиль Kept уже есть в чертеже (вставка/слияние) }
    Kept := Styles.AddStyle('Kept');
    Kept^.TextHeight := 7;
    N := LoadMLeaderStylesFromNOD(Model, Styles);
    CheckInt(1, N, 'synthetic: loaded styles (Good only)');
    CheckText(
      'Kept xdict= ver=2'#10 +
      '  leader 170=2 171=1 172=0 173=1 40=0 41=0 91=-1056964608 92=-2 340=:'#10 +
      '  landing 290=1 42=0.09 291=1 43=0.36 341=: 45=0.18'#10 +
      '  text 90=2 3="" 300="" 342=: 44=7 93=-1056964608 174=1 178=1 175=1 176=0 177=0 292=0 297=0 294=1'#10 +
      '  block 46=0.18 343=: 94=-1056964608 47=1 49=1 142=1 143=0.125 295=0 296=0'#10 +
      '  scale 140=1 293=1 141=0'#10 +
      'Good xdict=26 ver=3'#10 +
      '  leader 170=1 171=3 172=0 173=1 40=0 41=0 91=-1056964608 92=-2 340=14:ByBlock'#10 +
      '  landing 290=0 42=1.5 291=1 43=0.36 341=30:_Dot 45=0.18'#10 +
      '  text 90=2 3="  Good text" 300="Note" 342=11:Standard 44=2.5 93=-1056964608 174=1 178=1 175=1 176=0 177=0 292=0 297=0 294=1'#10 +
      '  block 46=0.18 343=11: 94=-1056964608 47=1 49=1 142=1 143=0.125 295=0 296=0'#10 +
      '  scale 140=1 293=0 141=0'#10 +
      '  extra 271="7"'#10,
      DumpMLeaderStyles(Styles), 'synthetic: styles');
    Check(Model.IsHandleClaimed($21) and Model.IsHandleClaimed($26),
      'synthetic: 21 and its xdict 26 claimed');
    { Повтор имени и существующий стиль — объекты ветки всё равно забраны
      (не переносятся как неизвестные) }
    Check(Model.IsHandleClaimed($24) and Model.IsHandleClaimed($25),
      'synthetic: duplicate 24 and kept 25 claimed');
    Check(not Model.IsHandleClaimed($22) and not Model.IsHandleClaimed($23) and
      not Model.IsHandleClaimed($28),
      'synthetic: XRECORD 22, unnamed 23, xdict entry 28 not claimed');
    CheckInt(4, Model.ClaimedHandleCount, 'synthetic: claimed count (21, 24, 25, 26)');
    CheckInt(0, LoadMLeaderStylesFromNOD(nil, Styles), 'LoadMLeaderStylesFromNOD(nil)');
  finally
    Styles.done;
    Model.Free;
  end;
end;

{ ---------- 5. Standard по умолчанию ---------- }

{ Стиль Standard образца AutoCAD (+mleader2008, D8) — дамп без хэндлов }
function AutoCADStandardDump: string;
var
  Drawing: TSimpleDrawing;
  S: PTGDBDXFMLeaderStyle;
begin
  Drawing.init(nil);
  try
    LoadDrawing(Root + LoaderCases[0].FileName, Drawing);
    S := Drawing.DXFMLeaderStyleTable.getAddres('Standard');
    if S = nil then
      raise Exception.Create('+mleader2008: no Standard');
    Result := DumpMLeaderStyle(S, True);
  finally
    DoneDrawing(Drawing);
  end;
end;

procedure TestDefaultStandard;
const
  Files: array[0..3] of string = (NodDataDir + 'nod_load_r12.dxf',
    NodDataDir + 'nod_no_tablestyle.dxf', NodDataDir + 'nod_load_no_objects.dxf',
    EtalonFile);
var
  I: Integer;
  Drawing: TSimpleDrawing;
  Styles: GDBDXFMLeaderStyleArray;
  Expected: string;
begin
  Expected := AutoCADStandardDump;
  for I := Low(Files) to High(Files) do begin
    Drawing.init(nil);
    try
      LoadDrawing(Root + Files[I], Drawing);
      CheckText(Expected, DumpMLeaderStyles(Drawing.DXFMLeaderStyleTable, True),
        ExtractFileName(Files[I]) + ': default Standard equals AutoCAD Standard');
    finally
      DoneDrawing(Drawing);
    end;
  end;

  Styles.init(10);
  try
    Check(EnsureDefaultMLeaderStyle(Styles) <> nil, 'EnsureDefaultMLeaderStyle: empty table → Standard');
    Check(EnsureDefaultMLeaderStyle(Styles) = nil, 'EnsureDefaultMLeaderStyle: non-empty table → nil');
    CheckInt(1, Styles.Count, 'EnsureDefaultMLeaderStyle: one style');
  finally
    Styles.done;
  end;
end;

{ ---------- 6. Round-trip DXF 2007 ---------- }

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

{ Число записей CLASS с именем AName и значение 91 последней ('' — нет) }
procedure CountClass(const P: TDXFPairs; const AName: string;
  out ACount: Integer; out A91: string);
var
  S, E, I: Integer;
begin
  ACount := 0;
  A91 := '';
  S := SectionStart(P, 'CLASSES');
  if S < 0 then
    Exit;
  E := SectionEnd(P, S);
  for I := S to E - 1 do
    if (P.Codes[I] = 0) and (P.Values[I] = 'CLASS') and
       (P.Codes[I + 1] = 1) and (P.Values[I + 1] = AName) then begin
      Inc(ACount);
      A91 := GroupValue(P, I, 91);
    end;
end;

{ Число записей APPID с именем AName }
function CountAppId(const P: TDXFPairs; const AName: string): Integer;
var
  S, E, I: Integer;
begin
  Result := 0;
  S := SectionStart(P, 'TABLES');
  if S < 0 then
    Exit;
  E := SectionEnd(P, S);
  for I := S to E - 1 do
    if (P.Codes[I] = 0) and (P.Values[I] = 'APPID') and
       SameText(GroupValue(P, I, 2), AName) then
      Inc(Result);
end;

{ Проверки структуры сохранённого файла: ветка в NOD, владельцы,
  ссылки 340/341/342/343 — на записи нужных таблиц }
procedure CheckSavedBranch(const AFileName, ACase: string; AStyles: Integer);
const
  RefCodes: array[0..3] of Integer = (340, 341, 342, 343);
  RefTables: array[0..3] of string = ('LTYPE', 'BLOCK_RECORD', 'STYLE', 'BLOCK_RECORD');
var
  P: TDXFPairs;
  Model: TZNODModel;
  Nod, Dict: TZDXFDictionary;
  Obj: TZDXFRawObject;
  I, J, K, Cnt: Integer;
  H: TDWGHandle;
  Name, V91: string;
begin
  P := ReadPairs(AFileName);
  CheckInt(AStyles, CountObjects(P, 'MLEADERSTYLE'), ACase + ': MLEADERSTYLE objects');
  CountClass(P, 'MLEADERSTYLE', Cnt, V91);
  CheckInt(1, Cnt, ACase + ': MLEADERSTYLE class written once');
  CheckEquals(IntToStr(AStyles), V91, ACase + ': MLEADERSTYLE class 91');
  CheckInt(1, CountAppId(P, 'ACAD_MLEADERVER'), ACase + ': APPID ACAD_MLEADERVER');

  Model := ModelOfFile(AFileName);
  try
    Nod := Model.NOD;
    Check(Nod <> nil, ACase + ': NOD found');
    Dict := Model.ResolveDictionary(CNODMLeaderStyleKey);
    Check(Dict <> nil, ACase + ': ACAD_MLEADERSTYLE in NOD');
    if (Nod = nil) or (Dict = nil) then
      Exit;
    CheckEquals(DXFHandleToStr(Nod.Handle), DXFHandleToStr(Dict.OwnerHandle),
      ACase + ': branch owner is NOD');
    CheckInt(AStyles, Dict.Count, ACase + ': branch entries');
    for I := 0 to Dict.Count - 1 do begin
      Obj := Model.FindObject(Dict[I].TargetHandle);
      Check((Obj <> nil) and SameText(Obj.ObjType, 'MLEADERSTYLE'),
        Format('%s: entry "%s" → MLEADERSTYLE', [ACase, Dict[I].Key]));
      if Obj = nil then
        Continue;
      CheckEquals(DXFHandleToStr(Dict.Handle), DXFHandleToStr(Obj.OwnerHandle),
        Format('%s: "%s" owner', [ACase, Dict[I].Key]));
      CheckEquals('ACAD_MLEADERVER', Obj.ValueOf(1001),
        Format('%s: "%s" xdata app', [ACase, Dict[I].Key]));
      for J := 0 to Obj.PairCount - 1 do
        for K := Low(RefCodes) to High(RefCodes) do
          if Obj.Pairs[J].Code = RefCodes[K] then begin
            if not TryDXFStrToHandle(Obj.Pairs[J].Value, H) then
              H := 0;
            Name := '';
            Check(Model.FindSymbolName(H, RefTables[K], Name),
              Format('%s: "%s" %d/%s → %s "%s"', [ACase, Dict[I].Key,
                RefCodes[K], Obj.Pairs[J].Value, RefTables[K], Name]));
          end;
    end;
  finally
    Model.Free;
  end;
end;

procedure TestRoundTrip2007;
var
  I: Integer;
  Drawing: TSimpleDrawing;
  OutFile, Expected: string;
  NStyles: Integer;
begin
  for I := Low(LoaderCases) to High(LoaderCases) do begin
    OutFile := GetTempDir(False) + 'nodstage6_' + ChangeFileExt(LoaderCases[I].Golden, '.dxf');
    Drawing.init(nil);
    try
      LoadDrawing(Root + LoaderCases[I].FileName, Drawing);
      NStyles := Drawing.DXFMLeaderStyleTable.Count;
      Expected := DumpMLeaderStyles(Drawing.DXFMLeaderStyleTable, True);
      if not savedxf20XX(OutFile, Root + TemplatesDir + Template2007, Drawing, ZCDxf2007) then
        Fail('savedxf20XX failed for ' + OutFile);
    finally
      DoneDrawing(Drawing);
    end;

    CheckSavedBranch(OutFile, LoaderCases[I].Golden, NStyles);

    Drawing.init(nil);
    try
      LoadDrawing(OutFile, Drawing);
      CheckText(Expected, DumpMLeaderStyles(Drawing.DXFMLeaderStyleTable, True),
        LoaderCases[I].Golden + ': reloaded styles equal loaded');
    finally
      DoneDrawing(Drawing);
    end;
  end;
end;

{ Стиль по умолчанию (файл без ветки) сохраняется со ссылками на ByBlock
  и Standard сохранённого файла }
procedure TestDefaultSaved;
var
  Drawing: TSimpleDrawing;
  OutFile: string;
begin
  OutFile := GetTempDir(False) + 'nodstage6_default_2007.dxf';
  Drawing.init(nil);
  try
    LoadDrawing(Root + EtalonFile, Drawing);
    if not savedxf20XX(OutFile, Root + TemplatesDir + Template2007, Drawing, ZCDxf2007) then
      Fail('savedxf20XX failed for ' + OutFile);
  finally
    DoneDrawing(Drawing);
  end;
  CheckSavedBranch(OutFile, 'default', 1);
end;

{ ---------- 7. DXF 2000 и пустой чертёж ---------- }

{ AAppIds — ожидаемое число записей APPID ACAD_MLEADERVER: приложение
  регистрируется для всех файлов версии обработчика (как у AutoCAD), даже
  без стилей }
procedure CheckNoBranch(const AFileName, ACase: string; AAppIds: Integer);
var
  P: TDXFPairs;
  Cnt: Integer;
  V91: string;
  Model: TZNODModel;
begin
  P := ReadPairs(AFileName);
  CheckInt(0, CountObjects(P, 'MLEADERSTYLE'), ACase + ': no MLEADERSTYLE objects');
  CountClass(P, 'MLEADERSTYLE', Cnt, V91);
  CheckInt(0, Cnt, ACase + ': no MLEADERSTYLE class');
  CheckInt(AAppIds, CountAppId(P, 'ACAD_MLEADERVER'), ACase + ': APPID ACAD_MLEADERVER');
  Model := ModelOfFile(AFileName);
  try
    Check(Model.NOD <> nil, ACase + ': NOD found');
    Check(Model.ResolveDictionary(CNODMLeaderStyleKey) = nil,
      ACase + ': no ACAD_MLEADERSTYLE in NOD');
    { Индекс TABLES при загрузке такого файла не строится }
    Check(not NODLoadNeedsSymbolTables(Model), ACase + ': not NODLoadNeedsSymbolTables');
  finally
    Model.Free;
  end;
end;

procedure TestNoBranch;
var
  Drawing: TSimpleDrawing;
  OutFile: string;
begin
  OutFile := GetTempDir(False) + 'nodstage6_mleader2008_2000.dxf';
  Drawing.init(nil);
  try
    LoadDrawing(Root + LoaderCases[0].FileName, Drawing);
    if not savedxf20XX(OutFile, Root + TemplatesDir + Template2000, Drawing, ZCDxf2000) then
      Fail('savedxf20XX failed for ' + OutFile);
  finally
    DoneDrawing(Drawing);
  end;
  CheckNoBranch(OutFile, '+mleader2008 → 2000', 0);

  OutFile := GetTempDir(False) + 'nodstage6_empty_2007.dxf';
  Drawing.init(nil);
  try
    CheckInt(0, Drawing.DXFMLeaderStyleTable.Count, 'empty drawing: no styles');
    if not savedxf20XX(OutFile, Root + TemplatesDir + Template2007, Drawing, ZCDxf2007) then
      Fail('savedxf20XX failed for ' + OutFile);
  finally
    DoneDrawing(Drawing);
  end;
  CheckNoBranch(OutFile, 'empty → 2007', 1);
end;

var
  I: Integer;
begin
  Root := '';
  Update := False;
  for I := 1 to ParamCount do
    if ParamStr(I) = '--update' then
      Update := True
    else
      Root := IncludeTrailingPathDelimiter(ParamStr(I));
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Failed := 0;
  Run('TestHandlerRegistered', @TestHandlerRegistered);
  Run('TestLoaderGolden', @TestLoaderGolden);
  Run('TestResolvedNames', @TestResolvedNames);
  Run('TestClaimedHandles', @TestClaimedHandles);
  Run('TestSyntheticBranch', @TestSyntheticBranch);
  Run('TestDefaultStandard', @TestDefaultStandard);
  Run('TestRoundTrip2007', @TestRoundTrip2007);
  Run('TestDefaultSaved', @TestDefaultSaved);
  Run('TestNoBranch', @TestNoBranch);

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
