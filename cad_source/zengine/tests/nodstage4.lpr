program nodstage4;

// issue #1450: этап 4 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (запись веток NOD через реестр, адаптер ACAD_TABLESTYLE). Тест проверяет:
//
//  1. Адаптер ACAD_TABLESTYLE зарегистрирован в initialization uzeffdxfout
//     (MinVersion AC1015, DefaultName 'Standard', процедуры записи заданы).
//  2. Без стилей таблиц вывод побайтно совпадает с эталонами этапа 0
//     (empty, tablestyleetalon; шаблоны 2000 и 2007): ветка
//     ACAD_TABLESTYLE шаблона копируется как есть.
//  3. Со стилями таблиц (Standard, ZCAD1442):
//     - DXF 2007 совпадает с эталоном этапа 0 (data/nod/stage0) с точностью
//       до хэндлов и порядка объектов OBJECTS (каноническая форма: хэндлы
//       переименованы обходом от NOD, объекты отсортированы);
//     - DXF 2000 (в шаблоне ключа нет): запись ACAD_TABLESTYLE вставлена в
//       NOD по алфавиту, словарь-ветка со стилями, класс TABLESTYLE без
//       группы 91;
//     - в обеих версиях: одна запись ACAD_TABLESTYLE в NOD, владелец
//       словаря — NOD, в файле только стили чертежа (TABLESTYLE шаблона
//       пропущен), у каждого стиля — расширенный словарь с CELLSTYLEMAP,
//       хэндлы уникальны и меньше $HANDSEED, неразрешённые ссылки — те
//       же, что в эталоне этапа 0.
//  4. Ссылка 342 сущности ACAD_TABLE (+testtable.dxf) ведёт на TABLESTYLE
//     ветки.
//  5. Сохранённый файл загружается и сохраняется повторно.
//  6. TZNODSaveSession на синтетических шаблонах: множество пропускаемых
//     объектов ветки (с расширенными словарями и их записями), перемапка
//     словаря-ветки и внешних ссылок на объекты ветки (по имени через
//     FindObjectHandleProc, иначе DefaultName), вставка ключей в NOD по
//     алфавиту и однократно, шаблон без NOD, повторяющиеся имена стилей.
//  7. Трасса этапа 4 при включённом логе форматируется без ошибок.
//
// Использование:
//   nodstage4 [<корень репозитория>]

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager, uzeffdxfsupport,
  uzgldrawcontext, uzeconsts, uzeTypes, uzctnrVectorBytesStream,
  uzeffdxfobjects, uzeffdxfnod, uzeffdxfnodregistry, usimplegenerics,
  uzbLogTypes, uzclog, uzeffdxfnodlog,
  uzestylestablesdxf,
  // Регистрация загрузчика сущности ACAD_TABLE (как в nodstage0) и запись
  // CELLSTYLEMAP стилей таблиц.
  uzeacadtable_types, uzeacadtable_model, uzeacadtable_dxf_write;

const
  TemplatesDir = 'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/';
  GoldenDir = 'cad_source/zengine/tests/data/nod/golden/';
  Stage0Dir = 'cad_source/zengine/tests/data/nod/stage0/';
  EtalonFile = 'cad_source/test/tablestyleetalon.dxf';
  { Чертёж с сущностями ACAD_TABLE (стиль Standard) }
  TableFile = 'cad_source/test/+testtable.dxf';

  TableStyleKey = 'ACAD_TABLESTYLE';
  CellStyleMapKey = 'ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP';

  { Переменные HEADER, зависящие от времени сохранения (как в nodstage0) }
  VolatileHeaderVars: array[0..3] of string = (
    '$TDCREATE', '$TDUCREATE', '$TDUPDATE', '$TDUUPDATE');

  { Синтетический шаблон с веткой ACAD_TABLESTYLE:
    NOD C: ACAD_GROUP → D, ACAD_TABLESTYLE → 20, ZZZ → 30;
    словарь 20: Standard → 21, Other → 22; у 21 расширенный словарь 23 с
    CELLSTYLEMAP 24. Вне ветки на неё ссылаются XRECORD 30 (340 → 22,
    «Other»), DICTIONARYVAR 31 (340 → 21, «Standard») и XRECORD 32
    (340 → 24, запись расширенного словаря — нет стиля с таким именем). }
  TemplateWithBranch =
    '0'#10'SECTION'#10'2'#10'OBJECTS'#10 +
    '0'#10'DICTIONARY'#10'5'#10'C'#10'330'#10'0'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'ACAD_GROUP'#10'350'#10'D'#10 +
    '3'#10'ACAD_TABLESTYLE'#10'350'#10'20'#10 +
    '3'#10'ZZZ'#10'350'#10'30'#10 +
    '0'#10'DICTIONARY'#10'5'#10'D'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '0'#10'DICTIONARY'#10'5'#10'20'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'Standard'#10'350'#10'21'#10'3'#10'Other'#10'350'#10'22'#10 +
    '0'#10'TABLESTYLE'#10'5'#10'21'#10'102'#10'{ACAD_XDICTIONARY'#10'360'#10'23'#10'102'#10'}'#10 +
    '330'#10'20'#10'100'#10'AcDbTableStyle'#10 +
    '0'#10'TABLESTYLE'#10'5'#10'22'#10'330'#10'20'#10'100'#10'AcDbTableStyle'#10 +
    '0'#10'DICTIONARY'#10'5'#10'23'#10'330'#10'21'#10'100'#10'AcDbDictionary'#10'280'#10'1'#10 +
    '3'#10 + CellStyleMapKey + #10'360'#10'24'#10 +
    '0'#10'CELLSTYLEMAP'#10'5'#10'24'#10'330'#10'23'#10'100'#10'AcDbCellStyleMap'#10 +
    '0'#10'XRECORD'#10'5'#10'30'#10'330'#10'C'#10'100'#10'AcDbXrecord'#10'340'#10'22'#10 +
    '0'#10'DICTIONARYVAR'#10'5'#10'31'#10'330'#10'C'#10'100'#10'DictionaryVariables'#10'340'#10'21'#10 +
    '0'#10'XRECORD'#10'5'#10'32'#10'330'#10'C'#10'100'#10'AcDbXrecord'#10'340'#10'24'#10 +
    '0'#10'ENDSEC'#10;

  { Синтетический шаблон без ключа ACAD_TABLESTYLE:
    NOD C: ACAD_GROUP → D, ACAD_MLINESTYLE → E, ZZZ → F. }
  TemplateWithoutBranch =
    '0'#10'SECTION'#10'2'#10'OBJECTS'#10 +
    '0'#10'DICTIONARY'#10'5'#10'C'#10'330'#10'0'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'ACAD_GROUP'#10'350'#10'D'#10 +
    '3'#10'ACAD_MLINESTYLE'#10'350'#10'E'#10 +
    '3'#10'ZZZ'#10'350'#10'F'#10 +
    '0'#10'DICTIONARY'#10'5'#10'D'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10 +
    '0'#10'DICTIONARY'#10'5'#10'E'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10 +
    '0'#10'DICTIONARY'#10'5'#10'F'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10 +
    '0'#10'ENDSEC'#10;

  { Секция OBJECTS без NOD (у словаря есть владелец) }
  TemplateWithoutNOD =
    '0'#10'SECTION'#10'2'#10'OBJECTS'#10 +
    '0'#10'DICTIONARY'#10'5'#10'D'#10'330'#10'1F'#10'100'#10'AcDbDictionary'#10 +
    '0'#10'ENDSEC'#10;

var
  Root: string;
  Failed: Integer;
  { Хэндлы, выделенные тестовыми обработчиками (п. 6) }
  TestReserveHandle: TDWGHandle;

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

type
  TTestProc = procedure;

{ Запускает проверку; исключение считается провалом проверки. После
  проверки из реестра снимаются все обработчики, кроме адаптера. }
procedure Run(const AName: string; AProc: TTestProc);
var
  I: Integer;
begin
  writeln('--- ', AName);
  try
    AProc();
  except
    on E: Exception do
      Fail(AName + ': ' + E.ClassName + ': ' + E.Message);
  end;
  for I := NODHandlerCount - 1 downto 0 do
    if not SameText(GetNODHandler(I).Key, TableStyleKey) then
      UnregisterNODHandler(GetNODHandler(I).Key);
  Flush(Output);
end;

{ ---------- Вспомогательные ---------- }

{ Добавляет стиль с тремя форматами ячеек (как nodstage0). }
procedure AddSampleStyle(var ATable: GDBDXFTableStyleArray;
  const AName: string; AHeight: Double);
var
  Style: PTGDBDXFTableStyle;
  Cell: TGDBDXFTableCellStyle;
  I: Integer;
begin
  Style := ATable.AddStyle(AName);
  if Style = nil then begin
    Fail('AddStyle("' + AName + '") returned nil');
    Exit;
  end;
  for I := 0 to 2 do begin
    FillChar(Cell, SizeOf(Cell), 0);
    Cell.TextHeight := AHeight * (I + 1);
    Cell.Alignment := 5;
    Cell.TextColor := 256;
    Cell.BackgroundColor := 257;
    Cell.BackgroundColorEnabled := False;
    Style^.CellFormats.PushBackData(Cell);
    Style^.CellTextStyleName[I] := 'Standard';
  end;
end;

{ Стили как в эталоне этапа 0 tablestyles_* }
procedure AddStage0Styles(var ADrawing: TSimpleDrawing);
begin
  AddSampleStyle(ADrawing.DXFTableStyleTable, 'Standard', 2.5);
  AddSampleStyle(ADrawing.DXFTableStyleTable, 'ZCAD1442', 3.5);
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

{ Значение $HANDSEED (savedxf20XX пишет handle + $100000000). }
function HandSeed(const P: TDXFPairs): TDWGHandle;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to P.Count - 3 do
    if (P.Codes[I] = 9) and (P.Values[I] = '$HANDSEED') then begin
      TryDXFStrToHandle(P.Values[I + 1], Result);
      if Result >= $100000000 then
        Dec(Result, $100000000);
      Exit;
    end;
end;

procedure NormalizeDXF(AText: TStringList);
var
  I, J: Integer;
begin
  I := 0;
  while I < AText.Count - 3 do begin
    if (Trim(AText[I]) = '9') then
      for J := Low(VolatileHeaderVars) to High(VolatileHeaderVars) do
        if Trim(AText[I + 1]) = VolatileHeaderVars[J] then begin
          AText[I + 3] := '<time>';
          Break;
        end;
    Inc(I);
  end;
end;

{ Сравнивает файл с эталоном (построчно, время нормализовано) }
procedure CheckGolden(const AOutFile, AGolden: string);
var
  Actual, Expected: TStringList;
  I: Integer;
begin
  Actual := TStringList.Create;
  Expected := TStringList.Create;
  try
    Actual.LoadFromFile(AOutFile);
    NormalizeDXF(Actual);
    Expected.LoadFromFile(Root + AGolden);
    for I := 0 to Actual.Count - 1 do
      if (I >= Expected.Count) or (Actual[I] <> Expected[I]) then begin
        if I < Expected.Count then
          Fail(Format('%s: line %d: expected "%s", got "%s" (actual output: %s)',
            [AGolden, I + 1, Expected[I], Actual[I], AOutFile]))
        else
          Fail(Format('%s: output is longer than golden (%d > %d lines, actual output: %s)',
            [AGolden, Actual.Count, Expected.Count, AOutFile]));
        Exit;
      end;
    if Actual.Count <> Expected.Count then
      Fail(Format('%s: output is shorter than golden (%d < %d lines, actual output: %s)',
        [AGolden, Actual.Count, Expected.Count, AOutFile]))
    else
      Ok(Format('%s: output matches golden %s (%d lines)',
        [ExtractFileName(AOutFile), AGolden, Actual.Count]));
  finally
    Actual.Free;
    Expected.Free;
  end;
end;

type
  TDrawingSource = (dsEmpty, dsStyles, dsEtalon, dsTableStyles);

{ Готовит чертёж и сохраняет по шаблону. }
procedure SaveSample(ASource: TDrawingSource; const ATemplate: string;
  AVer: TZCDxfVersion; const AOutFile: string);
var
  Drawing: TSimpleDrawing;
begin
  Drawing.init(nil);
  try
    if ASource = dsEtalon then
      LoadDrawing(Root + EtalonFile, Drawing)
    else if ASource = dsTableStyles then
      LoadDrawing(Root + TableFile, Drawing);
    if ASource = dsTableStyles then
      AddSampleStyle(Drawing.DXFTableStyleTable, 'Standard', 2.5)
    else if ASource = dsStyles then
      AddStage0Styles(Drawing);
    if not savedxf20XX(AOutFile, Root + TemplatesDir + ATemplate, Drawing, AVer) then
      Fail('savedxf20XX failed for ' + AOutFile);
  finally
    DoneDrawing(Drawing);
  end;
end;

function OutPath(const AName: string): string;
begin
  Result := GetTempDir(False) + 'nodstage4_' + AName;
end;

{ ---------- Каноническая форма DXF ----------
  Та же, что experiments/issue1450/dxfcanon.py: хэндлы вне OBJECTS
  именуются в порядке определения, объекты OBJECTS — обходом от NOD
  (сначала расширенный словарь, затем записи словарей по ключу и 360),
  остальные — в порядке определения; объекты OBJECTS сортируются по
  каноническому имени; $HANDSEED и $TD* нормализуются. }

function IsRefCode(ACode: Integer): Boolean;
begin
  Result := (ACode = 320) or ((ACode >= 330) and (ACode <= 361)) or
    (ACode = 390) or (ACode = 391) or (ACode = 480) or (ACode = 481) or
    (ACode = 1005);
end;

function CompareStrItems(AList: TStringList; A, B: Integer): Integer;
begin
  Result := CompareStr(AList[A], AList[B]);
end;

type
  TDXFCanon = class
  private
    P: TDXFPairs;
    Names: TStringList;
    SecNames: array of string;
    { Первая пара после 2/<имя> и индекс 0/ENDSEC }
    SecFrom, SecTo: array of Integer;
    ObjFrom, ObjTo: array of Integer;
    procedure AddName(const AHandle: string);
    function NameOf(const AHandle: string): string;
    function ObjHandle(AIndex: Integer): string;
    procedure SubPairs(AFrom, ATo: Integer; AOut: TStringList);
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    procedure Build(AOut: TStringList);
  end;

constructor TDXFCanon.Create(const AFileName: string);
begin
  P := ReadPairs(AFileName);
  Names := TStringList.Create;
  Names.Sorted := True;
  Names.CaseSensitive := True;
end;

destructor TDXFCanon.Destroy;
begin
  Names.Free;
  inherited;
end;

procedure TDXFCanon.AddName(const AHandle: string);
var
  N: Integer;
begin
  if (AHandle = '') or (Names.IndexOf(UpperCase(AHandle)) >= 0) then
    Exit;
  N := Names.Count;
  Names.AddObject(UpperCase(AHandle), TObject(PtrInt(N)));
end;

function TDXFCanon.NameOf(const AHandle: string): string;
var
  I: Integer;
begin
  I := Names.IndexOf(UpperCase(AHandle));
  if I < 0 then
    Result := '?' + AHandle
  else
    Result := Format('H%.4d', [PtrInt(Names.Objects[I])]);
end;

function TDXFCanon.ObjHandle(AIndex: Integer): string;
var
  I: Integer;
begin
  for I := ObjFrom[AIndex] to ObjTo[AIndex] do
    if (P.Codes[I] = 5) or (P.Codes[I] = 105) then
      Exit(UpperCase(P.Values[I]));
  Result := '';
end;

procedure TDXFCanon.SubPairs(AFrom, ATo: Integer; AOut: TStringList);
var
  I: Integer;
  V: string;
begin
  for I := AFrom to ATo do begin
    V := P.Values[I];
    if (P.Codes[I] = 5) or (P.Codes[I] = 105) or IsRefCode(P.Codes[I]) then
      V := NameOf(V);
    AOut.Add(IntToStr(P.Codes[I]) + '|' + V);
  end;
end;

procedure TDXFCanon.Build(AOut: TStringList);
var
  I, J, S, N, ObjSec: Integer;
  ByHandle, Queue, Kids, Order: TStringList;
  H, Key, V: string;
  InX: Boolean;
begin
  { Секции }
  N := 0;
  I := 0;
  while I < P.Count - 1 do begin
    if (P.Codes[I] = 0) and (P.Values[I] = 'SECTION') then begin
      SetLength(SecNames, N + 1);
      SetLength(SecFrom, N + 1);
      SetLength(SecTo, N + 1);
      SecNames[N] := P.Values[I + 1];
      SecFrom[N] := I + 2;
      SecTo[N] := SectionEnd(P, I + 2);
      I := SecTo[N];
      Inc(N);
    end;
    Inc(I);
  end;
  ObjSec := -1;
  for S := 0 to N - 1 do
    if SecNames[S] = 'OBJECTS' then
      ObjSec := S
    else if SecNames[S] <> 'HEADER' then
      for I := SecFrom[S] to SecTo[S] - 1 do
        if (P.Codes[I] = 5) or (P.Codes[I] = 105) then
          AddName(P.Values[I]);
  { Объекты OBJECTS }
  SetLength(ObjFrom, 0);
  SetLength(ObjTo, 0);
  if ObjSec >= 0 then
    for I := SecFrom[ObjSec] to SecTo[ObjSec] - 1 do
      if P.Codes[I] = 0 then begin
        J := Length(ObjFrom);
        SetLength(ObjFrom, J + 1);
        SetLength(ObjTo, J + 1);
        ObjFrom[J] := I;
        ObjTo[J] := I;
        if J > 0 then
          ObjTo[J - 1] := I - 1;
      end;
  if Length(ObjTo) > 0 then
    ObjTo[High(ObjTo)] := SecTo[ObjSec] - 1;
  ByHandle := TStringList.Create;
  Queue := TStringList.Create;
  Kids := TStringList.Create;
  Order := TStringList.Create;
  try
    ByHandle.Sorted := True;
    ByHandle.CaseSensitive := True;
    for J := 0 to High(ObjFrom) do
      if (ObjHandle(J) <> '') and (ByHandle.IndexOf(ObjHandle(J)) < 0) then
        ByHandle.AddObject(ObjHandle(J), TObject(PtrInt(J)));
    if Length(ObjFrom) > 0 then
      Queue.Add(ObjHandle(0));
    while Queue.Count > 0 do begin
      H := Queue[0];
      Queue.Delete(0);
      if (H = '') or (Names.IndexOf(H) >= 0) then
        Continue;
      AddName(H);
      S := ByHandle.IndexOf(H);
      if S < 0 then
        Continue;
      J := PtrInt(ByHandle.Objects[S]);
      Kids.Clear;
      Key := #0;
      InX := False;
      for I := ObjFrom[J] to ObjTo[J] do
        case P.Codes[I] of
          102: InX := P.Values[I] = '{ACAD_XDICTIONARY';
          3: Key := P.Values[I];
          350, 360:
            if (Key <> #0) or InX or (P.Codes[I] = 360) then begin
              if Key = #0 then
                Key := '';
              Kids.Add(Format('%d'#1'%s'#1'%s',
                [Ord(not InX), Key, UpperCase(P.Values[I])]));
              Key := #0;
            end;
        end;
      Kids.CustomSort(@CompareStrItems);
      for I := 0 to Kids.Count - 1 do begin
        V := Kids[I];
        Queue.Add(Copy(V, LastDelimiter(#1, V) + 1, MaxInt));
      end;
    end;
    for J := 0 to High(ObjFrom) do
      AddName(ObjHandle(J));
    { Вывод }
    for S := 0 to N - 1 do begin
      AOut.Add('SECTION ' + SecNames[S]);
      if SecNames[S] = 'HEADER' then begin
        Key := '';
        for I := SecFrom[S] to SecTo[S] - 1 do begin
          V := P.Values[I];
          if P.Codes[I] = 9 then
            Key := V
          else if (Key = '$HANDSEED') or (Copy(Key, 1, 3) = '$TD') then
            V := '<norm>'
          else if IsRefCode(P.Codes[I]) then
            V := NameOf(V);
          AOut.Add(IntToStr(P.Codes[I]) + '|' + V);
        end;
      end else if S = ObjSec then begin
        Order.Clear;
        for J := 0 to High(ObjFrom) do
          if ObjHandle(J) <> '' then
            Order.AddObject(NameOf(ObjHandle(J)), TObject(PtrInt(J)))
          else
            Order.AddObject('~', TObject(PtrInt(J)));
        Order.CustomSort(@CompareStrItems);
        for I := 0 to Order.Count - 1 do begin
          J := PtrInt(Order.Objects[I]);
          SubPairs(ObjFrom[J], ObjTo[J], AOut);
        end;
      end else
        SubPairs(SecFrom[S], SecTo[S] - 1, AOut);
    end;
  finally
    ByHandle.Free;
    Queue.Free;
    Kids.Free;
    Order.Free;
  end;
end;

procedure CanonDXF(const AFileName: string; AOut: TStringList);
var
  C: TDXFCanon;
begin
  C := TDXFCanon.Create(AFileName);
  try
    C.Build(AOut);
  finally
    C.Free;
  end;
end;

{ Сравнивает канонические формы двух файлов }
procedure CheckCanonEqual(const AActualFile, AExpectedFile, S: string);
var
  A, E: TStringList;
  I: Integer;
begin
  A := TStringList.Create;
  E := TStringList.Create;
  try
    CanonDXF(AActualFile, A);
    CanonDXF(AExpectedFile, E);
    for I := 0 to A.Count - 1 do
      if (I >= E.Count) or (A[I] <> E[I]) then begin
        if I < E.Count then
          Fail(Format('%s: canonical line %d: expected "%s", got "%s"',
            [S, I + 1, E[I], A[I]]))
        else
          Fail(Format('%s: canonical form is longer (%d > %d)', [S, A.Count, E.Count]));
        Exit;
      end;
    if A.Count <> E.Count then
      Fail(Format('%s: canonical form is shorter (%d < %d)', [S, A.Count, E.Count]))
    else
      Ok(Format('%s: equal up to handles and order of OBJECTS (%d canonical lines)',
        [S, A.Count]));
  finally
    A.Free;
    E.Free;
  end;
end;

{ Неразрешённые ссылки файла вне HEADER: 'код:хэндл' через пробел }
function UnresolvedRefs(const AFileName: string): string;
var
  P: TDXFPairs;
  Defs, Bad: TStringList;
  I: Integer;
  InHeader: Boolean;
begin
  P := ReadPairs(AFileName);
  Defs := TStringList.Create;
  Bad := TStringList.Create;
  try
    Defs.Sorted := True;
    Defs.Duplicates := dupIgnore;
    Bad.Sorted := True;
    Bad.Duplicates := dupIgnore;
    InHeader := False;
    for I := 0 to P.Count - 1 do begin
      if (P.Codes[I] = 0) and (P.Values[I] = 'SECTION') and (I + 1 < P.Count) then
        InHeader := P.Values[I + 1] = 'HEADER';
      if not InHeader and ((P.Codes[I] = 5) or (P.Codes[I] = 105)) then
        Defs.Add(UpperCase(P.Values[I]));
    end;
    InHeader := False;
    for I := 0 to P.Count - 1 do begin
      if (P.Codes[I] = 0) and (P.Values[I] = 'SECTION') and (I + 1 < P.Count) then
        InHeader := P.Values[I + 1] = 'HEADER';
      if not InHeader and IsRefCode(P.Codes[I]) and
         (P.Values[I] <> '0') and (P.Values[I] <> '') and
         (Defs.IndexOf(UpperCase(P.Values[I])) < 0) then
        Bad.Add(Format('%d:%s', [P.Codes[I], UpperCase(P.Values[I])]));
    end;
    Bad.Sorted := False;
    Bad.Delimiter := ' ';
    Result := Bad.DelimitedText;
  finally
    Defs.Free;
    Bad.Free;
  end;
end;

{ ---------- 1. Адаптер зарегистрирован ---------- }

procedure TestAdapterRegistered;
var
  H: TZNODHandler;
begin
  Check(FindNODHandler(TableStyleKey, H), 'adapter ' + TableStyleKey + ' is registered');
  CheckEquals('TABLESTYLE', H.ObjectType, 'adapter: ObjectType');
  CheckEquals('AC1015', ACDWGVerName(H.MinVersion), 'adapter: MinVersion (DXF 2000)');
  CheckEquals('Standard', H.DefaultName, 'adapter: DefaultName');
  Check(Assigned(H.ReserveHandlesProc) and Assigned(H.SaveProc) and
    Assigned(H.ClassesProc) and Assigned(H.FindObjectHandleProc),
    'adapter: ReserveHandlesProc, SaveProc, ClassesProc, FindObjectHandleProc');
  Check(not Assigned(H.LoadProc), 'adapter: LoadProc is nil (reading is stage 5)');
end;

{ ---------- 2. Без стилей — эталоны этапа 0 ---------- }

type
  TGoldenCase = record
    Source: TDrawingSource;
    Template: string;
    Ver: TZCDxfVersion;
    Golden: string;
  end;

const
  NoStyleCases: array[0..3] of TGoldenCase = (
    (Source: dsEmpty;  Template: 'savetemplate2000.dxf'; Ver: ZCDxf2000; Golden: 'empty_2000.dxf'),
    (Source: dsEmpty;  Template: 'savetemplate2007.dxf'; Ver: ZCDxf2007; Golden: 'empty_2007.dxf'),
    (Source: dsEtalon; Template: 'savetemplate2000.dxf'; Ver: ZCDxf2000; Golden: 'tablestyleetalon_2000.dxf'),
    (Source: dsEtalon; Template: 'savetemplate2007.dxf'; Ver: ZCDxf2007; Golden: 'tablestyleetalon_2007.dxf')
  );

procedure TestNoStylesGolden;
var
  I: Integer;
  C: TGoldenCase;
  OutFile: string;
begin
  for I := Low(NoStyleCases) to High(NoStyleCases) do begin
    C := NoStyleCases[I];
    OutFile := OutPath('nostyles_' + C.Golden);
    SaveSample(C.Source, C.Template, C.Ver, OutFile);
    CheckGolden(OutFile, GoldenDir + C.Golden);
  end;
end;

{ ---------- 3. Со стилями ---------- }

{ Структура ветки ACAD_TABLESTYLE в сохранённом файле }
procedure CheckTableStyleBranch(const AOutFile, AName, AExpectedStyles: string;
  ATemplateHasClass: Boolean; const AExpectedUnresolved: string);
var
  P: TDXFPairs;
  Model: TZNODModel;
  Dict, XDict: TZDXFDictionary;
  Entry: TZDXFDictEntry;
  Obj: TZDXFRawObject;
  I, KeyCount, StyleObjects, S, E, ClassCount, Group91: Integer;
  Keys, Styles: string;
  H, Seed, MaxHandle: TDWGHandle;
  SortedKeys, StylesOk: Boolean;
begin
  P := ReadPairs(AOutFile);
  Model := ObjectsModelOfFile(AOutFile);
  try
    Check(Model.NOD <> nil, AName + ': NOD found');
    if Model.NOD = nil then
      Exit;
    CheckInt(0, Model.DuplicateHandleCount, AName + ': duplicate handles in OBJECTS');
    KeyCount := 0;
    SortedKeys := True;
    Keys := '';
    for I := 0 to Model.NOD.Count - 1 do begin
      if SameText(Model.NOD[I].Key, TableStyleKey) then
        Inc(KeyCount);
      if (I > 0) and (CompareText(Model.NOD[I - 1].Key, Model.NOD[I].Key) > 0) then
        SortedKeys := False;
      Keys := Keys + ' ' + Model.NOD[I].Key;
    end;
    CheckInt(1, KeyCount, AName + ': ' + TableStyleKey + ' entries in NOD');
    Check(SortedKeys, AName + ': NOD keys are sorted:' + Keys);
    Dict := Model.ResolveDictionary(TableStyleKey);
    Check(Dict <> nil, AName + ': ' + TableStyleKey + ' refers to a dictionary');
    if Dict = nil then
      Exit;
    Check(Dict.OwnerHandle = Model.NOD.Handle, AName + ': dictionary owner is NOD');
    Styles := '';
    StylesOk := True;
    for I := 0 to Dict.Count - 1 do begin
      if Styles <> '' then
        Styles := Styles + ',';
      Styles := Styles + Dict[I].Key;
      Obj := Model.FindObject(Dict[I].TargetHandle);
      if (Obj = nil) or (Obj.ObjType <> 'TABLESTYLE') or
         (Obj.OwnerHandle <> Dict.Handle) or (Obj.XDictHandle = 0) then begin
        StylesOk := False;
        Continue;
      end;
      XDict := Model.FindDictionary(Obj.XDictHandle);
      if (XDict = nil) or (XDict.OwnerHandle <> Obj.Handle) or
         not XDict.FindEntry(CellStyleMapKey, Entry) or
         (Model.FindObject(Entry.TargetHandle) = nil) or
         (Model.FindObject(Entry.TargetHandle).ObjType <> 'CELLSTYLEMAP') then
        StylesOk := False;
    end;
    CheckEquals(AExpectedStyles, Styles, AName + ': dictionary entries');
    Check(StylesOk, AName + ': entries are TABLESTYLE owned by the dictionary, '
      + 'with extension dictionary and CELLSTYLEMAP');
    StyleObjects := 0;
    for I := 0 to Model.Objects.Count - 1 do
      if Model.Objects[I].ObjType = 'TABLESTYLE' then
        Inc(StyleObjects);
    CheckInt(Dict.Count, StyleObjects,
      AName + ': TABLESTYLE objects (template TABLESTYLE is skipped)');
  finally
    Model.Free;
  end;

  { Хэндлы меньше $HANDSEED }
  Seed := HandSeed(P);
  MaxHandle := 0;
  S := SectionStart(P, 'HEADER');
  E := SectionEnd(P, S);
  for I := E to P.Count - 1 do
    if ((P.Codes[I] = 5) or (P.Codes[I] = 105)) and
       TryDXFStrToHandle(P.Values[I], H) and (H > MaxHandle) then
      MaxHandle := H;
  Check((MaxHandle > 0) and (MaxHandle < Seed),
    Format('%s: handles < $HANDSEED (max %s, $HANDSEED %s)',
      [AName, DXFHandleToStr(MaxHandle), DXFHandleToStr(Seed)]));
  CheckEquals(AExpectedUnresolved, UnresolvedRefs(AOutFile),
    AName + ': unresolved references (as in stage 0 golden)');

  { Класс TABLESTYLE: ровно один; в DXF 2000 — без группы 91 }
  S := SectionStart(P, 'CLASSES');
  E := SectionEnd(P, S);
  ClassCount := 0;
  Group91 := 0;
  for I := S to E do
    if (P.Codes[I] = 1) and (P.Values[I] = 'TABLESTYLE') and
       (P.Codes[I - 1] = 0) and (P.Values[I - 1] = 'CLASS') then begin
      Inc(ClassCount);
      H := I + 1;
      while (H < TDWGHandle(E)) and (P.Codes[H] <> 0) do begin
        if P.Codes[H] = 91 then
          Inc(Group91);
        Inc(H);
      end;
    end;
  CheckInt(1, ClassCount, AName + ': TABLESTYLE classes in CLASSES');
  if not ATemplateHasClass then
    CheckInt(0, Group91, AName + ': TABLESTYLE class has no group 91 (DXF 2000)');
end;

procedure TestStylesGolden;
var
  OutFile: string;
begin
  { 2007: эквивалентно выводу до этапа 4 }
  OutFile := OutPath('tablestyles_2007.dxf');
  SaveSample(dsStyles, 'savetemplate2007.dxf', ZCDxf2007, OutFile);
  CheckGolden(OutFile, GoldenDir + 'tablestyles_2007.dxf');
  CheckCanonEqual(OutFile, Root + Stage0Dir + 'tablestyles_2007.dxf',
    'tablestyles_2007 vs stage 0 golden');
  CheckTableStyleBranch(OutFile, 'tablestyles_2007', 'Standard,ZCAD1442', True,
    UnresolvedRefs(Root + Stage0Dir + 'tablestyles_2007.dxf'));

  { 2000: ветки в шаблоне нет — ключ добавлен в NOD }
  OutFile := OutPath('tablestyles_2000.dxf');
  SaveSample(dsStyles, 'savetemplate2000.dxf', ZCDxf2000, OutFile);
  CheckGolden(OutFile, GoldenDir + 'tablestyles_2000.dxf');
  CheckTableStyleBranch(OutFile, 'tablestyles_2000', 'Standard,ZCAD1442', False,
    UnresolvedRefs(Root + Stage0Dir + 'tablestyles_2000.dxf'));
end;

{ ---------- 4. Ссылка 342 ACAD_TABLE ---------- }

procedure CheckTableEntityRef(const AOutFile, AName: string);
var
  P: TDXFPairs;
  Model: TZNODModel;
  Dict: TZDXFDictionary;
  Entry: TZDXFDictEntry;
  S, E, I, J, Tables, Good: Integer;
  H: TDWGHandle;
begin
  P := ReadPairs(AOutFile);
  Model := ObjectsModelOfFile(AOutFile);
  try
    Dict := Model.ResolveDictionary(TableStyleKey);
    Check((Dict <> nil) and Dict.FindEntry('Standard', Entry),
      AName + ': ' + TableStyleKey + '/Standard exists');
    if Dict = nil then
      Exit;
    S := SectionStart(P, 'ENTITIES');
    E := SectionEnd(P, S);
    Tables := 0;
    Good := 0;
    for I := S to E do
      if (P.Codes[I] = 0) and (P.Values[I] = 'ACAD_TABLE') then begin
        Inc(Tables);
        J := I + 1;
        while (J < E) and (P.Codes[J] <> 0) do begin
          if (P.Codes[J] = 342) and TryDXFStrToHandle(P.Values[J], H) and
             (H = Entry.TargetHandle) then begin
            Inc(Good);
            Break;
          end;
          Inc(J);
        end;
      end;
    Check(Tables > 0, Format('%s: ACAD_TABLE entities: %d', [AName, Tables]));
    CheckInt(Tables, Good, AName + ': ACAD_TABLE with 342 → '
      + TableStyleKey + '/Standard (' + DXFHandleToStr(Entry.TargetHandle) + ')');
  finally
    Model.Free;
  end;
end;

procedure TestTableEntityRef;
var
  OutFile: string;
begin
  { Стиль таблицы (в исходном файле — хэндл BA) определяется по стилям
    чертежа — Standard; 342 ACAD_TABLE ведёт на Standard ветки, и
    неразрешённых ссылок нет (без стилей в чертеже 342 остаётся BA —
    хэндлом исходного файла, которого в сохранённом нет). }
  OutFile := OutPath('testtable_2007.dxf');
  SaveSample(dsTableStyles, 'savetemplate2007.dxf', ZCDxf2007, OutFile);
  CheckTableEntityRef(OutFile, '+testtable + Standard, 2007');
  CheckTableStyleBranch(OutFile, '+testtable + Standard, 2007', 'Standard', True, '');

  OutFile := OutPath('testtable_2000.dxf');
  SaveSample(dsTableStyles, 'savetemplate2000.dxf', ZCDxf2000, OutFile);
  CheckTableEntityRef(OutFile, '+testtable + Standard, 2000');
  CheckTableStyleBranch(OutFile, '+testtable + Standard, 2000', 'Standard', False, '');
end;

{ ---------- 5. Повторная загрузка ---------- }

procedure TestReload;
const
  Templates: array[0..1] of string = ('savetemplate2000.dxf', 'savetemplate2007.dxf');
  Vers: array[0..1] of TZCDxfVersion = (ZCDxf2000, ZCDxf2007);
var
  I: Integer;
  First, Second: string;
  Drawing: TSimpleDrawing;
  Count1: Integer;
begin
  for I := 0 to 1 do begin
    First := OutPath('reload_1_' + Templates[I]);
    Second := OutPath('reload_2_' + Templates[I]);
    SaveSample(dsTableStyles, Templates[I], Vers[I], First);
    Drawing.init(nil);
    try
      LoadDrawing(First, Drawing);
      Count1 := Drawing.pObjRoot^.ObjArray.Count;
      Check(Count1 > 0, Format('%s: reloaded, %d entities', [Templates[I], Count1]));
      AddSampleStyle(Drawing.DXFTableStyleTable, 'Standard', 2.5);
      Check(savedxf20XX(Second, Root + TemplatesDir + Templates[I], Drawing, Vers[I]),
        Templates[I] + ': saved again');
    finally
      DoneDrawing(Drawing);
    end;
    CheckTableEntityRef(Second, 'reload ' + Templates[I]);
    CheckCanonEqual(Second, First, 'reload ' + Templates[I] + ': second save vs first');
  end;
end;

{ ---------- 6. TZNODSaveSession на синтетических шаблонах ---------- }

function AdapterIndex(ASession: TZNODSaveSession): Integer;
var
  I: Integer;
begin
  for I := 0 to ASession.Count - 1 do
    if SameText(ASession.Handlers[I].Key, TableStyleKey) then
      Exit(I);
  Result := -1;
end;

function MapText(AMap: TMapHandleToHandle; const AHandles: array of TDWGHandle): string;
var
  I: Integer;
  V: TDWGHandle;
begin
  Result := '';
  for I := Low(AHandles) to High(AHandles) do begin
    if Result <> '' then
      Result := Result + ' ';
    if AMap.TryGetValue(AHandles[I], V) then
      Result := Result + DXFHandleToStr(AHandles[I]) + '>' + DXFHandleToStr(V)
    else
      Result := Result + DXFHandleToStr(AHandles[I]) + '>-';
  end;
end;

function EntriesText(const AEntries: TZDXFDictEntries): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AEntries) do begin
    if Result <> '' then
      Result := Result + ' ';
    Result := Result + AEntries[I].Key + '>' + DXFHandleToStr(AEntries[I].TargetHandle)
      + ':' + IntToStr(AEntries[I].OwnershipCode);
  end;
end;

procedure TestSessionBranch;
var
  Session: TZNODSaveSession;
  Drawing: TSimpleDrawing;
  Ctx: TIODXFSaveContext;
  Map: TMapHandleToHandle;
  K, I: Integer;
  Refs: string;
begin
  Drawing.init(nil);
  Ctx.InitRec;
  Map := TMapHandleToHandle.Create;
  Session := TZNODSaveSession.Create(AC1021);
  try
    AddSampleStyle(Drawing.DXFTableStyleTable, 'Standard', 2.5);
    AddSampleStyle(Drawing.DXFTableStyleTable, 'Other', 3.5);
    K := AdapterIndex(Session);
    Check(K >= 0, 'session: adapter takes part in DXF 2007');
    Check(Session.LoadTemplateText(TemplateWithBranch), 'branch template: NOD found');
    CheckEquals('20', DXFHandleToStr(Session.TemplateDictHandles[K]),
      'branch template: ACAD_TABLESTYLE dictionary');
    Refs := '';
    for I := 0 to High(Session.TemplateRefs) do
      Refs := Refs + Format(' %s:"%s"', [DXFHandleToStr(Session.TemplateRefs[I].Handle),
        Session.TemplateRefs[I].Name]);
    CheckEquals(' 20:"" 22:"Other" 21:"Standard" 24:"' + CellStyleMapKey + '"', Refs,
      'branch template: references to branch objects from outside');
    { До ReserveHandles ничего не пропускается и не перемапливается }
    Check(not Session.IsTemplateHandleSkipped($21) and not Session.HasTemplateSkips,
      'before ReserveHandles: nothing is skipped');
    Session.MapTemplateHandles(Map, Ctx);
    CheckInt(0, Map.Count, 'before ReserveHandles: MapTemplateHandles does nothing');

    Ctx.handle := $100;
    Session.ReserveHandles(Drawing, Ctx);
    CheckEquals('100', DXFHandleToStr(Session.DictHandles[K]), 'adapter dictionary handle');
    { Словарь 100, Standard 101 (+102 xdict, 103 map), Other 104 (..106) }
    CheckEquals('107', DXFHandleToStr(Ctx.handle), 'handles allocated by adapter');
    Check(Session.HasTemplateSkips, 'HasTemplateSkips');
    for I := $20 to $24 do
      Check(Session.IsTemplateHandleSkipped(I),
        'skipped: branch object ' + DXFHandleToStr(I));
    Check(not Session.IsTemplateHandleSkipped($C) and
      not Session.IsTemplateHandleSkipped($D) and
      not Session.IsTemplateHandleSkipped($30) and
      not Session.IsTemplateHandleSkipped($31) and
      not Session.IsTemplateHandleSkipped($32),
      'not skipped: NOD and objects outside the branch');

    Session.MapTemplateHandles(Map, Ctx);
    CheckEquals('20>100 21>101 22>104 23>- 24>101',
      MapText(Map, [$20, $21, $22, $23, $24]),
      'MapTemplateHandles: dictionary, by name, DefaultName for CELLSTYLEMAP');
    { Идемпотентность: повторный вызов не меняет карту }
    Map.AddOrSetValue($21, $999);
    Session.MapTemplateHandles(Map, Ctx);
    CheckEquals('999', DXFHandleToStr(Map.MyGetValue($21)), 'MapTemplateHandles is idempotent');
    CheckEquals('', EntriesText(Session.TakeNODInsertions('')),
      'TakeNODInsertions: key is in template NOD — nothing to insert');
  finally
    Session.Free;
    Map.Free;
    Ctx.Done;
    DoneDrawing(Drawing);
  end;

  { Без стилей ветка шаблона копируется как есть }
  Drawing.init(nil);
  Ctx.InitRec;
  Map := TMapHandleToHandle.Create;
  Session := TZNODSaveSession.Create(AC1021);
  try
    K := AdapterIndex(Session);
    Session.LoadTemplateText(TemplateWithBranch);
    Ctx.handle := $100;
    Session.ReserveHandles(Drawing, Ctx);
    Session.MapTemplateHandles(Map, Ctx);
    Check((Session.DictHandles[K] = 0) and (Ctx.handle = $100),
      'no styles: adapter allocates nothing');
    Check(not Session.HasTemplateSkips and not Session.IsTemplateHandleSkipped($21),
      'no styles: template branch is not skipped');
    CheckInt(0, Map.Count, 'no styles: nothing is remapped');
  finally
    Session.Free;
    Map.Free;
    Ctx.Done;
    DoneDrawing(Drawing);
  end;
end;

function TestReserveA(var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext): TDWGHandle;
begin
  Result := AIODXFContext.handle;
  Inc(AIODXFContext.handle);
  TestReserveHandle := Result;
end;

function TestReserveZ(var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext): TDWGHandle;
begin
  Result := AIODXFContext.handle;
  Inc(AIODXFContext.handle);
end;

function TestReserveNone(var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext): TDWGHandle;
begin
  Result := 0;
end;

procedure TestSessionInsertions;
var
  Session: TZNODSaveSession;
  Drawing: TSimpleDrawing;
  Ctx: TIODXFSaveContext;
  Map: TMapHandleToHandle;
begin
  { Порядок регистрации: адаптер, затем ZZZZ_TEST, ACAD_A_TEST, ACAD_NONE }
  RegisterNODHandler('ZZZZ_TEST', 'X', nil, @TestReserveZ, nil);
  RegisterNODHandler('ACAD_A_TEST', 'X', nil, @TestReserveA, nil);
  RegisterNODHandler('ACAD_NONE', 'X', nil, @TestReserveNone, nil);

  Drawing.init(nil);
  AddStage0Styles(Drawing);
  Ctx.InitRec;
  Map := TMapHandleToHandle.Create;
  Session := TZNODSaveSession.Create(AC1021);
  try
    Check(Session.LoadTemplateText(TemplateWithoutBranch), 'template without branch: NOD found');
    Ctx.handle := $100;
    Session.ReserveHandles(Drawing, Ctx);
    Session.MapTemplateHandles(Map, Ctx);
    CheckInt(0, Map.Count, 'template without branch: nothing is remapped');
    Check(not Session.HasTemplateSkips, 'template without branch: nothing is skipped');
    { Адаптер: 100 (словарь) + 2×3; ZZZZ_TEST: 107; ACAD_A_TEST: 108 }
    CheckEquals('ACAD_A_TEST>108:350', EntriesText(Session.TakeNODInsertions('ACAD_GROUP')),
      'insertions before ACAD_GROUP');
    CheckEquals('', EntriesText(Session.TakeNODInsertions('ACAD_GROUP')),
      'insertions are taken once');
    CheckEquals('', EntriesText(Session.TakeNODInsertions('acad_mlinestyle')),
      'insertions before ACAD_MLINESTYLE (case-insensitive)');
    CheckEquals('ACAD_TABLESTYLE>100:350', EntriesText(Session.TakeNODInsertions('ZZZ')),
      'insertions before ZZZ');
    CheckEquals('ZZZZ_TEST>107:350', EntriesText(Session.TakeNODInsertions('')),
      'insertions at the end of NOD (handler without dictionary is skipped)');
    CheckEquals('', EntriesText(Session.TakeNODInsertions('')), 'nothing left');
  finally
    Session.Free;
    Map.Free;
    Ctx.Done;
  end;

  { Все вставки одним вызовом — по алфавиту, а не в порядке регистрации }
  Ctx.InitRec;
  Session := TZNODSaveSession.Create(AC1021);
  try
    Session.LoadTemplateText(TemplateWithoutBranch);
    Ctx.handle := $100;
    Session.ReserveHandles(Drawing, Ctx);
    CheckEquals('ACAD_A_TEST>108:350 ACAD_TABLESTYLE>100:350 ZZZZ_TEST>107:350',
      EntriesText(Session.TakeNODInsertions('')), 'all insertions are sorted by key');
  finally
    Session.Free;
    Ctx.Done;
  end;

  { Шаблон без NOD: вставлять некуда }
  Ctx.InitRec;
  Map := TMapHandleToHandle.Create;
  Session := TZNODSaveSession.Create(AC1021);
  try
    Check(not Session.LoadTemplateText(TemplateWithoutNOD), 'template without NOD: False');
    Ctx.handle := $100;
    Session.ReserveHandles(Drawing, Ctx);
    Session.MapTemplateHandles(Map, Ctx);
    CheckInt(0, Map.Count, 'template without NOD: nothing is remapped');
    CheckEquals('', EntriesText(Session.TakeNODInsertions('')),
      'template without NOD: no insertions');
  finally
    Session.Free;
    Map.Free;
    Ctx.Done;
    DoneDrawing(Drawing);
  end;
end;

{ Повторяющиеся имена стилей: второй стиль пропускается }
procedure TestDuplicateStyleNames;
var
  Session: TZNODSaveSession;
  Drawing: TSimpleDrawing;
  Ctx: TIODXFSaveContext;
  Style: PTGDBDXFTableStyle;
  HS: string;
  OutFile: string;
  Model: TZNODModel;
  Dict: TZDXFDictionary;
begin
  Drawing.init(nil);
  Ctx.InitRec;
  Session := TZNODSaveSession.Create(AC1021);
  try
    AddSampleStyle(Drawing.DXFTableStyleTable, 'Standard', 2.5);
    AddSampleStyle(Drawing.DXFTableStyleTable, 'Copy', 3.5);
    Style := Drawing.DXFTableStyleTable.getAddres('Copy');
    Check(Style <> nil, 'duplicate: second style exists');
    if Style = nil then
      Exit;
    Style^.Name := 'Standard';
    Ctx.handle := $100;
    Session.ReserveHandles(Drawing, Ctx);
    CheckEquals('104', DXFHandleToStr(Ctx.handle),
      'duplicate: handles for one style (dictionary + 3)');
    Check(Ctx.TableStyleNameHandleMap.MyGetValue('Standard', HS) and (HS = '101'),
      'duplicate: TableStyleNameHandleMap[Standard] = first style');
  finally
    Session.Free;
    Ctx.Done;
  end;
  try
    OutFile := OutPath('duplicate_2007.dxf');
    Check(savedxf20XX(OutFile, Root + TemplatesDir + 'savetemplate2007.dxf',
      Drawing, ZCDxf2007), 'duplicate: saved');
    Model := ObjectsModelOfFile(OutFile);
    try
      CheckInt(0, Model.DuplicateHandleCount, 'duplicate: duplicate handles in OBJECTS');
      Dict := Model.ResolveDictionary(TableStyleKey);
      Check((Dict <> nil) and (Dict.Count = 1) and (Dict[0].Key = 'Standard'),
        'duplicate: one Standard entry in ' + TableStyleKey);
    finally
      Model.Free;
    end;
  finally
    DoneDrawing(Drawing);
  end;
end;

{ ---------- 7. Лог ---------- }

procedure TestTraceLog;
var
  OldLevel: TLogLevel;
  Session: TZNODSaveSession;
  Drawing: TSimpleDrawing;
  Ctx: TIODXFSaveContext;
  Map: TMapHandleToHandle;
begin
  Check(not programlog.isModuleEnabled(NODLogModuleId),
    'log: module ' + NOD_LOG_MODULE_NAME + ' is disabled by default');
  OldLevel := programlog.GetCurrentLogLevel;
  programlog.EnableModule(NODLogModuleId);
  programlog.SetCurrentLogLevel(LM_Info, True);
  Drawing.init(nil);
  Ctx.InitRec;
  Map := TMapHandleToHandle.Create;
  Session := TZNODSaveSession.Create(AC1021);
  try
    AddSampleStyle(Drawing.DXFTableStyleTable, 'Standard', 2.5);
    Session.LoadTemplateText(TemplateWithBranch);
    Ctx.handle := $100;
    Session.ReserveHandles(Drawing, Ctx);
    Map.AddOrSetValue($20, $1);
    Session.MapTemplateHandles(Map, Ctx);
    Session.TakeNODInsertions('');
    SaveSample(dsStyles, 'savetemplate2000.dxf', ZCDxf2000, OutPath('log_2000.dxf'));
    SaveSample(dsStyles, 'savetemplate2007.dxf', ZCDxf2007, OutPath('log_2007.dxf'));
    Ok('log: stage 4 trace messages are formatted without errors');
  finally
    Session.Free;
    Map.Free;
    Ctx.Done;
    DoneDrawing(Drawing);
    programlog.DisableModule(NODLogModuleId);
    programlog.SetCurrentLogLevel(OldLevel, True);
  end;
end;

begin
  Root := '';
  if ParamCount > 0 then
    Root := IncludeTrailingPathDelimiter(ParamStr(1));
  Failed := 0;
  Run('TestAdapterRegistered', @TestAdapterRegistered);
  Run('TestNoStylesGolden', @TestNoStylesGolden);
  Run('TestStylesGolden', @TestStylesGolden);
  Run('TestTableEntityRef', @TestTableEntityRef);
  Run('TestReload', @TestReload);
  Run('TestSessionBranch', @TestSessionBranch);
  Run('TestSessionInsertions', @TestSessionInsertions);
  Run('TestDuplicateStyleNames', @TestDuplicateStyleNames);
  Run('TestTraceLog', @TestTraceLog);

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
