program nodstage7;

// issue #1456: этап 7 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (сохранение «чужих» веток NOD и резерв ключа ZCAD_DATA). Тест
// проверяет:
//
//  1. Ключ ZCAD_DATA зарезервирован обработчиком (ObjectType DICTIONARY);
//     IsNODPreservableKey: ключи шаблона, обработчиков и ACAD_FIELDLIST не
//     сохраняются.
//  2. Загрузка исходного файла (tablestyleetalon.dxf, в который тест
//     вставляет ветки MY_PLUGIN, MY_HARD, ZCAD_DATA и ACAD_FIELDLIST, класс
//     MYPLUGINOBJ и $DWGCODEPAGE ANSI_1251): в
//     TSimpleDrawing.PreservedNODBranches — ветки MY_PLUGIN (5 объектов:
//     словарь, XRECORD с расширенным словарём и его записью, объект
//     неизвестного класса MYPLUGINOBJ) и MY_HARD (запись NOD 360), класс
//     MYPLUGINOBJ, ссылка на стиль таблиц Standard (ветка обработчика);
//     веток ZCAD_DATA, ACAD_FIELDLIST, ACAD_GROUP, ACAD_TABLESTYLE нет.
//  3. Сохранение в DXF 2007 и 2000: ветки записаны в NOD (коды 350/360
//     исходного файла), все ссылки перемаплены (330/340/360/1005 внутри
//     ветки, 340 на стиль таблиц — на стиль сохранённого файла по имени,
//     ссылка на несуществующий объект — 0, запись словаря на
//     несуществующий объект — удалена), целостность (владельцы, реакторы,
//     уникальные хэндлы < $HANDSEED, классы неизвестных объектов в
//     CLASSES — один раз, APPID приложения xdata — один раз), строки: в
//     2007 — UTF-8, в 2000 — в кодовой странице чертежа.
//  4. Round-trip: исходный файл → 2007 → 2007 — ветка MY_PLUGIN не меняется
//     (с точностью до хэндлов); повторная загрузка 2000 даёт те же строки.
//  5. TZNODSaveSession: ветка не пишется, если её ключ есть в NOD шаблона
//     или для него зарегистрирован обработчик; без шаблона — не пишется.
//
// Использование:
//   nodstage7 [<корень репозитория>]

{$mode objfpc}{$H+}
{$codepage utf8}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager, uzeffdxfsupport,
  uzgldrawcontext, uzeconsts, uzeTypes,
  uzeffdxfobjects, uzeffdxfnod, uzeffdxfnodregistry, uzeffdxfnodpreserved,
  // Регистрация загрузчика сущности ACAD_TABLE (как в nodstage0/4/5/6)
  uzeacadtable_types, uzeacadtable_model, uzeacadtable_dxf_write;

const
  TemplatesDir = 'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/';
  Template2007 = 'savetemplate2007.dxf';
  Template2000 = 'savetemplate2000.dxf';
  { Чертёж AutoCAD 2007 со стилем таблиц Standard — основа исходного файла }
  EtalonFile = 'cad_source/test/tablestyleetalon.dxf';

  PluginKey = 'MY_PLUGIN';
  HardKey = 'MY_HARD';
  PluginClass = 'MYPLUGINOBJ';
  PluginApp = 'MYPLUGINAPP';
  PluginText = 'Привет мир';

  { Класс объекта MYPLUGINOBJ (вставляется в CLASSES исходного файла) }
  SourceClasses =
    '0'#10'CLASS'#10'1'#10'MYPLUGINOBJ'#10'2'#10'AcDbMyPluginObj'#10 +
    '3'#10'MyPlugin'#10'90'#10'1'#10'91'#10'1'#10'280'#10'0'#10'281'#10'0'#10;

  { Записи NOD исходного файла (вставляются перед первой записью NOD) }
  SourceNODEntries =
    '3'#10'MY_PLUGIN'#10'350'#10'A001'#10 +
    '3'#10'MY_HARD'#10'360'#10'A020'#10 +
    '3'#10'ZCAD_DATA'#10'350'#10'A010'#10 +
    '3'#10'ACAD_FIELDLIST'#10'350'#10'A030'#10;

  { Объекты веток (вставляются в конец OBJECTS; %0:s — хэндл NOD,
    %1:s — хэндл стиля таблиц Standard исходного файла):
    A001 DICTIONARY ветки MY_PLUGIN: Broken → AFFF (объекта нет),
      Obj → A003, Settings → A002 (360);
    A002 XRECORD: расширенный словарь A004, строка, 330 → A003 (внутри
      ветки), 340 → стиль таблиц, 360 → AFFE (объекта нет), xdata
      MYPLUGINAPP с 1005 → A003;
    A003 MYPLUGINOBJ (класс из CLASSES): 90 42, 340 → A001;
    A004 DICTIONARY (расширенный словарь A002): Nested → A005;
    A005 XRECORD;
    A020 XRECORD ветки MY_HARD;
    A010/A011 — ветка ZCAD_DATA (не сохраняется);
    A030 FIELDLIST — ветка ACAD_FIELDLIST (не сохраняется). }
  SourceObjects =
    '0'#10'DICTIONARY'#10'5'#10'A001'#10 +
    '102'#10'{ACAD_REACTORS'#10'330'#10'%0:s'#10'102'#10'}'#10 +
    '330'#10'%0:s'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'Broken'#10'350'#10'AFFF'#10 +
    '3'#10'Obj'#10'350'#10'A003'#10 +
    '3'#10'Settings'#10'360'#10'A002'#10 +
    '0'#10'XRECORD'#10'5'#10'A002'#10 +
    '102'#10'{ACAD_XDICTIONARY'#10'360'#10'A004'#10'102'#10'}'#10 +
    '102'#10'{ACAD_REACTORS'#10'330'#10'A001'#10'102'#10'}'#10 +
    '330'#10'A001'#10'100'#10'AcDbXrecord'#10'280'#10'1'#10 +
    '1'#10'Привет мир'#10'40'#10'3.5'#10'330'#10'A003'#10 +
    '340'#10'%1:s'#10'360'#10'AFFE'#10 +
    '1001'#10'MYPLUGINAPP'#10'1000'#10'x'#10'1005'#10'A003'#10 +
    '0'#10'MYPLUGINOBJ'#10'5'#10'A003'#10 +
    '102'#10'{ACAD_REACTORS'#10'330'#10'A001'#10'102'#10'}'#10 +
    '330'#10'A001'#10'100'#10'AcDbMyPluginObj'#10'90'#10'42'#10'340'#10'A001'#10 +
    '0'#10'DICTIONARY'#10'5'#10'A004'#10'330'#10'A002'#10 +
    '100'#10'AcDbDictionary'#10'280'#10'1'#10'281'#10'1'#10 +
    '3'#10'Nested'#10'360'#10'A005'#10 +
    '0'#10'XRECORD'#10'5'#10'A005'#10 +
    '102'#10'{ACAD_REACTORS'#10'330'#10'A004'#10'102'#10'}'#10 +
    '330'#10'A004'#10'100'#10'AcDbXrecord'#10'280'#10'1'#10'1'#10'nested'#10 +
    '0'#10'XRECORD'#10'5'#10'A020'#10 +
    '102'#10'{ACAD_REACTORS'#10'330'#10'%0:s'#10'102'#10'}'#10 +
    '330'#10'%0:s'#10'100'#10'AcDbXrecord'#10'280'#10'1'#10'70'#10'7'#10 +
    '0'#10'DICTIONARY'#10'5'#10'A010'#10'330'#10'%0:s'#10 +
    '100'#10'AcDbDictionary'#10'281'#10'1'#10'3'#10'Item'#10'350'#10'A011'#10 +
    '0'#10'XRECORD'#10'5'#10'A011'#10'330'#10'A010'#10 +
    '100'#10'AcDbXrecord'#10'280'#10'1'#10'1'#10'zcad'#10 +
    '0'#10'FIELDLIST'#10'5'#10'A030'#10'330'#10'%0:s'#10 +
    '100'#10'AcDbIdSet'#10'90'#10'0'#10'100'#10'AcDbFieldList'#10;

var
  Root: string;
  Failed: Integer;
  SourceFile: string;
  { Хэндл стиля таблиц Standard в исходном файле }
  SourceTableStyle: TDWGHandle;

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

{ Загрузка DXF с установкой кодовой страницы чертежа из $DWGCODEPAGE —
  как LoadDXFviaZEnfine (cad_source/zcad/register/uzcregfileformats.pas) }
procedure LoadDrawing(const AFileName: string; var ADrawing: TSimpleDrawing);
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
  Hdr: TDXFHeaderInfo;
begin
  DC := ADrawing.CreateDrawingRC;
  ZDC.CreateRec(ADrawing, ADrawing.pObjRoot^, TLOLoad, DC);
  Hdr := AddFromDXF(AFileName, ZDC);
  if Hdr.DWGCodePage <> CP_INVALID then
    ADrawing.DXFCodePage := SysCP2ZCCodePage(Hdr.iDWGCodePage);
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
  if AStart < 0 then
    Exit(-1);
  for I := AStart to P.Count - 1 do
    if (P.Codes[I] = 0) and (P.Values[I] = 'ENDSEC') then
      Exit(I);
  Result := -1;
end;

{ Текст секции AName файла ('' — секции нет) }
function SectionText(const P: TDXFPairs; const AName: string): string;
var
  S, I: Integer;
  SB: TStringList;
begin
  Result := '';
  S := SectionStart(P, AName);
  if S < 0 then
    Exit;
  SB := TStringList.Create;
  try
    for I := S - 1 to SectionEnd(P, S) do begin
      SB.Add(IntToStr(P.Codes[I]));
      SB.Add(P.Values[I]);
    end;
    Result := SB.Text;
  finally
    SB.Free;
  end;
end;

{ Модель секции OBJECTS файла }
function ModelOfFile(const AFileName: string): TZNODModel;
var
  P: TDXFPairs;
begin
  P := ReadPairs(AFileName);
  Result := TZNODModel.Create;
  if not Result.LoadFromText(SectionText(P, 'OBJECTS')) then
    Fail(AFileName + ': OBJECTS parse error: ' + Result.ParseError);
end;

function HandleOf(const AStr: string): TDWGHandle;
begin
  if not TryDXFStrToHandle(AStr, Result) then
    Result := 0;
end;

{ $HANDSEED файла. Писатель ZCAD выводит его с ведущей «1» в 9 знаках
  (место под значение резервируется до конца записи: '100000066'),
  поэтому старший разряд отбрасывается. }
function HandSeed(const P: TDXFPairs): TDWGHandle;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to P.Count - 2 do
    if (P.Codes[I] = 9) and (P.Values[I] = '$HANDSEED') then
      Exit(HandleOf(P.Values[I + 1]) and $FFFFFFFF);
end;

{ Хэндл цели записи AKey словаря ADict (0 — записи нет) }
function EntryTarget(ADict: TZDXFDictionary; const AKey: string): TDWGHandle;
var
  E: TZDXFDictEntry;
begin
  Result := 0;
  if (ADict <> nil) and ADict.FindEntry(AKey, E) then
    Result := E.TargetHandle;
end;

function EntryCode(ADict: TZDXFDictionary; const AKey: string): Integer;
var
  E: TZDXFDictEntry;
begin
  Result := 0;
  if (ADict <> nil) and ADict.FindEntry(AKey, E) then
    Result := E.OwnershipCode;
end;

{ Количество записей с ключом AKey (без учёта регистра) }
function EntryCount(ADict: TZDXFDictionary; const AKey: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  if ADict <> nil then
    for I := 0 to ADict.Count - 1 do
      if SameText(ADict[I].Key, AKey) then
        Inc(Result);
end;

{ Значения пар объекта с кодом ACode после первой группы 100 (данные
  объекта, без заголовка), через '|' }
function DataValues(AObj: TZDXFRawObject; ACode: Integer): string;
var
  I: Integer;
  InData: Boolean;
begin
  Result := '';
  if AObj = nil then
    Exit('<nil>');
  InData := False;
  for I := 0 to AObj.PairCount - 1 do begin
    if AObj.Pairs[I].Code = 100 then
      InData := True
    else if InData and (AObj.Pairs[I].Code = ACode) then begin
      if Result <> '' then
        Result := Result + '|';
      Result := Result + Trim(AObj.Pairs[I].Value);
    end;
  end;
end;

function HasReactor(AObj: TZDXFRawObject; AHandle: TDWGHandle): Boolean;
var
  H: TDWGHandle;
begin
  Result := False;
  if AObj <> nil then
    for H in AObj.Reactors do
      if H = AHandle then
        Exit(True);
end;

{ Ключ записи NOD, которой (через цепочку владельцев 330) принадлежит
  AObj; '' — объект вне веток NOD }
function NODKeyOf(AModel: TZNODModel; AObj: TZDXFRawObject): string;
var
  I, Guard: Integer;
begin
  Result := '';
  if AModel.NOD = nil then
    Exit;
  Guard := 0;
  while (AObj <> nil) and (Guard < 1000) do begin
    if AObj.OwnerHandle = AModel.NOD.Handle then begin
      for I := 0 to AModel.NOD.Count - 1 do
        if AModel.NOD[I].TargetHandle = AObj.Handle then
          Exit(AModel.NOD[I].Key);
      Exit;
    end;
    AObj := AModel.FindObject(AObj.OwnerHandle);
    Inc(Guard);
  end;
end;

{ Нормализованный дамп ветки AKey хранилища: хэндлы объектов ветки
  заменены номерами (#0, #1...) в порядке хранения, прочие ненулевые
  ссылки — 'ext'. Сравнивается до и после пересохранения. }
function DumpBranch(AStore: TZNODPreservedBranches; const AKey: string): string;
var
  B, I, K: Integer;
  Local: TStringList;
  Obj: TZDXFRawObject;
  H: TDWGHandle;
  V: string;

  function RefName(AHandle: TDWGHandle): string;
  var
    J: Integer;
  begin
    if AHandle = 0 then
      Exit('0');
    J := Local.IndexOf(DXFHandleToStr(AHandle));
    if J >= 0 then
      Result := '#' + IntToStr(J)
    else
      Result := 'ext';
  end;

begin
  Result := '';
  if AStore = nil then
    Exit('<no storage>');
  B := AStore.IndexOfKey(AKey);
  if B < 0 then
    Exit('<no branch>');
  Local := TStringList.Create;
  try
    for I := 0 to AStore.ObjectCount - 1 do
      if AStore.ObjectBranch[I] = B then
        Local.Add(DXFHandleToStr(AStore.Objects[I].Handle));
    Result := Format('%s code=%d objects=%d', [AStore.Branches[B].Key,
      AStore.Branches[B].OwnershipCode, Local.Count]) + #10;
    for I := 0 to AStore.ObjectCount - 1 do begin
      if AStore.ObjectBranch[I] <> B then
        Continue;
      Obj := AStore.Objects[I];
      Result := Result + Obj.ObjType + ' ' + RefName(Obj.Handle) + #10;
      for K := 0 to Obj.PairCount - 1 do begin
        if Obj.Pairs[K].Code = 5 then
          Continue;
        V := Trim(Obj.Pairs[K].Value);
        if IsNODPreservedRefGroupCode(Obj.Pairs[K].Code) and
           TryDXFStrToHandle(V, H) then
          V := RefName(H);
        Result := Result + Format('  %d=%s', [Obj.Pairs[K].Code, V]) + #10;
      end;
    end;
  finally
    Local.Free;
  end;
end;

{ ---------- Исходный файл ---------- }

{ Вставляет текст AText (пары через #10) в L перед строкой AIndex }
procedure InsertText(L: TStringList; AIndex: Integer; const AText: string);
var
  T: TStringList;
  I: Integer;
begin
  T := TStringList.Create;
  try
    T.Text := AText;
    for I := T.Count - 1 downto 0 do
      L.Insert(AIndex, T[I]);
  finally
    T.Free;
  end;
end;

{ Индекс строки-значения (нечётная строка пары) AValue после строки AFrom }
function FindValueLine(L: TStringList; AFrom: Integer; const AValue: string): Integer;
var
  I: Integer;
begin
  I := AFrom;
  if I < 1 then
    I := 1;
  if not Odd(I) then
    Inc(I);
  while I < L.Count do begin
    if Trim(L[I]) = AValue then
      Exit(I);
    Inc(I, 2);
  end;
  Result := -1;
end;

{ Строит исходный файл: tablestyleetalon.dxf + класс, записи NOD и объекты
  веток, $DWGCODEPAGE ANSI_1251, $HANDSEED B000 }
function BuildSourceFile: Boolean;
var
  L: TStringList;
  I, NODLine: Integer;
  Model: TZNODModel;
  NODHandle: TDWGHandle;
begin
  Result := False;
  Model := ModelOfFile(Root + EtalonFile);
  try
    if Model.NOD = nil then begin
      Fail('etalon: no NOD');
      Exit;
    end;
    NODHandle := Model.NOD.Handle;
    SourceTableStyle := EntryTarget(
      Model.FindDictionary(EntryTarget(Model.NOD, 'ACAD_TABLESTYLE')), 'Standard');
    Check(SourceTableStyle <> 0, 'etalon: table style Standard found');
    Check((Model.FindObject(HandleOf('A001')) = nil) and
      (Model.FindObject(HandleOf('A030')) = nil), 'etalon: handles A001..A030 are free');
  finally
    Model.Free;
  end;

  L := TStringList.Create;
  try
    L.LoadFromFile(Root + EtalonFile);
    I := FindValueLine(L, 0, '$DWGCODEPAGE');
    L[I + 2] := 'ANSI_1251';
    I := FindValueLine(L, 0, '$HANDSEED');
    L[I + 2] := 'B000';
    { CLASSES: перед 0/ENDSEC секции }
    I := FindValueLine(L, FindValueLine(L, 0, 'CLASSES'), 'ENDSEC');
    InsertText(L, I - 1, SourceClasses);
    { NOD: перед первой записью 3 }
    NODLine := FindValueLine(L, FindValueLine(L, 0, 'OBJECTS'), 'DICTIONARY');
    I := NODLine + 1;
    while (I < L.Count) and (Trim(L[I]) <> '3') do
      Inc(I, 2);
    InsertText(L, I, SourceNODEntries);
    { Объекты: перед 0/ENDSEC секции OBJECTS }
    I := FindValueLine(L, NODLine, 'ENDSEC');
    InsertText(L, I - 1, Format(SourceObjects,
      [DXFHandleToStr(NODHandle), DXFHandleToStr(SourceTableStyle)]));
    SourceFile := GetTempDir(False) + 'nodstage7_source_2007.dxf';
    L.SaveToFile(SourceFile);
    Result := True;
  finally
    L.Free;
  end;
end;

{ ---------- 1. Реестр ---------- }

procedure TestRegistry;
var
  H: TZNODHandler;
begin
  Check(FindNODHandler(CNODZCADDataKey, H), 'handler ' + CNODZCADDataKey + ' is registered');
  CheckEquals('DICTIONARY', H.ObjectType, 'ZCAD_DATA: ObjectType');
  Check(Assigned(H.LoadProc), 'ZCAD_DATA: LoadProc');
  Check(not Assigned(H.SaveProc), 'ZCAD_DATA: no SaveProc (content is out of scope)');
  Check(not IsNODPreservableKey(CNODZCADDataKey), 'ZCAD_DATA is not preservable');
  Check(not IsNODPreservableKey('ACAD_TABLESTYLE'), 'ACAD_TABLESTYLE (handler) is not preservable');
  Check(not IsNODPreservableKey('acad_group'), 'ACAD_GROUP (template) is not preservable');
  Check(not IsNODPreservableKey('ACAD_LAYOUT'), 'ACAD_LAYOUT (template) is not preservable');
  Check(not IsNODPreservableKey('ACAD_FIELDLIST'), 'ACAD_FIELDLIST is not preservable');
  Check(not IsNODPreservableKey(''), 'empty key is not preservable');
  Check(IsNODPreservableKey(PluginKey), PluginKey + ' is preservable');
  Check(IsNODPreservableKey('DWGPROPS'), 'DWGPROPS is preservable');
  Check(IsNODPreservedRefGroupCode(330) and IsNODPreservedRefGroupCode(340) and
    IsNODPreservedRefGroupCode(360) and IsNODPreservedRefGroupCode(390) and
    IsNODPreservedRefGroupCode(480) and IsNODPreservedRefGroupCode(1005),
    'ref group codes: 330, 340, 360, 390, 480, 1005');
  Check(not IsNODPreservedRefGroupCode(5) and not IsNODPreservedRefGroupCode(1) and
    not IsNODPreservedRefGroupCode(370) and not IsNODPreservedRefGroupCode(1000),
    'not ref group codes: 5, 1, 370, 1000');
end;

{ ---------- 2. Загрузка ---------- }

procedure CheckLoadedStore(AStore: TZNODPreservedBranches; const ACase: string;
  AFromSource: Boolean);
var
  B, I, N: Integer;
  Obj: TZDXFRawObject;
  R: TZNODPreservedRef;
begin
  Check(AStore <> nil, ACase + ': PreservedNODBranches created');
  if AStore = nil then
    Exit;
  B := AStore.IndexOfKey(PluginKey);
  Check(B >= 0, ACase + ': branch ' + PluginKey);
  Check(AStore.IndexOfKey(HardKey) >= 0, ACase + ': branch ' + HardKey);
  if AStore.IndexOfKey(HardKey) >= 0 then
    CheckInt(360, AStore.Branches[AStore.IndexOfKey(HardKey)].OwnershipCode,
      ACase + ': ' + HardKey + ' ownership code');
  if B >= 0 then
    CheckInt(350, AStore.Branches[B].OwnershipCode, ACase + ': ' + PluginKey + ' ownership code');
  CheckInt(-1, AStore.IndexOfKey(CNODZCADDataKey), ACase + ': no branch ZCAD_DATA');
  CheckInt(-1, AStore.IndexOfKey('ACAD_FIELDLIST'), ACase + ': no branch ACAD_FIELDLIST');
  CheckInt(-1, AStore.IndexOfKey('ACAD_GROUP'), ACase + ': no branch ACAD_GROUP (template)');
  CheckInt(-1, AStore.IndexOfKey('ACAD_TABLESTYLE'), ACase + ': no branch ACAD_TABLESTYLE (handler)');
  N := 0;
  for I := 0 to AStore.ObjectCount - 1 do
    if AStore.ObjectBranch[I] = B then
      Inc(N);
  CheckInt(5, N, ACase + ': objects of ' + PluginKey);
  Check(AStore.FindClass(PluginClass) <> nil, ACase + ': class ' + PluginClass + ' kept');
  if AStore.FindClass(PluginClass) <> nil then
    CheckEquals('AcDbMyPluginObj', AStore.FindClass(PluginClass).ValueOf(2),
      ACase + ': class C++ name');
  { Строка XRECORD в UTF-8 }
  Obj := nil;
  for I := 0 to AStore.ObjectCount - 1 do
    if (AStore.ObjectBranch[I] = B) and (AStore.Objects[I].ObjType = 'XRECORD') and
       (AStore.Objects[I].XDictHandle <> 0) then
      Obj := AStore.Objects[I];
  CheckEquals(PluginText, DataValues(Obj, 1), ACase + ': XRECORD string (UTF-8)');
  if AFromSource then begin
    Check(AStore.ContainsHandle(HandleOf('A001')) and AStore.ContainsHandle(HandleOf('A005')),
      ACase + ': source handles kept (A001, A005)');
    Check(not AStore.ContainsHandle(HandleOf('A010')) and
      not AStore.ContainsHandle(HandleOf('A030')),
      ACase + ': ZCAD_DATA and FIELDLIST objects are not kept');
    Check(AStore.FindRef(SourceTableStyle, R) and (R.HandlerKey = 'ACAD_TABLESTYLE') and
      (R.Name = 'Standard'), ACase + ': ref to table style Standard remembered by name');
  end;
end;

procedure TestLoad;
var
  Drawing: TSimpleDrawing;
begin
  Drawing.init(nil);
  try
    LoadDrawing(SourceFile, Drawing);
    CheckLoadedStore(Drawing.PreservedNODBranches, 'source', True);
  finally
    DoneDrawing(Drawing);
  end;

  { Файл без чужих веток — хранилище не создаётся }
  Drawing.init(nil);
  try
    LoadDrawing(Root + EtalonFile, Drawing);
    Check(Drawing.PreservedNODBranches = nil, 'etalon: no preserved branches, no storage');
  finally
    DoneDrawing(Drawing);
  end;
end;

{ ---------- 3. Сохранение ---------- }

function CountClass(const P: TDXFPairs; const AName: string; out A91: string): Integer;
var
  S, E, I, K: Integer;
begin
  Result := 0;
  A91 := '';
  S := SectionStart(P, 'CLASSES');
  E := SectionEnd(P, S);
  for I := S to E - 1 do
    if (P.Codes[I] = 1) and (P.Values[I] = AName) and
       (P.Codes[I - 1] = 0) and (P.Values[I - 1] = 'CLASS') then begin
      Inc(Result);
      K := I + 1;
      while (K < E) and (P.Codes[K] <> 0) do begin
        if P.Codes[K] = 91 then
          A91 := P.Values[K];
        Inc(K);
      end;
    end;
end;

{ Значение группы 0 объекта, которому принадлежит пара AIndex }
function PrevZero(const P: TDXFPairs; AIndex: Integer): string;
var
  I: Integer;
begin
  for I := AIndex downto 0 do
    if P.Codes[I] = 0 then
      Exit(P.Values[I]);
  Result := '';
end;

{ Количество записей APPID с именем AName }
function CountAppId(const P: TDXFPairs; const AName: string): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to P.Count - 1 do
    if (P.Codes[I] = 2) and SameText(P.Values[I], AName) and
       (PrevZero(P, I) = 'APPID') then
      Inc(Result);
end;

{ Хэндлы (5 и 105) всех объектов файла: уникальны и меньше $HANDSEED }
procedure CheckFileHandles(const P: TDXFPairs; const ACase: string);
var
  Seen: TStringList;
  I, Dup, Above: Integer;
  H, Seed: TDWGHandle;
begin
  Seed := HandSeed(P);
  Seen := TStringList.Create;
  try
    Seen.Sorted := True;
    Seen.Duplicates := dupAccept;
    Dup := 0;
    Above := 0;
    for I := 1 to P.Count - 1 do
      if ((P.Codes[I] = 5) or (P.Codes[I] = 105)) and
         not ((P.Codes[I - 1] = 9) and (P.Values[I - 1] = '$HANDSEED')) and
         TryDXFStrToHandle(P.Values[I], H) and (H <> 0) then begin
        if Seen.IndexOf(DXFHandleToStr(H)) >= 0 then begin
          Inc(Dup);
          writeln('  duplicate handle ', DXFHandleToStr(H));
        end;
        Seen.Add(DXFHandleToStr(H));
        if H >= Seed then
          Inc(Above);
      end;
    CheckInt(0, Dup, ACase + ': duplicate handles in file');
    CheckInt(0, Above, ACase + ': handles >= $HANDSEED ' + DXFHandleToStr(Seed));
  finally
    Seen.Free;
  end;
end;

{ Целостность объектов веток AKeys сохранённого файла (ТЗ 6.2): владелец
  существует; записи словарей указывают на объекты, владелец которых —
  словарь и у которых есть реактор на словарь; расширенный словарь
  принадлежит объекту; объекты неизвестных классов описаны в CLASSES.
  Возвращает количество объектов веток. }
function CheckBranchIntegrity(const P: TDXFPairs; AModel: TZNODModel;
  const AKeys: array of string; const ACase: string): Integer;
var
  I, K, Bad: Integer;
  Obj, T: TZDXFRawObject;
  D: TZDXFDictionary;
  Key, A91: string;
  InBranch: Boolean;
begin
  Result := 0;
  Bad := 0;
  for I := 0 to AModel.Objects.Count - 1 do begin
    Obj := AModel.Objects[I];
    Key := NODKeyOf(AModel, Obj);
    InBranch := False;
    for K := Low(AKeys) to High(AKeys) do
      if SameText(Key, AKeys[K]) then
        InBranch := True;
    if not InBranch then
      Continue;
    Inc(Result);
    if (Obj.OwnerHandle <> AModel.NOD.Handle) and
       (AModel.FindObject(Obj.OwnerHandle) = nil) then begin
      Inc(Bad);
      writeln('  ', Obj.ObjType, ' ', Obj.HandleStr, ': owner ',
        DXFHandleToStr(Obj.OwnerHandle), ' not found');
    end;
    if Obj.XDictHandle <> 0 then begin
      T := AModel.FindObject(Obj.XDictHandle);
      if (T = nil) or (T.OwnerHandle <> Obj.Handle) then begin
        Inc(Bad);
        writeln('  ', Obj.ObjType, ' ', Obj.HandleStr, ': bad xdictionary');
      end;
    end;
    D := AModel.FindDictionary(Obj.Handle);
    if D <> nil then
      for K := 0 to D.Count - 1 do begin
        T := AModel.FindObject(D[K].TargetHandle);
        if (T = nil) or (T.OwnerHandle <> D.Handle) or not HasReactor(T, D.Handle) then begin
          Inc(Bad);
          writeln('  ', Obj.HandleStr, ': entry "', D[K].Key, '" → ',
            DXFHandleToStr(D[K].TargetHandle), ': target/owner/reactor mismatch');
        end;
      end;
    if (Obj.ObjType <> 'DICTIONARY') and (Obj.ObjType <> 'XRECORD') and
       (Obj.ObjType <> 'ACDBDICTIONARYWDFLT') and
       (CountClass(P, Obj.ObjType, A91) <> 1) then begin
      Inc(Bad);
      writeln('  ', Obj.ObjType, ' ', Obj.HandleStr, ': no single CLASS');
    end;
  end;
  CheckInt(0, Bad, ACase + ': integrity of preserved branches');
end;

{ Проверки сохранённого файла; AIs2007 — DXF 2007 (UTF-8, группа 91) }
procedure CheckSaved(const AFileName, ACase: string; AIs2007: Boolean);
var
  P: TDXFPairs;
  Model: TZNODModel;
  NOD, Dict, XDict, StyleDict: TZDXFDictionary;
  Settings, PluginObj, Nested, Hard: TZDXFRawObject;
  A91: string;
  Expected, Actual: RawByteString;
begin
  P := ReadPairs(AFileName);
  CheckFileHandles(P, ACase);
  CheckInt(1, CountClass(P, PluginClass, A91), ACase + ': CLASS ' + PluginClass);
  if AIs2007 then
    CheckEquals('1', A91, ACase + ': CLASS 91 (instance count)')
  else
    CheckEquals('', A91, ACase + ': CLASS without 91 (before AC1018)');
  CheckInt(1, CountAppId(P, PluginApp), ACase + ': APPID ' + PluginApp);

  Model := ModelOfFile(AFileName);
  try
    NOD := Model.NOD;
    Check(NOD <> nil, ACase + ': NOD');
    if NOD = nil then
      Exit;
    CheckInt(1, EntryCount(NOD, PluginKey), ACase + ': NOD entry ' + PluginKey);
    CheckInt(350, EntryCode(NOD, PluginKey), ACase + ': NOD ' + PluginKey + ' code');
    CheckInt(1, EntryCount(NOD, HardKey), ACase + ': NOD entry ' + HardKey);
    CheckInt(360, EntryCode(NOD, HardKey), ACase + ': NOD ' + HardKey + ' code');
    CheckInt(0, EntryCount(NOD, CNODZCADDataKey), ACase + ': no NOD entry ZCAD_DATA');
    CheckInt(0, EntryCount(NOD, 'ACAD_FIELDLIST'), ACase + ': no NOD entry ACAD_FIELDLIST');
    CheckInt(1, EntryCount(NOD, 'ACAD_GROUP'), ACase + ': ACAD_GROUP once (template)');

    Dict := Model.FindDictionary(EntryTarget(NOD, PluginKey));
    Check(Dict <> nil, ACase + ': ' + PluginKey + ' is a dictionary');
    if Dict = nil then
      Exit;
    Check((Dict.OwnerHandle = NOD.Handle) and HasReactor(Dict.RawObject, NOD.Handle),
      ACase + ': ' + PluginKey + ' owner and reactor = NOD');
    CheckInt(2, Dict.Count, ACase + ': ' + PluginKey + ' entries (Broken dropped)');
    CheckInt(0, EntryCount(Dict, 'Broken'), ACase + ': entry Broken dropped');
    CheckInt(360, EntryCode(Dict, 'Settings'), ACase + ': entry Settings code');

    Settings := Model.FindObject(EntryTarget(Dict, 'Settings'));
    PluginObj := Model.FindObject(EntryTarget(Dict, 'Obj'));
    Check((Settings <> nil) and (Settings.ObjType = 'XRECORD'), ACase + ': Settings is XRECORD');
    Check((PluginObj <> nil) and (PluginObj.ObjType = PluginClass),
      ACase + ': Obj is ' + PluginClass);
    if (Settings = nil) or (PluginObj = nil) then
      Exit;

    if AIs2007 then
      CheckEquals(PluginText, DataValues(Settings, 1), ACase + ': string in UTF-8')
    else begin
      Expected := PluginText;
      SetCodePage(Expected, 1251, True);
      { Сравнение байтов: строки в DXF 2000 — в кодовой странице чертежа }
      Actual := DataValues(Settings, 1);
      Check((Length(Actual) = Length(Expected)) and
        CompareMem(Pointer(Actual), Pointer(Expected), Length(Expected)),
        ACase + ': string in code page of drawing (1251)');
    end;
    CheckEquals('3.5', DataValues(Settings, 40), ACase + ': Settings 40');
    CheckEquals(PluginObj.HandleStr, DataValues(Settings, 330),
      ACase + ': Settings 330 → Obj (remapped)');
    StyleDict := Model.FindDictionary(EntryTarget(NOD, 'ACAD_TABLESTYLE'));
    CheckEquals(DXFHandleToStr(EntryTarget(StyleDict, 'Standard')),
      DataValues(Settings, 340), ACase + ': Settings 340 → table style Standard of saved file');
    CheckEquals('0', DataValues(Settings, 360), ACase + ': Settings 360 → missing object = 0');
    CheckEquals(PluginApp, DataValues(Settings, 1001), ACase + ': Settings xdata app');
    CheckEquals(PluginObj.HandleStr, DataValues(Settings, 1005),
      ACase + ': Settings xdata 1005 → Obj (remapped)');

    XDict := Model.FindDictionary(Settings.XDictHandle);
    Check((XDict <> nil) and (XDict.OwnerHandle = Settings.Handle),
      ACase + ': Settings xdictionary owned by Settings');
    Nested := Model.FindObject(EntryTarget(XDict, 'Nested'));
    Check(Nested <> nil, ACase + ': xdictionary entry Nested');
    CheckEquals('nested', DataValues(Nested, 1), ACase + ': Nested 1');

    CheckEquals('42', DataValues(PluginObj, 90), ACase + ': Obj 90');
    CheckEquals(Dict.RawObject.HandleStr, DataValues(PluginObj, 340),
      ACase + ': Obj 340 → ' + PluginKey + ' (remapped)');

    Hard := Model.FindObject(EntryTarget(NOD, HardKey));
    Check((Hard <> nil) and (Hard.ObjType = 'XRECORD') and
      (Hard.OwnerHandle = NOD.Handle) and (DataValues(Hard, 70) = '7'),
      ACase + ': ' + HardKey + ' XRECORD owned by NOD');

    CheckInt(6, CheckBranchIntegrity(P, Model, [PluginKey, HardKey], ACase),
      ACase + ': objects of preserved branches');
  finally
    Model.Free;
  end;
end;

procedure SaveDrawing(const ASource, AOutFile, ATemplate: string;
  AVersion: TZCDxfVersion; out ADump: string);
var
  Drawing: TSimpleDrawing;
begin
  Drawing.init(nil);
  try
    LoadDrawing(ASource, Drawing);
    ADump := DumpBranch(Drawing.PreservedNODBranches, PluginKey);
    if not savedxf20XX(AOutFile, Root + TemplatesDir + ATemplate, Drawing, AVersion) then
      Fail('savedxf20XX failed for ' + AOutFile);
  finally
    DoneDrawing(Drawing);
  end;
end;

procedure TestSave2007;
var
  Out1, Out2, Dump0, Dump1, Dump2: string;
  Drawing: TSimpleDrawing;
begin
  Out1 := GetTempDir(False) + 'nodstage7_2007.dxf';
  Out2 := GetTempDir(False) + 'nodstage7_2007_2007.dxf';
  SaveDrawing(SourceFile, Out1, Template2007, ZCDxf2007, Dump0);
  CheckSaved(Out1, 'source → 2007', True);
  { Повторная загрузка и пересохранение: ветка не меняется }
  SaveDrawing(Out1, Out2, Template2007, ZCDxf2007, Dump1);
  CheckSaved(Out2, 'source → 2007 → 2007', True);
  Drawing.init(nil);
  try
    LoadDrawing(Out2, Drawing);
    CheckLoadedStore(Drawing.PreservedNODBranches, '2007 → 2007 reloaded', False);
    Dump2 := DumpBranch(Drawing.PreservedNODBranches, PluginKey);
  finally
    DoneDrawing(Drawing);
  end;
  CheckText(Dump1, Dump2, 'round-trip 2007 → 2007: branch ' + PluginKey + ' unchanged');
  Check(Dump0 <> Dump1, 'source branch differs from saved one (Broken entry and missing ref dropped)');
end;

procedure TestSave2000;
var
  Out1, Dump0: string;
  Drawing: TSimpleDrawing;
begin
  Out1 := GetTempDir(False) + 'nodstage7_2000.dxf';
  SaveDrawing(SourceFile, Out1, Template2000, ZCDxf2000, Dump0);
  CheckSaved(Out1, 'source → 2000', False);
  Drawing.init(nil);
  try
    LoadDrawing(Out1, Drawing);
    CheckLoadedStore(Drawing.PreservedNODBranches, '2000 reloaded', False);
  finally
    DoneDrawing(Drawing);
  end;
end;

{ ---------- 5. Выбор записываемых веток ---------- }

const
  TemplateObjects =
    '0'#10'SECTION'#10'2'#10'OBJECTS'#10 +
    '0'#10'DICTIONARY'#10'5'#10'C'#10'330'#10'0'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '3'#10'ACAD_GROUP'#10'350'#10'D'#10 +
    '3'#10'IN_TEMPLATE'#10'350'#10'E'#10 +
    '0'#10'DICTIONARY'#10'5'#10'D'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '0'#10'DICTIONARY'#10'5'#10'E'#10'330'#10'C'#10'100'#10'AcDbDictionary'#10'281'#10'1'#10 +
    '0'#10'ENDSEC'#10;

procedure DummyLoad(const AModel: TZNODModel; const ADict: TZDXFDictionary;
  var ADrawing: TSimpleDrawing);
begin
end;

{ Сырой объект XRECORD с хэндлом AHandle и владельцем AOwner }
function RawXRecord(const AHandle, AOwner: string): TZDXFRawObject;
begin
  Result := TZDXFRawObject.Create;
  Result.ObjType := 'XRECORD';
  Result.Handle := HandleOf(AHandle);
  Result.OwnerHandle := HandleOf(AOwner);
  Result.HasOwnerGroup := True;
  Result.AddPair(5, AHandle);
  Result.AddPair(330, AOwner);
  Result.AddPair(100, 'AcDbXrecord');
end;

procedure TestSessionSelect;
var
  Store: TZNODPreservedBranches;
  Session: TZNODSaveSession;
  Ins: TZDXFDictEntries;
  I: Integer;
  Keys: string;
  Raw: TZDXFRawObject;
  Drawing: TSimpleDrawing;
  Ctx: TIODXFSaveContext;
begin
  Store := TZNODPreservedBranches.Create;
  Drawing.init(nil);
  try
    Store.AddBranch('IN_TEMPLATE', HandleOf('A1'), 350, HandleOf('C'));
    Store.AddBranch('HANDLED', HandleOf('A2'), 350, HandleOf('C'));
    Store.AddBranch('WRITTEN', HandleOf('A3'), 360, HandleOf('C'));
    for I := 0 to 2 do begin
      Raw := RawXRecord('A' + IntToStr(I + 1), 'C');
      Store.AddObjectCopy(I, Raw);
      Raw.Free;
    end;
    CheckInt(3, Store.ObjectCount, 'storage: objects');
    Check(Store.ContainsHandle(HandleOf('A3')) and (Store.IndexOfHandle(HandleOf('A2')) = 1),
      'storage: handle index');
    Check(RegisterNODHandler('HANDLED', 'XRECORD', @DummyLoad, nil, nil),
      'register handler HANDLED');
    try
      { Без шаблона ветки не пишутся }
      Session := TZNODSaveSession.Create(AC1021, Store);
      try
        Check(not Session.PreservedWritten[2], 'no template: branch is not written');
      finally
        Session.Free;
      end;

      Session := TZNODSaveSession.Create(AC1021, Store);
      try
        Check(Session.LoadTemplateText(TemplateObjects), 'LoadTemplateText');
        Check(Session.TemplateNODKeys.IndexOf('in_template') >= 0,
          'TemplateNODKeys (case-insensitive)');
        Check(not Session.PreservedWritten[0], 'key in template NOD: not written');
        Check(not Session.PreservedWritten[1], 'key with handler: not written');
        Check(Session.PreservedWritten[2], 'other key: written');
        Ctx := Default(TIODXFSaveContext);
        Ctx.handle := $100;
        Session.ReserveHandles(Drawing, Ctx);
        Check(Session.PreservedNewHandle(HandleOf('A3')) >= $100,
          'ReserveHandles: written branch object gets a new handle');
        Check((Session.PreservedNewHandle(HandleOf('A1')) = 0) and
          (Session.PreservedNewHandle(HandleOf('A2')) = 0),
          'ReserveHandles: objects of not written branches get no handle');
        Check(Ctx.handle > Session.PreservedNewHandle(HandleOf('A3')),
          'ReserveHandles: context handle advanced');
        Ins := Session.TakeNODInsertions('');
        Keys := '';
        for I := 0 to High(Ins) do
          if Ins[I].Key = 'WRITTEN' then
            Keys := Keys + Format('%s:%d:%s ', [Ins[I].Key, Ins[I].OwnershipCode,
              DXFHandleToStr(Ins[I].TargetHandle)])
          else
            Keys := Keys + Ins[I].Key + ' ';
        CheckEquals(Format('WRITTEN:360:%s', [DXFHandleToStr(Session.PreservedNewHandle(HandleOf('A3')))]),
          Trim(Keys), 'TakeNODInsertions: only written branch, its code and new handle');
        CheckInt(0, Length(Session.TakeNODInsertions('')), 'TakeNODInsertions: each entry once');
      finally
        Session.Free;
      end;
    finally
      UnregisterNODHandler('HANDLED');
    end;
  finally
    DoneDrawing(Drawing);
    Store.Free;
  end;
end;

begin
  Root := '';
  if ParamCount > 0 then
    Root := IncludeTrailingPathDelimiter(ParamStr(1));
  Failed := 0;
  Run('TestRegistry', @TestRegistry);
  if BuildSourceFile then begin
    Run('TestLoad', @TestLoad);
    Run('TestSave2007', @TestSave2007);
    Run('TestSave2000', @TestSave2000);
  end;
  Run('TestSessionSelect', @TestSessionSelect);

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
