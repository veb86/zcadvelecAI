program nodstage1;

// issue #1444: этап 1 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (модель OBJECTS/NOD, только чтение, без побочных эффектов). Тест проверяет:
//
//  1. Вспомогательные функции хэндлов (NormalizeDXFHandleStr,
//     TryDXFStrToHandle, DXFHandleToStr).
//  2. Разбор секции OBJECTS (uzeffdxfobjects) и поиск NOD (uzeffdxfnod) на
//     cad_source/test/tablestyleetalon.dxf, шаблонах savetemplate2000/2007
//     и empty.dxf: количество объектов, NOD = C, ключи NOD, целостность
//     ссылок владелец/словарь.
//  3. ExtractTableStyleDictionaryFromNOD (замена глобального поиска
//     ExtractTableStyleDictionary) на тех же файлах.
//  4. Синтетические файлы cad_source/zengine/tests/data/nod/nod_*.dxf
//     (генератор experiments/issue1444/gen_nod_testdata.py): NOD не первый;
//     NOD без ACAD_TABLESTYLE; 360 вместо 350, \r\n и пробелы у кодов;
//     неизвестные ключи и ACDB_RECOMPOSE_DATA; несколько корневых
//     словарей; нарушенная структура секции.
//  5. Одинаковый результат при переводах строк \n и \r\n.
//  6. Загрузка чертежа: модель RawObjectsSection совпадает с моделью файла;
//     с этапа 5 (issue #1452) DXFTableStyleTable заполняет NOD-обработчик
//     ACAD_TABLESTYLE (до этапа 5 таблица оставалась пустой).
//  7. Детальная трасса (модуль лога NOD) выключена по умолчанию; при
//     включении все сообщения форматируются без ошибок.
//  8. Новые модули не зависят от модулей слоя zcad.
//
// Использование:
//   nodstage1 [<корень репозитория>]

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzedrawingsimple, uzeffmanager,
  uzgldrawcontext, uzeconsts, uzeTypes,
  uzestylestablesdxf,
  uzeffdxfobjects, uzeffdxfnod, uzestylestablesdxfnod,
  uzbLogTypes, uzclog, uzeffdxfnodlog,
  // Регистрация загрузчика сущности ACAD_TABLE (как в nodstage0), чтобы
  // tablestyleetalon.dxf загружался так же, как в ZCAD.
  uzeacadtable_types, uzeacadtable_model, uzeacadtable_dxf_write;

const
  TemplatesDir = 'environment/runtimefiles/AllCPU-AllOS/common/cfg/components/';
  DataDir = 'cad_source/zengine/tests/data/nod/';
  EtalonFile = 'cad_source/test/tablestyleetalon.dxf';

type
  TRealFileCase = record
    FileName: string;
    ObjectCount: Integer;
    { Ключи NOD в порядке файла через запятую }
    NODKeys: string;
    { Хэндл словаря ACAD_TABLESTYLE ('' — ключа нет) }
    TableStyleDict: string;
    { Ожидаемое содержимое словаря: 'хэндл=имя' через запятую }
    TableStyles: string;
  end;

const
  AllNODKeys =
    'ACAD_COLOR,ACAD_GROUP,ACAD_LAYOUT,ACAD_MATERIAL,ACAD_MLINESTYLE,' +
    'ACAD_PLOTSETTINGS,ACAD_PLOTSTYLENAME,ACAD_TABLESTYLE,ACAD_VISUALSTYLE,' +
    'AcDbVariableDictionary';
  R2000NODKeys =
    'ACAD_GROUP,ACAD_LAYOUT,ACAD_MLINESTYLE,ACAD_PLOTSETTINGS,' +
    'ACAD_PLOTSTYLENAME';

  RealFiles: array[0..3] of TRealFileCase = (
    (FileName: EtalonFile; ObjectCount: 41; NODKeys: AllNODKeys;
     TableStyleDict: '86'; TableStyles: 'BE=aits,87=Standard,BA=vebts'),
    (FileName: TemplatesDir + 'savetemplate2000.dxf'; ObjectCount: 11;
     NODKeys: R2000NODKeys; TableStyleDict: ''; TableStyles: ''),
    (FileName: TemplatesDir + 'savetemplate2007.dxf'; ObjectCount: 39;
     NODKeys: AllNODKeys; TableStyleDict: '86'; TableStyles: '87=Standard'),
    (FileName: TemplatesDir + 'empty.dxf'; ObjectCount: 11;
     NODKeys: R2000NODKeys; TableStyleDict: ''; TableStyles: '')
  );

var
  Root: string;
  Failed: Integer;

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

procedure CheckHandle(AExpected, AActual: TDWGHandle; const S: string);
begin
  CheckEquals(DXFHandleToStr(AExpected), DXFHandleToStr(AActual), S);
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

{ Текст файла без изменений (переводы строк сохраняются). }
function ReadFileText(const AFileName: string): string;
var
  F: TFileStream;
begin
  F := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, F.Size);
    if F.Size > 0 then
      F.ReadBuffer(Result[1], F.Size);
  finally
    F.Free;
  end;
end;

{ Секция OBJECTS файла (0/SECTION … 0/ENDSEC) так же, как её сохраняет
  загрузчик в RawObjectsSection (строки через LineEnding). }
function ExtractObjectsSection(const AFileName: string): string;
var
  L: TStringList;
  I, J: Integer;
begin
  Result := '';
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    I := 0;
    while I + 3 < L.Count do begin
      if (Trim(L[I]) = '0') and (Trim(L[I + 1]) = 'SECTION') and
         (Trim(L[I + 2]) = '2') and (Trim(L[I + 3]) = 'OBJECTS') then begin
        J := I;
        while J + 1 < L.Count do begin
          Result := Result + L[J] + LineEnding + L[J + 1] + LineEnding;
          if (Trim(L[J]) = '0') and (Trim(L[J + 1]) = 'ENDSEC') then
            Exit;
          Inc(J, 2);
        end;
        Exit;
      end;
      Inc(I, 2);
    end;
  finally
    L.Free;
  end;
end;

{ Замена всех переводов строк на AEol. }
function ConvertEol(const AText, AEol: string): string;
begin
  Result := StringReplace(AText, #13#10, #10, [rfReplaceAll]);
  if AEol <> #10 then
    Result := StringReplace(Result, #10, AEol, [rfReplaceAll]);
end;

function DictKeys(ADict: TZDXFDictionary): string;
var
  I: Integer;
begin
  Result := '';
  if ADict = nil then
    Exit('<nil>');
  for I := 0 to ADict.Count - 1 do begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + ADict[I].Key;
  end;
end;

{ 'хэндл=имя' через запятую в порядке словаря }
function StyleMapText(AMap: TStringList): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AMap.Count - 1 do begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + AMap[I];
  end;
end;

function HandlesText(const AHandles: TZDXFHandleArray): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(AHandles) do begin
    if I > 0 then
      Result := Result + ',';
    Result := Result + DXFHandleToStr(AHandles[I]);
  end;
end;

function ObjectText(AObj: TZDXFRawObject): string;
begin
  if AObj = nil then
    Result := '<nil>'
  else
    Result := AObj.ObjType + ' ' + AObj.HandleStr;
end;

{ Загружает модель из текста; ошибка разбора — провал проверки. }
function LoadModel(const AText, AName: string): TZNODModel;
begin
  Result := TZNODModel.Create;
  if not Result.LoadFromText(AText) then
    Fail(AName + ': parse error: ' + Result.ParseError);
end;

function LoadModelFromFile(const AFileName: string): TZNODModel;
begin
  Result := LoadModel(ReadFileText(Root + DataDir + AFileName), AFileName);
end;

{ 1. Хэндлы }
procedure TestHandleHelpers;
var
  H: TDWGHandle;
begin
  CheckEquals('AB', NormalizeDXFHandleStr(' 00ab '), 'NormalizeDXFHandleStr('' 00ab '')');
  CheckEquals('0', NormalizeDXFHandleStr('000'), 'NormalizeDXFHandleStr(''000'')');
  CheckEquals('0', NormalizeDXFHandleStr('0'), 'NormalizeDXFHandleStr(''0'')');
  CheckEquals('', NormalizeDXFHandleStr(''), 'NormalizeDXFHandleStr('''')');
  Check(TryDXFStrToHandle('00c', H) and (H = $C), 'TryDXFStrToHandle(''00c'') = C');
  Check(TryDXFStrToHandle('FFFFFFFFFFFFFFFF', H) and (H = High(QWord)),
    'TryDXFStrToHandle(16 x F) = High(QWord)');
  Check(not TryDXFStrToHandle('', H) and (H = 0), 'TryDXFStrToHandle('''') = False');
  Check(not TryDXFStrToHandle('XYZ', H) and (H = 0), 'TryDXFStrToHandle(''XYZ'') = False');
  Check(not TryDXFStrToHandle('1FFFFFFFFFFFFFFFF', H),
    'TryDXFStrToHandle(17 digits) = False');
  CheckEquals('0', DXFHandleToStr(0), 'DXFHandleToStr(0)');
  CheckEquals('1A', DXFHandleToStr($1A), 'DXFHandleToStr($1A)');
end;

{ Проверки целостности: у каждого объекта, кроме NOD, владелец есть в
  секции; объекты записей словарей существуют и принадлежат словарю. }
procedure CheckIntegrity(AModel: TZNODModel; const AName: string);
var
  I, J, Errors: Integer;
  Obj, Target: TZDXFRawObject;
  Dict: TZDXFDictionary;
begin
  Errors := 0;
  for I := 0 to AModel.Objects.Count - 1 do begin
    Obj := AModel.Objects[I];
    if (AModel.NOD <> nil) and (Obj = AModel.NOD.RawObject) then
      Continue;
    if AModel.FindObject(Obj.OwnerHandle) = nil then begin
      Fail(Format('%s: owner %s of %s not found',
        [AName, DXFHandleToStr(Obj.OwnerHandle), ObjectText(Obj)]));
      Inc(Errors);
    end;
  end;
  for I := 0 to AModel.Dictionaries.Count - 1 do begin
    Dict := AModel.Dictionaries[I];
    for J := 0 to Dict.Count - 1 do begin
      Target := AModel.FindObject(Dict[J].TargetHandle);
      if Target = nil then begin
        Fail(Format('%s: %s/%s -> %s not found', [AName,
          Dict.RawObject.HandleStr, Dict[J].Key,
          DXFHandleToStr(Dict[J].TargetHandle)]));
        Inc(Errors);
      end else if Target.OwnerHandle <> Dict.Handle then begin
        Fail(Format('%s: owner of %s/%s (%s) is %s', [AName,
          Dict.RawObject.HandleStr, Dict[J].Key, ObjectText(Target),
          DXFHandleToStr(Target.OwnerHandle)]));
        Inc(Errors);
      end;
    end;
  end;
  if Errors = 0 then
    Ok(Format('%s: owners and dictionary entries are consistent (%d dictionaries)',
      [AName, AModel.Dictionaries.Count]));
end;

{ 2, 3. Реальные файлы }
procedure TestRealFile(const ACase: TRealFileCase);
var
  Model: TZNODModel;
  Map: TStringList;
  DictHandle: string;
  Found: Boolean;
  Name: string;
begin
  Name := ExtractFileName(ACase.FileName);
  Model := LoadModel(ExtractObjectsSection(Root + ACase.FileName), Name);
  Map := TStringList.Create;
  try
    CheckInt(ACase.ObjectCount, Model.Objects.Count, Name + ': object count');
    CheckInt(0, Model.DuplicateHandleCount, Name + ': duplicate handles');
    CheckInt(1, Model.RootDictionaryCount, Name + ': root dictionaries');
    if Model.NOD = nil then begin
      Fail(Name + ': NOD not found');
      Exit;
    end;
    CheckHandle($C, Model.NOD.Handle, Name + ': NOD handle');
    Check(Model.NOD.RawObject.HasOwnerGroup and (Model.NOD.OwnerHandle = 0),
      Name + ': NOD has 330=0');
    CheckEquals(ACase.NODKeys, DictKeys(Model.NOD), Name + ': NOD keys');
    Check(Model.ResolvePath('') = Model.NOD.RawObject,
      Name + ': ResolvePath('''') is NOD');
    CheckIntegrity(Model, Name);

    Found := ExtractTableStyleDictionaryFromNOD(Model, Map, DictHandle);
    Check(Found = (ACase.TableStyleDict <> ''),
      Name + ': ExtractTableStyleDictionaryFromNOD = ' + BoolToStr(Found, True));
    CheckEquals(ACase.TableStyleDict, DictHandle, Name + ': ACAD_TABLESTYLE handle');
    CheckEquals(ACase.TableStyles, StyleMapText(Map), Name + ': table styles');
    if Found then begin
      CheckEquals('TABLESTYLE 87', ObjectText(Model.ResolvePath('ACAD_TABLESTYLE/Standard')),
        Name + ': ResolvePath(ACAD_TABLESTYLE/Standard)');
      CheckEquals('TABLESTYLE 87', ObjectText(Model.ResolvePath('acad_tablestyle/STANDARD')),
        Name + ': ResolvePath is case-insensitive');
      CheckEquals('86', HandlesText(Model.ResolvePath('ACAD_TABLESTYLE/Standard').Reactors),
        Name + ': Standard reactors');
      CheckEquals('<nil>', ObjectText(Model.ResolvePath('ACAD_TABLESTYLE/Standard/X')),
        Name + ': path through non-dictionary');
    end;
    CheckEquals('<nil>', ObjectText(Model.ResolvePath('NO_SUCH_KEY')),
      Name + ': ResolvePath(NO_SUCH_KEY)');
  finally
    Map.Free;
    Model.Free;
  end;
end;

procedure TestRealFiles;
var
  I: Integer;
begin
  for I := Low(RealFiles) to High(RealFiles) do
    try
      TestRealFile(RealFiles[I]);
    except
      on E: Exception do
        Fail(RealFiles[I].FileName + ': ' + E.ClassName + ': ' + E.Message);
    end;
end;

{ Эталон: имена стилей по NOD совпадают с результатом загрузчика
  LoadTableStylesFromNOD (этап 5; до него сравнение велось со старым
  разбором ReadTableStylesFromDXFObjects, удалённым на этапе 5). }
procedure TestEtalonMatchesLoader;
var
  Section: string;
  Model: TZNODModel;
  Map: TStringList;
  DictHandle: string;
  Styles: GDBDXFTableStyleArray;
  I: Integer;
  Style: PTGDBDXFTableStyle;
begin
  Section := ExtractObjectsSection(Root + EtalonFile);
  Model := LoadModel(Section, 'tablestyleetalon.dxf');
  Map := TStringList.Create;
  Styles.init(10);
  try
    ExtractTableStyleDictionaryFromNOD(Model, Map, DictHandle);
    LoadTableStylesFromNOD(Model, Styles);
    CheckInt(Map.Count, Styles.Count, 'etalon: loader style count equals NOD');
    for I := 0 to Map.Count - 1 do begin
      Style := Styles.GetStyleByHandle(Map.Names[I]);
      if Style = nil then
        Fail('etalon: loader has no style ' + Map[I])
      else
        CheckEquals(Map.ValueFromIndex[I], Style^.Name,
          'etalon: loader style ' + Map.Names[I]);
    end;
  finally
    Styles.Done;
    Map.Free;
    Model.Free;
  end;
end;

{ 4. NOD не первый; сторонний словарь с ключом ACAD_TABLESTYLE до NOD. }
procedure TestNODNotFirst;
var
  Model: TZNODModel;
  Map: TStringList;
  DictHandle: string;
  Styles: GDBDXFTableStyleArray;
begin
  Model := LoadModelFromFile('nod_not_first.dxf');
  Map := TStringList.Create;
  Styles.init(10);
  try
    CheckInt(8, Model.Objects.Count, 'not_first: object count');
    Check((Model.NOD <> nil) and (Model.Objects.IndexOf(Model.NOD.RawObject) = 3),
      'not_first: NOD is the 4th object');
    CheckHandle($C, Model.NOD.Handle, 'not_first: NOD handle');
    CheckEquals('ACAD_GROUP,ACAD_TABLESTYLE,THIRD_PARTY', DictKeys(Model.NOD),
      'not_first: NOD keys');
    Check(ExtractTableStyleDictionaryFromNOD(Model, Map, DictHandle),
      'not_first: ExtractTableStyleDictionaryFromNOD = True');
    CheckEquals('40', DictHandle, 'not_first: ACAD_TABLESTYLE handle');
    CheckEquals('41=Standard,42=ZCAD1444', StyleMapText(Map), 'not_first: table styles');
    CheckEquals('XRECORD 21', ObjectText(Model.ResolvePath('THIRD_PARTY/ACAD_TABLESTYLE')),
      'not_first: THIRD_PARTY/ACAD_TABLESTYLE is the decoy XRECORD');
    CheckIntegrity(Model, 'not_first');

    // Старый глобальный поиск 3/ACAD_TABLESTYLE (удалён на этапе 5) находил
    // словарь 20 (не NOD) и не читал стили; загрузчик по NOD читает оба.
    CheckInt(2, LoadTableStylesFromNOD(Model, Styles),
      'not_first: loader reads the styles of NOD/ACAD_TABLESTYLE');
    Check((Styles.GetStyleByHandle('41') <> nil)
      and (Styles.GetStyleByHandle('41')^.Name = 'Standard')
      and (Styles.GetStyleByHandle('42') <> nil)
      and (Styles.GetStyleByHandle('42')^.Name = 'ZCAD1444'),
      'not_first: loader styles 41=Standard, 42=ZCAD1444');
  finally
    Styles.Done;
    Map.Free;
    Model.Free;
  end;
end;

{ NOD без ACAD_TABLESTYLE; пустой словарь. }
procedure TestNODWithoutTableStyle;
var
  Model: TZNODModel;
  Map: TStringList;
  DictHandle: string;
begin
  Model := LoadModelFromFile('nod_no_tablestyle.dxf');
  Map := TStringList.Create;
  try
    CheckEquals('ACAD_GROUP,ACAD_MLINESTYLE', DictKeys(Model.NOD), 'no_tablestyle: NOD keys');
    Check(not ExtractTableStyleDictionaryFromNOD(Model, Map, DictHandle),
      'no_tablestyle: ExtractTableStyleDictionaryFromNOD = False');
    CheckEquals('', DictHandle, 'no_tablestyle: ACAD_TABLESTYLE handle');
    CheckInt(0, Map.Count, 'no_tablestyle: table styles');
    CheckInt(0, Model.ResolveDictionary('ACAD_GROUP').Count, 'no_tablestyle: ACAD_GROUP entries');
    CheckEquals('MLINESTYLE 18', ObjectText(Model.ResolvePath('ACAD_MLINESTYLE/Standard')),
      'no_tablestyle: ACAD_MLINESTYLE/Standard');
    Check(Model.ResolveDictionary('ACAD_MLINESTYLE/Standard') = nil,
      'no_tablestyle: MLINESTYLE is not a dictionary');
  finally
    Map.Free;
    Model.Free;
  end;
end;

{ 360 вместо 350, 280=1, \r\n, пробелы у кодов, хэндлы с нулями и в
  нижнем регистре, расширенный словарь. }
procedure TestHardOwner360;
var
  Model: TZNODModel;
  Map: TStringList;
  DictHandle: string;
  Style: TZDXFRawObject;
  XDict: TZDXFDictionary;
begin
  Model := LoadModelFromFile('nod_hardowner_360.dxf');
  Map := TStringList.Create;
  try
    CheckInt(6, Model.Objects.Count, 'hardowner_360: object count');
    CheckHandle($C, Model.NOD.Handle, 'hardowner_360: NOD handle (00c)');
    CheckInt(1, Model.NOD.HardOwner, 'hardowner_360: NOD 280');
    CheckInt(1, Model.NOD.CloningFlag, 'hardowner_360: NOD 281');
    CheckEquals('ACAD_TABLESTYLE,ACAD_GROUP', DictKeys(Model.NOD), 'hardowner_360: NOD keys');
    CheckInt(360, Model.NOD[0].OwnershipCode, 'hardowner_360: NOD entry group');
    CheckHandle($40, Model.NOD[0].TargetHandle, 'hardowner_360: ACAD_TABLESTYLE (0040)');
    CheckHandle($D, Model.NOD[1].TargetHandle, 'hardowner_360: ACAD_GROUP (d)');
    Check(ExtractTableStyleDictionaryFromNOD(Model, Map, DictHandle),
      'hardowner_360: ExtractTableStyleDictionaryFromNOD = True');
    CheckEquals('4A=Standard', StyleMapText(Map), 'hardowner_360: table styles');
    Style := Model.ResolvePath('ACAD_TABLESTYLE/Standard');
    CheckEquals('TABLESTYLE 4A', ObjectText(Style), 'hardowner_360: ACAD_TABLESTYLE/Standard');
    if Style <> nil then begin
      CheckHandle($40, Style.OwnerHandle, 'hardowner_360: style owner');
      CheckEquals('40', HandlesText(Style.Reactors), 'hardowner_360: style reactors');
      CheckHandle($4B, Style.XDictHandle, 'hardowner_360: style xdictionary');
      XDict := Model.FindDictionary(Style.XDictHandle);
      CheckEquals('ACAD_XREC_ROUNDTRIP', DictKeys(XDict), 'hardowner_360: xdictionary keys');
    end;
    CheckIntegrity(Model, 'hardowner_360');
  finally
    Map.Free;
    Model.Free;
  end;
end;

{ Неизвестные ключи, ACDB_RECOMPOSE_DATA, данные XRECORD с 330/360/102,
  битая ссылка, ключ без хэндла, ACDBDICTIONARYWDFLT. }
procedure TestUnknownKeys;
var
  Model: TZNODModel;
  Map: TStringList;
  DictHandle: string;
  Data: TZDXFRawObject;
  Plugin, PlotStyles: TZDXFDictionary;
begin
  Model := LoadModelFromFile('nod_unknown_keys.dxf');
  Map := TStringList.Create;
  try
    CheckEquals('ACAD_PLOTSTYLENAME,ACAD_TABLESTYLE,ACDB_RECOMPOSE_DATA,BROKEN_LINK,MY_PLUGIN',
      DictKeys(Model.NOD), 'unknown_keys: NOD keys');
    CheckEquals('XRECORD 60', ObjectText(Model.ResolvePath('ACDB_RECOMPOSE_DATA')),
      'unknown_keys: ACDB_RECOMPOSE_DATA');
    CheckEquals('<nil>', ObjectText(Model.ResolvePath('BROKEN_LINK')),
      'unknown_keys: BROKEN_LINK (missing object)');
    Check(Model.NOD.IndexOfKey('broken_link') = 3, 'unknown_keys: IndexOfKey is case-insensitive');

    Plugin := Model.ResolveDictionary('MY_PLUGIN');
    CheckEquals('Settings,Data', DictKeys(Plugin), 'unknown_keys: MY_PLUGIN keys (key without handle skipped)');
    if (Plugin <> nil) and (Plugin.Count = 2) then begin
      CheckInt(350, Plugin[0].OwnershipCode, 'unknown_keys: MY_PLUGIN/Settings group');
      CheckInt(360, Plugin[1].OwnershipCode, 'unknown_keys: MY_PLUGIN/Data group');
    end;
    Data := Model.ResolvePath('MY_PLUGIN/Data');
    CheckEquals('XRECORD 72', ObjectText(Data), 'unknown_keys: MY_PLUGIN/Data');
    if Data <> nil then begin
      // 330, 360 и 102-блок после группы 100 — данные XRECORD
      CheckHandle($70, Data.OwnerHandle, 'unknown_keys: Data owner');
      CheckEquals('70', HandlesText(Data.Reactors), 'unknown_keys: Data reactors');
      CheckHandle(0, Data.XDictHandle, 'unknown_keys: Data xdictionary');
      Check(Data.IndexOfCode(360) >= 0, 'unknown_keys: Data keeps 360 in Pairs');
    end;

    PlotStyles := Model.ResolveDictionary('ACAD_PLOTSTYLENAME');
    Check((PlotStyles <> nil) and SameText(PlotStyles.RawObject.ObjType,
      CDXFDictionaryWithDefaultObjType), 'unknown_keys: ACAD_PLOTSTYLENAME is ACDBDICTIONARYWDFLT');
    if PlotStyles <> nil then begin
      CheckHandle($F, PlotStyles.DefaultHandle, 'unknown_keys: WDFLT default (340)');
      CheckEquals('ACDBPLACEHOLDER F', ObjectText(Model.ResolvePath('ACAD_PLOTSTYLENAME/Normal')),
        'unknown_keys: ACAD_PLOTSTYLENAME/Normal');
    end;

    Check(ExtractTableStyleDictionaryFromNOD(Model, Map, DictHandle),
      'unknown_keys: empty ACAD_TABLESTYLE found');
    CheckInt(0, Map.Count, 'unknown_keys: table styles');
  finally
    Map.Free;
    Model.Free;
  end;
end;

{ Два словаря с 330=0: берётся первый. }
procedure TestMultipleRoots;
var
  Model: TZNODModel;
  Map: TStringList;
  DictHandle: string;
begin
  Model := LoadModelFromFile('nod_multiple_roots.dxf');
  Map := TStringList.Create;
  try
    CheckInt(2, Model.RootDictionaryCount, 'multiple_roots: root dictionaries');
    CheckHandle($C, Model.NOD.Handle, 'multiple_roots: NOD is the first root');
    Check(ExtractTableStyleDictionaryFromNOD(Model, Map, DictHandle),
      'multiple_roots: ExtractTableStyleDictionaryFromNOD = True');
    CheckEquals('41=Standard', StyleMapText(Map), 'multiple_roots: table styles');
  finally
    Map.Free;
    Model.Free;
  end;
end;

{ Нарушенная структура: код группы «XYZ». }
procedure TestBroken;
var
  Model: TZNODModel;
begin
  Model := TZNODModel.Create;
  try
    Check(not Model.LoadFromText(ReadFileText(Root + DataDir + 'nod_broken.dxf')),
      'broken: LoadFromText = False');
    Check(Pos('line 23', Model.ParseError) > 0, 'broken: ParseError "' + Model.ParseError + '"');
    Check(Model.Objects.Count >= 1, Format('broken: %d object(s) parsed before the error',
      [Model.Objects.Count]));
    Check(Model.LoadFromText(''), 'broken: model reloads from empty text');
    CheckInt(0, Model.Objects.Count, 'empty text: object count');
    Check(Model.NOD = nil, 'empty text: no NOD');
    Check(Model.ResolvePath('ACAD_TABLESTYLE') = nil, 'empty text: ResolvePath = nil');
  finally
    Model.Free;
  end;
end;

{ ClaimHandle }
procedure TestClaimedHandles;
var
  Model: TZNODModel;
begin
  Model := LoadModelFromFile('nod_not_first.dxf');
  try
    CheckInt(0, Model.ClaimedHandleCount, 'claim: initially empty');
    Model.ClaimHandle($40);
    Model.ClaimHandle($40);
    Model.ClaimHandle($41);
    CheckInt(2, Model.ClaimedHandleCount, 'claim: two handles');
    Check(Model.IsHandleClaimed($40) and not Model.IsHandleClaimed($42),
      'claim: IsHandleClaimed');
    Model.LoadFromText('');
    CheckInt(0, Model.ClaimedHandleCount, 'claim: cleared on reload');
  finally
    Model.Free;
  end;
end;

{ Суммарное описание модели для сравнения разборов. }
function ModelSignature(AModel: TZNODModel): string;
var
  I, J: Integer;
  Obj: TZDXFRawObject;
begin
  Result := Format('objects=%d dicts=%d roots=%d', [AModel.Objects.Count,
    AModel.Dictionaries.Count, AModel.RootDictionaryCount]);
  for I := 0 to AModel.Objects.Count - 1 do begin
    Obj := AModel.Objects[I];
    Result := Result + Format('|%s %s o=%s r=%s x=%s n=%d', [Obj.ObjType,
      Obj.HandleStr, DXFHandleToStr(Obj.OwnerHandle), HandlesText(Obj.Reactors),
      DXFHandleToStr(Obj.XDictHandle), Obj.PairCount]);
    for J := 0 to Obj.PairCount - 1 do
      Result := Result + Format(';%d=%s', [Obj.Pairs[J].Code, Obj.Pairs[J].Value]);
  end;
end;

{ 5. \n и \r\n дают одинаковую модель. }
procedure TestLineEndings;
const
  Files: array[0..2] of string = (
    'nod_not_first.dxf', 'nod_hardowner_360.dxf', 'nod_unknown_keys.dxf');
var
  I: Integer;
  Text, SigLF, SigCRLF: string;
  Model: TZNODModel;
begin
  for I := Low(Files) to High(Files) do begin
    Text := ReadFileText(Root + DataDir + Files[I]);
    Model := LoadModel(ConvertEol(Text, #10), Files[I] + ' (LF)');
    try
      SigLF := ModelSignature(Model);
    finally
      Model.Free;
    end;
    Model := LoadModel(ConvertEol(Text, #13#10), Files[I] + ' (CRLF)');
    try
      SigCRLF := ModelSignature(Model);
    finally
      Model.Free;
    end;
    Check(SigLF = SigCRLF, Files[I] + ': LF and CRLF give the same model');
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

{ 6. Загрузка чертежа: RawObjectsSection разбирается в ту же модель, что
  и секция файла. До этапа 5 таблица стилей DXF после загрузки была пуста;
  с этапа 5 её заполняет NOD-обработчик ACAD_TABLESTYLE (3 стиля эталона). }
procedure TestLoadUnchanged;
var
  Drawing: TSimpleDrawing;
  FromDrawing, FromFile: TZNODModel;
begin
  Drawing.init(nil);
  FromDrawing := nil;
  FromFile := nil;
  try
    LoadDrawing(Root + EtalonFile, Drawing);
    CheckInt(3, Drawing.DXFTableStyleTable.Count,
      'load: DXFTableStyleTable holds the 3 etalon styles after AddFromDXF');
    Check(Drawing.RawObjectsSection <> '', 'load: RawObjectsSection is filled');
    FromDrawing := LoadModel(Drawing.RawObjectsSection, 'RawObjectsSection');
    FromFile := LoadModel(ExtractObjectsSection(Root + EtalonFile), 'etalon');
    Check(ModelSignature(FromDrawing) = ModelSignature(FromFile),
      'load: model of RawObjectsSection equals model of the file');
  finally
    FromFile.Free;
    FromDrawing.Free;
    Drawing.done;
  end;
end;

{ Текст секции uses интерфейса и реализации модуля, в нижнем регистре. }
function AllUses(const AFileName: string): string;
var
  S: TStringList;
  Text: string;
  P, E: Integer;
begin
  Result := '';
  S := TStringList.Create;
  try
    S.LoadFromFile(AFileName);
    Text := LowerCase(S.Text);
  finally
    S.Free;
  end;
  P := Pos('uses', Text);
  while P > 0 do begin
    E := Pos(';', Copy(Text, P, MaxInt)) + P - 1;
    Result := Result + Copy(Text, P, E - P) + ' ';
    Text := Copy(Text, E + 1, MaxInt);
    P := Pos(LineEnding + 'uses', Text);
  end;
end;

{ 7. Модуль лога NOD выключен по умолчанию; при включённом модуле разбор
  всех файлов проходит без ошибок форматирования сообщений трассы и
  предупреждений. }
procedure TestTraceLog;
const
  Files: array[0..5] of string = (
    'nod_not_first.dxf', 'nod_no_tablestyle.dxf', 'nod_hardowner_360.dxf',
    'nod_unknown_keys.dxf', 'nod_multiple_roots.dxf', 'nod_broken.dxf');
var
  I: Integer;
  Model: TZNODModel;
  Map: TStringList;
  DictHandle: string;
  OldLevel: TLogLevel;
begin
  Check(not programlog.isModuleEnabled(NODLogModuleId),
    'log: module ' + NOD_LOG_MODULE_NAME + ' is disabled by default');
  OldLevel := programlog.GetCurrentLogLevel;
  programlog.EnableModule(NODLogModuleId);
  programlog.SetCurrentLogLevel(LM_Info, True);
  Model := TZNODModel.Create;
  Map := TStringList.Create;
  try
    for I := Low(Files) to High(Files) do begin
      Model.LoadFromText(ReadFileText(Root + DataDir + Files[I]));
      ExtractTableStyleDictionaryFromNOD(Model, Map, DictHandle);
    end;
    Model.LoadFromText(ExtractObjectsSection(Root + EtalonFile));
    ExtractTableStyleDictionaryFromNOD(Model, Map, DictHandle);
    Ok('log: trace messages are formatted without errors');
  finally
    Map.Free;
    Model.Free;
    programlog.DisableModule(NODLogModuleId);
    programlog.SetCurrentLogLevel(OldLevel, True);
  end;
end;

{ 8. Новые модули zengine не используют модули слоя zcad (uzc*),
  кроме модуля лога uzclog (общий programlog движка) и контейнеров
  uzctnr* (пакет zcontainers, не слой zcad). }
procedure TestNoZcadDependency;
const
  Units: array[0..3] of string = (
    'cad_source/zengine/fileformats/uzeffdxfobjects.pas',
    'cad_source/zengine/fileformats/uzeffdxfnod.pas',
    'cad_source/zengine/fileformats/uzeffdxfnodlog.pas',
    'cad_source/zengine/styles/uzestylestablesdxfnod.pas');
var
  I: Integer;
  UsesText, Rest: string;
begin
  for I := Low(Units) to High(Units) do begin
    UsesText := AllUses(Root + Units[I]);
    Rest := StringReplace(UsesText, 'uzclog', '', [rfReplaceAll]);
    Rest := StringReplace(Rest, 'uzctnr', '', [rfReplaceAll]);
    if UsesText = '' then
      Fail('cannot find uses clause in ' + Units[I])
    else if (Pos('uzc', Rest) > 0) or (Pos('uzeffdxf,', Rest + ',') > 0) then
      Fail(Units[I] + ' depends on zcad units or uzeffdxf: ' + UsesText)
    else
      Ok(ExtractFileName(Units[I]) + ' does not depend on zcad units');
  end;
end;

var
  I: Integer;
begin
  Root := '';
  for I := 1 to ParamCount do
    Root := IncludeTrailingPathDelimiter(ParamStr(I));
  Failed := 0;

  Run('TestHandleHelpers', @TestHandleHelpers);
  Run('TestRealFiles', @TestRealFiles);
  Run('TestEtalonMatchesLoader', @TestEtalonMatchesLoader);
  Run('TestNODNotFirst', @TestNODNotFirst);
  Run('TestNODWithoutTableStyle', @TestNODWithoutTableStyle);
  Run('TestHardOwner360', @TestHardOwner360);
  Run('TestUnknownKeys', @TestUnknownKeys);
  Run('TestMultipleRoots', @TestMultipleRoots);
  Run('TestBroken', @TestBroken);
  Run('TestClaimedHandles', @TestClaimedHandles);
  Run('TestLineEndings', @TestLineEndings);
  Run('TestLoadUnchanged', @TestLoadUnchanged);
  Run('TestTraceLog', @TestTraceLog);
  Run('TestNoZcadDependency', @TestNoZcadDependency);

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
