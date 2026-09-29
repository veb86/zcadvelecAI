program nodstage9;

// issue #1459: этап 9 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md
// (NOD при загрузке DWG). LibreDWG в тестах не подключается: объекты DWG
// строятся в памяти (записи dwg.pp, как в fpdwg/tests/uzedwgtestdwgproc).
// Тест проверяет:
//
//  1. Модель NOD по объектам DWG (BuildDWGNODModel): NOD — словарь из
//     заголовка (header_vars.DICTIONARY_NAMED_OBJECT), даже если он не
//     первый и есть другие словари без владельца; сущности в модель не
//     попадают, прочие объекты — заглушками (тип, хэндл, владелец);
//     индекс символьных таблиц — LTYPE, STYLE, BLOCK_RECORD; имена в
//     кодировке заголовка переводятся в UTF-8.
//  2. Группы DXF объектов DICTIONARY (350/360, 280, 281, запись без
//     хэндла пропускается), ACDBDICTIONARYWDFLT (340), TABLESTYLE (R2004:
//     все группы; R2010+: только 3 и 70) и MLEADERSTYLE (179, 271–273, 298
//     по версии) — эталонные тексты.
//  3. Загрузка в чертёж (DWGNODLoad) теми же обработчиками, что для DXF:
//     стили таблиц (отступы, флаги, имена текстовых стилей ячеек, хэндл
//     объекта, расширенный словарь) и мультивыносок (имена записей
//     340–343, упакованные цвета); забраны объекты стилей, их словари и
//     CELLSTYLEMAP, но не объекты неизвестных веток.
//  4. Хэндл NOD из заголовка не указывает на словарь — NOD ищется по
//     владельцу (330=0); словарей нет — BuildDWGNODModel = False, а
//     DWGNODLoad всё равно создаёт стили Standard (EnsureDefaults).
//  5. Загрузчик DWG вызывает DWGNODLoad (uzedwgimport, фаза
//     dwg-import.scan.nod); nodtests.sh собирает fpdwg (dwg.pp) для тестов;
//     в ТЗ — раздел и статус этапа 9.
//
// Использование:
//   nodstage9 [<корень репозитория>]

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  gzctnrVectorTypes,
  dwg,
  uzedrawingsimple, uzeTypes,
  uzeffdxfobjects, uzeffdxfnod, uzeffdxfnodregistry,
  uzestylestablesdxf, uzestylestablesdxfnod,
  uzestylesmleaderdxf, uzestylesmleaderdxfnod,
  uzedwgnod;

const
  ImportFile = 'cad_source/zengine/fileformats/dwg/uzedwgimport.pas';
  NodTestsScript = 'cad_source/zengine/tests/nodtests.sh';
  TZFile = 'cad_source/zengine/TZ_NOD_NamedObjectDictionary.md';

  { Кодовая страница заголовка DWG (индекс LibreDWG): ANSI_1251 }
  CodePage1251 = 29;

  { Хэндлы образца }
  HNOD = $C;
  HDecoy = $9;
  HTableStyles = $20;
  HTableStyleStd = $21;
  HTableStyleRus = $22;
  HMLeaderStyles = $23;
  HGroups = $24;
  HUnknown = $25;
  HXDict = $26;
  HCellStyleMap = $27;
  HMLeaderStyleStd = $28;
  HLine = $40;
  HLTypeByBlock = $14;
  HLTypeByLayer = $15;
  HStyleStd = $11;
  HStyleRus = $12;
  HBlockModel = $1F;
  HBlockDot = $30;

type
  PDWGTableStyle = ^Dwg_Object_TABLESTYLE;
  PDWGMLeaderStyle = ^Dwg_Object_MLEADERSTYLE;

  { Объекты DWG в памяти: массивы фиксированного размера (указатели на
    элементы не меняются), остальные блоки — GetMem с обнулением. }
  TDWGFixture = class
  private
    FBlocks: TList;
    FTexts: array of RawByteString;
    FUTexts: array of UnicodeString;
  public
    Data: Dwg_Data;
    Objects: array[0..63] of Dwg_Object;
    Inners: array[0..63] of Dwg_Object_Object;
    Count: Integer;
    constructor Create(AVersion: DWG_VERSION_TYPE; ACodePage: Integer = CodePage1251);
    destructor Destroy; override;
    function Alloc(ASize: PtrInt): Pointer;
    function Ref(AHandle: QWord): BITCODE_H;
    { Строка DWG: до R2007 — байты кодовой страницы заголовка, R2007+ —
      UTF-16 (AText в UTF-8) }
    function Text(const AText: RawByteString): BITCODE_T;
    function AddObject(AType: DWG_OBJECT_TYPE; AHandle, AOwner: QWord;
      const ADXFName: string = ''): Integer;
    function AddEntity(AType: DWG_OBJECT_TYPE; AHandle: QWord): Integer;
    procedure AddDictionary(AHandle, AOwner: QWord;
      const AKeys: array of RawByteString; const AItems: array of QWord;
      AHardOwner: Byte = 0);
    function AddDictionaryWithDefault(AHandle, AOwner, ADefault: QWord;
      const AKeys: array of RawByteString; const AItems: array of QWord): Integer;
    procedure AddSymbol(AType: DWG_OBJECT_TYPE; AHandle: QWord;
      const AName: RawByteString);
    function AddTableStyle(AHandle, AOwner: QWord; const AName: RawByteString;
      const ATextStyles: array of QWord): PDWGTableStyle;
    function AddMLeaderStyle(AHandle, AOwner: QWord): PDWGMLeaderStyle;
    procedure SetXDict(AIndex: Integer; AHandle: QWord);
    procedure SetReactors(AIndex: Integer; const AHandles: array of QWord);
    procedure SetHeaderNOD(AHandle: QWord);
    function IsUnicode: Boolean;
  end;

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

{ ---------- TDWGFixture ---------- }

constructor TDWGFixture.Create(AVersion: DWG_VERSION_TYPE; ACodePage: Integer);
begin
  inherited Create;
  FBlocks := TList.Create;
  FillChar(Data, SizeOf(Data), 0);
  FillChar(Objects, SizeOf(Objects), 0);
  FillChar(Inners, SizeOf(Inners), 0);
  Data.header.version := AVersion;
  Data.header.from_version := AVersion;
  Data.header.codepage := ACodePage;
  Data.&object := @Objects[0];
end;

destructor TDWGFixture.Destroy;
var
  I: Integer;
begin
  for I := 0 to FBlocks.Count - 1 do
    FreeMem(FBlocks[I]);
  FBlocks.Free;
  inherited Destroy;
end;

function TDWGFixture.Alloc(ASize: PtrInt): Pointer;
begin
  Result := GetMem(ASize);
  FillChar(Result^, ASize, 0);
  FBlocks.Add(Result);
end;

function TDWGFixture.Ref(AHandle: QWord): BITCODE_H;
begin
  if AHandle = 0 then
    Exit(nil);
  Result := Alloc(SizeOf(Dwg_Object_Ref));
  Result^.absolute_ref := AHandle;
  Result^.handleref.value := AHandle;
end;

function TDWGFixture.IsUnicode: Boolean;
begin
  Result := Data.header.version >= R_2007;
end;

function TDWGFixture.Text(const AText: RawByteString): BITCODE_T;
begin
  if IsUnicode then begin
    SetLength(FUTexts, Length(FUTexts) + 1);
    FUTexts[High(FUTexts)] := UTF8Decode(AText);
    if FUTexts[High(FUTexts)] = '' then
      Exit(BITCODE_T(PUnicodeChar(#0)));
    Result := BITCODE_T(PUnicodeChar(FUTexts[High(FUTexts)]));
  end else begin
    SetLength(FTexts, Length(FTexts) + 1);
    FTexts[High(FTexts)] := AText;
    Result := BITCODE_T(PAnsiChar(FTexts[High(FTexts)]));
  end;
end;

function TDWGFixture.AddObject(AType: DWG_OBJECT_TYPE; AHandle, AOwner: QWord;
  const ADXFName: string): Integer;
begin
  if Count > High(Objects) then
    raise Exception.Create('fixture: too many objects');
  Result := Count;
  Inc(Count);
  Data.num_objects := Count;
  Objects[Result].index := Result;
  Objects[Result].fixedtype := AType;
  Objects[Result].supertype := DWG_SUPERTYPE_OBJECT;
  Objects[Result].handle.value := AHandle;
  Objects[Result].parent := @Data;
  Objects[Result].tio.&object := @Inners[Result];
  if ADXFName <> '' then
    Objects[Result].dxfname := PAnsiChar(Text(ADXFName));
  Inners[Result].objid := Result;
  Inners[Result].ownerhandle := Ref(AOwner);
end;

function TDWGFixture.AddEntity(AType: DWG_OBJECT_TYPE; AHandle: QWord): Integer;
var
  Ent: ^Dwg_Object_Entity;
begin
  Result := Count;
  Inc(Count);
  Data.num_objects := Count;
  Ent := Alloc(SizeOf(Dwg_Object_Entity));
  Objects[Result].index := Result;
  Objects[Result].fixedtype := AType;
  Objects[Result].supertype := DWG_SUPERTYPE_ENTITY;
  Objects[Result].handle.value := AHandle;
  Objects[Result].parent := @Data;
  Objects[Result].tio.entity := Ent;
end;

{ Массивы texts / itemhandles словаря }
procedure FillItems(AFixture: TDWGFixture; const AKeys: array of RawByteString;
  const AItems: array of QWord; out ATexts: Pointer; out ARefs: Pointer);
var
  I: Integer;
  T: ^BITCODE_T;
  R: ^BITCODE_H;
begin
  ATexts := AFixture.Alloc(SizeOf(BITCODE_T) * (Length(AKeys) + 1));
  ARefs := AFixture.Alloc(SizeOf(BITCODE_H) * (Length(AKeys) + 1));
  T := ATexts;
  R := ARefs;
  for I := 0 to High(AKeys) do begin
    T[I] := AFixture.Text(AKeys[I]);
    R[I] := AFixture.Ref(AItems[I]);
  end;
end;

procedure TDWGFixture.AddDictionary(AHandle, AOwner: QWord;
  const AKeys: array of RawByteString; const AItems: array of QWord;
  AHardOwner: Byte);
var
  I: Integer;
  D: ^Dwg_Object_DICTIONARY;
  T, R: Pointer;
begin
  I := AddObject(DWG_TYPE_DICTIONARY, AHandle, AOwner);
  D := Alloc(SizeOf(Dwg_Object_DICTIONARY));
  D^.parent := @Inners[I];
  D^.numitems := Length(AKeys);
  D^.is_hardowner := AHardOwner;
  D^.cloning := 1;
  FillItems(Self, AKeys, AItems, T, R);
  D^.texts := T;
  D^.itemhandles := R;
  Inners[I].tio.DICTIONARY := D;
end;

function TDWGFixture.AddDictionaryWithDefault(AHandle, AOwner, ADefault: QWord;
  const AKeys: array of RawByteString; const AItems: array of QWord): Integer;
var
  D: ^Dwg_Object_DICTIONARYWDFLT;
  T, R: Pointer;
begin
  Result := AddObject(DWG_TYPE_DICTIONARYWDFLT, AHandle, AOwner);
  D := Alloc(SizeOf(Dwg_Object_DICTIONARYWDFLT));
  D^.parent := @Inners[Result];
  D^.numitems := Length(AKeys);
  D^.cloning := 1;
  FillItems(Self, AKeys, AItems, T, R);
  D^.texts := T;
  D^.itemhandles := R;
  D^.defaultid := Ref(ADefault);
  Inners[Result].tio.DICTIONARYWDFLT := D;
end;

procedure TDWGFixture.AddSymbol(AType: DWG_OBJECT_TYPE; AHandle: QWord;
  const AName: RawByteString);
var
  I: Integer;
begin
  I := AddObject(AType, AHandle, 0);
  { У STYLE, LTYPE и BLOCK_HEADER поле name на одном месте (после flag) }
  case AType of
    DWG_TYPE_STYLE: begin
      Inners[I].tio.STYLE := Alloc(SizeOf(Dwg_Object_STYLE));
      Inners[I].tio.STYLE^.name := Text(AName);
    end;
    DWG_TYPE_LTYPE: begin
      Inners[I].tio.LTYPE := Alloc(SizeOf(Dwg_Object_LTYPE));
      Inners[I].tio.LTYPE^.name := Text(AName);
    end;
    DWG_TYPE_BLOCK_HEADER: begin
      Inners[I].tio.BLOCK_HEADER := Alloc(SizeOf(Dwg_Object_BLOCK_HEADER));
      Inners[I].tio.BLOCK_HEADER^.name := Text(AName);
    end;
  end;
end;

function TDWGFixture.AddTableStyle(AHandle, AOwner: QWord;
  const AName: RawByteString; const ATextStyles: array of QWord): PDWGTableStyle;
var
  I, R, B: Integer;
  Rows: ^Dwg_TABLESTYLE_rowstyles;
  Borders: ^Dwg_TABLESTYLE_border;
begin
  I := AddObject(DWG_TYPE_TABLESTYLE, AHandle, AOwner);
  Result := Alloc(SizeOf(Dwg_Object_TABLESTYLE));
  Result^.parent := @Inners[I];
  Result^.name := Text(AName);
  Result^.num_rowstyles := Length(ATextStyles);
  Rows := Alloc(SizeOf(Dwg_TABLESTYLE_rowstyles) * Length(ATextStyles));
  Result^.rowstyles := Rows;
  for R := 0 to High(ATextStyles) do begin
    Rows[R].text_style := Ref(ATextStyles[R]);
    Rows[R].num_borders := 6;
    Borders := Alloc(SizeOf(Dwg_TABLESTYLE_border) * 6);
    Rows[R].borders := Borders;
    for B := 0 to 5 do begin
      Borders[B].linewt := -2;
      Borders[B].visible := 1;
      Borders[B].color.index := 0;
      Borders[B].color.method := DWG_COLOR_METHOD_BYBLOCK;
    end;
  end;
  Inners[I].tio.TABLESTYLE := Result;
end;

function TDWGFixture.AddMLeaderStyle(AHandle, AOwner: QWord): PDWGMLeaderStyle;
var
  I: Integer;
begin
  I := AddObject(DWG_TYPE_MLEADERSTYLE, AHandle, AOwner);
  Result := Alloc(SizeOf(Dwg_Object_MLEADERSTYLE));
  Result^.parent := @Inners[I];
  Inners[I].tio.MLEADERSTYLE := Result;
end;

procedure TDWGFixture.SetXDict(AIndex: Integer; AHandle: QWord);
begin
  Inners[AIndex].xdicobjhandle := Ref(AHandle);
end;

procedure TDWGFixture.SetReactors(AIndex: Integer; const AHandles: array of QWord);
var
  I: Integer;
  R: ^BITCODE_H;
begin
  R := Alloc(SizeOf(BITCODE_H) * Length(AHandles));
  for I := 0 to High(AHandles) do
    R[I] := Ref(AHandles[I]);
  Inners[AIndex].num_reactors := Length(AHandles);
  Inners[AIndex].reactors := Pointer(R);
end;

procedure TDWGFixture.SetHeaderNOD(AHandle: QWord);
begin
  Data.header_vars.DICTIONARY_NAMED_OBJECT := Ref(AHandle);
end;

{ ---------- Образец ---------- }

{ Имена в UTF-8; в R2004 они пишутся в CP1251 (как в DWG) }
function DWGName(AFixture: TDWGFixture; const AUTF8: RawByteString): RawByteString;
begin
  if AFixture.IsUnicode then
    Result := AUTF8
  else begin
    Result := UTF8ToAnsi(AUTF8);
    Result := AUTF8;
    SetCodePage(Result, CP_UTF8, False);
    SetCodePage(Result, 1251, True);
  end;
end;

const
  NameRusStyle = 'Стиль';
  NameRusTable = 'Таблица';

{ Образец DWG: символьные таблицы, словарь-приманка без владельца (первым),
  отрезок, NOD (не первым), ветки ACAD_TABLESTYLE, ACAD_MLEADERSTYLE,
  ACAD_GROUP и неизвестная ветка MYAPP (XRECORD). }
function BuildSample(AVersion: DWG_VERSION_TYPE): TDWGFixture;
var
  F: TDWGFixture;
  TS: PDWGTableStyle;
  MS: PDWGMLeaderStyle;
  I: Integer;
begin
  F := TDWGFixture.Create(AVersion);
  F.AddSymbol(DWG_TYPE_BLOCK_HEADER, HBlockModel, '*Model_Space');
  F.AddSymbol(DWG_TYPE_BLOCK_HEADER, HBlockDot, '_Dot');
  F.AddSymbol(DWG_TYPE_LTYPE, HLTypeByBlock, 'ByBlock');
  F.AddSymbol(DWG_TYPE_LTYPE, HLTypeByLayer, 'ByLayer');
  F.AddSymbol(DWG_TYPE_STYLE, HStyleStd, 'Standard');
  F.AddSymbol(DWG_TYPE_STYLE, HStyleRus, DWGName(F, NameRusStyle));
  { Словарь без владельца до NOD: без заголовка NOD был бы он }
  F.AddDictionary(HDecoy, 0, ['DECOY'], [HUnknown]);
  F.AddEntity(DWG_TYPE_LINE, HLine);
  F.AddDictionary(HNOD, 0,
    ['ACAD_GROUP', 'ACAD_TABLESTYLE', 'ACAD_MLEADERSTYLE', 'MYAPP', 'NOHANDLE'],
    [HGroups, HTableStyles, HMLeaderStyles, HUnknown, 0]);
  F.AddDictionary(HGroups, HNOD, [], []);
  F.AddDictionary(HTableStyles, HNOD, ['Standard', DWGName(F, NameRusTable)],
    [HTableStyleStd, HTableStyleRus]);
  I := F.AddObject(DWG_TYPE_XRECORD, HUnknown, HNOD);
  F.Objects[I].name := 'XRECORD';

  TS := F.AddTableStyle(HTableStyleStd, HTableStyles, 'Standard',
    [HStyleStd, HStyleRus, HStyleStd]);
  I := F.Count - 1;
  F.SetReactors(I, [HTableStyles]);
  F.SetXDict(I, HXDict);
  TS^.flags := 0;
  TS^.flow_direction := 1;
  TS^.horiz_cell_margin := 0.06;
  TS^.vert_cell_margin := 0.07;
  TS^.is_title_suppressed := 1;
  TS^.rowstyles[0].text_height := 0.18;
  TS^.rowstyles[0].text_alignment := 5;
  TS^.rowstyles[0].text_color.index := 3;
  TS^.rowstyles[0].text_color.method := DWG_COLOR_METHOD_ACI;
  TS^.rowstyles[0].fill_color.index := 7;
  TS^.rowstyles[0].fill_color.method := DWG_COLOR_METHOD_ACI;
  TS^.rowstyles[0].has_bgcolor := 1;
  TS^.rowstyles[1].text_height := 0.25;
  TS^.rowstyles[1].text_alignment := 2;
  TS^.rowstyles[2].text_height := 0.18;
  TS^.rowstyles[2].text_alignment := 2;
  F.AddTableStyle(HTableStyleRus, HTableStyles, DWGName(F, NameRusTable), []);
  F.AddDictionary(HXDict, HTableStyleStd,
    ['ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP'], [HCellStyleMap], 1);
  I := F.AddObject(DWG_TYPE_CELLSTYLEMAP, HCellStyleMap, HXDict);
  F.Objects[I].dxfname := 'CELLSTYLEMAP';

  F.AddDictionary(HMLeaderStyles, HNOD, ['Standard'], [HMLeaderStyleStd]);
  MS := F.AddMLeaderStyle(HMLeaderStyleStd, HMLeaderStyles);
  MS^.class_version := 2;
  MS^.content_type := 2;
  MS^.mleader_order := 1;
  MS^.leader_order := 0;
  MS^.max_points := 2;
  MS^._type := 1;
  MS^.line_color.rgb := $C1000000;
  MS^.line_type := F.Ref(HLTypeByBlock);
  MS^.linewt := -2;
  MS^.has_landing := 1;
  MS^.landing_gap := 2;
  MS^.has_dogleg := 1;
  MS^.landing_dist := 8;
  MS^.description := F.Text('Main');
  MS^.arrow_head := F.Ref(HBlockDot);
  MS^.arrow_head_size := 4;
  MS^.text_default := F.Text('');
  MS^.text_style := F.Ref(HStyleRus);
  MS^.attach_left := 1;
  MS^.attach_right := 1;
  MS^.text_angle_type := 1;
  { Цвет R2000: только индекс ACI }
  MS^.text_color.index := 5;
  MS^.text_height := 4;
  MS^.align_space := 4;
  MS^.block := F.Ref(HBlockDot);
  MS^.block_color.index := 0;
  MS^.block_color.method := DWG_COLOR_METHOD_BYBLOCK;
  MS^.block_scale.x := 1;
  MS^.block_scale.y := 1;
  MS^.block_scale.z := 1;
  MS^.use_block_scale := 1;
  MS^.use_block_rotation := 1;
  MS^.scale := 1;
  MS^.is_changed := 1;
  MS^.break_size := 3.75;
  MS^.attach_dir := 0;
  MS^.attach_top := 9;
  MS^.attach_bottom := 9;
  MS^.text_extended := 1;

  F.SetHeaderNOD(HNOD);
  Result := F;
end;

function FindIndex(AFixture: TDWGFixture; AHandle: QWord): Integer;
begin
  for Result := 0 to AFixture.Count - 1 do
    if AFixture.Objects[Result].handle.value = AHandle then
      Exit;
  raise Exception.CreateFmt('fixture: no object %s', [IntToHex(AHandle, 1)]);
end;

function DumpPairs(AObj: TZDXFRawObject): string;
var
  I: Integer;
begin
  Result := AObj.ObjType + #10;
  for I := 0 to AObj.PairCount - 1 do
    Result := Result + Format('%d=%s', [AObj.Pairs[I].Code, AObj.Pairs[I].Value]) + #10;
end;

function FindRaw(AModel: TZNODModel; AHandle: QWord): TZDXFRawObject;
begin
  Result := AModel.FindObject(AHandle);
  if Result = nil then
    raise Exception.CreateFmt('object %s not in model', [DXFHandleToStr(AHandle)]);
end;

{ ---------- 1. Модель NOD ---------- }

procedure TestBuildModel;
var
  F: TDWGFixture;
  Model: TZNODModel;
  Name: string;
  Dict: TZDXFDictionary;
begin
  F := BuildSample(R_2004);
  Model := TZNODModel.Create;
  try
    Check(BuildDWGNODModel(F.Data, Model), 'BuildDWGNODModel = True');
    CheckInt(HNOD, DWGHeaderNODHandle(F.Data), 'DWGHeaderNODHandle');
    Check((Model.NOD <> nil) and (Model.NOD.Handle = HNOD),
      'NOD from header (not the first root dictionary)');
    CheckInt(2, Model.RootDictionaryCount, 'root dictionaries');
    { Запись без хэндла пропущена }
    if Model.NOD <> nil then
      CheckInt(4, Model.NOD.Count, 'NOD keys');
    Check(Model.FindObject(HLine) = nil, 'entity is not in the model');
    CheckInt(F.Count - 7, Model.Objects.Count, 'objects (without symbols and entity)');
    CheckEquals('XRECORD', FindRaw(Model, HUnknown).ObjType, 'stub type');
    CheckInt(HNOD, FindRaw(Model, HUnknown).OwnerHandle, 'stub owner');
    CheckEquals('CELLSTYLEMAP', FindRaw(Model, HCellStyleMap).ObjType, 'CELLSTYLEMAP stub');
    CheckInt(6, Model.SymbolRecordCount, 'symbol records');
    Check(Model.FindSymbolName(HStyleRus, 'STYLE', Name) and (Name = NameRusStyle),
      'STYLE 12 name decoded from CP1251: ' + Name);
    Check(Model.FindSymbolName(HBlockDot, 'BLOCK_RECORD', Name) and (Name = '_Dot'),
      'BLOCK_HEADER → BLOCK_RECORD _Dot');
    Check(not Model.FindSymbolName(HBlockDot, 'STYLE', Name),
      'BLOCK_RECORD is not a STYLE');
    Dict := Model.ResolveDictionary('ACAD_TABLESTYLE');
    Check((Dict <> nil) and (Dict.Count = 2), 'ACAD_TABLESTYLE: 2 entries');
    if Dict <> nil then
      CheckEquals(NameRusTable, Dict[1].Key, 'ACAD_TABLESTYLE key decoded');
    Check(Model.ResolvePath('ACAD_TABLESTYLE/Standard') <> nil,
      'ResolvePath ACAD_TABLESTYLE/Standard');
    Check(Model.FindDictionary(HXDict) <> nil, 'extension dictionary indexed');

    { Повторная сборка той же модели — тот же результат }
    Check(BuildDWGNODModel(F.Data, Model), 'rebuild = True');
    CheckInt(F.Count - 7, Model.Objects.Count, 'rebuild: objects');
    CheckInt(6, Model.SymbolRecordCount, 'rebuild: symbol records');
  finally
    Model.Free;
    F.Free;
  end;
end;

{ ---------- 2. Группы DXF ---------- }

const
  ExpectedNOD =
    'DICTIONARY'#10'5=C'#10'330=0'#10'100=AcDbDictionary'#10'281=1'#10 +
    '3=ACAD_GROUP'#10'350=24'#10'3=ACAD_TABLESTYLE'#10'350=20'#10 +
    '3=ACAD_MLEADERSTYLE'#10'350=23'#10'3=MYAPP'#10'350=25'#10;
  ExpectedXDict =
    'DICTIONARY'#10'5=26'#10'330=21'#10'100=AcDbDictionary'#10'280=1'#10'281=1'#10 +
    '3=ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP'#10'360=27'#10;
  ExpectedTableStyle2004 =
    'TABLESTYLE'#10'5=21'#10'102={ACAD_REACTORS'#10'330=20'#10'102=}'#10 +
    '102={ACAD_XDICTIONARY'#10'360=26'#10'102=}'#10'330=20'#10 +
    '100=AcDbTableStyle'#10'3=Standard'#10'70=1'#10'71=0'#10'40=0.06'#10'41=0.07'#10 +
    '280=1'#10'281=0'#10 +
    '7=Standard'#10'140=0.18'#10'170=5'#10'62=3'#10'63=7'#10'283=1'#10 +
    '274=-2'#10'284=1'#10'64=0'#10'275=-2'#10'285=1'#10'65=0'#10 +
    '276=-2'#10'286=1'#10'66=0'#10'277=-2'#10'287=1'#10'67=0'#10 +
    '278=-2'#10'288=1'#10'68=0'#10'279=-2'#10'289=1'#10'69=0'#10 +
    '7=' + NameRusStyle + #10'140=0.25'#10'170=2'#10'62=0'#10'63=0'#10'283=0'#10 +
    '274=-2'#10'284=1'#10'64=0'#10'275=-2'#10'285=1'#10'65=0'#10 +
    '276=-2'#10'286=1'#10'66=0'#10'277=-2'#10'287=1'#10'67=0'#10 +
    '278=-2'#10'288=1'#10'68=0'#10'279=-2'#10'289=1'#10'69=0'#10 +
    '7=Standard'#10'140=0.18'#10'170=2'#10'62=0'#10'63=0'#10'283=0'#10 +
    '274=-2'#10'284=1'#10'64=0'#10'275=-2'#10'285=1'#10'65=0'#10 +
    '276=-2'#10'286=1'#10'66=0'#10'277=-2'#10'287=1'#10'67=0'#10 +
    '278=-2'#10'288=1'#10'68=0'#10'279=-2'#10'289=1'#10'69=0'#10;
  ExpectedMLeaderStyleBody =
    '170=2'#10'171=1'#10'172=0'#10'90=2'#10'40=0'#10'41=0'#10'173=1'#10 +
    '91=-1056964608'#10'340=14'#10'92=-2'#10'290=1'#10'42=2'#10'291=1'#10'43=8'#10 +
    '3=Main'#10'341=30'#10'44=4'#10'300='#10'342=12'#10'174=1'#10'178=1'#10 +
    '175=1'#10'176=0'#10'93=-1023410171'#10'45=4'#10'292=0'#10'297=0'#10'46=4'#10 +
    '343=30'#10'94=-1056964608'#10'47=1'#10'49=1'#10'140=1'#10'293=1'#10'141=0'#10 +
    '294=1'#10'177=0'#10'142=1'#10'295=1'#10'296=0'#10'143=3.75'#10;
  ExpectedMLeaderStyle2004 =
    'MLEADERSTYLE'#10'5=28'#10'330=23'#10'100=AcDbMLeaderStyle'#10 +
    ExpectedMLeaderStyleBody;
  ExpectedMLeaderStyle2013 =
    'MLEADERSTYLE'#10'5=28'#10'330=23'#10'100=AcDbMLeaderStyle'#10'179=2'#10 +
    ExpectedMLeaderStyleBody + '271=0'#10'273=9'#10'272=9'#10'298=1'#10;
  ExpectedTableStyle2013 =
    'TABLESTYLE'#10'5=21'#10'102={ACAD_REACTORS'#10'330=20'#10'102=}'#10 +
    '102={ACAD_XDICTIONARY'#10'360=26'#10'102=}'#10'330=20'#10 +
    '100=AcDbTableStyle'#10'3=Standard'#10'70=0'#10;

procedure TestDXFGroups;
var
  F: TDWGFixture;
  Model: TZNODModel;
  Obj: TZDXFRawObject;
  I: Integer;
begin
  F := BuildSample(R_2004);
  Model := TZNODModel.Create;
  try
    BuildDWGNODModel(F.Data, Model);
    CheckText(ExpectedNOD, DumpPairs(FindRaw(Model, HNOD)), 'R2004: NOD pairs');
    CheckText(ExpectedXDict, DumpPairs(FindRaw(Model, HXDict)),
      'R2004: hard-owner dictionary pairs (280, 360)');
    CheckText(ExpectedTableStyle2004, DumpPairs(FindRaw(Model, HTableStyleStd)),
      'R2004: TABLESTYLE pairs');
    CheckText(ExpectedMLeaderStyle2004, DumpPairs(FindRaw(Model, HMLeaderStyleStd)),
      'R2004: MLEADERSTYLE pairs (no 179/271-273/298)');
    Obj := FindRaw(Model, HTableStyleStd);
    CheckInt(HXDict, Obj.XDictHandle, 'TABLESTYLE XDictHandle');
    Check((Length(Obj.Reactors) = 1) and (Obj.Reactors[0] = HTableStyles),
      'TABLESTYLE reactors');
    Check(Obj.HasOwnerGroup and (Obj.OwnerHandle = HTableStyles), 'TABLESTYLE owner');
  finally
    Model.Free;
    F.Free;
  end;

  F := BuildSample(R_2013);
  Model := TZNODModel.Create;
  try
    { LibreDWG не разбирает строки TABLESTYLE R2010+ (num_rowstyles = 0),
      а flow_direction (16 бит) = property_override_flags & 0x10000 = 0 }
    F.Inners[FindIndex(F, HTableStyleStd)].tio.TABLESTYLE^.num_rowstyles := 0;
    F.Inners[FindIndex(F, HTableStyleStd)].tio.TABLESTYLE^.flow_direction := 0;
    Check(BuildDWGNODModel(F.Data, Model), 'R2013: BuildDWGNODModel = True');
    CheckText(ExpectedMLeaderStyle2013, DumpPairs(FindRaw(Model, HMLeaderStyleStd)),
      'R2013: MLEADERSTYLE pairs (179, 271-273, 298)');
    CheckText(ExpectedTableStyle2013, DumpPairs(FindRaw(Model, HTableStyleStd)),
      'R2013: TABLESTYLE pairs (name only, 70 = 0)');
    Check(Model.ResolvePath('ACAD_TABLESTYLE/' + NameRusTable) <> nil,
      'R2013: UTF-16 key decoded');
  finally
    Model.Free;
    F.Free;
  end;

  { ACDBDICTIONARYWDFLT: 100 AcDbDictionaryWithDefault, 340 }
  F := TDWGFixture.Create(R_2004);
  try
    I := F.AddDictionaryWithDefault($E, HNOD, $F, ['Normal'], [$F]);
    Obj := DWGObjectToNODRawObject(F.Data, F.Objects[I], nil);
    try
      CheckText('ACDBDICTIONARYWDFLT'#10'5=E'#10'330=C'#10'100=AcDbDictionary'#10 +
        '281=1'#10'3=Normal'#10'350=F'#10'100=AcDbDictionaryWithDefault'#10'340=F'#10,
        DumpPairs(Obj), 'ACDBDICTIONARYWDFLT pairs');
      Check(IsDXFDictionaryObjType(Obj.ObjType), 'ACDBDICTIONARYWDFLT is a dictionary');
    finally
      Obj.Free;
    end;
    I := F.AddEntity(DWG_TYPE_LINE, HLine);
    Check(DWGObjectToNODRawObject(F.Data, F.Objects[I], nil) = nil, 'entity → nil');
  finally
    F.Free;
  end;
end;

{ ---------- 3. Загрузка в чертёж ---------- }

procedure DoneDrawing(var ADrawing: TSimpleDrawing);
begin
  ADrawing.done;
  ADrawing.RawClassesSection := '';
  ADrawing.RawObjectsSection := '';
end;

procedure TestLoadDrawing;
var
  F: TDWGFixture;
  Drawing: TSimpleDrawing;
  TS: PTGDBDXFTableStyle;
  MS: PTGDBDXFMLeaderStyle;
  Model: TZNODModel;
  N: Integer;
  H: QWord;
begin
  F := BuildSample(R_2004);
  Drawing.init(nil);
  try
    N := DWGNODLoad(F.Data, Drawing);
    Check(N >= 2, Format('DWGNODLoad: %d handlers', [N]));

    CheckInt(2, Drawing.DXFTableStyleTable.Count, 'table styles');
    TS := Drawing.DXFTableStyleTable.getAddres('Standard');
    Check(TS <> nil, 'table style Standard');
    if TS <> nil then begin
      CheckEquals('21', TS^.DXFHandle, 'Standard: DXFHandle');
      CheckEquals('26', TS^.XDictHandle, 'Standard: XDictHandle');
      CheckInt(1, TS^.Flags70, 'Standard: 70');
      CheckEquals('0.06', FloatToStr(TS^.HorzCellMargin, FS), 'Standard: 40');
      CheckEquals('0.07', FloatToStr(TS^.VertCellMargin, FS), 'Standard: 41');
      Check(TS^.TitleSuppressed and not TS^.ColumnHeadingSuppressed, 'Standard: 280/281');
      CheckEquals('Standard,' + NameRusStyle + ',Standard',
        TS^.CellTextStyleName[0] + ',' + TS^.CellTextStyleName[1] + ',' +
        TS^.CellTextStyleName[2], 'Standard: cell text styles');
      CheckInt(3, TS^.CellFormats.Count, 'Standard: cell formats');
      if TS^.CellFormats.Count = 3 then begin
        CheckEquals('0.25', FloatToStr(TS^.CellFormats.getData(1).TextHeight, FS),
          'Standard: title text height');
        CheckInt(3, TS^.CellFormats.getData(0).TextColor, 'Standard: data text color');
        Check(TS^.CellFormats.getData(0).BackgroundColorEnabled, 'Standard: data 283');
      end;
    end;
    Check(Drawing.DXFTableStyleTable.getAddres(NameRusTable) <> nil,
      'table style ' + NameRusTable);

    CheckInt(1, Drawing.DXFMLeaderStyleTable.Count, 'mleader styles');
    MS := Drawing.DXFMLeaderStyleTable.getAddres('Standard');
    Check(MS <> nil, 'mleader style Standard');
    if MS <> nil then begin
      CheckEquals('ByBlock', MS^.LeaderLinetypeName, 'Standard: 340 name');
      CheckEquals('_Dot', MS^.ArrowHeadBlockName, 'Standard: 341 name');
      CheckEquals(NameRusStyle, MS^.TextStyleName, 'Standard: 342 name');
      CheckEquals('_Dot', MS^.BlockContentName, 'Standard: 343 name');
      CheckInt(Integer($C1000000), MS^.LeaderLineColor, 'Standard: 91 ByBlock');
      CheckInt(Integer($C3000005), MS^.TextColor, 'Standard: 93 ACI 5');
      CheckEquals('Main', MS^.Description, 'Standard: 3');
      CheckEquals('3.75', FloatToStr(MS^.BlockContentRotation, FS), 'Standard: 143');
      CheckInt(0, Length(MS^.ExtraPairs), 'Standard: no extra pairs');
    end;
  finally
    DoneDrawing(Drawing);
    F.Free;
  end;

  { Забранные хэндлы }
  F := BuildSample(R_2004);
  Model := TZNODModel.Create;
  Drawing.init(nil);
  try
    BuildDWGNODModel(F.Data, Model);
    RunNODLoadHandlers(Model, Drawing);
    for H in [HTableStyles, HTableStyleStd, HTableStyleRus, HXDict, HCellStyleMap,
      HMLeaderStyles, HMLeaderStyleStd] do
      Check(Model.IsHandleClaimed(H), 'claimed: ' + DXFHandleToStr(H));
    Check(not Model.IsHandleClaimed(HUnknown), 'not claimed: MYAPP XRECORD');
  finally
    DoneDrawing(Drawing);
    Model.Free;
    F.Free;
  end;
end;

{ ---------- 4. Без NOD в заголовке / без словарей ---------- }

procedure TestFallbacks;
var
  F: TDWGFixture;
  Model: TZNODModel;
  Drawing: TSimpleDrawing;
begin
  { Нет ссылки в заголовке — первый словарь без владельца (приманка) }
  F := BuildSample(R_2004);
  Model := TZNODModel.Create;
  try
    F.Data.header_vars.DICTIONARY_NAMED_OBJECT := nil;
    CheckInt(0, DWGHeaderNODHandle(F.Data), 'no header ref → 0');
    Check(BuildDWGNODModel(F.Data, Model) and (Model.NOD.Handle = HDecoy),
      'no header ref: first root dictionary');
    { Ссылка на не словарь — тоже поиск по владельцу }
    F.SetHeaderNOD(HUnknown);
    Check(BuildDWGNODModel(F.Data, Model) and (Model.NOD.Handle = HDecoy),
      'header ref to XRECORD: first root dictionary');
  finally
    Model.Free;
    F.Free;
  end;

  { Ни одного словаря: False, стили Standard по умолчанию }
  F := TDWGFixture.Create(R_2004);
  Model := TZNODModel.Create;
  Drawing.init(nil);
  try
    F.AddSymbol(DWG_TYPE_STYLE, HStyleStd, 'Standard');
    F.AddEntity(DWG_TYPE_LINE, HLine);
    F.SetHeaderNOD(HNOD);
    Check(not BuildDWGNODModel(F.Data, Model), 'no dictionaries: BuildDWGNODModel = False');
    Check(Model.NOD = nil, 'no dictionaries: NOD = nil');
    CheckInt(0, DWGNODLoad(F.Data, Drawing), 'no dictionaries: no handlers');
    Check(Drawing.DXFTableStyleTable.getAddres('Standard') <> nil,
      'no dictionaries: default table style Standard');
    Check(Drawing.DXFMLeaderStyleTable.getAddres('Standard') <> nil,
      'no dictionaries: default mleader style Standard');
  finally
    DoneDrawing(Drawing);
    Model.Free;
    F.Free;
  end;

  { Пустой Dwg_Data (нет объектов) }
  F := TDWGFixture.Create(R_2000);
  Model := TZNODModel.Create;
  try
    F.Data.&object := nil;
    Check(not BuildDWGNODModel(F.Data, Model), 'empty Dwg_Data: False');
    Check(not BuildDWGNODModel(F.Data, nil), 'nil model: False');
  finally
    Model.Free;
    F.Free;
  end;
end;

{ ---------- 5. Подключение к загрузчику DWG и сборке ---------- }

function ReadText(const AFileName: string): string;
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.LoadFromFile(AFileName);
    Result := L.Text;
  finally
    L.Free;
  end;
end;

procedure TestWiring;
var
  S: string;
begin
  S := ReadText(Root + ImportFile);
  Check(Pos('uzedwgnod', S) > 0, 'uzedwgimport uses uzedwgnod');
  Check(Pos('DWGNODLoad(', S) > 0, 'uzedwgimport calls DWGNODLoad');
  Check(Pos('dwg-import.scan.nod', S) > 0, 'uzedwgimport: phase dwg-import.scan.nod');
  S := ReadText(Root + TZFile);
  Check(Pos('### Этап 9. ', S) > 0, 'TZ: stage 9 section');
  Check(Pos('**Статус: выполнен (issue #1459).**', S) > 0, 'TZ: stage 9 status');
  S := ReadText(Root + NodTestsScript);
  Check(Pos('-Fucad_source/components/fpdwg', S) > 0, 'nodtests.sh: unit path of fpdwg');
end;

var
  I: Integer;
begin
  Root := '';
  for I := 1 to ParamCount do
    Root := IncludeTrailingPathDelimiter(ParamStr(I));
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  Failed := 0;
  Run('TestBuildModel', @TestBuildModel);
  Run('TestDXFGroups', @TestDXFGroups);
  Run('TestLoadDrawing', @TestLoadDrawing);
  Run('TestFallbacks', @TestFallbacks);
  Run('TestWiring', @TestWiring);

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
