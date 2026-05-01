unit fpdwg_test_smoke;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGSmokeTest = class(TTestCase)
  published
    procedure R2000DWGFixtureRunsTextInspectorPipeline;
    procedure R2007DWGFixtureRunsJSONInspectorPipeline;
  end;

implementation

uses
  Classes,
  SysUtils,
  dwg,
  fpdwg_cli,
  fpdwg_reader,
  fpdwg_reporter,
  fpdwg_types;

type
  TFPDWGSmokeLibreDWGApi = class(TInterfacedObject, IDWGLibreDWGApi)
  private
    FExpectedMagic: string;
    FVersion: DWG_VERSION_TYPE;
    FObjects: array[0..3] of Dwg_Object;
    FObjectData: array[0..2] of Dwg_Object_Object;
    FEntityData: Dwg_Object_Entity;
    FLinetypeData: Dwg_Object_LTYPE;
    FLayerData: Dwg_Object_LAYER;
    FBlockData: Dwg_Object_BLOCK_HEADER;
    FLineData: Dwg_Entity_LINE;
    FRefs: array[0..3] of Dwg_Object_Ref;
    FLinetypeName: AnsiString;
    FLinetypeDescription: AnsiString;
    FLayerName: AnsiString;
    FBlockName: AnsiString;
    procedure PrepareRawData(out Raw: Dwg_Data);
  public
    LoadCount: Integer;
    ReadCount: Integer;
    FreeCount: Integer;
    LastLibraryPath: string;
    LastFileName: string;
    constructor Create(const AMagic: string);
    procedure Load(const LibraryPath: string; ReloadLibrary: Boolean);
    function ReadFile(const FileName: string; out Raw: Dwg_Data): Integer;
    procedure FreeData(var Raw: Dwg_Data);
    function DefaultLibraryName: string;
  end;

function CreateTempDWGFixture(const Magic: string): string;
var
  Stream: TFileStream;
  Padding: array[0..63] of Byte;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('fpdwg-smoke-%s-%d.dwg', [Magic, Random(1000000)]);

  Stream := TFileStream.Create(Result, fmCreate);
  try
    Stream.WriteBuffer(Magic[1], Length(Magic));
    FillChar(Padding, SizeOf(Padding), 0);
    Stream.WriteBuffer(Padding, SizeOf(Padding));
  finally
    Stream.Free;
  end;
end;

function ReadDWGMagic(const FileName: string): string;
var
  Stream: TFileStream;
  Buffer: array[0..5] of AnsiChar;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Stream.ReadBuffer(Buffer[0], SizeOf(Buffer));
    SetString(Result, PAnsiChar(@Buffer[0]), SizeOf(Buffer));
  finally
    Stream.Free;
  end;
end;

procedure InitRawObject(var Raw: Dwg_Object; RawType: DWG_OBJECT_TYPE;
  Supertype: DWG_OBJECT_SUPERTYPE; Handle: TDWGHandle; Index: Integer);
begin
  FillChar(Raw, SizeOf(Raw), 0);
  Raw.fixedtype := RawType;
  Raw.supertype := Supertype;
  Raw.handle.value := Handle;
  Raw.index := Index;
end;

procedure InitHandleRef(var Ref: Dwg_Object_Ref; Handle: TDWGHandle);
begin
  FillChar(Ref, SizeOf(Ref), 0);
  Ref.absolute_ref := Handle;
end;

constructor TFPDWGSmokeLibreDWGApi.Create(const AMagic: string);
begin
  inherited Create;
  FExpectedMagic := AMagic;
  if AMagic = 'AC1015' then
    FVersion := R_2000
  else if AMagic = 'AC1021' then
    FVersion := R_2007
  else
    FVersion := R_INVALID;

  FLinetypeName := 'Continuous';
  FLinetypeDescription := 'Solid line';
  FLayerName := 'Walls';
  FBlockName := '*Model_Space';
end;

procedure TFPDWGSmokeLibreDWGApi.Load(const LibraryPath: string;
  ReloadLibrary: Boolean);
begin
  Inc(LoadCount);
  LastLibraryPath := LibraryPath;
end;

procedure TFPDWGSmokeLibreDWGApi.PrepareRawData(out Raw: Dwg_Data);
begin
  FillChar(Raw, SizeOf(Raw), 0);
  FillChar(FObjects, SizeOf(FObjects), 0);
  FillChar(FObjectData, SizeOf(FObjectData), 0);
  FillChar(FEntityData, SizeOf(FEntityData), 0);
  FillChar(FLinetypeData, SizeOf(FLinetypeData), 0);
  FillChar(FLayerData, SizeOf(FLayerData), 0);
  FillChar(FBlockData, SizeOf(FBlockData), 0);
  FillChar(FLineData, SizeOf(FLineData), 0);
  FillChar(FRefs, SizeOf(FRefs), 0);

  Raw.header.version := FVersion;
  Raw.header.from_version := R_INVALID;
  Raw.header.codepage := 65001;
  Raw.num_objects := Length(FObjects);
  Raw.num_alloced_objects := Length(FObjects);
  Raw.num_entities := 1;
  Raw.num_classes := 1;
  Raw.&object := @FObjects[0];

  InitHandleRef(FRefs[0], $20);
  InitHandleRef(FRefs[1], $30);
  InitHandleRef(FRefs[2], $10);
  InitHandleRef(FRefs[3], $20);

  InitRawObject(FObjects[0], DWG_TYPE_LTYPE, DWG_SUPERTYPE_OBJECT, $20, 0);
  FObjects[0].tio.&object := @FObjectData[0];
  FObjectData[0].tio.LTYPE := @FLinetypeData;
  FLinetypeData.name := PAnsiChar(FLinetypeName);
  FLinetypeData.description := PAnsiChar(FLinetypeDescription);
  FLinetypeData.pattern_len := 0.0;

  InitRawObject(FObjects[1], DWG_TYPE_LAYER, DWG_SUPERTYPE_OBJECT, $10, 1);
  FObjects[1].tio.&object := @FObjectData[1];
  FObjectData[1].tio.LAYER := @FLayerData;
  FLayerData.name := PAnsiChar(FLayerName);
  FLayerData.color.index := 7;
  FLayerData.linewt := 25;
  FLayerData.plotflag := 1;
  FLayerData.ltype := @FRefs[0];

  InitRawObject(FObjects[2], DWG_TYPE_BLOCK_HEADER, DWG_SUPERTYPE_OBJECT,
    $30, 2);
  FObjects[2].tio.&object := @FObjectData[2];
  FObjectData[2].tio.BLOCK_HEADER := @FBlockData;
  FBlockData.name := PAnsiChar(FBlockName);
  FBlockData.base_pt.x := 0.0;
  FBlockData.base_pt.y := 0.0;
  FBlockData.base_pt.z := 0.0;

  InitRawObject(FObjects[3], DWG_TYPE_LINE, DWG_SUPERTYPE_ENTITY, $40, 3);
  FObjects[3].tio.entity := @FEntityData;
  FEntityData.tio.LINE := @FLineData;
  FEntityData.ownerhandle := @FRefs[1];
  FEntityData.layer := @FRefs[2];
  FEntityData.ltype := @FRefs[3];
  FEntityData.color.index := 256;
  FEntityData.linewt := 18;
  FEntityData.invisible := 0;
  FLineData.start.x := 0.0;
  FLineData.start.y := 0.0;
  FLineData.start.z := 1.0;
  FLineData.end_.x := 3.0;
  FLineData.end_.y := 4.0;
  FLineData.end_.z := 13.0;
end;

function TFPDWGSmokeLibreDWGApi.ReadFile(const FileName: string;
  out Raw: Dwg_Data): Integer;
begin
  Inc(ReadCount);
  LastFileName := FileName;
  if ReadDWGMagic(FileName) <> FExpectedMagic then
    raise Exception.CreateFmt('unexpected DWG magic in %s', [FileName]);

  PrepareRawData(Raw);
  Result := 0;
end;

procedure TFPDWGSmokeLibreDWGApi.FreeData(var Raw: Dwg_Data);
begin
  Inc(FreeCount);
  FillChar(Raw, SizeOf(Raw), 0);
end;

function TFPDWGSmokeLibreDWGApi.DefaultLibraryName: string;
begin
  Result := 'fake-libredwg.so';
end;

procedure TFPDWGSmokeTest.R2000DWGFixtureRunsTextInspectorPipeline;
var
  Api: IDWGLibreDWGApi;
  FakeApi: TFPDWGSmokeLibreDWGApi;
  FileName: string;
  Options: TDWGInspectorOptions;
  OutputText: string;
  ExitCode: Integer;
begin
  FileName := CreateTempDWGFixture('AC1015');
  FakeApi := TFPDWGSmokeLibreDWGApi.Create('AC1015');
  Api := FakeApi;
  try
    Options := TDWGInspectorOptions.Default;
    Options.InputFile := FileName;
    Options.Format := drfText;

    ExitCode := RunDWGInspectorWithApi(Options, OutputText, nil, Api);

    AssertEquals(DWG_INSPECT_EXIT_SUCCESS, ExitCode);
    AssertEquals(1, FakeApi.LoadCount);
    AssertEquals(1, FakeApi.ReadCount);
    AssertEquals(1, FakeApi.FreeCount);
    AssertTrue(Pos('DWG version: R_2000', OutputText) > 0);
    AssertTrue(Pos('Layers: 1', OutputText) > 0);
    AssertTrue(Pos('LINE handle=40', OutputText) > 0);
    AssertTrue(Pos('length_3d=13.000', OutputText) > 0);
    AssertTrue(Pos('length_xy=5.000', OutputText) > 0);
    AssertTrue(Pos('Warnings: 0', OutputText) > 0);
  finally
    Api := nil;
    DeleteFile(FileName);
  end;
end;

procedure TFPDWGSmokeTest.R2007DWGFixtureRunsJSONInspectorPipeline;
var
  Api: IDWGLibreDWGApi;
  FakeApi: TFPDWGSmokeLibreDWGApi;
  FileName: string;
  Options: TDWGInspectorOptions;
  OutputText: string;
  ExitCode: Integer;
begin
  FileName := CreateTempDWGFixture('AC1021');
  FakeApi := TFPDWGSmokeLibreDWGApi.Create('AC1021');
  Api := FakeApi;
  try
    Options := TDWGInspectorOptions.Default;
    Options.InputFile := FileName;
    Options.Format := drfJSON;

    ExitCode := RunDWGInspectorWithApi(Options, OutputText, nil, Api);

    AssertEquals(DWG_INSPECT_EXIT_SUCCESS, ExitCode);
    AssertEquals(1, FakeApi.LoadCount);
    AssertEquals(1, FakeApi.ReadCount);
    AssertEquals(1, FakeApi.FreeCount);
    AssertTrue(Pos('"version": "R_2007"', OutputText) > 0);
    AssertTrue(Pos('"layers": 1', OutputText) > 0);
    AssertTrue(Pos('"handle": "40"', OutputText) > 0);
    AssertTrue(Pos('"length_3d": 13', OutputText) > 0);
    AssertTrue(Pos('"length_xy": 5', OutputText) > 0);
    AssertTrue(Pos('"warnings": [', OutputText) > 0);
  finally
    Api := nil;
    DeleteFile(FileName);
  end;
end;

begin
  RegisterTests([TFPDWGSmokeTest]);
end.
