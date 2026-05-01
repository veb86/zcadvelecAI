unit fpdwg_test_reader;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGReaderTest = class(TTestCase)
  published
    procedure DefaultOptionsUsePlatformLibreDWGName;
    procedure MissingFileDoesNotLoadLibreDWG;
    procedure ReadFileUsesExplicitLibraryPath;
    procedure ReadFailureFreesPartialDWGData;
    procedure NonCriticalReadCodeKeepsDWGDataLoaded;
    procedure LibraryLoadFailureIsReported;
    procedure WithReadFileFreesDWGDataWhenConsumerRaises;
  end;

implementation

uses
  Classes,
  SysUtils,
  dwg,
  dwgproc,
  fpdwg_logger,
  fpdwg_reader,
  fpdwg_types;

type
  EReaderConsumerError = class(Exception);

  TFakeLibreDWGApi = class(TInterfacedObject, IDWGLibreDWGApi)
  public
    LoadCount: Integer;
    ReadCount: Integer;
    FreeCount: Integer;
    LastLibraryPath: string;
    LastReloadLibrary: Boolean;
    RaiseOnLoad: Boolean;
    ReadResultCode: Integer;
    DefaultName: string;
    procedure Load(const LibraryPath: string; ReloadLibrary: Boolean);
    function ReadFile(const FileName: string; out Raw: Dwg_Data): Integer;
    procedure FreeData(var Raw: Dwg_Data);
    function DefaultLibraryName: string;
  end;

  TConsumerState = record
    Called: Boolean;
    ObjectCount: Integer;
  end;
  PConsumerState = ^TConsumerState;

procedure TFakeLibreDWGApi.Load(const LibraryPath: string;
  ReloadLibrary: Boolean);
begin
  Inc(LoadCount);
  LastLibraryPath := LibraryPath;
  LastReloadLibrary := ReloadLibrary;
  if RaiseOnLoad then
    raise Exception.Create('fake library load failure');
end;

function TFakeLibreDWGApi.ReadFile(const FileName: string;
  out Raw: Dwg_Data): Integer;
begin
  Inc(ReadCount);
  FillChar(Raw, SizeOf(Raw), 0);
  Raw.header.version := R_2007;
  Raw.header.from_version := R_2004;
  Raw.header.codepage := 1251;
  Raw.num_objects := 7;
  Raw.num_classes := 3;
  Result := ReadResultCode;
end;

procedure TFakeLibreDWGApi.FreeData(var Raw: Dwg_Data);
begin
  Inc(FreeCount);
  FillChar(Raw, SizeOf(Raw), 0);
end;

function TFakeLibreDWGApi.DefaultLibraryName: string;
begin
  Result := DefaultName;
end;

function CreateTempDWGInput: string;
var
  Stream: TFileStream;
begin
  Result := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    Format('fpdwg-reader-%s-%d.dwg',
      [FormatDateTime('hhnnsszzz', Now), Random(1000000)]);
  Stream := TFileStream.Create(Result, fmCreate);
  try
  finally
    Stream.Free;
  end;
end;

procedure RaisingConsumer(var Raw: Dwg_Data; Data: Pointer);
var
  State: PConsumerState;
begin
  State := PConsumerState(Data);
  State^.Called := True;
  State^.ObjectCount := Raw.num_objects;
  raise EReaderConsumerError.Create('consumer failed');
end;

procedure TFPDWGReaderTest.DefaultOptionsUsePlatformLibreDWGName;
var
  Fake: TFakeLibreDWGApi;
  Api: IDWGLibreDWGApi;
  Reader: TDWGReader;
  Options: TDWGReaderOptions;
begin
  Fake := TFakeLibreDWGApi.Create;
  Fake.DefaultName := LibreDWG_Lib;
  Api := Fake;
  Reader := TDWGReader.Create(nil, Api);
  try
    Options := TDWGReaderOptions.Default;

    AssertEquals('', Options.LibraryPath);
    AssertFalse(Options.ReloadLibrary);
    AssertEquals(LibreDWG_Lib, Reader.DefaultLibraryName);
  finally
    Reader.Free;
    Api := nil;
  end;
end;

procedure TFPDWGReaderTest.MissingFileDoesNotLoadLibreDWG;
var
  Fake: TFakeLibreDWGApi;
  Api: IDWGLibreDWGApi;
  Reader: TDWGReader;
  Logger: IDWGLogger;
  MemoryLogger: TDWGMemoryLogger;
  Raw: Dwg_Data;
  ResultInfo: TDWGReadResult;
begin
  Fake := TFakeLibreDWGApi.Create;
  Fake.DefaultName := LibreDWG_Lib;
  Api := Fake;
  MemoryLogger := TDWGMemoryLogger.Create;
  Logger := MemoryLogger;
  Reader := TDWGReader.Create(Logger, Api);
  try
    ResultInfo := Reader.ReadFile('/tmp/fpdwg-reader-missing-file.dwg',
      Raw, TDWGReaderOptions.Default);

    AssertFalse(ResultInfo.Success);
    AssertEquals(Ord(drsFileNotFound), Ord(ResultInfo.Status));
    AssertEquals(0, Fake.LoadCount);
    AssertEquals(0, Fake.ReadCount);
    AssertEquals(1, MemoryLogger.ErrorCount);
    AssertEquals(1002, MemoryLogger.GetError(0).Code);
  finally
    Reader.Free;
    Logger := nil;
    Api := nil;
  end;
end;

procedure TFPDWGReaderTest.ReadFileUsesExplicitLibraryPath;
var
  Fake: TFakeLibreDWGApi;
  Api: IDWGLibreDWGApi;
  Reader: TDWGReader;
  Options: TDWGReaderOptions;
  Raw: Dwg_Data;
  ResultInfo: TDWGReadResult;
  FileName: string;
begin
  FileName := CreateTempDWGInput;
  Fake := TFakeLibreDWGApi.Create;
  Fake.DefaultName := LibreDWG_Lib;
  Api := Fake;
  Reader := TDWGReader.Create(nil, Api);
  try
    Options := TDWGReaderOptions.Default;
    Options.LibraryPath := '/opt/lib/libredwg-custom.so';
    Options.ReloadLibrary := True;

    ResultInfo := Reader.ReadFile(FileName, Raw, Options);

    AssertTrue(ResultInfo.Success);
    AssertEquals(1, Fake.LoadCount);
    AssertEquals('/opt/lib/libredwg-custom.so', Fake.LastLibraryPath);
    AssertTrue(Fake.LastReloadLibrary);
    AssertEquals(1, Fake.ReadCount);
    AssertEquals(7, Raw.num_objects);

    Reader.FreeData(Raw);
    AssertEquals(1, Fake.FreeCount);
    AssertEquals(0, Raw.num_objects);
  finally
    Reader.Free;
    Api := nil;
    DeleteFile(FileName);
  end;
end;

procedure TFPDWGReaderTest.ReadFailureFreesPartialDWGData;
var
  Fake: TFakeLibreDWGApi;
  Api: IDWGLibreDWGApi;
  Reader: TDWGReader;
  Logger: IDWGLogger;
  MemoryLogger: TDWGMemoryLogger;
  Raw: Dwg_Data;
  ResultInfo: TDWGReadResult;
  FileName: string;
begin
  FileName := CreateTempDWGInput;
  Fake := TFakeLibreDWGApi.Create;
  Fake.DefaultName := LibreDWG_Lib;
  Fake.ReadResultCode := Ord(DWG_ERR_CLASSESNOTFOUND);
  Api := Fake;
  MemoryLogger := TDWGMemoryLogger.Create;
  Logger := MemoryLogger;
  Reader := TDWGReader.Create(Logger, Api);
  try
    ResultInfo := Reader.ReadFile(FileName, Raw, TDWGReaderOptions.Default);

    AssertFalse(ResultInfo.Success);
    AssertEquals(Ord(drsReadFailed), Ord(ResultInfo.Status));
    AssertEquals(Ord(DWG_ERR_CLASSESNOTFOUND), ResultInfo.Code);
    AssertEquals(1, Fake.FreeCount);
    AssertEquals(0, Raw.num_objects);
    AssertEquals(1, MemoryLogger.ErrorCount);
    AssertEquals(1001, MemoryLogger.GetError(0).Code);
  finally
    Reader.Free;
    Logger := nil;
    Api := nil;
    DeleteFile(FileName);
  end;
end;

procedure TFPDWGReaderTest.NonCriticalReadCodeKeepsDWGDataLoaded;
var
  Fake: TFakeLibreDWGApi;
  Api: IDWGLibreDWGApi;
  Reader: TDWGReader;
  Logger: IDWGLogger;
  MemoryLogger: TDWGMemoryLogger;
  Raw: Dwg_Data;
  ResultInfo: TDWGReadResult;
  FileName: string;
begin
  FileName := CreateTempDWGInput;
  Fake := TFakeLibreDWGApi.Create;
  Fake.DefaultName := LibreDWG_Lib;
  Fake.ReadResultCode := Ord(DWG_ERR_INVALIDHANDLE);
  Api := Fake;
  MemoryLogger := TDWGMemoryLogger.Create;
  Logger := MemoryLogger;
  Reader := TDWGReader.Create(Logger, Api);
  try
    ResultInfo := Reader.ReadFile(FileName, Raw, TDWGReaderOptions.Default);

    AssertTrue(ResultInfo.Success);
    AssertEquals(Ord(DWG_ERR_INVALIDHANDLE), ResultInfo.Code);
    AssertEquals(0, Fake.FreeCount);
    AssertEquals(7, Raw.num_objects);
    AssertEquals(1, MemoryLogger.ErrorCount);
    AssertEquals(1003, MemoryLogger.GetError(0).Code);

    Reader.FreeData(Raw);
  finally
    Reader.Free;
    Logger := nil;
    Api := nil;
    DeleteFile(FileName);
  end;
end;

procedure TFPDWGReaderTest.LibraryLoadFailureIsReported;
var
  Fake: TFakeLibreDWGApi;
  Api: IDWGLibreDWGApi;
  Reader: TDWGReader;
  Logger: IDWGLogger;
  MemoryLogger: TDWGMemoryLogger;
  Raw: Dwg_Data;
  ResultInfo: TDWGReadResult;
  FileName: string;
begin
  FileName := CreateTempDWGInput;
  Fake := TFakeLibreDWGApi.Create;
  Fake.DefaultName := LibreDWG_Lib;
  Fake.RaiseOnLoad := True;
  Api := Fake;
  MemoryLogger := TDWGMemoryLogger.Create;
  Logger := MemoryLogger;
  Reader := TDWGReader.Create(Logger, Api);
  try
    ResultInfo := Reader.ReadFile(FileName, Raw, TDWGReaderOptions.Default);

    AssertFalse(ResultInfo.Success);
    AssertEquals(Ord(drsLibraryLoadFailed), Ord(ResultInfo.Status));
    AssertEquals(1, Fake.LoadCount);
    AssertEquals(0, Fake.ReadCount);
    AssertEquals(1, MemoryLogger.ErrorCount);
    AssertEquals(1000, MemoryLogger.GetError(0).Code);
  finally
    Reader.Free;
    Logger := nil;
    Api := nil;
    DeleteFile(FileName);
  end;
end;

procedure TFPDWGReaderTest.WithReadFileFreesDWGDataWhenConsumerRaises;
var
  Fake: TFakeLibreDWGApi;
  Api: IDWGLibreDWGApi;
  Reader: TDWGReader;
  State: TConsumerState;
  Raised: Boolean;
  FileName: string;
begin
  FileName := CreateTempDWGInput;
  Fake := TFakeLibreDWGApi.Create;
  Fake.DefaultName := LibreDWG_Lib;
  Api := Fake;
  Reader := TDWGReader.Create(nil, Api);
  try
    FillChar(State, SizeOf(State), 0);
    Raised := False;
    try
      Reader.WithReadFile(FileName, @RaisingConsumer, @State,
        TDWGReaderOptions.Default);
    except
      on EReaderConsumerError do
        Raised := True;
    end;

    AssertTrue(Raised);
    AssertTrue(State.Called);
    AssertEquals(7, State.ObjectCount);
    AssertEquals(1, Fake.ReadCount);
    AssertEquals(1, Fake.FreeCount);
  finally
    Reader.Free;
    Api := nil;
    DeleteFile(FileName);
  end;
end;

begin
  RegisterTests([TFPDWGReaderTest]);
end.
