unit fpdwg_reader;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  dwg,
  dwgproc,
  fpdwg_types,
  fpdwg_logger;

const
  DWG_READER_ERROR_LIBRARY = 1000;
  DWG_READER_ERROR_READ = 1001;
  DWG_READER_ERROR_FILE_NOT_FOUND = 1002;
  DWG_READER_WARNING_READ = 1003;

type
  EDWGReaderError = class(Exception);

  TDWGReadStatus = (
    drsSuccess,
    drsFileNotFound,
    drsLibraryLoadFailed,
    drsReadFailed
  );

  TDWGReadResult = record
    Status: TDWGReadStatus;
    Code: Integer;
    Message: string;
    function Success: Boolean;
    class function Ok: TDWGReadResult; static;
    class function Failure(AStatus: TDWGReadStatus; ACode: Integer;
      const AMessage: string): TDWGReadResult; static;
  end;

  TDWGReaderOptions = record
    LibraryPath: string;
    ReloadLibrary: Boolean;
    class function Default: TDWGReaderOptions; static;
  end;

  TDWGReadDataProc = procedure(var Raw: Dwg_Data; Data: Pointer);

  IDWGLibreDWGApi = interface
    ['{1C59D07A-1220-4521-A99C-77B8EAC5E31E}']
    procedure Load(const LibraryPath: string; ReloadLibrary: Boolean);
    function ReadFile(const FileName: string; out Raw: Dwg_Data): Integer;
    procedure FreeData(var Raw: Dwg_Data);
    function DefaultLibraryName: string;
  end;

  TDWGProcLibreDWGApi = class(TInterfacedObject, IDWGLibreDWGApi)
  public
    procedure Load(const LibraryPath: string; ReloadLibrary: Boolean);
    function ReadFile(const FileName: string; out Raw: Dwg_Data): Integer;
    procedure FreeData(var Raw: Dwg_Data);
    function DefaultLibraryName: string;
  end;

  TDWGReader = class
  private
    FLogger: IDWGLogger;
    FApi: IDWGLibreDWGApi;
    function ResolveLibraryPath(const Options: TDWGReaderOptions): string;
    procedure LogError(Code: Integer; const Msg: string);
    procedure LogWarning(Code: Integer; const Msg: string);
    procedure FreeDataSilently(var Raw: Dwg_Data);
  public
    constructor Create(ALogger: IDWGLogger = nil;
      AApi: IDWGLibreDWGApi = nil);
    function DefaultLibraryName: string;
    function LoadLibrary(const Options: TDWGReaderOptions): TDWGReadResult;
    function ReadFile(const FileName: string; out Raw: Dwg_Data;
      const Options: TDWGReaderOptions): TDWGReadResult;
    function WithReadFile(const FileName: string; Callback: TDWGReadDataProc;
      Data: Pointer; const Options: TDWGReaderOptions): TDWGReadResult;
    procedure FreeData(var Raw: Dwg_Data);
  end;

implementation

function TDWGReadResult.Success: Boolean;
begin
  Result := Status = drsSuccess;
end;

class function TDWGReadResult.Ok: TDWGReadResult;
begin
  Result.Status := drsSuccess;
  Result.Code := 0;
  Result.Message := '';
end;

class function TDWGReadResult.Failure(AStatus: TDWGReadStatus; ACode: Integer;
  const AMessage: string): TDWGReadResult;
begin
  Result.Status := AStatus;
  Result.Code := ACode;
  Result.Message := AMessage;
end;

class function TDWGReaderOptions.Default: TDWGReaderOptions;
begin
  Result.LibraryPath := '';
  Result.ReloadLibrary := False;
end;

procedure TDWGProcLibreDWGApi.Load(const LibraryPath: string;
  ReloadLibrary: Boolean);
var
  LibName: string;
begin
  LibName := LibraryPath;
  if LibName = '' then
    LibName := DefaultLibraryName;

  LoadLibreDWG(PChar(LibName), ReloadLibrary);

  if (not Assigned(dwg_read_file)) or (not Assigned(dwg_free)) then
    raise EDWGReaderError.CreateFmt(
      'LibreDWG library "%s" does not export required functions',
      [LibName]);
end;

function TDWGProcLibreDWGApi.ReadFile(const FileName: string;
  out Raw: Dwg_Data): Integer;
begin
  if not Assigned(dwg_read_file) then
    raise EDWGReaderError.Create('LibreDWG is not loaded');

  FillChar(Raw, SizeOf(Raw), 0);
  Raw.opts := 0;
  Result := dwg_read_file(PChar(FileName), @Raw);
end;

procedure TDWGProcLibreDWGApi.FreeData(var Raw: Dwg_Data);
begin
  if Assigned(dwg_free) then
    dwg_free(@Raw);
  FillChar(Raw, SizeOf(Raw), 0);
end;

function TDWGProcLibreDWGApi.DefaultLibraryName: string;
begin
  Result := LibreDWG_Lib;
end;

constructor TDWGReader.Create(ALogger: IDWGLogger; AApi: IDWGLibreDWGApi);
begin
  inherited Create;
  FLogger := ALogger;
  if AApi <> nil then
    FApi := AApi
  else
    FApi := TDWGProcLibreDWGApi.Create;
end;

function TDWGReader.DefaultLibraryName: string;
begin
  Result := FApi.DefaultLibraryName;
end;

function TDWGReader.ResolveLibraryPath(const Options: TDWGReaderOptions
  ): string;
begin
  Result := Options.LibraryPath;
  if Result = '' then
    Result := DefaultLibraryName;
end;

procedure TDWGReader.LogError(Code: Integer; const Msg: string);
begin
  if FLogger <> nil then
    FLogger.Error(Code, Msg);
end;

procedure TDWGReader.LogWarning(Code: Integer; const Msg: string);
begin
  if FLogger <> nil then
    FLogger.Warning(Code, Msg);
end;

procedure TDWGReader.FreeDataSilently(var Raw: Dwg_Data);
begin
  try
    FreeData(Raw);
  except
    on E: Exception do
      LogError(DWG_READER_ERROR_READ,
        Format('Failed to free DWG data: %s', [E.Message]));
  end;
end;

function TDWGReader.LoadLibrary(const Options: TDWGReaderOptions
  ): TDWGReadResult;
var
  LibName: string;
begin
  LibName := ResolveLibraryPath(Options);
  try
    FApi.Load(LibName, Options.ReloadLibrary);
    Result := TDWGReadResult.Ok;
  except
    on E: Exception do
    begin
      LogError(DWG_READER_ERROR_LIBRARY,
        Format('Could not load LibreDWG library "%s": %s',
          [LibName, E.Message]));
      Result := TDWGReadResult.Failure(drsLibraryLoadFailed,
        DWG_READER_ERROR_LIBRARY, E.Message);
    end;
  end;
end;

function IsCriticalReadCode(Code: Integer): Boolean;
const
  CriticalMask =
    Ord(DWG_ERR_CLASSESNOTFOUND) or
    Ord(DWG_ERR_SECTIONNOTFOUND) or
    Ord(DWG_ERR_PAGENOTFOUND) or
    Ord(DWG_ERR_INTERNALERROR) or
    Ord(DWG_ERR_INVALIDDWG) or
    Ord(DWG_ERR_IOERROR) or
    Ord(DWG_ERR_OUTOFMEM);
begin
  Result := (Code and CriticalMask) <> 0;
end;

function TDWGReader.ReadFile(const FileName: string; out Raw: Dwg_Data;
  const Options: TDWGReaderOptions): TDWGReadResult;
var
  ReadCode: Integer;
  LoadResult: TDWGReadResult;
begin
  FillChar(Raw, SizeOf(Raw), 0);

  if not FileExists(FileName) then
  begin
    LogError(DWG_READER_ERROR_FILE_NOT_FOUND,
      Format('DWG file not found: %s', [FileName]));
    Exit(TDWGReadResult.Failure(drsFileNotFound,
      DWG_READER_ERROR_FILE_NOT_FOUND, FileName));
  end;

  LoadResult := LoadLibrary(Options);
  if not LoadResult.Success then
    Exit(LoadResult);

  try
    ReadCode := FApi.ReadFile(FileName, Raw);
  except
    on E: Exception do
    begin
      FreeDataSilently(Raw);
      LogError(DWG_READER_ERROR_READ,
        Format('dwg_read_file failed for "%s": %s', [FileName, E.Message]));
      Exit(TDWGReadResult.Failure(drsReadFailed,
        DWG_READER_ERROR_READ, E.Message));
    end;
  end;

  if IsCriticalReadCode(ReadCode) then
  begin
    FreeDataSilently(Raw);
    LogError(DWG_READER_ERROR_READ,
      Format('dwg_read_file returned critical error %d for "%s"',
        [ReadCode, FileName]));
    Exit(TDWGReadResult.Failure(drsReadFailed, ReadCode,
      Format('dwg_read_file returned critical error %d', [ReadCode])));
  end;

  Result := TDWGReadResult.Ok;
  Result.Code := ReadCode;
  if ReadCode <> 0 then
  begin
    Result.Message := Format('dwg_read_file returned non-critical code %d',
      [ReadCode]);
    LogWarning(DWG_READER_WARNING_READ,
      Format('dwg_read_file returned non-critical code %d for "%s"',
        [ReadCode, FileName]));
  end;
end;

function TDWGReader.WithReadFile(const FileName: string;
  Callback: TDWGReadDataProc; Data: Pointer;
  const Options: TDWGReaderOptions): TDWGReadResult;
var
  Raw: Dwg_Data;
begin
  Result := ReadFile(FileName, Raw, Options);
  if not Result.Success then
    Exit;

  try
    if Assigned(Callback) then
      Callback(Raw, Data);
  finally
    FreeData(Raw);
  end;
end;

procedure TDWGReader.FreeData(var Raw: Dwg_Data);
begin
  FApi.FreeData(Raw);
end;

end.
