unit fpdwg_logger;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  dwg,
  fpdwg_types;

type
  IDWGLogger = interface
    ['{AB8E1000-0DF4-466D-BA7F-6243391E9783}']
    procedure Log(const Error: TDWGError);
    procedure Info(const Msg: string);
    procedure Warning(Code: Integer; const Msg: string; Handle: TDWGHandle = 0);
    procedure Error(Code: Integer; const Msg: string; Handle: TDWGHandle = 0);
  end;

  TDWGConsoleLogger = class(TInterfacedObject, IDWGLogger)
  private
    FVerbose: Boolean;
    function SeverityToString(Severity: TDWGErrorSeverity): string;
  public
    constructor Create(AVerbose: Boolean = True);
    procedure Log(const Error: TDWGError);
    procedure Info(const Msg: string);
    procedure Warning(Code: Integer; const Msg: string; Handle: TDWGHandle = 0);
    procedure Error(Code: Integer; const Msg: string; Handle: TDWGHandle = 0);
    property Verbose: Boolean read FVerbose write FVerbose;
  end;

  TDWGMemoryLogger = class(TInterfacedObject, IDWGLogger)
  private
    FErrors: array of TDWGError;
    FMessages: TStringList;
    procedure Append(const Error: TDWGError);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Log(const Error: TDWGError);
    procedure Info(const Msg: string);
    procedure Warning(Code: Integer; const Msg: string; Handle: TDWGHandle = 0);
    procedure Error(Code: Integer; const Msg: string; Handle: TDWGHandle = 0);
    function ErrorCount: Integer;
    function GetError(Index: Integer): TDWGError;
    property Messages: TStringList read FMessages;
  end;

implementation

function MakeLogError(Severity: TDWGErrorSeverity; Code: Integer;
  const Msg: string; Handle: TDWGHandle): TDWGError;
begin
  Result.Code := Code;
  Result.Severity := Severity;
  Result.Handle := Handle;
  Result.ObjectType := DWG_TYPE_UNUSED;
  Result.Message := Msg;
end;

constructor TDWGConsoleLogger.Create(AVerbose: Boolean);
begin
  inherited Create;
  FVerbose := AVerbose;
end;

function TDWGConsoleLogger.SeverityToString(Severity: TDWGErrorSeverity): string;
begin
  case Severity of
    desInfo: Result := 'info';
    desWarning: Result := 'warning';
    desError: Result := 'error';
    desFatal: Result := 'fatal';
  else
    Result := 'unknown';
  end;
end;

procedure TDWGConsoleLogger.Log(const Error: TDWGError);
var
  Prefix: string;
begin
  if (Error.Severity = desInfo) and not FVerbose then
    Exit;

  Prefix := Format('[%s] %d', [SeverityToString(Error.Severity), Error.Code]);
  if Error.Handle <> 0 then
    Prefix := Prefix + Format(' handle=%s', [IntToHex(Error.Handle, 1)]);

  WriteLn(ErrOutput, Prefix + ': ' + Error.Message);
end;

procedure TDWGConsoleLogger.Info(const Msg: string);
begin
  Log(MakeLogError(desInfo, 0, Msg, 0));
end;

procedure TDWGConsoleLogger.Warning(Code: Integer; const Msg: string;
  Handle: TDWGHandle);
begin
  Log(MakeLogError(desWarning, Code, Msg, Handle));
end;

procedure TDWGConsoleLogger.Error(Code: Integer; const Msg: string;
  Handle: TDWGHandle);
begin
  Log(MakeLogError(desError, Code, Msg, Handle));
end;

constructor TDWGMemoryLogger.Create;
begin
  inherited Create;
  FMessages := TStringList.Create;
end;

destructor TDWGMemoryLogger.Destroy;
begin
  FMessages.Free;
  inherited Destroy;
end;

procedure TDWGMemoryLogger.Append(const Error: TDWGError);
begin
  SetLength(FErrors, Length(FErrors) + 1);
  FErrors[High(FErrors)] := Error;
end;

procedure TDWGMemoryLogger.Log(const Error: TDWGError);
begin
  Append(Error);
  FMessages.Add(Error.Message);
end;

procedure TDWGMemoryLogger.Info(const Msg: string);
begin
  Log(MakeLogError(desInfo, 0, Msg, 0));
end;

procedure TDWGMemoryLogger.Warning(Code: Integer; const Msg: string;
  Handle: TDWGHandle);
begin
  Log(MakeLogError(desWarning, Code, Msg, Handle));
end;

procedure TDWGMemoryLogger.Error(Code: Integer; const Msg: string;
  Handle: TDWGHandle);
begin
  Log(MakeLogError(desError, Code, Msg, Handle));
end;

function TDWGMemoryLogger.ErrorCount: Integer;
begin
  Result := Length(FErrors);
end;

function TDWGMemoryLogger.GetError(Index: Integer): TDWGError;
begin
  if (Index < 0) or (Index >= Length(FErrors)) then
    raise ERangeError.CreateFmt('DWG logger error index %d out of range', [Index]);
  Result := FErrors[Index];
end;

end.
