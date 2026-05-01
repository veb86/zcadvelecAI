unit fpdwg_registry;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  fpdwg_types,
  fpdwg_logger,
  fpdwg_model_base,
  fpdwg_model_unknown;

type
  EDWGRegistryError = class(Exception);
  EDWGDuplicateHandleError = class(EDWGRegistryError);

  TDWGRegistryCallback = procedure(Obj: TDWGObject; Data: Pointer);

  TObjectRegistry = class
  private
    type
      TDWGRegistryEntry = record
        Handle: TDWGHandle;
        Obj: TDWGObject;
      end;
  private
    FEntries: array of TDWGRegistryEntry;
    FDuplicates: array of TDWGDuplicateHandleObject;
    FLogger: IDWGLogger;
    FMode: TDWGLoadMode;
    function FindIndex(AHandle: TDWGHandle; out Index: Integer): Boolean;
    procedure InsertEntry(Index: Integer; Obj: TDWGObject);
    procedure AppendDuplicate(Obj: TDWGDuplicateHandleObject);
    function AddDuplicate(Obj: TDWGObject; Existing: TDWGObject
      ): TDWGDuplicateHandleObject;
  public
    constructor Create(AMode: TDWGLoadMode = lmTolerant;
      ALogger: IDWGLogger = nil);
    destructor Destroy; override;
    { Add takes ownership of Obj after successful registration. If Add raises,
      caller still owns Obj. In tolerant duplicate mode Obj is replaced by an
      owned TDWGDuplicateHandleObject and the duplicate input is freed. }
    function Add(Obj: TDWGObject): TDWGObject;
    function TryGet(AHandle: TDWGHandle; out Obj: TDWGObject): Boolean;
    function Get(AHandle: TDWGHandle): TDWGObject;
    function Count: Integer;
    function UniqueCount: Integer;
    function DuplicateCount: Integer;
    function ObjectAt(Index: Integer): TDWGObject;
    procedure Iterate(Callback: TDWGRegistryCallback; Data: Pointer = nil);
    property Mode: TDWGLoadMode read FMode;
  end;

implementation

constructor TObjectRegistry.Create(AMode: TDWGLoadMode; ALogger: IDWGLogger);
begin
  inherited Create;
  FMode := AMode;
  FLogger := ALogger;
end;

destructor TObjectRegistry.Destroy;
var
  I: Integer;
begin
  for I := 0 to High(FEntries) do
    FEntries[I].Obj.Free;
  for I := 0 to High(FDuplicates) do
    FDuplicates[I].Free;
  inherited Destroy;
end;

function TObjectRegistry.FindIndex(AHandle: TDWGHandle; out Index: Integer
  ): Boolean;
var
  L, H, M: Integer;
begin
  Result := False;
  L := 0;
  H := High(FEntries);
  while L <= H do
  begin
    M := L + (H - L) div 2;
    if FEntries[M].Handle = AHandle then
    begin
      Index := M;
      Exit(True);
    end
    else if FEntries[M].Handle < AHandle then
      L := M + 1
    else
      H := M - 1;
  end;
  Index := L;
end;

procedure TObjectRegistry.InsertEntry(Index: Integer; Obj: TDWGObject);
var
  I: Integer;
begin
  SetLength(FEntries, Length(FEntries) + 1);
  for I := High(FEntries) downto Index + 1 do
    FEntries[I] := FEntries[I - 1];
  FEntries[Index].Handle := Obj.Handle;
  FEntries[Index].Obj := Obj;
end;

procedure TObjectRegistry.AppendDuplicate(Obj: TDWGDuplicateHandleObject);
begin
  SetLength(FDuplicates, Length(FDuplicates) + 1);
  FDuplicates[High(FDuplicates)] := Obj;
end;

function TObjectRegistry.AddDuplicate(Obj: TDWGObject; Existing: TDWGObject
  ): TDWGDuplicateHandleObject;
begin
  Result := TDWGDuplicateHandleObject.CreateDuplicate(Obj, Existing);
  AppendDuplicate(Result);
  if FLogger <> nil then
    FLogger.Warning(1201,
      Format('Duplicate DWG handle %s ignored; first object remains indexed',
        [IntToHex(Obj.Handle, 1)]),
      Obj.Handle);
  Obj.Free;
end;

function TObjectRegistry.Add(Obj: TDWGObject): TDWGObject;
var
  Index: Integer;
  Existing: TDWGObject;
begin
  if Obj = nil then
    raise EDWGRegistryError.Create('Cannot register nil DWG object');

  if Obj.Handle = 0 then
    raise EDWGRegistryError.Create('Cannot register DWG object with zero handle');

  if FindIndex(Obj.Handle, Index) then
  begin
    Existing := FEntries[Index].Obj;
    if FMode = lmStrict then
      raise EDWGDuplicateHandleError.CreateFmt('Duplicate DWG handle %s',
        [IntToHex(Obj.Handle, 1)]);
    Exit(AddDuplicate(Obj, Existing));
  end;

  InsertEntry(Index, Obj);
  Result := Obj;
end;

function TObjectRegistry.TryGet(AHandle: TDWGHandle; out Obj: TDWGObject
  ): Boolean;
var
  Index: Integer;
begin
  Result := FindIndex(AHandle, Index);
  if Result then
    Obj := FEntries[Index].Obj
  else
    Obj := nil;
end;

function TObjectRegistry.Get(AHandle: TDWGHandle): TDWGObject;
begin
  if not TryGet(AHandle, Result) then
    raise EDWGRegistryError.CreateFmt('DWG handle %s not found',
      [IntToHex(AHandle, 1)]);
end;

function TObjectRegistry.Count: Integer;
begin
  Result := Length(FEntries) + Length(FDuplicates);
end;

function TObjectRegistry.UniqueCount: Integer;
begin
  Result := Length(FEntries);
end;

function TObjectRegistry.DuplicateCount: Integer;
begin
  Result := Length(FDuplicates);
end;

function TObjectRegistry.ObjectAt(Index: Integer): TDWGObject;
begin
  if (Index < 0) or (Index >= Count) then
    raise EDWGRegistryError.CreateFmt('DWG registry index %d out of range',
      [Index]);

  if Index < Length(FEntries) then
    Result := FEntries[Index].Obj
  else
    Result := FDuplicates[Index - Length(FEntries)];
end;

procedure TObjectRegistry.Iterate(Callback: TDWGRegistryCallback; Data: Pointer);
var
  I: Integer;
begin
  if not Assigned(Callback) then
    Exit;

  for I := 0 to High(FEntries) do
    Callback(FEntries[I].Obj, Data);
  for I := 0 to High(FDuplicates) do
    Callback(FDuplicates[I], Data);
end;

end.
