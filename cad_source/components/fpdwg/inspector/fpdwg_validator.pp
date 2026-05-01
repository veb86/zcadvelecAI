unit fpdwg_validator;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  fpdwg_types,
  fpdwg_logger,
  fpdwg_document,
  fpdwg_model_base;

type
  TDWGBrokenReference = record
    Source: TDWGObject;
    SourceHandle: TDWGHandle;
    TargetHandle: TDWGHandle;
    RefName: string;
    Required: Boolean;
    Reason: string;
  end;

  TDWGOrphanObject = record
    Obj: TDWGObject;
    Handle: TDWGHandle;
    Reason: string;
  end;

  TDWGOwnerCycle = record
    StartObject: TDWGObject;
    StartHandle: TDWGHandle;
    RepeatedHandle: TDWGHandle;
    Message: string;
  end;

  TDWGValidationResult = class
  private
    FBrokenRefs: array of TDWGBrokenReference;
    FOrphans: array of TDWGOrphanObject;
    FCycles: array of TDWGOwnerCycle;
  public
    procedure AddBrokenRef(const Issue: TDWGBrokenReference);
    procedure AddOrphan(const Issue: TDWGOrphanObject);
    procedure AddCycle(const Issue: TDWGOwnerCycle);
    function BrokenRefCount: Integer;
    function OrphanCount: Integer;
    function CycleCount: Integer;
    function BrokenRefAt(Index: Integer): TDWGBrokenReference;
    function OrphanAt(Index: Integer): TDWGOrphanObject;
    function CycleAt(Index: Integer): TDWGOwnerCycle;
  end;

  TDWGValidator = class
  private
    FDocument: TDWGDocument;
    FLogger: IDWGLogger;
    function MissingOrWrongType(const Ref: TDWGHandleRef;
      ExpectedClass: TClass; out Reason: string): Boolean;
    procedure CheckReference(ResultInfo: TDWGValidationResult;
      Source: TDWGObject; const Ref: TDWGHandleRef; const RefName: string;
      ExpectedClass: TClass; Required: Boolean);
    procedure ValidateObjectRefs(ResultInfo: TDWGValidationResult;
      Obj: TDWGObject);
    procedure ValidateOrphan(ResultInfo: TDWGValidationResult;
      Obj: TDWGObject);
    procedure ValidateOwnerCycle(ResultInfo: TDWGValidationResult;
      Obj: TDWGObject);
  public
    constructor Create(ADocument: TDWGDocument; ALogger: IDWGLogger = nil);
    function Validate: TDWGValidationResult;
  end;

implementation

uses
  fpdwg_model_blocks,
  fpdwg_model_entities,
  fpdwg_model_tables,
  fpdwg_model_unknown;

type
  PValidationContext = ^TValidationContext;
  TValidationContext = record
    Validator: TDWGValidator;
    ResultInfo: TDWGValidationResult;
  end;

procedure TDWGValidationResult.AddBrokenRef(
  const Issue: TDWGBrokenReference);
begin
  SetLength(FBrokenRefs, Length(FBrokenRefs) + 1);
  FBrokenRefs[High(FBrokenRefs)] := Issue;
end;

procedure TDWGValidationResult.AddOrphan(const Issue: TDWGOrphanObject);
begin
  SetLength(FOrphans, Length(FOrphans) + 1);
  FOrphans[High(FOrphans)] := Issue;
end;

procedure TDWGValidationResult.AddCycle(const Issue: TDWGOwnerCycle);
begin
  SetLength(FCycles, Length(FCycles) + 1);
  FCycles[High(FCycles)] := Issue;
end;

function TDWGValidationResult.BrokenRefCount: Integer;
begin
  Result := Length(FBrokenRefs);
end;

function TDWGValidationResult.OrphanCount: Integer;
begin
  Result := Length(FOrphans);
end;

function TDWGValidationResult.CycleCount: Integer;
begin
  Result := Length(FCycles);
end;

function TDWGValidationResult.BrokenRefAt(
  Index: Integer): TDWGBrokenReference;
begin
  if (Index < 0) or (Index >= BrokenRefCount) then
    raise ERangeError.CreateFmt('DWG broken-ref index %d out of range',
      [Index]);
  Result := FBrokenRefs[Index];
end;

function TDWGValidationResult.OrphanAt(Index: Integer): TDWGOrphanObject;
begin
  if (Index < 0) or (Index >= OrphanCount) then
    raise ERangeError.CreateFmt('DWG orphan index %d out of range', [Index]);
  Result := FOrphans[Index];
end;

function TDWGValidationResult.CycleAt(Index: Integer): TDWGOwnerCycle;
begin
  if (Index < 0) or (Index >= CycleCount) then
    raise ERangeError.CreateFmt('DWG owner-cycle index %d out of range',
      [Index]);
  Result := FCycles[Index];
end;

constructor TDWGValidator.Create(ADocument: TDWGDocument;
  ALogger: IDWGLogger);
begin
  inherited Create;
  if ADocument = nil then
    raise EArgumentNilException.Create('Cannot validate nil DWG document');
  FDocument := ADocument;
  FLogger := ALogger;
end;

function TDWGValidator.MissingOrWrongType(const Ref: TDWGHandleRef;
  ExpectedClass: TClass; out Reason: string): Boolean;
var
  Target: TDWGObject;
begin
  Result := False;
  Reason := '';

  if not FDocument.Registry.TryGet(Ref.Value, Target) then
  begin
    Reason := 'target handle not found';
    Exit(True);
  end;

  if (ExpectedClass <> nil) and not Target.InheritsFrom(ExpectedClass) then
  begin
    Reason := Format('target type is %s, expected %s',
      [Target.ClassName, ExpectedClass.ClassName]);
    Exit(True);
  end;
end;

procedure TDWGValidator.CheckReference(ResultInfo: TDWGValidationResult;
  Source: TDWGObject; const Ref: TDWGHandleRef; const RefName: string;
  ExpectedClass: TClass; Required: Boolean);
var
  Issue: TDWGBrokenReference;
  Reason: string;
begin
  if Ref.IsNull then
  begin
    if not Required then
      Exit;
    Reason := 'required reference is null';
  end
  else if not MissingOrWrongType(Ref, ExpectedClass, Reason) then
    Exit;

  Issue.Source := Source;
  Issue.SourceHandle := Source.Handle;
  Issue.TargetHandle := Ref.Value;
  Issue.RefName := RefName;
  Issue.Required := Required;
  Issue.Reason := Reason;
  ResultInfo.AddBrokenRef(Issue);

  if FLogger <> nil then
    FLogger.Warning(1501,
      Format('Broken %s reference on %s: %s',
        [RefName, IntToHex(Source.Handle, 1), Reason]),
      Source.Handle);
end;

procedure TDWGValidator.ValidateObjectRefs(ResultInfo: TDWGValidationResult;
  Obj: TDWGObject);
var
  Layer: TDWGLayer;
  Entity: TDWGEntity;
  BlockHeader: TDWGBlockHeader;
begin
  if Obj is TDWGSyntheticTable then
    Exit;

  if not Obj.OwnerHandle.IsNull then
    CheckReference(ResultInfo, Obj, Obj.OwnerHandle, 'owner', nil, False);

  if Obj is TDWGLayer then
  begin
    Layer := TDWGLayer(Obj);
    CheckReference(ResultInfo, Obj, Layer.LinetypeHandle, 'linetype',
      TDWGLinetype, False);
  end;

  if Obj is TDWGEntity then
  begin
    Entity := TDWGEntity(Obj);
    CheckReference(ResultInfo, Obj, Entity.LayerHandle, 'layer',
      TDWGLayer, True);
    CheckReference(ResultInfo, Obj, Entity.LinetypeHandle, 'linetype',
      TDWGLinetype, False);
    CheckReference(ResultInfo, Obj, Entity.PrevEntityHandle, 'prev_entity',
      TDWGEntity, False);
    CheckReference(ResultInfo, Obj, Entity.NextEntityHandle, 'next_entity',
      TDWGEntity, False);
  end;

  if Obj is TDWGBlockHeader then
  begin
    BlockHeader := TDWGBlockHeader(Obj);
    CheckReference(ResultInfo, Obj, BlockHeader.BlockEntityHandle,
      'block_entity', nil, False);
    CheckReference(ResultInfo, Obj, BlockHeader.FirstEntityHandle,
      'first_entity', TDWGEntity, False);
    CheckReference(ResultInfo, Obj, BlockHeader.LastEntityHandle,
      'last_entity', TDWGEntity, False);
    CheckReference(ResultInfo, Obj, BlockHeader.EndBlockEntityHandle,
      'endblk_entity', nil, False);
    CheckReference(ResultInfo, Obj, BlockHeader.LayoutHandle, 'layout',
      nil, False);
  end;
end;

procedure TDWGValidator.ValidateOrphan(ResultInfo: TDWGValidationResult;
  Obj: TDWGObject);
var
  Issue: TDWGOrphanObject;
begin
  if (Obj is TDWGSyntheticTable) or (Obj is TDWGDuplicateHandleObject) then
    Exit;
  if Obj.Owner <> nil then
    Exit;

  Issue.Obj := Obj;
  Issue.Handle := Obj.Handle;
  if Obj.OwnerHandle.IsNull then
    Issue.Reason := 'owner reference is null'
  else
    Issue.Reason := 'owner reference is unresolved';
  ResultInfo.AddOrphan(Issue);

  if FLogger <> nil then
    FLogger.Warning(1502,
      Format('Orphan DWG object %s: %s',
        [IntToHex(Obj.Handle, 1), Issue.Reason]),
      Obj.Handle);
end;

function PathContainsHandle(const Path: array of TDWGHandle;
  AHandle: TDWGHandle): Boolean;
var
  I: Integer;
begin
  for I := Low(Path) to High(Path) do
    if Path[I] = AHandle then
      Exit(True);
  Result := False;
end;

procedure TDWGValidator.ValidateOwnerCycle(ResultInfo: TDWGValidationResult;
  Obj: TDWGObject);
var
  Path: array of TDWGHandle;
  Current: TDWGObject;
  Issue: TDWGOwnerCycle;
begin
  if Obj is TDWGSyntheticTable then
    Exit;

  SetLength(Path, 0);
  Current := Obj;
  while Current <> nil do
  begin
    if PathContainsHandle(Path, Current.Handle) then
    begin
      Issue.StartObject := Obj;
      Issue.StartHandle := Obj.Handle;
      Issue.RepeatedHandle := Current.Handle;
      Issue.Message := Format('Owner cycle reaches %s',
        [IntToHex(Current.Handle, 1)]);
      ResultInfo.AddCycle(Issue);

      if FLogger <> nil then
        FLogger.Warning(1503,
          Format('Owner cycle from %s reaches %s',
            [IntToHex(Obj.Handle, 1), IntToHex(Current.Handle, 1)]),
          Obj.Handle);
      Exit;
    end;

    SetLength(Path, Length(Path) + 1);
    Path[High(Path)] := Current.Handle;
    Current := Current.Owner;
  end;
end;

procedure ValidateRegistryObject(Obj: TDWGObject; Data: Pointer);
var
  Context: PValidationContext;
begin
  Context := PValidationContext(Data);
  Context^.Validator.ValidateObjectRefs(Context^.ResultInfo, Obj);
  Context^.Validator.ValidateOrphan(Context^.ResultInfo, Obj);
  Context^.Validator.ValidateOwnerCycle(Context^.ResultInfo, Obj);
end;

function TDWGValidator.Validate: TDWGValidationResult;
var
  Context: TValidationContext;
begin
  Result := TDWGValidationResult.Create;
  Context.Validator := Self;
  Context.ResultInfo := Result;
  FDocument.Registry.Iterate(@ValidateRegistryObject, @Context);
end;

end.
