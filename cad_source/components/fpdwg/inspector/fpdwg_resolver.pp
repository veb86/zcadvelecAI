unit fpdwg_resolver;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  fpdwg_types,
  fpdwg_logger,
  fpdwg_registry,
  fpdwg_model_base;

type
  TDWGResolver = class
  private
    FRegistry: TObjectRegistry;
    FLogger: IDWGLogger;
    function ResolveHandleRef(Source: TDWGObject; const Ref: TDWGHandleRef;
      const RefName: string; ExpectedClass: TClass; Required: Boolean;
      out Target: TDWGObject): Boolean;
    procedure MarkStatus(Obj: TDWGObject; RequiredBroken,
      OptionalBroken: Integer);
  public
    constructor Create(ARegistry: TObjectRegistry; ALogger: IDWGLogger = nil);
    procedure ResolveObject(Obj: TDWGObject);
    procedure ResolveAll;
  end;

implementation

uses
  fpdwg_model_blocks,
  fpdwg_model_entities,
  fpdwg_model_tables,
  fpdwg_model_unknown;

type
  PResolveContext = ^TResolveContext;
  TResolveContext = record
    Resolver: TDWGResolver;
  end;

constructor TDWGResolver.Create(ARegistry: TObjectRegistry;
  ALogger: IDWGLogger);
begin
  inherited Create;
  if ARegistry = nil then
    raise EDWGRegistryError.Create('Cannot resolve DWG objects without registry');
  FRegistry := ARegistry;
  FLogger := ALogger;
end;

function TDWGResolver.ResolveHandleRef(Source: TDWGObject;
  const Ref: TDWGHandleRef; const RefName: string; ExpectedClass: TClass;
  Required: Boolean; out Target: TDWGObject): Boolean;
begin
  Target := nil;

  if Ref.IsNull then
  begin
    Result := not Required;
    if Required and (FLogger <> nil) then
      FLogger.Warning(1301,
        Format('Required %s reference is null', [RefName]), Source.Handle);
    Exit;
  end;

  if not FRegistry.TryGet(Ref.Value, Target) then
  begin
    Result := False;
    if FLogger <> nil then
      FLogger.Warning(1301,
        Format('Broken %s reference %s -> %s',
          [RefName, IntToHex(Source.Handle, 1), Ref.ToString]),
        Source.Handle);
    Exit;
  end;

  if (ExpectedClass <> nil) and not Target.InheritsFrom(ExpectedClass) then
  begin
    Result := False;
    if FLogger <> nil then
      FLogger.Warning(1302,
        Format('Reference %s from %s points to %s, expected %s',
          [RefName, IntToHex(Source.Handle, 1), Target.ClassName,
           ExpectedClass.ClassName]),
        Source.Handle);
    Target := nil;
    Exit;
  end;

  Result := True;
end;

procedure TDWGResolver.MarkStatus(Obj: TDWGObject; RequiredBroken,
  OptionalBroken: Integer);
begin
  if Obj.Status = osFailed then
    Exit;
  if Obj is TDWGDuplicateHandleObject then
    Exit;
  if Obj is TDWGSyntheticTable then
  begin
    Obj.Status := osResolved;
    Exit;
  end;

  if RequiredBroken > 0 then
    Obj.Status := osBroken
  else if OptionalBroken > 0 then
    Obj.Status := osPartial
  else
    Obj.Status := osResolved;
end;

procedure TDWGResolver.ResolveObject(Obj: TDWGObject);
var
  Target: TDWGObject;
  RequiredBroken, OptionalBroken: Integer;
  Layer: TDWGLayer;
  Entity: TDWGEntity;
  BlockHeader: TDWGBlockHeader;
begin
  if Obj = nil then
    Exit;

  RequiredBroken := 0;
  OptionalBroken := 0;

  Obj.Owner := nil;
  if not Obj.OwnerHandle.IsNull then
  begin
    if ResolveHandleRef(Obj, Obj.OwnerHandle, 'owner', nil, False, Target) then
      Obj.Owner := Target
    else
      Inc(OptionalBroken);
  end;

  if Obj is TDWGLayer then
  begin
    Layer := TDWGLayer(Obj);
    Layer.Linetype := nil;
    if not Layer.LinetypeHandle.IsNull then
    begin
      if ResolveHandleRef(Obj, Layer.LinetypeHandle, 'linetype',
        TDWGLinetype, False, Target) then
        Layer.Linetype := TDWGLinetype(Target)
      else
        Inc(OptionalBroken);
    end;
  end;

  if Obj is TDWGEntity then
  begin
    Entity := TDWGEntity(Obj);
    Entity.Layer := nil;
    Entity.Linetype := nil;
    Entity.PrevEntity := nil;
    Entity.NextEntity := nil;

    if ResolveHandleRef(Obj, Entity.LayerHandle, 'layer', TDWGLayer, True,
      Target) then
      Entity.Layer := Target
    else
      Inc(RequiredBroken);

    if not Entity.LinetypeHandle.IsNull then
    begin
      if ResolveHandleRef(Obj, Entity.LinetypeHandle, 'linetype',
        TDWGLinetype, False, Target) then
        Entity.Linetype := Target
      else
        Inc(OptionalBroken);
    end;

    if not Entity.PrevEntityHandle.IsNull then
    begin
      if ResolveHandleRef(Obj, Entity.PrevEntityHandle, 'prev_entity',
        TDWGEntity, False, Target) then
        Entity.PrevEntity := TDWGEntity(Target)
      else
        Inc(OptionalBroken);
    end;

    if not Entity.NextEntityHandle.IsNull then
    begin
      if ResolveHandleRef(Obj, Entity.NextEntityHandle, 'next_entity',
        TDWGEntity, False, Target) then
        Entity.NextEntity := TDWGEntity(Target)
      else
        Inc(OptionalBroken);
    end;
  end;

  if Obj is TDWGBlockHeader then
  begin
    BlockHeader := TDWGBlockHeader(Obj);
    BlockHeader.BlockEntity := nil;
    BlockHeader.FirstEntity := nil;
    BlockHeader.LastEntity := nil;
    BlockHeader.EndBlockEntity := nil;
    BlockHeader.Layout := nil;

    if not BlockHeader.BlockEntityHandle.IsNull then
    begin
      if ResolveHandleRef(Obj, BlockHeader.BlockEntityHandle, 'block_entity',
        nil, False, Target) then
        BlockHeader.BlockEntity := Target
      else
        Inc(OptionalBroken);
    end;

    if not BlockHeader.FirstEntityHandle.IsNull then
    begin
      if ResolveHandleRef(Obj, BlockHeader.FirstEntityHandle, 'first_entity',
        TDWGEntity, False, Target) then
        BlockHeader.FirstEntity := Target
      else
        Inc(OptionalBroken);
    end;

    if not BlockHeader.LastEntityHandle.IsNull then
    begin
      if ResolveHandleRef(Obj, BlockHeader.LastEntityHandle, 'last_entity',
        TDWGEntity, False, Target) then
        BlockHeader.LastEntity := Target
      else
        Inc(OptionalBroken);
    end;

    if not BlockHeader.EndBlockEntityHandle.IsNull then
    begin
      if ResolveHandleRef(Obj, BlockHeader.EndBlockEntityHandle,
        'endblk_entity', nil, False, Target) then
        BlockHeader.EndBlockEntity := Target
      else
        Inc(OptionalBroken);
    end;

    if not BlockHeader.LayoutHandle.IsNull then
    begin
      if ResolveHandleRef(Obj, BlockHeader.LayoutHandle, 'layout', nil, False,
        Target) then
        BlockHeader.Layout := Target
      else
        Inc(OptionalBroken);
    end;
  end;

  MarkStatus(Obj, RequiredBroken, OptionalBroken);
end;

procedure ResolveRegistryObject(Obj: TDWGObject; Data: Pointer);
var
  Context: PResolveContext;
begin
  Context := PResolveContext(Data);
  Context^.Resolver.ResolveObject(Obj);
end;

procedure TDWGResolver.ResolveAll;
var
  Context: TResolveContext;
begin
  Context.Resolver := Self;
  FRegistry.Iterate(@ResolveRegistryObject, @Context);
end;

end.
