unit fpdwg_factory;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  dwg,
  fpdwg_types,
  fpdwg_logger,
  fpdwg_libredwg_utils,
  fpdwg_filter,
  fpdwg_model_base;

type
  TDWGBuilderContext = record
    Version: TDWGVersion;
    RawIndex: Integer;
    Codepage: Integer;
    Mode: TDWGLoadMode;
    DumpUnknown: Boolean;
    Logger: IDWGLogger;
    class function Default: TDWGBuilderContext; static;
  end;

  IDWGObjectMapper = interface
    ['{71F19F22-3D37-4E0E-A66D-E13E81D087A2}']
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
    procedure FillObject(Obj: TDWGObject; const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext);
  end;

  EDWGFactoryError = class(Exception);

  TDWGObjectFactory = class
  private
    type
      TDWGMapperEntry = record
        RawType: DWG_OBJECT_TYPE;
        Mapper: IDWGObjectMapper;
      end;
  private
    FEntries: array of TDWGMapperEntry;
    FUnknownMapper: IDWGObjectMapper;
    function FindMapperIndex(DWGType: DWG_OBJECT_TYPE): Integer;
    function CreateMaterializedObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
    function CreateStubObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject;
  public
    constructor Create;
    destructor Destroy; override;
    class function CreateDefault: TDWGObjectFactory;
    procedure RegisterMapper(DWGType: DWG_OBJECT_TYPE;
      Mapper: IDWGObjectMapper);
    function TryGetMapper(DWGType: DWG_OBJECT_TYPE;
      out Mapper: IDWGObjectMapper): Boolean;
    function MapperCount: Integer;
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext): TDWGObject; overload;
    function CreateObject(const Raw: Dwg_Object;
      const Ctx: TDWGBuilderContext; Filter: TFilterStrategy
      ): TDWGObject; overload;
  end;

procedure FillCommonObjectFields(Obj: TDWGObject; const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext);
procedure FillCommonEntityFields(Entity: TDWGEntity; const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext);
function SyntheticHandleRef(AHandle: TDWGHandle): TDWGHandleRef;

implementation

uses
  fpdwg_map_arc,
  fpdwg_map_block,
  fpdwg_map_circle,
  fpdwg_map_layer,
  fpdwg_map_layer_control,
  fpdwg_map_line,
  fpdwg_map_lwpolyline,
  fpdwg_map_linetype,
  fpdwg_map_text,
  fpdwg_map_unknown;

class function TDWGBuilderContext.Default: TDWGBuilderContext;
begin
  Result.Version := dvInvalid;
  Result.RawIndex := -1;
  Result.Codepage := 0;
  Result.Mode := lmTolerant;
  Result.DumpUnknown := False;
  Result.Logger := nil;
end;

function RawOwnerHandle(const Raw: Dwg_Object): TDWGHandleRef;
begin
  case Raw.supertype of
    DWG_SUPERTYPE_ENTITY:
      if Raw.tio.entity <> nil then
        Exit(HandleRefFromBitCode(Raw.tio.entity^.ownerhandle));
    DWG_SUPERTYPE_OBJECT:
      if Raw.tio.&object <> nil then
        Exit(HandleRefFromBitCode(Raw.tio.&object^.ownerhandle));
  end;
  Result := TDWGHandleRef.Null;
end;

function SyntheticHandleRef(AHandle: TDWGHandle): TDWGHandleRef;
begin
  Result.Value := AHandle;
  Result.Source := hsAbsoluteRef;
end;

procedure FillCommonObjectFields(Obj: TDWGObject; const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext);
begin
  if Obj = nil then
    Exit;

  Obj.Handle := DWGHandleValue(Raw.handle);
  Obj.OwnerHandle := RawOwnerHandle(Raw);
  Obj.RawObjectType := Raw.fixedtype;
  Obj.Version := Ctx.Version;
  Obj.RawIndex := Ctx.RawIndex;
  Obj.Name := SafeDecodeText(Raw.name, Ctx.Codepage, Ctx.Logger);
  Obj.DxfName := SafeDecodeText(Raw.dxfname, Ctx.Codepage, Ctx.Logger);
  Obj.Status := osRaw;
end;

procedure FillCommonEntityFields(Entity: TDWGEntity; const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext);
var
  RawEntity: ^Dwg_Object_Entity;
begin
  if Entity = nil then
    Exit;
  if Raw.tio.entity = nil then
  begin
    Entity.Status := osFailed;
    if Ctx.Logger <> nil then
      Ctx.Logger.Error(1101, 'Entity object has no LibreDWG entity data',
        Entity.Handle);
    Exit;
  end;

  RawEntity := Raw.tio.entity;
  Entity.LayerHandle := HandleRefFromBitCode(RawEntity^.layer);
  Entity.LinetypeHandle := HandleRefFromBitCode(RawEntity^.ltype);
  Entity.MaterialHandle := HandleRefFromBitCode(RawEntity^.material);
  Entity.PlotStyleHandle := HandleRefFromBitCode(RawEntity^.plotstyle);
  Entity.PrevEntityHandle := HandleRefFromBitCode(RawEntity^.prev_entity);
  Entity.NextEntityHandle := HandleRefFromBitCode(RawEntity^.next_entity);
  Entity.ColorIndex := RawEntity^.color.index;
  Entity.LineWeight := RawEntity^.linewt;
  Entity.Visible := RawEntity^.invisible = 0;
end;

constructor TDWGObjectFactory.Create;
begin
  inherited Create;
  FUnknownMapper := TDWGUnknownMapper.Create;
end;

destructor TDWGObjectFactory.Destroy;
begin
  SetLength(FEntries, 0);
  FUnknownMapper := nil;
  inherited Destroy;
end;

class function TDWGObjectFactory.CreateDefault: TDWGObjectFactory;
begin
  Result := TDWGObjectFactory.Create;
  Result.RegisterMapper(DWG_TYPE_LAYER_CONTROL, TDWGLayerControlMapper.Create);
  Result.RegisterMapper(DWG_TYPE_LAYER, TDWGLayerMapper.Create);
  Result.RegisterMapper(DWG_TYPE_LTYPE, TDWGLinetypeMapper.Create);
  Result.RegisterMapper(DWG_TYPE_BLOCK_HEADER, TDWGBlockHeaderMapper.Create);
  Result.RegisterMapper(DWG_TYPE_LINE, TDWGLineMapper.Create);
  Result.RegisterMapper(DWG_TYPE_ARC, TDWGArcMapper.Create);
  Result.RegisterMapper(DWG_TYPE_CIRCLE, TDWGCircleMapper.Create);
  Result.RegisterMapper(DWG_TYPE_LWPOLYLINE, TDWGLWPolylineMapper.Create);
  Result.RegisterMapper(DWG_TYPE_TEXT, TDWGTextMapper.Create);
end;

function TDWGObjectFactory.FindMapperIndex(DWGType: DWG_OBJECT_TYPE): Integer;
var
  I: Integer;
begin
  for I := 0 to High(FEntries) do
    if FEntries[I].RawType = DWGType then
      Exit(I);
  Result := -1;
end;

procedure TDWGObjectFactory.RegisterMapper(DWGType: DWG_OBJECT_TYPE;
  Mapper: IDWGObjectMapper);
var
  Index: Integer;
begin
  if Mapper = nil then
    raise EDWGFactoryError.Create('Cannot register nil DWG mapper');

  Index := FindMapperIndex(DWGType);
  if Index >= 0 then
  begin
    FEntries[Index].Mapper := Mapper;
    Exit;
  end;

  SetLength(FEntries, Length(FEntries) + 1);
  FEntries[High(FEntries)].RawType := DWGType;
  FEntries[High(FEntries)].Mapper := Mapper;
end;

function TDWGObjectFactory.TryGetMapper(DWGType: DWG_OBJECT_TYPE;
  out Mapper: IDWGObjectMapper): Boolean;
var
  Index: Integer;
begin
  Index := FindMapperIndex(DWGType);
  Result := Index >= 0;
  if Result then
    Mapper := FEntries[Index].Mapper
  else
    Mapper := nil;
end;

function TDWGObjectFactory.MapperCount: Integer;
begin
  Result := Length(FEntries);
end;

function TDWGObjectFactory.CreateMaterializedObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
var
  Mapper: IDWGObjectMapper;
begin
  if not TryGetMapper(Raw.fixedtype, Mapper) then
  begin
    Mapper := FUnknownMapper;
    if Ctx.Logger <> nil then
      Ctx.Logger.Warning(1101,
        Format('DWG object type %s mapped to unknown fallback: mapper not registered',
          [DWGObjectTypeName(Raw.fixedtype)]),
        DWGHandleValue(Raw.handle));
  end;

  Result := Mapper.CreateObject(Raw, Ctx);
  if Result = nil then
    raise EDWGFactoryError.CreateFmt('Mapper for %s returned nil',
      [DWGObjectTypeName(Raw.fixedtype)]);

  FillCommonObjectFields(Result, Raw, Ctx);
  Mapper.FillObject(Result, Raw, Ctx);
end;

function TDWGObjectFactory.CreateStubObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := TDWGStubObject.Create;
  FillCommonObjectFields(Result, Raw, Ctx);
  Result.DomainType := DWGDomainTypeFromRawType(Raw.fixedtype);
end;

function TDWGObjectFactory.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext): TDWGObject;
begin
  Result := CreateObject(Raw, Ctx, nil);
end;

function TDWGObjectFactory.CreateObject(const Raw: Dwg_Object;
  const Ctx: TDWGBuilderContext; Filter: TFilterStrategy): TDWGObject;
var
  Decision: TDWGFilterDecision;
begin
  if Filter <> nil then
    Decision := Filter.Decide(Raw)
  else
    Decision := fdMaterialize;

  case Decision of
    fdMaterialize:
      Result := CreateMaterializedObject(Raw, Ctx);
    fdStub:
      Result := CreateStubObject(Raw, Ctx);
    fdSkip:
      begin
        if Ctx.Logger <> nil then
          Ctx.Logger.Warning(1101,
            Format('Filter skipped DWG object type %s; references may be incomplete',
              [DWGObjectTypeName(Raw.fixedtype)]),
            DWGHandleValue(Raw.handle));
        Result := nil;
      end;
  else
    Result := nil;
  end;
end;

end.
