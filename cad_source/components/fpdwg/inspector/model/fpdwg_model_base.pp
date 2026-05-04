unit fpdwg_model_base;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  dwg,
  fpdwg_types,
  fpdwg_logger;

const
  DWG_SYNTHETIC_LAYER_TABLE_HANDLE = TDWGHandle($FFFFFFF1);
  DWG_SYNTHETIC_LTYPE_TABLE_HANDLE = TDWGHandle($FFFFFFF2);
  DWG_SYNTHETIC_STYLE_TABLE_HANDLE = TDWGHandle($FFFFFFF3);
  DWG_SYNTHETIC_DIMSTYLE_TABLE_HANDLE = TDWGHandle($FFFFFFF4);
  DWG_SYNTHETIC_VPORT_TABLE_HANDLE = TDWGHandle($FFFFFFF5);
  DWG_SYNTHETIC_BLOCK_RECORD_TABLE_HANDLE = TDWGHandle($FFFFFFF6);

type
  TDWGObject = class
  public
    Handle: TDWGHandle;
    OwnerHandle: TDWGHandleRef;
    Owner: TDWGObject;
    RawObjectType: DWG_OBJECT_TYPE;
    DomainType: TDWGDomainObjectType;
    Version: TDWGVersion;
    RawIndex: Integer;
    Name: string;
    DxfName: string;
    Status: TDWGObjectStatus;
    constructor Create; virtual;
    procedure ResolveLinks(Registry: TObject; Logger: IDWGLogger); virtual;
  end;

  TDWGTableRecord = class(TDWGObject)
  end;

  TDWGStubObject = class(TDWGObject)
  end;

  TDWGPoint3D = record
    X: Double;
    Y: Double;
    Z: Double;
  end;

  TDWGEntity = class(TDWGObject)
  public
    LayerHandle: TDWGHandleRef;
    LinetypeHandle: TDWGHandleRef;
    MaterialHandle: TDWGHandleRef;
    PlotStyleHandle: TDWGHandleRef;
    PrevEntityHandle: TDWGHandleRef;
    NextEntityHandle: TDWGHandleRef;
    Layer: TDWGObject;
    Linetype: TDWGObject;
    PrevEntity: TDWGEntity;
    NextEntity: TDWGEntity;
    ColorIndex: Integer;
    LineWeight: Integer;
    Visible: Boolean;
    constructor Create; override;
  end;

  TDWGSyntheticTable = class(TDWGObject)
  public
    TableKind: string;
    constructor CreateSynthetic(AHandle: TDWGHandle; const ATableKind: string);
  end;

function DWGObjectStatusToString(Status: TDWGObjectStatus): string;
function DWGDomainObjectTypeToString(DomainType: TDWGDomainObjectType): string;

implementation

constructor TDWGObject.Create;
begin
  inherited Create;
  Handle := 0;
  OwnerHandle := TDWGHandleRef.Null;
  Owner := nil;
  RawObjectType := DWG_TYPE_UNUSED;
  DomainType := dotUnknown;
  Version := dvInvalid;
  RawIndex := -1;
  Name := '';
  DxfName := '';
  Status := osRaw;
end;

procedure TDWGObject.ResolveLinks(Registry: TObject; Logger: IDWGLogger);
begin
end;

constructor TDWGEntity.Create;
begin
  inherited Create;
  LayerHandle := TDWGHandleRef.Null;
  LinetypeHandle := TDWGHandleRef.Null;
  MaterialHandle := TDWGHandleRef.Null;
  PlotStyleHandle := TDWGHandleRef.Null;
  PrevEntityHandle := TDWGHandleRef.Null;
  NextEntityHandle := TDWGHandleRef.Null;
  Layer := nil;
  Linetype := nil;
  PrevEntity := nil;
  NextEntity := nil;
  ColorIndex := 0;
  LineWeight := 0;
  Visible := True;
end;

constructor TDWGSyntheticTable.CreateSynthetic(AHandle: TDWGHandle;
  const ATableKind: string);
begin
  inherited Create;
  Handle := AHandle;
  DomainType := dotSyntheticTable;
  Status := osResolved;
  TableKind := ATableKind;
  Name := ATableKind + ' table';
  DxfName := ATableKind;
end;

function DWGObjectStatusToString(Status: TDWGObjectStatus): string;
begin
  case Status of
    osRaw: Result := 'raw';
    osResolved: Result := 'resolved';
    osPartial: Result := 'partial';
    osBroken: Result := 'broken';
    osFailed: Result := 'failed';
  else
    Result := 'unknown';
  end;
end;

function DWGDomainObjectTypeToString(DomainType: TDWGDomainObjectType): string;
begin
  case DomainType of
    dotHeader: Result := 'header';
    dotLayer: Result := 'layer';
    dotLinetype: Result := 'linetype';
    dotStyle: Result := 'style';
    dotBlockHeader: Result := 'block_header';
    dotBlock: Result := 'block';
    dotLine: Result := 'line';
    dotArc: Result := 'arc';
    dotCircle: Result := 'circle';
    dotText: Result := 'text';
    dotSyntheticTable: Result := 'synthetic_table';
    dotUnknown: Result := 'unknown';
  else
    Result := 'unknown';
  end;
end;

end.
