unit fpdwg_document;

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
  TDWGDocument = class
  private
    FRegistry: TObjectRegistry;
    FFileName: string;
    FVersion: TDWGVersion;
    FCodepage: Integer;
    FRawObjectCount: Integer;
    FRawClassCount: Integer;
    procedure AddSyntheticTable(AHandle: TDWGHandle; const ATableKind: string);
  public
    constructor Create(AMode: TDWGLoadMode = lmTolerant;
      ALogger: IDWGLogger = nil);
    destructor Destroy; override;
    function AddObject(Obj: TDWGObject): TDWGObject;
    procedure RegisterSyntheticTables;
    property Registry: TObjectRegistry read FRegistry;
    property FileName: string read FFileName write FFileName;
    property Version: TDWGVersion read FVersion write FVersion;
    property Codepage: Integer read FCodepage write FCodepage;
    property RawObjectCount: Integer read FRawObjectCount write FRawObjectCount;
    property RawClassCount: Integer read FRawClassCount write FRawClassCount;
  end;

implementation

constructor TDWGDocument.Create(AMode: TDWGLoadMode; ALogger: IDWGLogger);
begin
  inherited Create;
  FRegistry := TObjectRegistry.Create(AMode, ALogger);
  FFileName := '';
  FVersion := dvInvalid;
  FCodepage := 0;
  FRawObjectCount := 0;
  FRawClassCount := 0;
end;

destructor TDWGDocument.Destroy;
begin
  FRegistry.Free;
  inherited Destroy;
end;

function TDWGDocument.AddObject(Obj: TDWGObject): TDWGObject;
begin
  Result := FRegistry.Add(Obj);
end;

procedure TDWGDocument.AddSyntheticTable(AHandle: TDWGHandle;
  const ATableKind: string);
var
  Existing: TDWGObject;
begin
  if FRegistry.TryGet(AHandle, Existing) and
     (Existing is TDWGSyntheticTable) then
    Exit;
  FRegistry.Add(TDWGSyntheticTable.CreateSynthetic(AHandle, ATableKind));
end;

procedure TDWGDocument.RegisterSyntheticTables;
begin
  AddSyntheticTable(DWG_SYNTHETIC_LAYER_TABLE_HANDLE, 'LAYER');
  AddSyntheticTable(DWG_SYNTHETIC_LTYPE_TABLE_HANDLE, 'LTYPE');
  AddSyntheticTable(DWG_SYNTHETIC_STYLE_TABLE_HANDLE, 'STYLE');
  AddSyntheticTable(DWG_SYNTHETIC_DIMSTYLE_TABLE_HANDLE, 'DIMSTYLE');
  AddSyntheticTable(DWG_SYNTHETIC_VPORT_TABLE_HANDLE, 'VPORT');
  AddSyntheticTable(DWG_SYNTHETIC_BLOCK_RECORD_TABLE_HANDLE, 'BLOCK_RECORD');
end;

end.
