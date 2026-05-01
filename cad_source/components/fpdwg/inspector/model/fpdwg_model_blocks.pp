unit fpdwg_model_blocks;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpdwg_types,
  fpdwg_model_base;

type
  TDWGBlockHeader = class(TDWGTableRecord)
  public
    BlockName: string;
    BasePoint: TDWGPoint3D;
    BlockEntityHandle: TDWGHandleRef;
    FirstEntityHandle: TDWGHandleRef;
    LastEntityHandle: TDWGHandleRef;
    EndBlockEntityHandle: TDWGHandleRef;
    LayoutHandle: TDWGHandleRef;
    BlockEntity: TDWGObject;
    FirstEntity: TDWGObject;
    LastEntity: TDWGObject;
    EndBlockEntity: TDWGObject;
    Layout: TDWGObject;
    constructor Create; override;
  end;

implementation

constructor TDWGBlockHeader.Create;
begin
  inherited Create;
  DomainType := dotBlockHeader;
  BlockName := '';
  BasePoint.X := 0.0;
  BasePoint.Y := 0.0;
  BasePoint.Z := 0.0;
  BlockEntityHandle := TDWGHandleRef.Null;
  FirstEntityHandle := TDWGHandleRef.Null;
  LastEntityHandle := TDWGHandleRef.Null;
  EndBlockEntityHandle := TDWGHandleRef.Null;
  LayoutHandle := TDWGHandleRef.Null;
  BlockEntity := nil;
  FirstEntity := nil;
  LastEntity := nil;
  EndBlockEntity := nil;
  Layout := nil;
end;

end.
