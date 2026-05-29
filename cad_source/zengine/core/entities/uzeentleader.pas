{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}

unit uzeentleader;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeentityfactory,uzgldrawcontext,uzedrawingdef,uzecamera,UGDBVectorSnapArray,
  uzestyleslayers,uzeentsubordinated,uzeentcurve,UGDBSelectedObjArray,
  uzeentity,uzctnrVectorBytesStream,uzeTypes,uzeconsts,uzglviewareadata,
  uzegeometrytypes,uzegeometry,uzeffdxfsupport,SysUtils,uzesnap,
  uzMVReader,uzCtnrVectorpBaseEntity;

type
  PGDBObjLeader=^GDBObjLeader;

  GDBObjLeader=object(GDBObjCurve)
    DimStyleName:string;
    ArrowHeadFlag:integer;
    PathType:integer;
    AnnotationType:integer;
    HookLineDirectionFlag:integer;
    HookLineFlag:integer;
    TextHeight:double;
    TextWidth:double;
    AnnotationHandle:TDWGHandle;
    NormalVector:TzePoint3d;
    HorizontalDirection:TzePoint3d;
    BlockOffset:TzePoint3d;
    AnnotationOffset:TzePoint3d;

    constructor init(own:Pointer;layeraddres:PGDBLayerProp;LW:smallint);
    constructor initnul(owner:PGDBObjGenericWithSubordinated);
    destructor done;virtual;

    procedure LoadFromDXF(var rdr:TZMemReader;ptu:PExtensionData;
      var drawing:TDrawingDef;var context:TIODXFLoadContext);virtual;
    procedure SaveToDXF(var outStream:TZctnrVectorBytes;
      var drawing:TDrawingDef;var IODXFContext:TIODXFSaveContext);virtual;
    procedure FormatEntity(var drawing:TDrawingDef;
      var DC:TDrawContext;Stage:TEFStages=EFAllStages);virtual;
    procedure DrawGeometry(lw:integer;var DC:TDrawContext;
      const inFrustumState:TInBoundingVolume);virtual;
    function Clone(own:Pointer):PGDBObjEntity;virtual;
    function GetObjTypeName:string;virtual;
    function GetObjType:TObjID;virtual;
    function CalcTrueInFrustum(
      const frustum:TzeFrustum):TInBoundingVolume;virtual;
    class function CreateInstance:PGDBObjLeader;static;
  end;

  function AllocAndInitLeader(owner:PGDBObjGenericWithSubordinated):PGDBObjLeader;

implementation

procedure InitLeaderDefaults(var Leader:GDBObjLeader);
begin
  Leader.DimStyleName:='';
  Leader.ArrowHeadFlag:=1;
  Leader.PathType:=0;
  Leader.AnnotationType:=3;
  Leader.HookLineDirectionFlag:=0;
  Leader.HookLineFlag:=0;
  Leader.TextHeight:=0;
  Leader.TextWidth:=0;
  Leader.AnnotationHandle:=0;
  Leader.NormalVector:=CreateVertex(0,0,1);
  Leader.HorizontalDirection:=CreateVertex(1,0,0);
  Leader.BlockOffset:=NulVertex;
  Leader.AnnotationOffset:=NulVertex;
end;

function IsZeroVertex(const Vertex:TzePoint3d):boolean;
begin
  Result:=(Vertex.x=0)and(Vertex.y=0)and(Vertex.z=0);
end;

function IsSameVertex(const Left,Right:TzePoint3d):boolean;
begin
  Result:=(Left.x=Right.x)and(Left.y=Right.y)and(Left.z=Right.z);
end;

constructor GDBObjLeader.init(own:Pointer;layeraddres:PGDBLayerProp;LW:smallint);
begin
  inherited init(own,layeraddres,lw);
  InitLeaderDefaults(self);
end;

constructor GDBObjLeader.initnul(owner:PGDBObjGenericWithSubordinated);
begin
  inherited initnul(owner);
  bp.ListPos.Owner:=owner;
  InitLeaderDefaults(self);
end;

destructor GDBObjLeader.done;
begin
  DimStyleName:='';
  inherited;
end;

procedure GDBObjLeader.LoadFromDXF(var rdr:TZMemReader;ptu:PExtensionData;
  var drawing:TDrawingDef;var context:TIODXFLoadContext);
var
  DXFGroupCode:integer;
  CurrentVertex:TzePoint3d;
  HasCurrentVertex:boolean;
  VertexCount:integer;

  procedure PushCurrentVertex;
  begin
    if HasCurrentVertex then begin
      VertexArrayInOCS.PushBackData(CurrentVertex);
      CurrentVertex:=NulVertex;
      HasCurrentVertex:=False;
    end;
  end;

begin
  VertexArrayInOCS.Clear;
  CurrentVertex:=NulVertex;
  HasCurrentVertex:=False;
  VertexCount:=0;

  DXFGroupCode:=rdr.ParseInteger;
  while DXFGroupCode<>0 do begin
    if not LoadFromDXFObjShared(rdr,DXFGroupCode,ptu,drawing,context) then
      if dxfLoadGroupCodeString(rdr,3,DXFGroupCode,DimStyleName,context.Header) then
      else if dxfLoadGroupCodeInteger(rdr,71,DXFGroupCode,ArrowHeadFlag) then
      else if dxfLoadGroupCodeInteger(rdr,72,DXFGroupCode,PathType) then
      else if dxfLoadGroupCodeInteger(rdr,73,DXFGroupCode,AnnotationType) then
      else if dxfLoadGroupCodeInteger(rdr,74,DXFGroupCode,HookLineDirectionFlag) then
      else if dxfLoadGroupCodeInteger(rdr,75,DXFGroupCode,HookLineFlag) then
      else if dxfLoadGroupCodeInteger(rdr,76,DXFGroupCode,VertexCount) then
      else if dxfLoadGroupCodeDouble(rdr,40,DXFGroupCode,TextHeight) then
      else if dxfLoadGroupCodeDouble(rdr,41,DXFGroupCode,TextWidth) then
      else if dxfLoadGroupCodeVertex(rdr,210,DXFGroupCode,NormalVector) then
      else if dxfLoadGroupCodeVertex(rdr,211,DXFGroupCode,HorizontalDirection) then
      else if dxfLoadGroupCodeVertex(rdr,212,DXFGroupCode,BlockOffset) then
      else if dxfLoadGroupCodeVertex(rdr,213,DXFGroupCode,AnnotationOffset) then
      else begin
        case DXFGroupCode of
          10:begin
            PushCurrentVertex;
            CurrentVertex:=NulVertex;
            CurrentVertex.x:=rdr.ParseDouble;
            HasCurrentVertex:=True;
          end;
          20:begin
            CurrentVertex.y:=rdr.ParseDouble;
            HasCurrentVertex:=True;
          end;
          30:begin
            CurrentVertex.z:=rdr.ParseDouble;
            HasCurrentVertex:=True;
            PushCurrentVertex;
          end;
          340:begin
            AnnotationHandle:=DXFHandle(rdr.ParseShortString);
          end;
        else
          rdr.SkipString;
        end;
      end;
    DXFGroupCode:=rdr.ParseInteger;
  end;

  PushCurrentVertex;
  VertexArrayInOCS.Shrink;
end;

procedure GDBObjLeader.SaveToDXF(var outStream:TZctnrVectorBytes;
  var drawing:TDrawingDef;var IODXFContext:TIODXFSaveContext);
var
  i:integer;
begin
  SaveToDXFObjPrefix(outStream,'LEADER','AcDbLeader',IODXFContext);
  if DimStyleName<>'' then
    dxfStringout(outStream,3,DimStyleName,IODXFContext.Header);
  dxfIntegerout(outStream,71,ArrowHeadFlag);
  dxfIntegerout(outStream,72,PathType);
  dxfIntegerout(outStream,73,AnnotationType);
  dxfIntegerout(outStream,74,HookLineDirectionFlag);
  dxfIntegerout(outStream,75,HookLineFlag);
  if TextHeight<>0 then
    dxfDoubleout(outStream,40,TextHeight);
  if TextWidth<>0 then
    dxfDoubleout(outStream,41,TextWidth);
  dxfIntegerout(outStream,76,VertexArrayInOCS.Count);
  for i:=0 to VertexArrayInOCS.Count-1 do
    dxfvertexout(outStream,10,VertexArrayInOCS.Items[i]);
  if AnnotationHandle<>0 then
    dxfStringWithoutEncodeOut(outStream,340,IntToHex(AnnotationHandle,0));
  if not IsSameVertex(NormalVector,CreateVertex(0,0,1)) then
    dxfvertexout(outStream,210,NormalVector);
  if not IsSameVertex(HorizontalDirection,CreateVertex(1,0,0)) then
    dxfvertexout(outStream,211,HorizontalDirection);
  if not IsZeroVertex(BlockOffset) then
    dxfvertexout(outStream,212,BlockOffset);
  if not IsZeroVertex(AnnotationOffset) then
    dxfvertexout(outStream,213,AnnotationOffset);
end;

procedure GDBObjLeader.FormatEntity(var drawing:TDrawingDef;
  var DC:TDrawContext;Stage:TEFStages=EFAllStages);
begin
  if assigned(EntExtensions) then
    EntExtensions.RunOnBeforeEntityFormat(@self,drawing,DC);

  if (Stage=EFAllStages)or(EFCalcEntityCS in Stage) then begin
    FormatWithoutSnapArray;
    calcbb(dc);
    CalcActualVisible(dc.DrawingContext.VActuality);
  end;

  if ((Stage=EFAllStages)or(EFDraw in Stage))and
     (not (ESTemp in State))and(DCODrawable in DC.Options) then begin
    Representation.Clear;
    if VertexArrayInWCS.Count>1 then
      Representation.DrawPolyLineWithLT(dc,VertexArrayInWCS,vp,False,False);
  end;

  if assigned(EntExtensions) then
    EntExtensions.RunOnAfterEntityFormat(@self,drawing,DC);
end;

procedure GDBObjLeader.DrawGeometry(lw:integer;var DC:TDrawContext;
  const inFrustumState:TInBoundingVolume);
begin
  Representation.DrawGeometry(DC,VP.BoundingBox,inFrustumState);
end;

function GDBObjLeader.Clone(own:Pointer):PGDBObjEntity;
var
  Leader:PGDBObjLeader;
begin
  Leader:=AllocAndInitLeader(PGDBObjGenericWithSubordinated(own));
  Leader^.DimStyleName:=DimStyleName;
  Leader^.ArrowHeadFlag:=ArrowHeadFlag;
  Leader^.PathType:=PathType;
  Leader^.AnnotationType:=AnnotationType;
  Leader^.HookLineDirectionFlag:=HookLineDirectionFlag;
  Leader^.HookLineFlag:=HookLineFlag;
  Leader^.TextHeight:=TextHeight;
  Leader^.TextWidth:=TextWidth;
  Leader^.AnnotationHandle:=AnnotationHandle;
  Leader^.NormalVector:=NormalVector;
  Leader^.HorizontalDirection:=HorizontalDirection;
  Leader^.BlockOffset:=BlockOffset;
  Leader^.AnnotationOffset:=AnnotationOffset;
  CopyVPto(Leader^);
  CopyExtensionsTo(Leader^);
  Leader^.VertexArrayInOCS.SetSize(VertexArrayInOCS.Count);
  VertexArrayInOCS.copyto(Leader^.VertexArrayInOCS);
  Leader^.bp.ListPos.Owner:=own;
  Result:=Leader;
end;

function GDBObjLeader.GetObjTypeName:string;
begin
  Result:=ObjN_GDBObjLeader;
end;

function GDBObjLeader.GetObjType:TObjID;
begin
  Result:=GDBLeaderID;
end;

function GDBObjLeader.CalcTrueInFrustum(
  const frustum:TzeFrustum):TInBoundingVolume;
begin
  Result:=VertexArrayInWCS.CalcTrueInFrustum(frustum,False);
end;

function AllocLeader:PGDBObjLeader;
begin
  Getmem(pointer(Result),sizeof(GDBObjLeader));
  FillChar(Result^,sizeof(GDBObjLeader),0);
end;

function AllocAndInitLeader(owner:PGDBObjGenericWithSubordinated):PGDBObjLeader;
begin
  Result:=AllocLeader;
  Result^.initnul(owner);
  Result^.bp.ListPos.Owner:=owner;
end;

class function GDBObjLeader.CreateInstance:PGDBObjLeader;
begin
  Result:=AllocAndInitLeader(nil);
end;

begin
  RegisterDXFEntity(GDBLeaderID,'LEADER','Leader',@AllocLeader,@AllocAndInitLeader);
end.
