unit uzctentityvisibility;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Interfaces,
  fpcunit,
  testregistry,
  uzeentity,
  uzeffdxf,
  uzeentitiesprop,
  uzegeometrytypes,
  uzedrawingsimple,
  uzgldrawcontext,
  uzeblockdef,
  uzeTypes;

type
  TEntityVisibilityTest = class(TTestCase)
  published
    procedure DXFCode60PersistsOnEntityAndControlsCommonBehavior;
    procedure DXFCode60ZeroAndMissingRemainVisible;
    procedure VisibilityIsCopiedWithVisualProperties;
  end;

implementation

function WriteTempDXF(const Content:string):string;
var
  F:TextFile;
begin
  Result:=GetTempDir+'test_entity_visibility_'+
    IntToStr(Random(MaxInt))+'.dxf';
  AssignFile(F,Result);
  Rewrite(F);
  Write(F,Content);
  CloseFile(F);
end;

function LineEntity(const X:integer;const VisibilityPair:string):string;
begin
  Result:=
    '  0'+#13#10+'LINE'+#13#10+
    '100'+#13#10+'AcDbEntity'+#13#10+
    '  8'+#13#10+'0'+#13#10+
    VisibilityPair+
    '100'+#13#10+'AcDbLine'+#13#10+
    ' 10'+#13#10+IntToStr(X)+#13#10+
    ' 20'+#13#10+'0'+#13#10+
    ' 30'+#13#10+'0'+#13#10+
    ' 11'+#13#10+IntToStr(X+1)+#13#10+
    ' 21'+#13#10+'1'+#13#10+
    ' 31'+#13#10+'0'+#13#10;
end;

function VisibilityDXF:string;
begin
  Result:=
    '  0'+#13#10+'SECTION'+#13#10+
    '  2'+#13#10+'HEADER'+#13#10+
    '  0'+#13#10+'ENDSEC'+#13#10+
    '  0'+#13#10+'SECTION'+#13#10+
    '  2'+#13#10+'TABLES'+#13#10+
    '  0'+#13#10+'ENDSEC'+#13#10+
    '  0'+#13#10+'SECTION'+#13#10+
    '  2'+#13#10+'BLOCKS'+#13#10+
    '  0'+#13#10+'BLOCK'+#13#10+
    '  2'+#13#10+'*U1'+#13#10+
    ' 70'+#13#10+'1'+#13#10+
    ' 10'+#13#10+'0'+#13#10+
    ' 20'+#13#10+'0'+#13#10+
    ' 30'+#13#10+'0'+#13#10+
    LineEntity(100,' 60'+#13#10+'1'+#13#10)+
    LineEntity(10,' 60'+#13#10+'0'+#13#10)+
    LineEntity(20,'')+
    '  0'+#13#10+'ENDBLK'+#13#10+
    '  0'+#13#10+'ENDSEC'+#13#10+
    '  0'+#13#10+'EOF'+#13#10;
end;

procedure LoadVisibilityDXF(var Drawing:TSimpleDrawing);
var
  DC:TDrawContext;
  TempFile:string;
  ZDC:TZDrawingContext;
begin
  TempFile:=WriteTempDXF(VisibilityDXF);
  try
    Drawing.init(nil);
    DC:=Drawing.CreateDrawingRC;
    ZDC.CreateRec(Drawing,Drawing.pObjRoot^,TLOLoad,DC);
    AddFromDXF(TempFile,ZDC);
  finally
    SysUtils.DeleteFile(TempFile);
  end;
end;

procedure TEntityVisibilityTest.DXFCode60PersistsOnEntityAndControlsCommonBehavior;
var
  Drawing:TSimpleDrawing;
  BlockDef:PGDBObjBlockdef;
  Entity:PGDBObjEntity;
  SelectedCount:integer;
  DC:TDrawContext;
  Bounds:TBoundingBox;
begin
  LoadVisibilityDXF(Drawing);
  try
    BlockDef:=Drawing.BlockDefArray.getblockdef('*U1');
    CheckTrue(BlockDef<>nil,'anonymous block must load');
    CheckEquals(3,BlockDef^.ObjArray.Count,
      'invisible entity must be retained with its persistent state');
    Entity:=PGDBObjEntity(BlockDef^.ObjArray.GetData(0));
    CheckEquals(Ord(EVInvisible),Ord(Entity^.vp.Visibility),
      'group code 60=1 must set the visual visibility property');
    CheckTrue(Entity^.PExtAttrib=nil,
      'visibility must not allocate temporary DXF extension attributes');
    CheckFalse(Entity^.IsActualy,'invisible entity must not be drawable');
    SelectedCount:=0;
    CheckFalse(Entity^.select(SelectedCount,nil),
      'invisible entity must not be selectable');
    DC:=Drawing.CreateDrawingRC;
    Bounds:=BlockDef^.ObjArray.calcbb;
    CheckTrue(Bounds.RTF.x<100,
      'invisible geometry must not affect block bounds');
    Bounds:=BlockDef^.ObjArray.getonlyvisibleoutbound(DC);
    CheckTrue(Bounds.RTF.x<100,
      'invisible geometry must not affect visible bounds');
  finally
    Drawing.done;
  end;
end;

procedure TEntityVisibilityTest.DXFCode60ZeroAndMissingRemainVisible;
var
  Drawing:TSimpleDrawing;
  BlockDef:PGDBObjBlockdef;
  Entity:PGDBObjEntity;
begin
  LoadVisibilityDXF(Drawing);
  try
    BlockDef:=Drawing.BlockDefArray.getblockdef('*U1');
    Entity:=PGDBObjEntity(BlockDef^.ObjArray.GetData(1));
    CheckEquals(Ord(EVVisible),Ord(Entity^.vp.Visibility));
    CheckTrue(Entity^.IsActualy,'group code 60=0 must remain visible');
    Entity:=PGDBObjEntity(BlockDef^.ObjArray.GetData(2));
    CheckEquals(Ord(EVVisible),Ord(Entity^.vp.Visibility));
    CheckTrue(Entity^.IsActualy,'missing group code 60 must remain visible');
  finally
    Drawing.done;
  end;
end;

procedure TEntityVisibilityTest.VisibilityIsCopiedWithVisualProperties;
var
  SourceEntity,TargetEntity:PGDBObjEntity;
begin
  SourceEntity:=GDBObjEntity.CreateInstance;
  TargetEntity:=GDBObjEntity.CreateInstance;
  try
    SourceEntity^.vp.Visibility:=EVInvisible;
    SourceEntity^.CopyVPto(TargetEntity^);
    CheckEquals(Ord(EVInvisible),Ord(TargetEntity^.vp.Visibility),
      'visibility must be copied with the other visual properties');
  finally
    SourceEntity^.done;
    Freemem(SourceEntity);
    TargetEntity^.done;
    Freemem(TargetEntity);
  end;
end;

initialization
  RegisterTest(TEntityVisibilityTest);

end.
