unit uzctentleader;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  Math,
  Interfaces,
  fpcunit,
  testregistry,
  uzeconsts,
  uzeentity,
  uzeentblockinsert,
  uzeentityfactory,
  uzeentline,
  uzeentleader,
  uzeffdxf,
  uzeffmanager,
  uzegeometry,
  uzegeometrytypes,
  uzedrawingsimple,
  uzgldrawcontext,
  uzestylesdim,
  uzestyleslinetypes,
  uzeTypes;

type
  TLeaderEntityTest = class(TTestCase)
  published
    procedure RegistersDXFEntity;
    procedure MinimalDXFLoadsLeaderEntity;
    procedure FormatBuildsLeaderPathAndArrowBlock;
    procedure LeaderTypeIndexCombinesPathAndArrowFlag;
    procedure LeaderTypeIndexAppliesInspectorSelection;
    procedure CloneCopiesVerticesAndMetadata;
  end;

implementation

const
  DXF_LEADER_ENTITY_CONTENT =
    '  0' + #13#10 + 'SECTION'  + #13#10 +
    '  2' + #13#10 + 'HEADER'   + #13#10 +
    '  0' + #13#10 + 'ENDSEC'   + #13#10 +
    '  0' + #13#10 + 'SECTION'  + #13#10 +
    '  2' + #13#10 + 'TABLES'   + #13#10 +
    '  0' + #13#10 + 'ENDSEC'   + #13#10 +
    '  0' + #13#10 + 'SECTION'  + #13#10 +
    '  2' + #13#10 + 'ENTITIES' + #13#10 +
    '  0' + #13#10 + 'LEADER'   + #13#10 +
    '  5' + #13#10 + '297'      + #13#10 +
    '330' + #13#10 + '1F'       + #13#10 +
    '100' + #13#10 + 'AcDbEntity' + #13#10 +
    '  8' + #13#10 + '0'        + #13#10 +
    '100' + #13#10 + 'AcDbLeader' + #13#10 +
    '  3' + #13#10 + 'ISO-25'   + #13#10 +
    ' 73' + #13#10 + '0'        + #13#10 +
    ' 74' + #13#10 + '0'        + #13#10 +
    ' 75' + #13#10 + '1'        + #13#10 +
    ' 40' + #13#10 + '15.02046384720327' + #13#10 +
    ' 41' + #13#10 + '13.60845839017735' + #13#10 +
    ' 76' + #13#10 + '3'        + #13#10 +
    ' 10' + #13#10 + '1.0'      + #13#10 +
    ' 20' + #13#10 + '2.0'      + #13#10 +
    ' 30' + #13#10 + '0.0'      + #13#10 +
    ' 10' + #13#10 + '3.0'      + #13#10 +
    ' 20' + #13#10 + '4.0'      + #13#10 +
    ' 30' + #13#10 + '0.0'      + #13#10 +
    ' 10' + #13#10 + '5.0'      + #13#10 +
    ' 20' + #13#10 + '6.0'      + #13#10 +
    ' 30' + #13#10 + '0.0'      + #13#10 +
    '340' + #13#10 + '298'      + #13#10 +
    '213' + #13#10 + '5.609187033556736' + #13#10 +
    '223' + #13#10 + '30.6637564603418'  + #13#10 +
    '233' + #13#10 + '0.0'      + #13#10 +
    '  0' + #13#10 + 'ENDSEC'   + #13#10 +
    '  0' + #13#10 + 'EOF'      + #13#10;

function WriteLeaderTempDXF(const Content:string):string;
var
  F:TextFile;
begin
  Result:=GetTempDir+'test_leader_'+IntToStr(Random(MaxInt))+'.dxf';
  AssignFile(F,Result);
  Rewrite(F);
  Write(F,Content);
  CloseFile(F);
end;

function LoadLeaderDXFContent(const Content:string;
  var Drawing:TSimpleDrawing):integer;
var
  DC:TDrawContext;
  TempFile:string;
  ZDC:TZDrawingContext;
begin
  TempFile:=WriteLeaderTempDXF(Content);
  try
    Drawing.init(nil);
    DC:=Drawing.CreateDrawingRC;
    ZDC.CreateRec(Drawing,Drawing.pObjRoot^,TLOLoad,DC);
    AddFromDXF(TempFile,ZDC);
    Result:=Drawing.pObjRoot^.ObjArray.Count;
  finally
    SysUtils.DeleteFile(TempFile);
  end;
end;

procedure TLeaderEntityTest.RegistersDXFEntity;
var
  Info:TEntInfoData;
begin
  CheckTrue(DXFName2EntInfoData.MyGetValue('LEADER',Info),
    'LEADER must be registered as a DXF entity');
  CheckEquals(GDBLeaderID,Info.EntityID,'LEADER must use the leader entity id');
end;

procedure TLeaderEntityTest.MinimalDXFLoadsLeaderEntity;
var
  Drawing:TSimpleDrawing;
  Entity:PGDBObjEntity;
  EntityCount:integer;
  Leader:PGDBObjLeader;
  Vertex:PzePoint3d;
begin
  EntityCount:=LoadLeaderDXFContent(DXF_LEADER_ENTITY_CONTENT,Drawing);
  try
    CheckEquals(1,EntityCount,'DXF LEADER must load as one entity');
    Entity:=PGDBObjEntity(Drawing.pObjRoot^.ObjArray.GetData(0));
    CheckEquals(GDBLeaderID,Entity^.GetObjType);

    Leader:=PGDBObjLeader(Entity);
    CheckEquals('ISO-25',Leader^.DimStyleName);
    CheckEquals(LeaderTypeIndexLinearWithArrow,LeaderTypeToEnumIndex(Leader^));
    CheckEquals(0,Leader^.AnnotationType);
    CheckEquals(0,Leader^.HookLineDirectionFlag);
    CheckEquals(1,Leader^.HookLineFlag);
    CheckEquals(15.02046384720327,Leader^.TextHeight,1e-9);
    CheckEquals(13.60845839017735,Leader^.TextWidth,1e-9);
    CheckEquals(Int64($298),Int64(Leader^.AnnotationHandle));
    CheckEquals(3,Leader^.VertexArrayInOCS.Count);

    Vertex:=Leader^.VertexArrayInOCS.getDataMutable(2);
    CheckEquals(5.0,Vertex^.x,1e-9);
    CheckEquals(6.0,Vertex^.y,1e-9);
    CheckEquals(0.0,Vertex^.z,1e-9);
    CheckEquals(5.609187033556736,Leader^.AnnotationOffset.x,1e-9);
    CheckEquals(30.6637564603418,Leader^.AnnotationOffset.y,1e-9);
  finally
    Drawing.done;
  end;
end;

procedure TLeaderEntityTest.FormatBuildsLeaderPathAndArrowBlock;
var
  Drawing:TSimpleDrawing;
  Entity:PGDBObjEntity;
  EntityCount:integer;
  Leader:PGDBObjLeader;
  DimStyle:PGDBDimStyle;
  DC:TDrawContext;
  Child:PGDBObjEntity;
  Arrow:PGDBObjBlockInsert;
begin
  EntityCount:=LoadLeaderDXFContent(DXF_LEADER_ENTITY_CONTENT,Drawing);
  try
    CheckEquals(1,EntityCount,'DXF LEADER must load as one entity');
    Entity:=PGDBObjEntity(Drawing.pObjRoot^.ObjArray.GetData(0));
    Leader:=PGDBObjLeader(Entity);

    DimStyle:=PGDBDimStyle(Drawing.DimStyleTable.MergeItem('ISO-25',TLOLoad));
    DimStyle^.init('ISO-25');
    DimStyle^.Arrows.DIMLDRBLK:=TSOblique;
    DimStyle^.Arrows.DIMASZ:=2.0;
    DimStyle^.Units.DIMSCALE:=3.0;
    DimStyle^.Lines.DIMLTYPE:=Drawing.LTypeStyleTable.GetSystemLT(TLTByBlock);

    DC:=Drawing.CreateDrawingRC;
    Leader^.FormatEntity(Drawing,DC);

    CheckEquals(3,Leader^.ConstObjArray.Count,
      'leader must be a complex object with 2 path segments and 1 arrow block');

    Child:=PGDBObjEntity(Leader^.ConstObjArray.GetData(0));
    CheckEquals(GDBlineID,Child^.GetObjType,'first leader child must be a path line');
    Child:=PGDBObjEntity(Leader^.ConstObjArray.GetData(1));
    CheckEquals(GDBlineID,Child^.GetObjType,'second leader child must be a path line');

    Arrow:=PGDBObjBlockInsert(Leader^.ConstObjArray.GetData(2));
    CheckEquals(GDBBlockInsertID,Arrow^.GetObjType,'leader head must be a block insert');
    CheckEquals('_Oblique',Arrow^.Name);
    CheckEquals(1.0,Arrow^.Local.P_insert.x,1e-9);
    CheckEquals(2.0,Arrow^.Local.P_insert.y,1e-9);
    CheckEquals(6.0,Arrow^.scale.x,1e-9);
    CheckEquals(
      VertexAngle(CreateVertex2D(1,2),CreateVertex2D(3,4))-pi,
      Arrow^.rotate,1e-9);
  finally
    Drawing.done;
  end;
end;

procedure TLeaderEntityTest.LeaderTypeIndexCombinesPathAndArrowFlag;
var
  Leader:PGDBObjLeader;
begin
  Leader:=AllocAndInitLeader(nil);
  try
    Leader^.ArrowHeadFlag:=0;
    Leader^.PathType:=0;
    CheckEquals(LeaderTypeIndexLinearNoArrow,LeaderTypeToEnumIndex(Leader^));

    Leader^.ArrowHeadFlag:=0;
    Leader^.PathType:=1;
    CheckEquals(LeaderTypeIndexSplineNoArrow,LeaderTypeToEnumIndex(Leader^));

    Leader^.ArrowHeadFlag:=1;
    Leader^.PathType:=0;
    CheckEquals(LeaderTypeIndexLinearWithArrow,LeaderTypeToEnumIndex(Leader^));

    Leader^.ArrowHeadFlag:=1;
    Leader^.PathType:=1;
    CheckEquals(LeaderTypeIndexSplineWithArrow,LeaderTypeToEnumIndex(Leader^));

    Leader^.ArrowHeadFlag:=2;
    Leader^.PathType:=2;
    CheckEquals(LeaderTypeIndexLinearWithArrow,LeaderTypeToEnumIndex(Leader^));
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
  end;
end;

procedure TLeaderEntityTest.LeaderTypeIndexAppliesInspectorSelection;
var
  Leader:PGDBObjLeader;
begin
  Leader:=AllocAndInitLeader(nil);
  try
    ApplyLeaderTypeEnumIndex(Leader^,LeaderTypeIndexLinearNoArrow);
    CheckEquals(0,Leader^.ArrowHeadFlag);
    CheckEquals(0,Leader^.PathType);

    ApplyLeaderTypeEnumIndex(Leader^,LeaderTypeIndexSplineNoArrow);
    CheckEquals(0,Leader^.ArrowHeadFlag);
    CheckEquals(1,Leader^.PathType);

    ApplyLeaderTypeEnumIndex(Leader^,LeaderTypeIndexLinearWithArrow);
    CheckEquals(1,Leader^.ArrowHeadFlag);
    CheckEquals(0,Leader^.PathType);

    ApplyLeaderTypeEnumIndex(Leader^,LeaderTypeIndexSplineWithArrow);
    CheckEquals(1,Leader^.ArrowHeadFlag);
    CheckEquals(1,Leader^.PathType);
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
  end;
end;

procedure TLeaderEntityTest.CloneCopiesVerticesAndMetadata;
var
  Leader:PGDBObjLeader;
  Clone:PGDBObjLeader;
  Vertex:PzePoint3d;
begin
  Leader:=AllocAndInitLeader(nil);
  try
    Leader^.DimStyleName:='ISO-25';
    Leader^.ArrowHeadFlag:=0;
    Leader^.PathType:=1;
    Leader^.AnnotationType:=2;
    Leader^.HookLineDirectionFlag:=1;
    Leader^.HookLineFlag:=1;
    Leader^.TextHeight:=15.5;
    Leader^.TextWidth:=13.25;
    Leader^.AnnotationHandle:=$298;
    Leader^.NormalVector:=CreateVertex(0,0,1);
    Leader^.HorizontalDirection:=CreateVertex(1,0,0);
    Leader^.AnnotationOffset:=CreateVertex(5.6,30.6,0);
    Leader^.AddVertex(CreateVertex(1,2,3));
    Leader^.AddVertex(CreateVertex(4,5,6));

    Clone:=PGDBObjLeader(Leader^.Clone(nil));
    try
      CheckEquals(ObjN_GDBObjLeader,Clone^.GetObjTypeName);
      CheckEquals(GDBLeaderID,Clone^.GetObjType);
      CheckEquals('ISO-25',Clone^.DimStyleName);
      CheckEquals(0,Clone^.ArrowHeadFlag);
      CheckEquals(1,Clone^.PathType);
      CheckEquals(2,Clone^.AnnotationType);
      CheckEquals(1,Clone^.HookLineDirectionFlag);
      CheckEquals(1,Clone^.HookLineFlag);
      CheckEquals(15.5,Clone^.TextHeight,1e-9);
      CheckEquals(13.25,Clone^.TextWidth,1e-9);
      CheckEquals(Int64($298),Int64(Clone^.AnnotationHandle));
      CheckEquals(2,Clone^.VertexArrayInOCS.Count);

      Vertex:=Clone^.VertexArrayInOCS.getDataMutable(1);
      CheckEquals(4.0,Vertex^.x,1e-9);
      CheckEquals(5.0,Vertex^.y,1e-9);
      CheckEquals(6.0,Vertex^.z,1e-9);
      CheckEquals(5.6,Clone^.AnnotationOffset.x,1e-9);
      CheckEquals(30.6,Clone^.AnnotationOffset.y,1e-9);
    finally
      Clone^.done;
      FreeMem(Pointer(Clone));
    end;
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
  end;
end;

initialization
  RegisterTest(TLeaderEntityTest);

end.
