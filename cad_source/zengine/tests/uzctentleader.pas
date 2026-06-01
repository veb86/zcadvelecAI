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
  uzeentspline,
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
    procedure FormatBuildsSplinePathAndStraightLastSegment;
    procedure FormatBuildsWholeSplinePathWithoutTextTail;
    procedure FormatKeepsLeaderArrowVisibleOnShortFirstSegment;
    procedure SplineLeaderArrowUsesSplineTangent;
    procedure LeaderTypeIndexCombinesPathAndArrowFlag;
    procedure LeaderTypeIndexAppliesInspectorSelection;
    procedure CloneCopiesVerticesAndMetadata;
    procedure NewFieldsDefaultToInherit;
    procedure ResolveArrowStyleUsesOverrideThenStyle;
    procedure ResolveArrowSizeUsesOverrideThenStyle;
    procedure ResolveDimLineWeightUsesOverrideThenStyle;
    procedure ResolveDimLineColorUsesOverrideThenStyle;
    procedure CloneCopiesIndividualOverrides;
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

procedure TLeaderEntityTest.FormatBuildsSplinePathAndStraightLastSegment;
var
  Drawing:TSimpleDrawing;
  Leader:PGDBObjLeader;
  DC:TDrawContext;
  Child:PGDBObjEntity;
  LastSegment:PGDBObjLine;
begin
  Drawing.init(nil);
  Leader:=AllocAndInitLeader(nil);
  try
    Leader^.ArrowHeadFlag:=1;
    Leader^.PathType:=1;
    Leader^.ArrowSize:=2.0;
    Leader^.AddVertex(CreateVertex(0,0,0));
    Leader^.AddVertex(CreateVertex(2,3,0));
    Leader^.AddVertex(CreateVertex(4,1,0));
    Leader^.AddVertex(CreateVertex(6,1,0));

    DC:=Drawing.CreateDrawingRC;
    Leader^.FormatEntity(Drawing,DC);

    CheckEquals(3,Leader^.ConstObjArray.Count,
      'spline leader with text tail must build a spline, final line and arrow');

    Child:=PGDBObjEntity(Leader^.ConstObjArray.GetData(0));
    CheckEquals(GDBSplineID,Child^.GetObjType,
      'first spline leader child must be a spline');

    Child:=PGDBObjEntity(Leader^.ConstObjArray.GetData(1));
    CheckEquals(GDBlineID,Child^.GetObjType,
      'text tail segment length equal to arrow size must remain straight');

    LastSegment:=PGDBObjLine(Child);
    CheckEquals(4.0,LastSegment^.CoordInOCS.lBegin.x,1e-9);
    CheckEquals(1.0,LastSegment^.CoordInOCS.lBegin.y,1e-9);
    CheckEquals(6.0,LastSegment^.CoordInOCS.lEnd.x,1e-9);
    CheckEquals(1.0,LastSegment^.CoordInOCS.lEnd.y,1e-9);

    Child:=PGDBObjEntity(Leader^.ConstObjArray.GetData(2));
    CheckEquals(GDBBlockInsertID,Child^.GetObjType,
      'arrow head must still be created for spline leaders with arrow');
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
    Drawing.done;
  end;
end;

procedure TLeaderEntityTest.FormatBuildsWholeSplinePathWithoutTextTail;
var
  Drawing:TSimpleDrawing;
  Leader:PGDBObjLeader;
  DC:TDrawContext;
  Child:PGDBObjEntity;
  Spline:PGDBObjSpline;
  Vertex:PzePoint3d;
begin
  Drawing.init(nil);
  Leader:=AllocAndInitLeader(nil);
  try
    Leader^.ArrowHeadFlag:=1;
    Leader^.PathType:=1;
    Leader^.ArrowSize:=1.0;
    Leader^.AddVertex(CreateVertex(0,0,0));
    Leader^.AddVertex(CreateVertex(2,3,0));
    Leader^.AddVertex(CreateVertex(5,1,0));

    DC:=Drawing.CreateDrawingRC;
    Leader^.FormatEntity(Drawing,DC);

    CheckEquals(2,Leader^.ConstObjArray.Count,
      'spline leader without text tail must build one spline and one arrow');

    Child:=PGDBObjEntity(Leader^.ConstObjArray.GetData(0));
    CheckEquals(GDBSplineID,Child^.GetObjType,
      'first spline leader child must be a spline through all points');
    Spline:=PGDBObjSpline(Child);
    CheckEquals(3,Spline^.VertexArrayInOCS.Count,
      'quadratic spline must include the last leader point as a control point');
    Vertex:=Spline^.VertexArrayInOCS.getDataMutable(2);
    CheckEquals(5.0,Vertex^.x,1e-9);
    CheckEquals(1.0,Vertex^.y,1e-9);

    Child:=PGDBObjEntity(Leader^.ConstObjArray.GetData(1));
    CheckEquals(GDBBlockInsertID,Child^.GetObjType,
      'arrow head must still be created when the last segment is spline');
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
    Drawing.done;
  end;
end;

procedure TLeaderEntityTest.FormatKeepsLeaderArrowVisibleOnShortFirstSegment;
var
  Drawing:TSimpleDrawing;
  Leader:PGDBObjLeader;
  DC:TDrawContext;
  Child:PGDBObjEntity;
  FirstSegment:PGDBObjLine;
  Arrow:PGDBObjBlockInsert;
begin
  Drawing.init(nil);
  Leader:=AllocAndInitLeader(nil);
  try
    Leader^.ArrowHeadFlag:=1;
    Leader^.PathType:=0;
    Leader^.ArrowSize:=4.0;
    Leader^.AddVertex(CreateVertex(0,0,0));
    Leader^.AddVertex(CreateVertex(6,0,0));
    Leader^.AddVertex(CreateVertex(10,0,0));

    DC:=Drawing.CreateDrawingRC;
    Leader^.FormatEntity(Drawing,DC);

    CheckEquals(3,Leader^.ConstObjArray.Count,
      'short first segment leader must keep path segments and arrow');

    Child:=PGDBObjEntity(Leader^.ConstObjArray.GetData(0));
    CheckEquals(GDBlineID,Child^.GetObjType,
      'first leader child must remain a line segment');
    FirstSegment:=PGDBObjLine(Child);
    CheckEquals(3.0,FirstSegment^.CoordInOCS.lBegin.x,1e-9);
    CheckEquals(0.0,FirstSegment^.CoordInOCS.lBegin.y,1e-9);
    CheckEquals(6.0,FirstSegment^.CoordInOCS.lEnd.x,1e-9);
    CheckEquals(0.0,FirstSegment^.CoordInOCS.lEnd.y,1e-9);

    Arrow:=PGDBObjBlockInsert(Leader^.ConstObjArray.GetData(2));
    CheckEquals(GDBBlockInsertID,Arrow^.GetObjType,
      'arrow head must be present when first segment is shorter than 2 arrow sizes');
    CheckEquals('_ClosedFilled',Arrow^.Name);
    CheckEquals(4.0,Arrow^.scale.x,1e-9);
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
    Drawing.done;
  end;
end;

procedure TLeaderEntityTest.SplineLeaderArrowUsesSplineTangent;
var
  Drawing:TSimpleDrawing;
  Leader:PGDBObjLeader;
  DC:TDrawContext;
  Arrow:PGDBObjBlockInsert;
  ExpectedAngle:double;
begin
  Drawing.init(nil);
  Leader:=AllocAndInitLeader(nil);
  try
    Leader^.ArrowHeadFlag:=1;
    Leader^.PathType:=1;
    Leader^.ArrowSize:=1.0;
    Leader^.AddVertex(CreateVertex(0,0,0));
    Leader^.AddVertex(CreateVertex(2,3,0));
    Leader^.AddVertex(CreateVertex(4,0,0));
    Leader^.AddVertex(CreateVertex(5,0,0));

    DC:=Drawing.CreateDrawingRC;
    Leader^.FormatEntity(Drawing,DC);

    CheckEquals(3,Leader^.ConstObjArray.Count,
      'spline leader with text tail must build spline, tail and arrow');

    Arrow:=PGDBObjBlockInsert(Leader^.ConstObjArray.GetData(2));
    CheckEquals(GDBBlockInsertID,Arrow^.GetObjType,
      'spline leader arrow must be a block insert');
    ExpectedAngle:=VertexAngle(CreateVertex2D(0,0),CreateVertex2D(2,6))-pi;
    CheckEquals(ExpectedAngle,Arrow^.rotate,1e-9,
      'spline leader arrow must follow the spline start tangent');
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
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

// Размерный стиль на стеке: обнуляем и заполняем только нужные поля.
// Resolve-функции читают лишь простые поля Arrows/Lines/Units. Стиль
// размещаем в куче: AllocMem обнуляет память, а компилятор не вставляет
// авто-финализацию управляемого поля Name:AnsiString (она роняла AV на
// локальном объекте). Освобождать через FreeMem, как и сам выноску.
function NewTestDimStyle:PGDBDimStyle;
begin
  Result:=AllocMem(SizeOf(GDBDimStyle));
  Result^.Arrows.DIMLDRBLK:=TSOblique;
  Result^.Arrows.DIMASZ:=2.0;
  Result^.Units.DIMSCALE:=3.0;
  Result^.Lines.DIMLWD:=50;
  Result^.Lines.DIMCLRD:=7;
end;

procedure TLeaderEntityTest.NewFieldsDefaultToInherit;
var
  Leader:PGDBObjLeader;
begin
  Leader:=AllocAndInitLeader(nil);
  try
    CheckEquals(LeaderArrowStyleInherit,Leader^.ArrowStyleIndex,
      'новый объект наследует индекс стрелки из стиля');
    CheckEquals(LeaderArrowSizeInherit,Leader^.ArrowSize,1e-9,
      'новый объект наследует масштаб стрелки из стиля');
    CheckEquals(LeaderLineWeightInherit,Leader^.DimLineWeight,
      'новый объект наследует толщину линии из стиля');
    CheckEquals(LeaderColorInherit,Leader^.DimLineColor,
      'новый объект наследует цвет линии из стиля');
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
  end;
end;

procedure TLeaderEntityTest.ResolveArrowStyleUsesOverrideThenStyle;
var
  Leader:PGDBObjLeader;
  DimStyle:PGDBDimStyle;
begin
  DimStyle:=NewTestDimStyle;
  Leader:=AllocAndInitLeader(nil);
  try
    // По умолчанию индекс берётся из размерного стиля
    CheckEquals(ord(TSOblique),
      ResolveLeaderArrowStyleIndex(Leader^,DimStyle),
      'без переопределения индекс стрелки берётся из стиля');
    // Индивидуальное значение имеет приоритет над стилем
    Leader^.ArrowStyleIndex:=ord(TSDot);
    CheckEquals(ord(TSDot),
      ResolveLeaderArrowStyleIndex(Leader^,DimStyle),
      'переопределённый индекс стрелки имеет приоритет');
    // Без стиля и переопределения — закрытая заполненная стрелка
    Leader^.ArrowStyleIndex:=LeaderArrowStyleInherit;
    CheckEquals(ord(TSClosedFilled),
      ResolveLeaderArrowStyleIndex(Leader^,nil),
      'без стиля используется закрытая заполненная стрелка');
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
    FreeMem(DimStyle);
  end;
end;

procedure TLeaderEntityTest.ResolveArrowSizeUsesOverrideThenStyle;
var
  Leader:PGDBObjLeader;
  DimStyle:PGDBDimStyle;
begin
  DimStyle:=NewTestDimStyle;
  Leader:=AllocAndInitLeader(nil);
  try
    // Из стиля: DIMASZ(2)*DIMSCALE(3)=6
    CheckEquals(6.0,ResolveLeaderArrowSize(Leader^,DimStyle),1e-9,
      'без переопределения масштаб = DIMASZ*DIMSCALE');
    Leader^.ArrowSize:=4.0;
    CheckEquals(4.0,ResolveLeaderArrowSize(Leader^,DimStyle),1e-9,
      'переопределённый масштаб стрелки имеет приоритет');
    Leader^.ArrowSize:=LeaderArrowSizeInherit;
    CheckEquals(1.0,ResolveLeaderArrowSize(Leader^,nil),1e-9,
      'без стиля масштаб по умолчанию равен 1');
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
    FreeMem(DimStyle);
  end;
end;

procedure TLeaderEntityTest.ResolveDimLineWeightUsesOverrideThenStyle;
var
  Leader:PGDBObjLeader;
  DimStyle:PGDBDimStyle;
begin
  DimStyle:=NewTestDimStyle;
  Leader:=AllocAndInitLeader(nil);
  try
    CheckEquals(50,ResolveLeaderDimLineWeight(Leader^,DimStyle),
      'без переопределения толщина берётся из стиля (DIMLWD)');
    Leader^.DimLineWeight:=25;
    CheckEquals(25,ResolveLeaderDimLineWeight(Leader^,DimStyle),
      'переопределённая толщина линии имеет приоритет');
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
    FreeMem(DimStyle);
  end;
end;

procedure TLeaderEntityTest.ResolveDimLineColorUsesOverrideThenStyle;
var
  Leader:PGDBObjLeader;
  DimStyle:PGDBDimStyle;
begin
  DimStyle:=NewTestDimStyle;
  Leader:=AllocAndInitLeader(nil);
  try
    CheckEquals(7,ResolveLeaderDimLineColor(Leader^,DimStyle),
      'без переопределения цвет берётся из стиля (DIMCLRD)');
    Leader^.DimLineColor:=3;
    CheckEquals(3,ResolveLeaderDimLineColor(Leader^,DimStyle),
      'переопределённый цвет линии имеет приоритет');
  finally
    Leader^.done;
    FreeMem(Pointer(Leader));
    FreeMem(DimStyle);
  end;
end;

procedure TLeaderEntityTest.CloneCopiesIndividualOverrides;
var
  Leader:PGDBObjLeader;
  Clone:PGDBObjLeader;
begin
  Leader:=AllocAndInitLeader(nil);
  try
    Leader^.ArrowStyleIndex:=ord(TSDot);
    Leader^.ArrowSize:=2.5;
    Leader^.DimLineWeight:=35;
    Leader^.DimLineColor:=4;

    Clone:=PGDBObjLeader(Leader^.Clone(nil));
    try
      CheckEquals(ord(TSDot),Clone^.ArrowStyleIndex,
        'клон копирует индивидуальный индекс стрелки');
      CheckEquals(2.5,Clone^.ArrowSize,1e-9,
        'клон копирует индивидуальный масштаб стрелки');
      CheckEquals(35,Clone^.DimLineWeight,
        'клон копирует индивидуальную толщину линии');
      CheckEquals(4,Clone^.DimLineColor,
        'клон копирует индивидуальный цвет линии');
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
