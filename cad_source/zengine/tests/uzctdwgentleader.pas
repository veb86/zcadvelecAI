unit uzctdwgentleader;

{ Issue #1272: a LEADER entity is loaded correctly from DXF but was silently
  dropped when the same drawing was opened from a DWG file, because there was
  no DWG entity mapper registered for DWG_TYPE_LEADER. This test reproduces the
  regression at the unit level: it feeds a synthetic Dwg_Data carrying one
  LEADER entity through the registered DWG parser and asserts a GDBObjLeader
  with the expected vertices and flags is created in the drawing — exactly the
  GDBObjLeader the DXF path produces. }

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TDWGLeaderEntityTest = class(TTestCase)
  published
    procedure HandlerIsRegisteredForLeaderType;
    procedure LoadsLeaderEntityWithVerticesAndFlags;
  end;

implementation

uses
  SysUtils,
  Interfaces,
  dwg,
  dwgproc,
  uzeconsts,
  uzeentity,
  uzeentleader,
  uzegeometry,
  uzegeometrytypes,
  uzedrawingsimple,
  uzeffmanager,
  uzgldrawcontext,
  uzeTypes,
  uzedwgentityregistry,
  uzedwgentleader;

procedure InitRawEntity(var Obj: Dwg_Object; RawType: DWG_OBJECT_TYPE);
begin
  FillChar(Obj, SizeOf(Obj), 0);
  Obj.fixedtype := RawType;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
end;

procedure TDWGLeaderEntityTest.HandlerIsRegisteredForLeaderType;
begin
  CheckTrue(HasHandlerFor(DWG_TYPE_LEADER),
    'a DWG entity handler must be registered for LEADER (issue #1272)');
end;

procedure TDWGLeaderEntityTest.LoadsLeaderEntityWithVerticesAndFlags;
var
  Drawing: TSimpleDrawing;
  DC: TDrawContext;
  ZDC: TZDrawingContext;
  Raw: Dwg_Data;
  Objects: array[0..0] of Dwg_Object;
  Ent: Dwg_Object_Entity;
  Leader: Dwg_Entity_LEADER;
  Points: array[0..2] of BITCODE_3DPOINT;
  Entity: PGDBObjEntity;
  pobj: PGDBObjLeader;
  Vertex: PzePoint3d;
begin
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Leader, SizeOf(Leader), 0);
  FillChar(Points, SizeOf(Points), 0);

  Points[0].x := 1.0; Points[0].y := 2.0; Points[0].z := 0.0;
  Points[1].x := 3.0; Points[1].y := 4.0; Points[1].z := 0.0;
  Points[2].x := 5.0; Points[2].y := 6.0; Points[2].z := 0.0;

  Leader.num_points := Length(Points);
  Leader.points := @Points[0];
  Leader.path_type := 1;
  Leader.annot_type := 2;
  Leader.hookline_dir := 1;
  Leader.hookline_on := 1;
  Leader.arrowhead_on := 1;
  Leader.box_height := 15.5;
  Leader.box_width := 13.25;
  Leader.x_direction.x := 1.0;
  Leader.endptproj.x := 5.6;
  Leader.endptproj.y := 30.6;

  InitRawEntity(Objects[0], DWG_TYPE_LEADER);
  Ent.tio.LEADER := @Leader;
  Objects[0].tio.entity := @Ent;

  FillChar(Raw, SizeOf(Raw), 0);
  Raw.num_objects := Length(Objects);
  Raw.&object := @Objects[0];

  Drawing.init(nil);
  try
    DC := Drawing.CreateDrawingRC;
    ZDC.CreateRec(Drawing, Drawing.pObjRoot^, TLOLoad, DC);

    GetDWGParser.parseDwg_Data(ZDC, Raw, nil, 0);

    CheckEquals(1, Drawing.pObjRoot^.ObjArray.Count,
      'DWG LEADER must load as exactly one entity');

    Entity := PGDBObjEntity(Drawing.pObjRoot^.ObjArray.GetData(0));
    CheckEquals(GDBLeaderID, Entity^.GetObjType,
      'the loaded DWG entity must be a leader');

    pobj := PGDBObjLeader(Entity);
    CheckEquals(1, pobj^.PathType, 'path type (DXF 72) must be copied');
    CheckEquals(2, pobj^.AnnotationType, 'annotation type (DXF 73) must be copied');
    CheckEquals(1, pobj^.HookLineDirectionFlag, 'hookline direction (DXF 74)');
    CheckEquals(1, pobj^.HookLineFlag, 'hookline flag (DXF 75)');
    CheckEquals(1, pobj^.ArrowHeadFlag, 'arrowhead flag (DXF 71)');
    CheckEquals(15.5, pobj^.TextHeight, 1e-9, 'box height (DXF 40)');
    CheckEquals(13.25, pobj^.TextWidth, 1e-9, 'box width (DXF 41)');
    CheckEquals(3, pobj^.VertexArrayInOCS.Count,
      'all three leader vertices must be copied');

    Vertex := pobj^.VertexArrayInOCS.getDataMutable(2);
    CheckEquals(5.0, Vertex^.x, 1e-9, 'last vertex X');
    CheckEquals(6.0, Vertex^.y, 1e-9, 'last vertex Y');
    CheckEquals(0.0, Vertex^.z, 1e-9, 'last vertex Z');

    CheckEquals(1.0, pobj^.HorizontalDirection.x, 1e-9,
      'horizontal direction (DXF 211) must be copied');
    CheckEquals(5.6, pobj^.AnnotationOffset.x, 1e-9,
      'annotation offset X (DXF 213) must be copied');
    CheckEquals(30.6, pobj^.AnnotationOffset.y, 1e-9,
      'annotation offset Y (DXF 223) must be copied');
  finally
    Drawing.done;
  end;
end;

initialization
  RegisterTest(TDWGLeaderEntityTest);

end.
