unit uzctentpolyfacemesh;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  testregistry,
  uzgldrawerabstract, uzgldrawcontext, uzegeometrytypes, uzegeometry,
  uzeentpolyfacemesh;

type
  TPolyFaceMeshTest = class(TTestCase)
  published
    procedure AllocAndInitAllowsAddingFaces;
    procedure FilledFacesAreSubmitted;
  end;

implementation

type
  // Подмена только устройства вывода: геометрию строит настоящая сущность.
  TMeshRecordingDrawer = class(TZGLAbstractDrawer)
  public
    LineCount, TriangleCount: Integer;
    procedure DrawLine3DInModelSpace(const p1,p2:TzePoint3d;
      var matrixs:tmatrixs); override;
    procedure DrawTriangle3DInModelSpace(const normal,p1,p2,p3:TzePoint3d;
      var matrixs:tmatrixs); override;
  end;

// Считаем рёбра, не обращаясь к видеокарте.
procedure TMeshRecordingDrawer.DrawLine3DInModelSpace(const p1,p2:TzePoint3d;
  var matrixs:tmatrixs);
begin
  Inc(LineCount);
end;

// Считаем заполненные треугольники из реального пути DrawGeometry.
procedure TMeshRecordingDrawer.DrawTriangle3DInModelSpace(
  const normal,p1,p2,p3:TzePoint3d; var matrixs:tmatrixs);
begin
  Inc(TriangleCount);
end;

// Минимальная сетка: один треугольник с тремя видимыми рёбрами.
procedure TPolyFaceMeshTest.FilledFacesAreSubmitted;
var
  Mesh: PGDBObjPolyFaceMesh;
  Drawer: TMeshRecordingDrawer;
  Context: TDrawContext;
  Face: TFaceIndices;
begin
  Mesh := AllocAndInitPolyFaceMesh(nil);
  Drawer := TMeshRecordingDrawer.Create;
  try
    Mesh^.VertexArrayInWCS.PushBackData(TzePoint3d.Make(0,0,0));
    Mesh^.VertexArrayInWCS.PushBackData(TzePoint3d.Make(1,0,0));
    Mesh^.VertexArrayInWCS.PushBackData(TzePoint3d.Make(0,1,0));
    Face := Default(TFaceIndices);
    Face.Vertex1 := 1;
    Face.Vertex2 := 2;
    Face.Vertex3 := 3;
    Face.VertexCount := 3;
    Mesh^.AddFace(Face);
    Context := CreateAbstractRC;
    Context.drawer := Drawer;
    Mesh^.DrawGeometry(1,Context,IRFully);
    AssertEquals('visible edges', 3, Drawer.LineCount);
    AssertEquals('filled triangle', 1, Drawer.TriangleCount);
  finally
    Drawer.Free;
    Mesh^.done;
    FreeMem(Pointer(Mesh));
  end;
end;

procedure TPolyFaceMeshTest.AllocAndInitAllowsAddingFaces;
var
  Mesh: PGDBObjPolyFaceMesh;
  Face: TFaceIndices;
  StoredFace: TFaceIndices;
begin
  Mesh := AllocAndInitPolyFaceMesh(nil);
  try
    Face.Vertex1 := 11;
    Face.Vertex2 := 1;
    Face.Vertex3 := -2;
    Face.Vertex4 := 10;
    Face.VertexCount := 4;

    Mesh^.AddFace(Face);

    AssertEquals('face count', 1, Mesh^.GetFaceCount);
    StoredFace := Mesh^.GetFaceVertices(0);
    AssertEquals('vertex 1', 11, StoredFace.Vertex1);
    AssertEquals('vertex 2', 1, StoredFace.Vertex2);
    AssertEquals('vertex 3', -2, StoredFace.Vertex3);
    AssertEquals('vertex 4', 10, StoredFace.Vertex4);
    AssertEquals('vertex count', 4, StoredFace.VertexCount);
  finally
    Mesh^.done;
    FreeMem(Pointer(Mesh));
  end;
end;

begin
  RegisterTests([TPolyFaceMeshTest]);
end.
