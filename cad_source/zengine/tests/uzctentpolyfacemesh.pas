unit uzctentpolyfacemesh;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  testregistry,
  uzeentpolyfacemesh;

type
  TPolyFaceMeshTest = class(TTestCase)
  published
    procedure AllocAndInitAllowsAddingFaces;
  end;

implementation

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
