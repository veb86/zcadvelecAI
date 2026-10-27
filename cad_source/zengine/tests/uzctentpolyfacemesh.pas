unit uzctentpolyfacemesh;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, fpcunit, testregistry,
  uzgldrawcontext, uzegeometrytypes, uzegeometry, uzeentity,
  uzctmeshrecordingdrawer, uzeentpolyfacemesh;

type
  TPolyFaceMeshTest = class(TTestCase)
  private
    Mesh: PGDBObjPolyFaceMesh;
    Drawer: TMeshRecordingDrawer;
    Context: TDrawContext;
    Face: TFaceIndices;
    procedure Draw;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure AllocAndInitAllowsAddingFaces;
    procedure ConstructorsDefaultToWireframe;
    procedure FilledFacesAreSubmitted;
    procedure SwitchingBackStopsSubmittingFaces;
    procedure QuadUsesContourOrder;
    procedure HiddenEdgesDoNotHideFaces;
    procedure InvalidFacesAreSkipped;
    procedure DegenerateTrianglesAreSkipped;
    procedure ClonePreservesDisplayMode;
    procedure FillUsesTransformedVertices;
  end;

implementation

// Общий минимальный контур для треугольных и четырёхугольных граней.
procedure TPolyFaceMeshTest.SetUp;
const
  Points: array[0..3] of TzePoint3d = (
    (x:0;y:0;z:0),(x:1;y:0;z:0),(x:1;y:1;z:0),(x:0;y:1;z:0));
var
  Point: TzePoint3d;
begin
  Mesh := AllocAndInitPolyFaceMesh(nil);
  Drawer := TMeshRecordingDrawer.Create;
  Context := CreateAbstractRC;
  Context.drawer := Drawer;
  for Point in Points do
    Mesh^.VertexArrayInOCS.PushBackData(Point);
  Mesh^.FormatWithoutSnapArray;
  Face := Default(TFaceIndices);
  Face.Vertex1 := 1;
  Face.Vertex2 := 2;
  Face.Vertex3 := 3;
  Face.VertexCount := 3;
end;

// Освобождает обе части фикстуры даже после неудачной проверки.
procedure TPolyFaceMeshTest.TearDown;
begin
  Drawer.Free;
  Mesh^.done;
  FreeMem(Pointer(Mesh));
end;

// Повторно использует настоящее рисование после изменения геометрии или режима.
procedure TPolyFaceMeshTest.Draw;
begin
  Drawer.Reset;
  Mesh^.DrawGeometry(1,Context,IRFully);
end;

// Оба пути создания сохраняют прежний недорогой режим отображения.
procedure TPolyFaceMeshTest.ConstructorsDefaultToWireframe;
var
  InitializedMesh: GDBObjPolyFaceMesh;
begin
  AssertFalse('initnul defaults to wireframe',Mesh^.ShowSolid);
  InitializedMesh.init(nil,nil,-1);
  try
    AssertFalse('init defaults to wireframe',InitializedMesh.ShowSolid);
  finally
    InitializedMesh.done;
  end;
  Mesh^.AddFace(Face);
  Draw;
  AssertEquals('visible edges',3,Length(Drawer.Lines));
  AssertEquals('no filled triangles',0,Length(Drawer.Triangles));
end;

// Регрессия: до исправления отправлялись только три ребра и ни одной грани.
procedure TPolyFaceMeshTest.FilledFacesAreSubmitted;
begin
  Mesh^.AddFace(Face);
  Mesh^.ShowSolid := True;
  Draw;
  AssertEquals('visible edges',3,Length(Drawer.Lines));
  AssertEquals('filled triangle',1,Length(Drawer.Triangles));
  AssertEquals('unit normal',1.0,Drawer.Triangles[0].Normal.Length,1e-9);
  AssertEquals('front normal',1.0,Drawer.Triangles[0].Normal.z,1e-9);
end;

// Повторное переключение не оставляет закэшированную заливку на экране.
procedure TPolyFaceMeshTest.SwitchingBackStopsSubmittingFaces;
begin
  Mesh^.AddFace(Face);
  Mesh^.ShowSolid := True;
  Draw;
  AssertEquals('solid mode',1,Length(Drawer.Triangles));
  Mesh^.ShowSolid := False;
  Draw;
  AssertEquals('wireframe mode',0,Length(Drawer.Triangles));
  AssertEquals('wireframe edges',3,Length(Drawer.Lines));
end;

// Два треугольника покрывают весь контур без порядка углов SOLID.
procedure TPolyFaceMeshTest.QuadUsesContourOrder;
begin
  Face.Vertex4 := 4;
  Face.VertexCount := 4;
  Mesh^.AddFace(Face);
  Mesh^.ShowSolid := True;
  Draw;
  AssertEquals('four contour edges',4,Length(Drawer.Lines));
  AssertEquals('two triangles',2,Length(Drawer.Triangles));
  AssertTrue('first triangle ends at vertex 3',Drawer.Triangles[0].Points[2].IsEqual(
    Mesh^.VertexArrayInWCS.Items[2]));
  AssertTrue('second triangle starts at vertex 1',Drawer.Triangles[1].Points[0].IsEqual(
    Mesh^.VertexArrayInWCS.Items[0]));
  AssertTrue('second triangle uses vertex 3',Drawer.Triangles[1].Points[1].IsEqual(
    Mesh^.VertexArrayInWCS.Items[2]));
  AssertTrue('second triangle ends at vertex 4',Drawer.Triangles[1].Points[2].IsEqual(
    Mesh^.VertexArrayInWCS.Items[3]));
end;

// Знак DXF-индекса скрывает исходящее ребро, но сохраняет заливку.
procedure TPolyFaceMeshTest.HiddenEdgesDoNotHideFaces;
begin
  Face.Vertex1 := -1;
  Face.Vertex2 := -2;
  Face.Vertex3 := -3;
  Mesh^.AddFace(Face);
  Mesh^.ShowSolid := True;
  Draw;
  AssertEquals('hidden edges',0,Length(Drawer.Lines));
  AssertEquals('visible face',1,Length(Drawer.Triangles));
end;

// Неполные и выходящие за границы индексы не передаются устройству вывода.
procedure TPolyFaceMeshTest.InvalidFacesAreSkipped;
begin
  Mesh^.ShowSolid := True;
  Face.Vertex1 := 0;
  Mesh^.AddFace(Face);
  Face.Vertex1 := 5;
  Mesh^.AddFace(Face);
  Face.Vertex1 := Low(Integer);
  Mesh^.AddFace(Face);
  Face.Vertex1 := 1;
  Face.VertexCount := 2;
  Mesh^.AddFace(Face);
  Face.VertexCount := 5;
  Mesh^.AddFace(Face);
  Draw;
  AssertEquals('no invalid triangles',0,Length(Drawer.Triangles));
end;

// Совпадающие вершины не создают нулевую нормаль.
procedure TPolyFaceMeshTest.DegenerateTrianglesAreSkipped;
begin
  Face.Vertex3 := 2;
  Mesh^.AddFace(Face);
  Mesh^.ShowSolid := True;
  Draw;
  AssertEquals('no degenerate triangles',0,Length(Drawer.Triangles));
end;

// Копирование сохраняет выбранный режим независимо от дальнейших изменений.
procedure TPolyFaceMeshTest.ClonePreservesDisplayMode;
var
  CopyMesh: PGDBObjPolyFaceMesh;
begin
  Mesh^.ShowSolid := True;
  Mesh^.AddFace(Face);
  CopyMesh := PGDBObjPolyFaceMesh(Mesh^.Clone(nil));
  try
    AssertTrue('solid clone',CopyMesh^.ShowSolid);
    Mesh^.ShowSolid := False;
    AssertTrue('independent mode',CopyMesh^.ShowSolid);
    CopyMesh^.FormatWithoutSnapArray;
    CopyMesh^.DrawGeometry(1,Context,IRFully);
    AssertEquals('clone renders its face',1,Length(Drawer.Triangles));
  finally
    CopyMesh^.done;
    FreeMem(Pointer(CopyMesh));
  end;
  CopyMesh := PGDBObjPolyFaceMesh(Mesh^.Clone(nil));
  try
    AssertFalse('wireframe clone',CopyMesh^.ShowSolid);
  finally
    CopyMesh^.done;
    FreeMem(Pointer(CopyMesh));
  end;
end;

// Преобразование применяется и к заливке, и к каркасу без старого Representation.
procedure TPolyFaceMeshTest.FillUsesTransformedVertices;
var
  Matrix: TzeTypedMatrix4d;
  Expected: TzePoint3d;
begin
  Mesh^.ShowSolid := True;
  Mesh^.AddFace(Face);
  Matrix := CreateRotationMatrixX(Pi/2);
  Mesh^.transform(Matrix);
  Matrix := CreateTranslationMatrix(10,20,30);
  Mesh^.transform(Matrix);
  Mesh^.FormatWithoutSnapArray;
  Draw;
  Expected := TzePoint3d.Make(10,20,30);
  AssertTrue('transformed triangle',Drawer.Triangles[0].Points[0].IsEqual(Expected));
  AssertTrue('transformed edge',Drawer.Lines[0].Points[0].IsEqual(Expected));
  AssertEquals('rotated unit normal',1.0,Abs(Drawer.Triangles[0].Normal.y),1e-9);
  AssertEquals('normal is perpendicular to Z',0.0,Drawer.Triangles[0].Normal.z,1e-9);
end;

procedure TPolyFaceMeshTest.AllocAndInitAllowsAddingFaces;
var
  LocalMesh: PGDBObjPolyFaceMesh;
  LocalFace: TFaceIndices;
  StoredFace: TFaceIndices;
begin
  LocalMesh := AllocAndInitPolyFaceMesh(nil);
  try
    LocalFace.Vertex1 := 11;
    LocalFace.Vertex2 := 1;
    LocalFace.Vertex3 := -2;
    LocalFace.Vertex4 := 10;
    LocalFace.VertexCount := 4;

    LocalMesh^.AddFace(LocalFace);

    AssertEquals('face count', 1, LocalMesh^.GetFaceCount);
    StoredFace := LocalMesh^.GetFaceVertices(0);
    AssertEquals('vertex 1', 11, StoredFace.Vertex1);
    AssertEquals('vertex 2', 1, StoredFace.Vertex2);
    AssertEquals('vertex 3', -2, StoredFace.Vertex3);
    AssertEquals('vertex 4', 10, StoredFace.Vertex4);
    AssertEquals('vertex count', 4, StoredFace.VertexCount);
  finally
    LocalMesh^.done;
    FreeMem(Pointer(LocalMesh));
  end;
end;

begin
  RegisterTests([TPolyFaceMeshTest]);
end.
