program capture_mesh;

// Записывает реальную геометрию куба до/после переключения режима.
// Автор: OpenAI Codex, 2026-10-27. Зависимости: zengine, FPCUnit recorder.
{$mode objfpc}{$H+}

uses
  Interfaces, SysUtils, uzegeometrytypes, uzgldrawcontext,
  uzeentpolyfacemesh, uzctmeshrecordingdrawer;

const
  Points: array[0..7] of TzePoint3d = (
    (x:-1;y:-1;z:-1),(x:1;y:-1;z:-1),(x:1;y:1;z:-1),(x:-1;y:1;z:-1),
    (x:-1;y:-1;z:1),(x:1;y:-1;z:1),(x:1;y:1;z:1),(x:-1;y:1;z:1));
  Faces: array[0..5,0..3] of Integer = (
    (1,4,3,2),(5,6,7,8),(1,2,6,5),(2,3,7,6),(3,4,8,7),(4,1,5,8));
var
  Mesh: PGDBObjPolyFaceMesh;
  Drawer: TMeshRecordingDrawer;
  Context: TDrawContext;
  Face: TFaceIndices;
  i: Integer;
begin
  if ParamCount<>1 then
    raise Exception.Create('Usage: capture_mesh output-directory');
  ForceDirectories(ParamStr(1));
  Mesh := AllocAndInitPolyFaceMesh(nil);
  Drawer := TMeshRecordingDrawer.Create;
  try
    for i:=0 to High(Points) do
      Mesh^.VertexArrayInOCS.PushBackData(Points[i]);
    Mesh^.FormatWithoutSnapArray;
    for i:=0 to High(Faces) do begin
      Face.Vertex1 := Faces[i,0];
      Face.Vertex2 := Faces[i,1];
      Face.Vertex3 := Faces[i,2];
      Face.Vertex4 := Faces[i,3];
      Face.VertexCount := 4;
      Mesh^.AddFace(Face);
    end;
    Context := CreateAbstractRC;
    Context.drawer := Drawer;
    Mesh^.DrawGeometry(1,Context,IRFully);
    Drawer.SaveCapture(ParamStr(1)+'/wireframe.txt');
    {$IFNDEF POLYFACEMESH_BASELINE}
    Drawer.Reset;
    Mesh^.ShowSolid := True;
    Mesh^.DrawGeometry(1,Context,IRFully);
    Drawer.SaveCapture(ParamStr(1)+'/solid.txt');
    {$ENDIF}
  finally
    Drawer.Free;
    Mesh^.done;
    FreeMem(Pointer(Mesh));
  end;
end.
