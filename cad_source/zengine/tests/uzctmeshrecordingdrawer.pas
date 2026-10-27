unit uzctmeshrecordingdrawer;

// Запись команд настоящей сущности для регрессий и воспроизводимых рендеров.
// Автор: OpenAI Codex, 2026-10-27. Зависимости: zengine, RTL.
{$mode objfpc}{$H+}

interface

uses
  SysUtils, uzgldrawergeneral, uzegeometrytypes;

type
  TMeshLine = record
    Points: array[0..1] of TzePoint3d;
  end;
  TMeshTriangle = record
    Normal: TzePoint3d;
    Points: array[0..2] of TzePoint3d;
  end;
  TMeshRecordingDrawer = class(TZGLGeneralDrawer)
  public
    Lines: array of TMeshLine;
    Triangles: array of TMeshTriangle;
    procedure DrawLine3DInModelSpace(const p1,p2:TzePoint3d;
      var matrixs:tmatrixs); override;
    procedure DrawTriangle3DInModelSpace(const normal,p1,p2,p3:TzePoint3d;
      var matrixs:tmatrixs); override;
    // Сбрасывает запись между переключениями режима отображения.
    procedure Reset;
    // Сохраняет команды для headless OpenGL-рендера в experiments/issue1470.
    procedure SaveCapture(const FileName:string);
  end;

implementation

// Сохраняет ребро, не обращаясь к видеокарте.
procedure TMeshRecordingDrawer.DrawLine3DInModelSpace(const p1,p2:TzePoint3d;
  var matrixs:tmatrixs);
var
  Index: Integer;
begin
  Index := Length(Lines);
  SetLength(Lines,Index+1);
  Lines[Index].Points[0] := p1;
  Lines[Index].Points[1] := p2;
end;

// Сохраняет треугольник вместе с нормалью из реального пути DrawGeometry.
procedure TMeshRecordingDrawer.DrawTriangle3DInModelSpace(
  const normal,p1,p2,p3:TzePoint3d; var matrixs:tmatrixs);
var
  Index: Integer;
begin
  Index := Length(Triangles);
  SetLength(Triangles,Index+1);
  Triangles[Index].Normal := normal;
  Triangles[Index].Points[0] := p1;
  Triangles[Index].Points[1] := p2;
  Triangles[Index].Points[2] := p3;
end;

procedure TMeshRecordingDrawer.Reset;
begin
  SetLength(Lines,0);
  SetLength(Triangles,0);
end;

procedure TMeshRecordingDrawer.SaveCapture(const FileName:string);
var
  Output: TextFile;
  Triangle: TMeshTriangle;
  Line: TMeshLine;
  Point: TzePoint3d;
begin
  AssignFile(Output,FileName);
  Rewrite(Output);
  try
    for Triangle in Triangles do begin
      Write(Output,'triangle ',Triangle.Normal.x:0:9,' ',Triangle.Normal.y:0:9,
        ' ',Triangle.Normal.z:0:9);
      for Point in Triangle.Points do
        Write(Output,' ',Point.x:0:9,' ',Point.y:0:9,' ',Point.z:0:9);
      WriteLn(Output);
    end;
    for Line in Lines do begin
      Write(Output,'line');
      for Point in Line.Points do
        Write(Output,' ',Point.x:0:9,' ',Point.y:0:9,' ',Point.z:0:9);
      WriteLn(Output);
    end;
  finally
    CloseFile(Output);
  end;
end;

end.
