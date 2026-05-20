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
unit uzeenttransformscalars;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeentity,
  uzegeometrytypes;

function MatrixAxisScaleFactor(const Matrix: TzeTypedMatrix4d;
  AxisIndex: Integer): Double;
function MatrixPlanarScaleFactor(const Matrix: TzeTypedMatrix4d): Double;
procedure ApplyEntityScalarScale(Entity: PGDBObjEntity;
  const Matrix: TzeTypedMatrix4d);

implementation

uses
  Math,
  uzeconsts,
  uzegeometry,
  uzeenttext,
  uzeentmtext,
  uzeenthatch,
  uzeentlwpolyline;

function MatrixAxisScaleFactor(const Matrix: TzeTypedMatrix4d;
  AxisIndex: Integer): Double;
begin
  Result := oneVertexlength(PzePoint3d(@Matrix.mtr.v[AxisIndex])^);
  if Result < eps then
    Result := 1.0;
end;

function MatrixPlanarScaleFactor(const Matrix: TzeTypedMatrix4d): Double;
begin
  Result :=
    (MatrixAxisScaleFactor(Matrix, 0) + MatrixAxisScaleFactor(Matrix, 1)) / 2;
  if Result < eps then
    Result := 1.0;
end;

procedure ScaleLWPolylineWidths(LWPolyline: PGDBObjLWPolyline;
  const ScaleFactor: Double);
var
  I: Integer;
  Width: PGLLWWidth;
begin
  if LWPolyline = nil then
    Exit;

  if SameValue(ScaleFactor, 1.0)
    or (LWPolyline^.Width2D_in_OCS_Array.Count = 0) then
    Exit;

  for I := 0 to LWPolyline^.Width2D_in_OCS_Array.Count - 1 do
  begin
    Width := LWPolyline^.Width2D_in_OCS_Array.getDataMutable(I);
    if Width <> nil then
    begin
      Width^.startw := Width^.startw * ScaleFactor;
      Width^.endw := Width^.endw * ScaleFactor;
      Width^.hw := (not SameValue(Width^.startw, 0.0))
        or (not SameValue(Width^.endw, 0.0));
    end;
  end;
end;

procedure ApplyEntityScalarScale(Entity: PGDBObjEntity;
  const Matrix: TzeTypedMatrix4d);
var
  PlanarScale: Double;
begin
  if Entity = nil then
    Exit;

  case Entity^.GetObjType of
    GDBTextID:
      PGDBObjText(Entity)^.textprop.size :=
        PGDBObjText(Entity)^.textprop.size * MatrixAxisScaleFactor(Matrix, 1);
    GDBMTextID:
      begin
        PGDBObjMText(Entity)^.textprop.size :=
          PGDBObjMText(Entity)^.textprop.size * MatrixAxisScaleFactor(Matrix, 1);
        PGDBObjMText(Entity)^.Width :=
          PGDBObjMText(Entity)^.Width * MatrixAxisScaleFactor(Matrix, 0);
      end;
    GDBHatchID:
      PGDBObjHatch(Entity)^.Scale :=
        PGDBObjHatch(Entity)^.Scale * MatrixPlanarScaleFactor(Matrix);
    GDBLWPolylineID:
      begin
        PlanarScale := MatrixPlanarScaleFactor(Matrix);
        ScaleLWPolylineWidths(PGDBObjLWPolyline(Entity), PlanarScale);
      end;
  end;
end;

end.
