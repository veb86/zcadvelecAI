unit uzctenttransformscalars;
{$Codepage UTF8}
{$Mode delphi}{$H+}

interface

uses
  SysUtils,
  fpcunit,
  testregistry,
  uzegeometry,
  uzegeometrytypes,
  uzeentity,
  uzeenttransformscalars,
  uzeenttext,
  uzeentmtext,
  uzeenthatch,
  uzeentlwpolyline;

type
  TEntityTransformScalarTest = class(TTestCase)
  published
    procedure TextHeightScalesWithTransformedYAxis;
    procedure MTextHeightAndWidthScaleWithMatrixAxes;
    procedure HatchPatternScaleUsesPlanarScale;
    procedure LWPolylineWidthsUsePlanarScale;
  end;

implementation

procedure TEntityTransformScalarTest.TextHeightScalesWithTransformedYAxis;
var
  Text: PGDBObjText;
  Matrix: TzeTypedMatrix4d;
begin
  Text := GDBObjText.CreateInstance;
  try
    Text^.textprop.size := 2.0;
    Matrix := CreateScaleMatrix(0.5);

    ApplyEntityScalarScale(PGDBObjEntity(Text), Matrix);

    CheckEquals(1.0, Text^.textprop.size, 1e-9,
      'TEXT height must follow the transformed Y axis scale');
  finally
    Text^.done;
    FreeMem(Pointer(Text));
  end;
end;

procedure TEntityTransformScalarTest.MTextHeightAndWidthScaleWithMatrixAxes;
var
  MText: PGDBObjMText;
  Matrix: TzeTypedMatrix4d;
begin
  MText := GDBObjMText.CreateInstance;
  try
    MText^.textprop.size := 4.0;
    MText^.Width := 8.0;
    Matrix := CreateScaleMatrix(2.0, 0.5, 1.0);

    ApplyEntityScalarScale(PGDBObjEntity(MText), Matrix);

    CheckEquals(2.0, MText^.textprop.size, 1e-9,
      'MTEXT height must follow the transformed Y axis scale');
    CheckEquals(16.0, MText^.Width, 1e-9,
      'MTEXT width must follow the transformed X axis scale');
  finally
    MText^.done;
    FreeMem(Pointer(MText));
  end;
end;

procedure TEntityTransformScalarTest.HatchPatternScaleUsesPlanarScale;
var
  Hatch: PGDBObjHatch;
  Matrix: TzeTypedMatrix4d;
begin
  Hatch := GDBObjHatch.CreateInstance;
  try
    Hatch^.Scale := 1.0;
    Matrix := CreateScaleMatrix(0.5);

    ApplyEntityScalarScale(PGDBObjEntity(Hatch), Matrix);

    CheckEquals(0.5, Hatch^.Scale, 1e-9,
      'HATCH pattern scale must follow uniform block/proxy scale');
  finally
    Hatch^.done;
    FreeMem(Pointer(Hatch));
  end;
end;

procedure TEntityTransformScalarTest.LWPolylineWidthsUsePlanarScale;
var
  LWPolyline: PGDBObjLWPolyline;
  WidthRecord: GLLWWidth;
  StoredWidth: PGLLWWidth;
  Matrix: TzeTypedMatrix4d;
begin
  LWPolyline := GDBObjLWPolyline.CreateInstance;
  try
    FillChar(WidthRecord, SizeOf(WidthRecord), 0);
    WidthRecord.startw := 2.0;
    WidthRecord.endw := 4.0;
    WidthRecord.hw := True;
    LWPolyline^.Width2D_in_OCS_Array.PushBackData(WidthRecord);

    Matrix := CreateScaleMatrix(0.5);
    ApplyEntityScalarScale(PGDBObjEntity(LWPolyline), Matrix);

    StoredWidth := LWPolyline^.Width2D_in_OCS_Array.getDataMutable(0);
    CheckEquals(1.0, StoredWidth^.startw, 1e-9,
      'LWPOLYLINE start width must follow uniform block/proxy scale');
    CheckEquals(2.0, StoredWidth^.endw, 1e-9,
      'LWPOLYLINE end width must follow uniform block/proxy scale');
    CheckTrue(StoredWidth^.hw,
      'LWPOLYLINE width flag must stay set for non-zero scaled widths');
  finally
    LWPolyline^.done;
    FreeMem(Pointer(LWPolyline));
  end;
end;

initialization
  RegisterTest(TEntityTransformScalarTest);

end.
