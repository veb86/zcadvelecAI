unit uzvhttpwallpreview;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, uzedrawingsimple, uzglviewareaabstract, uzestyleslayers,
  uzvhttpwallgeometry;

type
  TWallPreview = class(TComponent)
  private
    FView: TAbstractViewArea;
    FDrawing: PTSimpleDrawing;
    FPreviousDraw: TOnDrawHeplGeometry;
    FCandidates: TWallCandidates;
    FRecognized: TWallRecognitions;
    FShowRecognized: Boolean;
    FDisplayed: Boolean;
    function PreviewLayer: PGDBLayerProp;
    procedure Draw;
  public
    constructor Create(ADrawing: PTSimpleDrawing); reintroduce;
    destructor Destroy; override;
    procedure ShowCandidates(const Candidates: TWallCandidates);
    procedure ShowRecognized(const Recognized: TWallRecognitions);
    procedure Refresh;
    function IsDisplayed: Boolean;
  end;

implementation

uses
  SysUtils, uzeTypes, uzgldrawcontext, uzgldrawerabstract, uzepalette,
  uzvhttpwallcollector;

function TWallPreview.PreviewLayer: PGDBLayerProp;
begin
  Result := FDrawing^.GetLayerTable^.getAddres(string(WallPreviewLayer));
end;

constructor TWallPreview.Create(ADrawing: PTSimpleDrawing);
var
  Layer: PGDBLayerProp;
begin
  inherited Create(ADrawing^.wa);
  FView := ADrawing^.wa;
  FDrawing := ADrawing;
  Layer := PreviewLayer;
  if Layer = nil then begin
    Layer := FDrawing^.GetLayerTable^.addlayer(WallPreviewLayer, 4, -3,
      True, False, False, 'Предпросмотр распознавания стен', TLOMerge);
    FDrawing^.GetLayerTable^.NewState;
  end;
  if (Layer = nil) or not Layer^._on or Layer^._print then
    raise Exception.Create('Слой ZCAD_AI_WALL_PREVIEW должен быть видимым и непечатаемым.');
  FPreviousDraw := FView.OnDrawHeplGeometry;
  FView.OnDrawHeplGeometry := @Draw;
end;

destructor TWallPreview.Destroy;
var
  DrawMethod: TOnDrawHeplGeometry;
begin
  DrawMethod := @Draw;
  if (FView <> nil) and
    (TMethod(FView.OnDrawHeplGeometry).Code = TMethod(DrawMethod).Code) and
    (TMethod(FView.OnDrawHeplGeometry).Data = Pointer(Self)) then
    FView.OnDrawHeplGeometry := FPreviousDraw;
  inherited Destroy;
end;

procedure TWallPreview.Draw;
var
  DC: TDrawContext;
  Layer: PGDBLayerProp;
  Candidate: TWallCandidate;
  Wall: TWallRecognition;
  I: Integer;

  procedure Line(const A, B: TWallPoint);
  begin
    DC.Drawer.DrawLine3DInModelSpace(ToCADPoint(A), ToCADPoint(B),
      DC.DrawingContext.matrixs);
  end;

begin
  if Assigned(FPreviousDraw) then FPreviousDraw;
  FDisplayed := False;
  Layer := PreviewLayer;
  if (Layer = nil) or not Layer^._on or Layer^._print then Exit;
  DC := FView.CreateRC(True);
  DC.Drawer.StartRender(TRM_ModelSpace, DC.DrawingContext.matrixs);
  try
    DC.Drawer.SetDrawMode(TDM_Normal);
    if FShowRecognized then begin
      DC.Drawer.SetColor(palette[6].RGB);
      DC.Drawer.SetLineWidth(4);
      for Wall in FRecognized do
        for I := 1 to High(Wall.Vertices) do
          Line(Wall.Vertices[I - 1], Wall.Vertices[I]);
    end else begin
      DC.Drawer.SetColor(palette[4].RGB);
      DC.Drawer.SetLineWidth(3);
      for Candidate in FCandidates do
        for I := 0 to 1 do
          Line(Candidate.Edges[I].First, Candidate.Edges[I].Last);
      DC.Drawer.SetColor(palette[2].RGB);
      DC.Drawer.SetLineWidth(2);
      for Candidate in FCandidates do
        Line(Candidate.Center.First, Candidate.Center.Last);
    end;
    FDisplayed := True;
  finally
    DC.Drawer.SetLineWidth(1);
    DC.Drawer.EndRender;
  end;
end;

procedure TWallPreview.ShowCandidates(const Candidates: TWallCandidates);
begin
  FCandidates := Candidates;
  FShowRecognized := False;
  FDisplayed := False;
  Refresh;
end;

procedure TWallPreview.ShowRecognized(const Recognized: TWallRecognitions);
begin
  FRecognized := Recognized;
  FShowRecognized := True;
  FDisplayed := False;
  Refresh;
end;

procedure TWallPreview.Refresh;
begin
  FView.DrawOrInvalidate;
end;

function TWallPreview.IsDisplayed: Boolean;
var
  Layer: PGDBLayerProp;
begin
  Layer := PreviewLayer;
  Result := FDisplayed and (Layer <> nil) and Layer^._on and not Layer^._print;
end;

end.
