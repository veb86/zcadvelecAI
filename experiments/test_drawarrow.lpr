program test_drawarrow;
{$mode objfpc}{$H+}
uses
  Interfaces, Classes, Controls, StdCtrls, Graphics, Types, Forms, SysUtils;

type
  TArrowStyle=(TSClosedFilled,TSClosedBlank,TSClosed,TSDot,TSArchitecturalTick,TSOblique,TSOpen,TSOriginIndicator,TSOriginIndicator2,
            TSRightAngle,TSOpen30,TSDotSmall,TSDotBlank,TSDotSmallBlank,TSBox,TSBoxFilled,TSDatumTriangle,TSDatumtTriangleFilled,TSIntegral,TSUserDef);

const
  COneSixth = 1.0/6.0;

procedure drawArrow(canvas: TCanvas; ARect: TRect; const s: string; arrowStyle: TArrowStyle);
const
  CMargin = 3;
var
  ih, scale, tipx, cy, iconW: integer;
  c, savePenColor, saveBrushColor: TColor;
  savePenWidth: integer;
  savePenStyle: TPenStyle;
  saveBrushStyle: TBrushStyle;

  function P(bx, by: double): TPoint;
  begin
    P.x := tipx - round(bx*scale);
    P.y := cy   - round(by*scale);
  end;

  procedure DrawLine(x1, y1, x2, y2: double);
  var p1, p2: TPoint;
  begin
    p1 := P(x1, y1);
    p2 := P(x2, y2);
    canvas.Line(p1.x, p1.y, p2.x, p2.y);
  end;

  procedure DrawCircle(bx, by, r: double; filled: boolean);
  var ce: TPoint; rr: integer;
  begin
    ce := P(bx, by);
    rr := round(r*scale);
    if rr < 1 then rr := 1;
    if filled then canvas.Brush.Style := bsSolid else canvas.Brush.Style := bsClear;
    canvas.Ellipse(ce.x-rr, ce.y-rr, ce.x+rr, ce.y+rr);
    canvas.Brush.Style := bsSolid;
  end;

  procedure DrawPoly(const pts: array of TPoint; filled: boolean);
  begin
    if filled then begin canvas.Brush.Style := bsSolid; canvas.Polygon(pts); end
    else begin canvas.Brush.Style := bsClear; canvas.Polygon(pts); canvas.Brush.Style := bsSolid; end;
  end;

  procedure DrawArc(bx, by, r, a1, a2: double);
  const CSeg = 12;
  var pts: array[0..CSeg] of TPoint; i: integer; a: double;
  begin
    for i := 0 to CSeg do
    begin
      a := a1 + (a2-a1)*i/CSeg;
      pts[i] := P(bx + r*cos(a), by + r*sin(a));
    end;
    canvas.Polyline(pts);
  end;

begin
  ih := ARect.Bottom - ARect.Top;
  scale := ih - 2*CMargin;
  if scale < 6 then scale := 6;
  tipx := ARect.Left + CMargin;
  cy := (ARect.Top + ARect.Bottom) div 2;
  iconW := scale + 2*CMargin;
  savePenColor := canvas.Pen.Color;
  savePenWidth := canvas.Pen.Width;
  savePenStyle := canvas.Pen.Style;
  saveBrushColor := canvas.Brush.Color;
  saveBrushStyle := canvas.Brush.Style;
  c := canvas.Font.Color;
  canvas.Pen.Color := c;
  canvas.Pen.Width := 1;
  canvas.Pen.Style := psSolid;
  canvas.Brush.Color := c;
  case arrowStyle of
    TSClosedFilled: DrawPoly([P(0,0), P(-1,-COneSixth), P(-1,COneSixth)], true);
    TSClosedBlank: DrawPoly([P(0,0), P(-1,-COneSixth), P(-1,COneSixth)], false);
    TSClosed: begin DrawPoly([P(0,0), P(-1,-COneSixth), P(-1,COneSixth)], false); DrawLine(0,0, -1,0); end;
    TSDot: begin DrawLine(-0.5,0, -1,0); DrawCircle(0,0, 0.45, true); end;
    TSArchitecturalTick: DrawLine(-0.5,-0.5, 0.5,0.5);
    TSOblique: DrawLine(-0.5,-0.5, 0.5,0.5);
    TSOpen: begin DrawLine(0,0, -1,-COneSixth); DrawLine(0,0, -1,COneSixth); DrawLine(0,0, -1,0); end;
    TSOriginIndicator: begin DrawLine(0,0, -1,0); DrawCircle(0,0, 0.5, false); end;
    TSOriginIndicator2: begin DrawLine(-1,0, -0.5,0); DrawCircle(0,0, 0.5, false); DrawCircle(0,0, 0.25, false); end;
    TSRightAngle: begin DrawLine(0,0, -1,0); DrawLine(-0.5,0.5, 0,0); DrawLine(-0.5,-0.5, 0,0); end;
    TSOpen30: begin DrawLine(0,0, -1,0); DrawLine(-1,0.2679, 0,0); DrawLine(-1,-0.2679, 0,0); end;
    TSDotSmall: DrawCircle(0,0, 0.25, true);
    TSDotBlank: begin DrawLine(-1,0, -0.5,0); DrawCircle(0,0, 0.5, false); end;
    TSDotSmallBlank: DrawCircle(0,0, 0.25, false);
    TSBox: begin DrawPoly([P(-0.5,0.5), P(0.5,0.5), P(0.5,-0.5), P(-0.5,-0.5)], false); DrawLine(-0.5,0, -1,0); end;
    TSBoxFilled: begin DrawPoly([P(-0.5,0.5), P(0.5,0.5), P(0.5,-0.5), P(-0.5,-0.5)], true); DrawLine(-0.5,0, -1,0); end;
    TSDatumTriangle: DrawPoly([P(0,0.5774), P(-1,0), P(0,-0.5774)], false);
    TSDatumtTriangleFilled: DrawPoly([P(0,0.5774), P(-1,0), P(0,-0.5774)], true);
    TSIntegral: begin DrawArc(-0.44424204, 0.09442656, 0.45416667, 4.92182849, 6.07374580); DrawArc( 0.44553400,-0.08824270, 0.45416667, 1.78023584, 2.93215314); end;
    TSUserDef: DrawPoly([P(0,0), P(-1,-COneSixth), P(-1,COneSixth)], true);
  end;
  canvas.Pen.Color := savePenColor;
  canvas.Pen.Width := savePenWidth;
  canvas.Pen.Style := savePenStyle;
  canvas.Brush.Color := saveBrushColor;
  canvas.Brush.Style := saveBrushStyle;
  ARect.Left := ARect.Left + iconW;
  canvas.TextRect(ARect, ARect.Left, (ARect.Top+ARect.Bottom-canvas.TextHeight(s)) div 2, s);
end;

var
  bmp: TBitmap;
  i: integer;
  png: TPortableNetworkGraphic;
  itemH, w, h: integer;
begin
  Application.Initialize;
  itemH := 22; w := 200; h := itemH*Ord(High(TArrowStyle))+itemH;
  bmp := TBitmap.Create;
  bmp.SetSize(w, h);
  bmp.Canvas.Brush.Color := clWhite;
  bmp.Canvas.FillRect(0,0,w,h);
  bmp.Canvas.Font.Color := clBlack;
  for i := 0 to Ord(High(TArrowStyle)) do
    drawArrow(bmp.Canvas, Rect(0, i*itemH, w, i*itemH+itemH), 'Style'+IntToStr(i), TArrowStyle(i));
  png := TPortableNetworkGraphic.Create;
  png.Assign(bmp);
  png.SaveToFile('/tmp/gh-issue-solver-1780398474987/experiments/arrows_preview.png');
  writeln('OK rows=', Ord(High(TArrowStyle))+1);
  png.Free; bmp.Free;
end.
