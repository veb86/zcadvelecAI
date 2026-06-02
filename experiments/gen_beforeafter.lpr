program gen_beforeafter;
{$mode objfpc}{$H+}
uses Interfaces, Classes, Controls, StdCtrls, Graphics, Types, Forms, SysUtils, typinfo;
type
  TArrowStyle=(TSClosedFilled,TSClosedBlank,TSClosed,TSDot,TSArchitecturalTick,TSOblique,TSOpen,TSOriginIndicator,TSOriginIndicator2,
            TSRightAngle,TSOpen30,TSDotSmall,TSDotBlank,TSDotSmallBlank,TSBox,TSBoxFilled,TSDatumTriangle,TSDatumtTriangleFilled,TSIntegral,TSUserDef);
{$I drawarrow_body.inc}
var bmp:TBitmap; png:TPortableNetworkGraphic; i,itemH,colW,h,half:integer; nm:string;
begin
  Application.Initialize;
  itemH:=22; colW:=210; half:=colW; h:=itemH*20+30;
  bmp:=TBitmap.Create; bmp.SetSize(colW*2, h);
  bmp.Canvas.Brush.Color:=clWhite; bmp.Canvas.FillRect(0,0,colW*2,h);
  bmp.Canvas.Font.Color:=clBlack;
  bmp.Canvas.Font.Style:=[fsBold];
  bmp.Canvas.TextOut(8,6,'BEFORE (text only)');
  bmp.Canvas.TextOut(colW+8,6,'AFTER (owner-draw preview)');
  bmp.Canvas.Font.Style:=[];
  for i:=0 to 19 do begin
    nm:=GetEnumName(TypeInfo(TArrowStyle),i);
    // before: plain text only
    bmp.Canvas.TextOut(8, 30+i*itemH+3, nm);
    // after: icon + text
    drawArrow(bmp.Canvas, Rect(colW, 30+i*itemH, colW*2, 30+i*itemH+itemH), nm, TArrowStyle(i));
  end;
  png:=TPortableNetworkGraphic.Create; png.Assign(bmp);
  png.SaveToFile('beforeafter.png');
  writeln('done'); png.Free; bmp.Free;
end.
