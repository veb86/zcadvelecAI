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
{
@author(Andrey Zubarev <zamtmn@yandex.ru>) 
}

unit UGDBControlPointArray;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}
interface
uses uzepalette,uzgldrawcontext,gzctnrVector,sysutils,uzbtypes,uzegeometry,
     uzegeometrytypes,uzglviewareadata,uzesnap;
type
{Export+}
PGDBControlPointArray=^GDBControlPointArray;
{REGISTEROBJECTTYPE GDBControlPointArray}
GDBControlPointArray= object(GZVector{-}<controlpointdesc>{//})
                           SelectedCount:Integer;

                           procedure done;virtual;
                           procedure draw(var DC:TDrawContext;const SelColor,UnSelColor:TRGB);virtual;
                           procedure selectcontrolpointinframe(f1,f2: GDBvertex2DI);virtual;
                           procedure getnearesttomouse(var td:tcontrolpointdist;mx,my:integer);virtual;
                           procedure selectcurrentcontrolpoint(key:Byte;mx,my,h:integer);virtual;
                     end;
{Export-}
implementation
procedure GDBControlPointArray.done;
begin
  destroy;
end;
procedure GDBControlPointArray.draw;
var point:^controlpointdesc;
    i:Integer;
    segmentDir,perpDir,p1,p2,p3,p4:GDBVertex;
    gripSize:Double;
begin
  if count<>0 then
  begin
       point:=GetParrayAsPointer;
       for i:=count-1 downto 0 do
       begin
            if point^.selected then
                                   dc.drawer.SetColor(SelColor)
                               else
                                   begin
                                        if point^.PDrawable<>nil then
                                                                   //dc.drawer.SetColor(0, 255, 50,0)
                                                               else
                                                                   dc.drawer.SetColor(UnSelColor)
                                   end;

            // Draw oriented rectangle for segment center grips
            if point^.pointtype=os_midle then begin
              // Calculate grip size in world coordinates based on screen size
              // Using a ratio to convert point size to world coordinates
              gripSize:=dc.DrawingContext.matrixs.pprojectmatrix.mtr[0][0];
              if gripSize<>0 then
                gripSize:=1.0/gripSize
              else
                gripSize:=1.0;

              // Get segment direction from dcoord
              segmentDir:=point^.dcoord;
              segmentDir:=NormalizeVector(segmentDir);

              // Calculate perpendicular direction
              perpDir:=CrossVertex(segmentDir,CreateVertex(0,0,1));
              if VertexLength(perpDir)<0.001 then
                perpDir:=CrossVertex(segmentDir,CreateVertex(0,1,0));
              perpDir:=NormalizeVector(perpDir);

              // Create rectangle corners (2:1 aspect ratio, long side along segment)
              p1:=VertexAdd(point^.worldcoord,VertexAdd(VertexMulOnSc(segmentDir,gripSize*3),VertexMulOnSc(perpDir,gripSize*1.5)));
              p2:=VertexAdd(point^.worldcoord,VertexAdd(VertexMulOnSc(segmentDir,gripSize*3),VertexMulOnSc(perpDir,-gripSize*1.5)));
              p3:=VertexAdd(point^.worldcoord,VertexAdd(VertexMulOnSc(segmentDir,-gripSize*3),VertexMulOnSc(perpDir,-gripSize*1.5)));
              p4:=VertexAdd(point^.worldcoord,VertexAdd(VertexMulOnSc(segmentDir,-gripSize*3),VertexMulOnSc(perpDir,gripSize*1.5)));

              // Draw the oriented rectangle
              dc.drawer.DrawQuad3DInModelSpace(p1,p2,p3,p4,dc.DrawingContext.matrixs);
            end else begin
              // Draw normal square grip for vertex points
              dc.drawer.DrawPoint3DInModelSpace(point^.worldcoord,dc.DrawingContext.matrixs);
            end;
            inc(point);
       end;
  end;
end;
procedure GDBControlPointArray.selectcontrolpointinframe(f1,f2: GDBvertex2DI);
var point:^controlpointdesc;
    i:Integer;
begin
  if count<>0 then
  begin
       point:=GetParrayAsPointer;
       for i:=count-1 downto 0 do
       begin
            if CPA_Strech in point^.attr then
            if (point^.dispcoord.x>=f1.x)
            and(point^.dispcoord.x<=f2.x)
            and(point^.dispcoord.y>=f1.y)
            and(point^.dispcoord.y<=f2.y) then
            begin
              point^.selected:=true;
              inc(SelectedCount);
            end;
            inc(point);
       end;
  end;
end;
procedure GDBControlPointArray.getnearesttomouse;
var point:pcontrolpointdesc;
    d:single;
    i:Integer;
begin
  if count<>0 then
  begin
       point:=GetParrayAsPointer;
       for i:=count-1 downto 0 do           { TODO 1 -ozamtmn -c1 : Переделать нахуй без GDB }
       begin
            //d := (vertexlen2id(GDB.GetCurrentDWG.OGLwindow1.param.md.mouse.x,GDB.GetCurrentDWG.OGLwindow1.param.height-GDB.GetCurrentDWG.OGLwindow1.param.md.mouse.y,point^.dispcoord.x,point^.dispcoord.y));
            d := (vertexlen2id(mx,my,point^.dispcoord.x,point^.dispcoord.y));
            if d < td.disttomouse then
                                      begin
                                           td.disttomouse:=round(d);
                                           td.pcontrolpoint:=point;
                                      end;
            inc(point);
       end;
  end;
end;
procedure GDBControlPointArray.selectcurrentcontrolpoint;
var point:pcontrolpointdesc;
//    d:single;
    i:Integer;
begin
  SelectedCount:=0;
  if count<>0 then
  begin
       point:=GetParrayAsPointer;
       for i:=count-1 downto 0 do
       begin
            //if (GDB.GetCurrentDWG.OGLwindow1.param.md.mouseglue.x=point^.dispcoord.x)and
            //   (GDB.GetCurrentDWG.OGLwindow1.param.md.mouseglue.y=GDB.GetCurrentDWG.OGLwindow1.param.height-point^.dispcoord.y)
            if (mx=point^.dispcoord.x)and
               (my=h-point^.dispcoord.y)
            then
            begin
            if (key and 128)<>0 then point.selected:=not point.selected
                                else point.selected:=true;
            end;
            if point.selected then inc(SelectedCount);
            inc(point);
       end;
  end;
end;
begin
end.

