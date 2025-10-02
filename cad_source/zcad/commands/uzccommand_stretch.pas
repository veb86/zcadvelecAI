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

unit uzccommand_stretch;
{$INCLUDE zengineconfig.inc}

interface

uses
  uzglviewareageneral,
  uzgldrawcontext,
  uzccommandsmanager,
  uzccommandsabstract,
  uzccommandsimpl,
  uzbtypes,
  uzcdrawings,
  uzglviewareadata,
  uzcinterface,
  uzeconsts,
  uzeentity,
  uzclog,
  uzegeometrytypes,
  uzegeometry,
  gzctnrVectorTypes,
  UGDBSelectedObjArray,
  uzccommand_selectframe,uzccommand_ondrawinged;

implementation

type
  TStretchComMode=(SM_GetEnts,SM_FirstPoint,SM_SecondPoint);

var
  StretchComMode:TStretchComMode;

function HasControlPoints:Boolean;
var
  tdesc:PSelectedObjDesc;
  ir:itrec;
begin
  Result:=False;
  if drawings.GetCurrentDWG.GetSelObjArray.Count>0 then begin
    tdesc:=drawings.GetCurrentDWG.GetSelObjArray.beginiterate(ir);
    if tdesc<>nil then
      repeat
        if tdesc^.pcontrolpoint<>nil then begin
          Result:=True;
          exit;
        end;
        tdesc:=drawings.GetCurrentDWG.GetSelObjArray.iterate(ir);
      until tdesc=nil;
  end;
end;

procedure Stretch_com_CommandStart(const Context:TZCADCommandContext;Operands:pansichar);
begin
  if (drawings.GetCurrentDWG.wa.param.seldesc.Selectedobjcount>0)and HasControlPoints then begin
    StretchComMode:=SM_FirstPoint;
    selectpoints;
    drawings.GetCurrentDWG.wa.SetMouseMode(MGet3DPoint or MMoveCamera or MRotateCamera);
    zcUI.Do_GUIaction(nil,zcMsgUIActionRedrawContent);
  end else begin
    StretchComMode:=SM_GetEnts;
    FrameEdit_com_CommandStart(Context,Operands);
  end;
end;

function Stretch_com_BeforeClick(const Context:TZCADCommandContext;wc:GDBvertex;
  mc:GDBvertex2DI;var button:byte;osp:pos_record;mclick:integer):integer;
begin
  case StretchComMode of
    SM_GetEnts:
      Result:=FrameEdit_com_BeforeClick(context,wc,mc,button,osp,mclick);
    SM_FirstPoint:
      if (button and MZW_LBUTTON)<>0 then begin
        OnDrawingEd.BeforeClick(context,wc,mc,button,osp);
        StretchComMode:=SM_SecondPoint;
        Result:=0;
      end;
    SM_SecondPoint:
    begin
      OnDrawingEd.mouseclic:=1;
      OnDrawingEd.AfterClick(context,wc,mc,button,osp);
      if (button and MZW_LBUTTON)<>0 then begin
        commandmanager.ExecuteCommandEnd;
        Result:=0;
      end;
    end;
  end;
end;

procedure selectpoints;
var
  DC:TDrawContext;
begin
  dc:=drawings.GetCurrentDWG.wa.CreateRC;
  drawings.GetCurrentDWG.GetSelObjArray.remappoints(
    drawings.GetCurrentDWG.GetPcamera.POSCOUNT,drawings.GetCurrentDWG.wa.param.scrollmode,
    drawings.GetCurrentDWG.GetPcamera^,drawings.GetCurrentDWG^.myGluProject2,dc);
  drawings.GetCurrentDWG.GetSelObjArray.selectcontrolpointinframe(
    drawings.GetCurrentDWG.wa.param.seldesc.Frame1,
    drawings.GetCurrentDWG.wa.param.seldesc.Frame2);
end;

function Stretch_com_AfterClick(const Context:TZCADCommandContext;wc:GDBvertex;
  mc:GDBvertex2DI;var button:byte;osp:pos_record;mclick:integer):integer;
begin
  Result:=0;
  if StretchComMode=SM_GetEnts then begin
    commandmanager.DisableExecuteCommandEnd;
    Result:=FrameEdit_com_AfterClick(context,wc,mc,button,osp,mclick);
    commandmanager.EnableExecuteCommandEnd;
    //button:=0;
    drawings.GetCurrentDWG.wa.Clear0Ontrackpoint;
    //убираем нулевую точку трассировки
  end;

  if commandmanager.hasDisabledExecuteCommandEnd then begin
    commandmanager.resetDisabledExecuteCommandEnd;
    if (button and MZW_LBUTTON)<>0 then begin
      if StretchComMode=SM_GetEnts then begin
        drawings.GetCurrentDWG.wa.SetMouseMode(MGet3DPoint or
          {MGet3DPointWoOP or }MMoveCamera or MRotateCamera);
        StretchComMode:=SM_FirstPoint;
        selectpoints;
        zcUI.Do_GUIaction(nil,zcMsgUIActionRedrawContent);
        //drawings.GetCurrentDWG.wa.Clear0Ontrackpoint;
        button:=0;
        //убираем нулевую точку трассировки, которая будет создана после выхода отсюда
      end;
      Result:=0;
    end;
  end;

  if (StretchComMode=SM_SecondPoint)and((button and MZW_LBUTTON)<>0) then
    commandmanager.executecommandend;

end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization',[{$INCLUDE %FILE%}],
    LM_Info,UnitsInitializeLMId);
  CreateCommandRTEdObjectPlugin(@Stretch_com_CommandStart,
    @FrameEdit_com_Command_End,
    nil,nil,
    @Stretch_com_BeforeClick,
    @Stretch_com_AfterClick,nil,nil,'Stretch',0,0);

finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization',[{$INCLUDE %FILE%}],
    LM_Info,UnitsFinalizeLMId);
end.
