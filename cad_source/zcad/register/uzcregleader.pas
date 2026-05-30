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

{$MODE OBJFPC}{$H+}
unit uzcregleader;
{$INCLUDE zengineconfig.inc}

interface

procedure RegisterLeaderProperties;

implementation

uses
  uzcoimultiproperties,uzcoimultipropertiesutil,
  uzeentleader,uzeconsts,uzegeometrytypes,uzestylesdim,
  uzsbVarmanDef,Varman,uzbUnits,gzctnrVectorTypes,
  UGDBPoint3DArray,uzcLog,uzcdrawing,uzcdrawings,
  zUndoCmdChgTypes,zUndoCmdChgVariable;

var
  ptdInteger:PUserTypeDescriptor=nil;
  ptdString:PUserTypeDescriptor=nil;

procedure LeaderLengthEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  l:Double;
begin
  l:=PGDBObjLeader(ChangedData.PEntity)^.GetLength;
  ChangedData.PGetDataInEtity:=@l;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

procedure LeaderSumLengthEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  l:Double;
begin
  l:=PGDBObjLeader(ChangedData.PEntity)^.GetLength;
  ChangedData.PGetDataInEtity:=@l;
  Double2SumEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

procedure LeaderVertex3DControlFromVarEntChangeProc(var UMPlaced:boolean;
  pu:PTEntityUnit;pdata:PVarDesk;ChangedData:TChangedData;mp:TMultiProperty);
var
  tv:PzePoint3d;
  v:TzePoint3d;
  pindex:pTArrayIndex;
  PGDBDTypeDesc:PUserTypeDescriptor;
begin
  if pdata^.name=mp.MPName then
    mp.MPType.CopyValueToInstance(pdata^.data.Addr.Instance,@Vertex3DControl)
  else begin
    PGDBDTypeDesc:=SysUnit.TypeName2PTD('Double');
    pindex:=pu^.FindValue(mp.MPName).data.Addr.Instance;
    tv:=PGDBObjLeader(ChangedData.pentity).VertexArrayInWCS.getDataMutable(pindex^);
    v:=tv^;

    if pdata^.name=mp.MPName+'x' then
      PGDBDTypeDesc.CopyValueToInstance(pdata^.data.Addr.Instance,@v.x);
    if pdata^.name=mp.MPName+'y' then
      PGDBDTypeDesc.CopyValueToInstance(pdata^.data.Addr.Instance,@v.y);
    if pdata^.name=mp.MPName+'z' then
      PGDBDTypeDesc.CopyValueToInstance(pdata^.data.Addr.Instance,@v.z);

    tv:=PGDBPoint3dArray(ChangedData.PSetDataInEtity).getDataMutable(pindex^);
    tv^:=v;
  end;
end;

procedure RegisterLeaderDoubleProperty(const name,username:string;
  category:TMultiPropertyCategory;getoffset,setoffset:PtrInt);
begin
  MultiPropertiesManager.RegisterPhysMultiproperty(
    name,username,sysunit^.TypeName2PTD('Double'),
    category,GDBLeaderID,nil,getoffset,setoffset,
    OneVarDataMIPD,OneVarDataEIPD);
end;

procedure RegisterLeaderIntegerProperty(const name,username:string;
  getoffset,setoffset:PtrInt);
begin
  MultiPropertiesManager.RegisterPhysMultiproperty(
    name,username,sysunit^.TypeName2PTD('Integer'),
    MPCMisc,GDBLeaderID,nil,getoffset,setoffset,
    OneVarDataMIPD,OneVarDataEIPD);
end;

function GetLeaderTypeData(mp:TMultiProperty;pu:PTEntityUnit):Pointer;
const
  LeaderTypeNames:array[0..3] of string=(
    'Линейная без стрелки',
    'Сплайн без стрелки',
    'Линейная со стрелкой',
    'Сплайн со стрелкой');
var
  PVD:pvardesk;
  t:PTEnumData;
  i:integer;
begin
  result:=GetTEnumData(mp,pu);
  PVD:=PTOneVarData(result).VDAddr.Instance;
  if PVD<>nil then begin
    t:=PVD^.data.Addr.Instance;
    for i:=low(LeaderTypeNames) to high(LeaderTypeNames) do
      t^.Enums.PushBackData(LeaderTypeNames[i]);
    t^.Selected:=LeaderTypeIndexLinearWithArrow;
  end;
end;

procedure LeaderTypeEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  enumindex:integer;
begin
  PVD:=PTOneVarData(pdata).VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD.attrib,vda_RO,0);
  enumindex:=LeaderTypeToEnumIndex(PGDBObjLeader(ChangedData.PEntity)^);
  if fistrun then
    PTEnumData(PVD.data.Addr.Instance)^.Selected:=enumindex
  else
    if PTEnumData(PVD.data.Addr.Instance)^.Selected<>enumindex then
      ProcessVariableAttributes(PVD.attrib,vda_different,0);
end;

procedure LeaderTypeEntChangeProc(var UMPlaced:boolean;pu:PTEntityUnit;
  pdata:PVarDesk;ChangedData:TChangedData;mp:TMultiProperty);
var
  cp:UCmdChgField;
  NewArrowHeadFlag,NewPathType:integer;
  OldTypeIndex,NewTypeIndex:integer;
begin
  OldTypeIndex:=LeaderTypeToEnumIndex(PGDBObjLeader(ChangedData.PEntity)^);
  NewTypeIndex:=PTEnumData(pvardesk(pdata)^.data.Addr.Instance)^.Selected;
  if OldTypeIndex=NewTypeIndex then
    exit;

  NewArrowHeadFlag:=PGDBObjLeader(ChangedData.PEntity)^.ArrowHeadFlag;
  NewPathType:=PGDBObjLeader(ChangedData.PEntity)^.PathType;
  case NewTypeIndex of
    LeaderTypeIndexLinearNoArrow:begin
      NewArrowHeadFlag:=0;
      NewPathType:=0;
    end;
    LeaderTypeIndexSplineNoArrow:begin
      NewArrowHeadFlag:=0;
      NewPathType:=1;
    end;
    LeaderTypeIndexSplineWithArrow:begin
      NewArrowHeadFlag:=1;
      NewPathType:=1;
    end;
  else
    NewArrowHeadFlag:=1;
    NewPathType:=0;
  end;

  if ptdInteger=nil then
    ptdInteger:=SysUnit^.TypeName2PTD('Integer');
  if ptdInteger=nil then
    exit;

  if PGDBObjLeader(ChangedData.PEntity)^.ArrowHeadFlag<>NewArrowHeadFlag then begin
    PlaceUndoStartMarkerPropertyChangedIfNeed(UMPlaced);
    cp:=UCmdChgField.CreateAndPush(
      PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack,
      TChangedFieldDesc.CreateRec(
        ptdInteger,
        @PGDBObjLeader(ChangedData.PEntity)^.ArrowHeadFlag,
        @PGDBObjLeader(ChangedData.PEntity)^.ArrowHeadFlag),
      TSharedPEntityData.CreateRec(ChangedData.PEntity),
      TAfterChangePDrawing.CreateRec(drawings.GetCurrentDWG));
    ptdInteger^.CopyValueToInstance(
      @NewArrowHeadFlag,@PGDBObjLeader(ChangedData.PEntity)^.ArrowHeadFlag);
  end;

  if PGDBObjLeader(ChangedData.PEntity)^.PathType<>NewPathType then begin
    PlaceUndoStartMarkerPropertyChangedIfNeed(UMPlaced);
    cp:=UCmdChgField.CreateAndPush(
      PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack,
      TChangedFieldDesc.CreateRec(
        ptdInteger,
        @PGDBObjLeader(ChangedData.PEntity)^.PathType,
        @PGDBObjLeader(ChangedData.PEntity)^.PathType),
      TSharedPEntityData.CreateRec(ChangedData.PEntity),
      TAfterChangePDrawing.CreateRec(drawings.GetCurrentDWG));
    ptdInteger^.CopyValueToInstance(
      @NewPathType,@PGDBObjLeader(ChangedData.PEntity)^.PathType);
  end;

  ProcessVariableAttributes(
    pvardesk(pdata)^.attrib,0,vda_approximately or vda_different);
end;

function GetLeaderDimStyle(const Leader:PGDBObjLeader):PGDBDimStyle;
begin
  result:=nil;
  if drawings.GetCurrentDWG=nil then
    exit;
  if (Leader<>nil)and(Leader^.DimStyleName<>'') then
    result:=PGDBDimStyle(
      drawings.GetCurrentDWG.DimStyleTable.getAddres(Leader^.DimStyleName));
end;

procedure LeaderDimStyleEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  CurrentStyle:PGDBDimStyle;
  Leader:PGDBObjLeader;
begin
  PVD:=PTOneVarData(pdata).VDAddr.Instance;
  Leader:=PGDBObjLeader(ChangedData.PEntity);
  CurrentStyle:=GetLeaderDimStyle(Leader);

  if @ecp=nil then
    ProcessVariableAttributes(PVD.attrib,vda_RO,0);
  if fistrun then begin
    PTOneVarData(pdata)^.StrValue:=Leader^.DimStyleName;
    PPGDBDimStyleObjInsp(PVD.data.Addr.Instance)^:=CurrentStyle;
  end else
    if (PTOneVarData(pdata)^.StrValue<>Leader^.DimStyleName)or
       (PPGDBDimStyleObjInsp(PVD.data.Addr.Instance)^<>CurrentStyle) then
      ProcessVariableAttributes(PVD.attrib,vda_different,0);
end;

procedure LeaderDimStyleEntChangeProc(var UMPlaced:boolean;pu:PTEntityUnit;
  pdata:PVarDesk;ChangedData:TChangedData;mp:TMultiProperty);
var
  cp:UCmdChgField;
  NewDimStyle:PGDBDimStyle;
  NewDimStyleName:string;
begin
  NewDimStyle:=PGDBDimStyle(
    PPGDBDimStyleObjInsp(pvardesk(pdata)^.data.Addr.Instance)^);
  if NewDimStyle=nil then
    exit;

  NewDimStyleName:=NewDimStyle^.Name;
  if PGDBObjLeader(ChangedData.PEntity)^.DimStyleName=NewDimStyleName then
    exit;

  if ptdString=nil then
    ptdString:=SysUnit^.TypeName2PTD('String');
  if ptdString=nil then
    exit;

  PlaceUndoStartMarkerPropertyChangedIfNeed(UMPlaced);
  cp:=UCmdChgField.CreateAndPush(
    PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack,
    TChangedFieldDesc.CreateRec(
      ptdString,
      @PGDBObjLeader(ChangedData.PEntity)^.DimStyleName,
      @PGDBObjLeader(ChangedData.PEntity)^.DimStyleName),
    TSharedPEntityData.CreateRec(ChangedData.PEntity),
    TAfterChangePDrawing.CreateRec(drawings.GetCurrentDWG));
  ptdString^.CopyValueToInstance(
    @NewDimStyleName,@PGDBObjLeader(ChangedData.PEntity)^.DimStyleName);
  ProcessVariableAttributes(
    pvardesk(pdata)^.attrib,0,vda_approximately or vda_different);
end;

procedure RegisterLeaderProperties;
const
  pleader:PGDBObjLeader=nil;
begin
  if sysunit=nil then
    exit;

  MultiPropertiesManager.RestartMultipropertySortID;

  MultiPropertiesManager.RegisterPhysMultiproperty(
    'VertexCount','Vertex count',sysunit^.TypeName2PTD('TArrayIndex'),
    MPCGeometry,GDBLeaderID,nil,
    PtrInt(@pleader^.VertexArrayInOCS.Count),
    PtrInt(@pleader^.VertexArrayInOCS.Count),
    OneVarDataMIPD,OneVarRODataEIPD);
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'Vertex3DControl_','Vertex control',sysunit^.TypeName2PTD('TArrayIndex'),
    MPCGeometry,GDBLeaderID,nil,
    PtrInt(@pleader^.VertexArrayInWCS),
    PtrInt(@pleader^.VertexArrayInOCS),
    TMainIterateProcsData.Create(@GetVertex3DControlData,@FreeVertex3DControlData),
    TEntIterateProcsData.Create(
      @PolylineVertex3DControlBeforeEntIterateProc,
      @PolylineVertex3DControlEntIterateProc,
      @LeaderVertex3DControlFromVarEntChangeProc));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'Length','Length',sysunit^.TypeName2PTD('Double'),
    MPCGeometry,GDBLeaderID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@LeaderLengthEntIterateProc,nil));

  RegisterLeaderDoubleProperty(
    'LeaderNormalX','Normal X',MPCGeometry,
    PtrInt(@pleader^.NormalVector.x),PtrInt(@pleader^.NormalVector.x));
  RegisterLeaderDoubleProperty(
    'LeaderNormalY','Normal Y',MPCGeometry,
    PtrInt(@pleader^.NormalVector.y),PtrInt(@pleader^.NormalVector.y));
  RegisterLeaderDoubleProperty(
    'LeaderNormalZ','Normal Z',MPCGeometry,
    PtrInt(@pleader^.NormalVector.z),PtrInt(@pleader^.NormalVector.z));
  RegisterLeaderDoubleProperty(
    'LeaderHorizontalDirectionX','Horizontal direction X',MPCGeometry,
    PtrInt(@pleader^.HorizontalDirection.x),PtrInt(@pleader^.HorizontalDirection.x));
  RegisterLeaderDoubleProperty(
    'LeaderHorizontalDirectionY','Horizontal direction Y',MPCGeometry,
    PtrInt(@pleader^.HorizontalDirection.y),PtrInt(@pleader^.HorizontalDirection.y));
  RegisterLeaderDoubleProperty(
    'LeaderHorizontalDirectionZ','Horizontal direction Z',MPCGeometry,
    PtrInt(@pleader^.HorizontalDirection.z),PtrInt(@pleader^.HorizontalDirection.z));
  RegisterLeaderDoubleProperty(
    'LeaderBlockOffsetX','Block offset X',MPCGeometry,
    PtrInt(@pleader^.BlockOffset.x),PtrInt(@pleader^.BlockOffset.x));
  RegisterLeaderDoubleProperty(
    'LeaderBlockOffsetY','Block offset Y',MPCGeometry,
    PtrInt(@pleader^.BlockOffset.y),PtrInt(@pleader^.BlockOffset.y));
  RegisterLeaderDoubleProperty(
    'LeaderBlockOffsetZ','Block offset Z',MPCGeometry,
    PtrInt(@pleader^.BlockOffset.z),PtrInt(@pleader^.BlockOffset.z));
  RegisterLeaderDoubleProperty(
    'LeaderAnnotationOffsetX','Annotation offset X',MPCGeometry,
    PtrInt(@pleader^.AnnotationOffset.x),PtrInt(@pleader^.AnnotationOffset.x));
  RegisterLeaderDoubleProperty(
    'LeaderAnnotationOffsetY','Annotation offset Y',MPCGeometry,
    PtrInt(@pleader^.AnnotationOffset.y),PtrInt(@pleader^.AnnotationOffset.y));
  RegisterLeaderDoubleProperty(
    'LeaderAnnotationOffsetZ','Annotation offset Z',MPCGeometry,
    PtrInt(@pleader^.AnnotationOffset.z),PtrInt(@pleader^.AnnotationOffset.z));

  MultiPropertiesManager.RegisterPhysMultiproperty(
    'LeaderDimStyle','Style',sysunit^.TypeName2PTD('PGDBDimStyleObjInsp'),
    MPCMisc,GDBLeaderID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(
      nil,@LeaderDimStyleEntIterateProc,@LeaderDimStyleEntChangeProc));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'LeaderType','Type',sysunit^.TypeName2PTD('TEnumData'),
    MPCMisc,GDBLeaderID,nil,0,0,
    TMainIterateProcsData.Create(@GetLeaderTypeData,@FreeTEnumData),
    TEntIterateProcsData.Create(
      nil,@LeaderTypeEntIterateProc,@LeaderTypeEntChangeProc),
    MPUM_AtLeastOneEntMatched);
  RegisterLeaderIntegerProperty(
    'LeaderAnnotationType','Annotation type',
    PtrInt(@pleader^.AnnotationType),PtrInt(@pleader^.AnnotationType));
  RegisterLeaderIntegerProperty(
    'LeaderHookLineDirectionFlag','Hook line direction flag',
    PtrInt(@pleader^.HookLineDirectionFlag),
    PtrInt(@pleader^.HookLineDirectionFlag));
  RegisterLeaderIntegerProperty(
    'LeaderHookLineFlag','Hook line flag',
    PtrInt(@pleader^.HookLineFlag),PtrInt(@pleader^.HookLineFlag));
  RegisterLeaderDoubleProperty(
    'LeaderTextHeight','Text height',MPCMisc,
    PtrInt(@pleader^.TextHeight),PtrInt(@pleader^.TextHeight));
  RegisterLeaderDoubleProperty(
    'LeaderTextWidth','Text width',MPCMisc,
    PtrInt(@pleader^.TextWidth),PtrInt(@pleader^.TextWidth));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'LeaderAnnotationHandle','Annotation handle',sysunit^.TypeName2PTD('QWord'),
    MPCMisc,GDBLeaderID,nil,
    PtrInt(@pleader^.AnnotationHandle),PtrInt(@pleader^.AnnotationHandle),
    OneVarDataMIPD,OneVarRODataEIPD);

  MultiPropertiesManager.RegisterPhysMultiproperty(
    'TotalVertexCount','Total vertex count',sysunit^.TypeName2PTD('TArrayIndex'),
    MPCSummary,GDBLeaderID,nil,
    PtrInt(@pleader^.VertexArrayInOCS.Count),
    PtrInt(@pleader^.VertexArrayInOCS.Count),
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@TArrayIndex2SumEntIterateProc,nil));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'TotalLength','Total length',sysunit^.TypeName2PTD('Double'),
    MPCSummary,GDBLeaderID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@LeaderSumLengthEntIterateProc,nil));

  MultiPropertiesManager.sort;
end;

initialization
  RegisterLeaderProperties;
finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization',[{$INCLUDE %FILE%}],
    LM_Info,UnitsFinalizeLMId);
end.
