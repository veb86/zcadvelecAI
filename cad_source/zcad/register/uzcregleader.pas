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
  uzeentleader,uzeconsts,uzegeometrytypes,
  uzsbVarmanDef,Varman,uzbUnits,gzctnrVectorTypes,
  UGDBPoint3DArray,uzcLog;

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
    'LeaderDimStyleName','Style',sysunit^.TypeName2PTD('String'),
    MPCMisc,GDBLeaderID,nil,
    PtrInt(@pleader^.DimStyleName),PtrInt(@pleader^.DimStyleName),
    OneVarDataMIPD,OneVarDataEIPD);
  RegisterLeaderIntegerProperty(
    'LeaderArrowHeadFlag','Arrow head flag',
    PtrInt(@pleader^.ArrowHeadFlag),PtrInt(@pleader^.ArrowHeadFlag));
  RegisterLeaderIntegerProperty(
    'LeaderPathType','Path type',
    PtrInt(@pleader^.PathType),PtrInt(@pleader^.PathType));
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
