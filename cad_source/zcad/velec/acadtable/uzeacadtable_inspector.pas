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
@author(Vladimir Bobrov)
}

{
  Модуль: uzeacadtable_inspector
  Назначение: Интеграция AcadTable с Object Inspector —
  регистрация свойств таблицы для отображения и редактирования
  в инспекторе объектов ZCAD.
  Зависимости: uzeacadtable_model, uzeconsts,
               uzcoimultiproperties, uzcoimultipropertiesutil, и др.
}

unit uzeacadtable_inspector;
{$MODE OBJFPC}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeacadtable_model,
  uzeconsts,
  uzeentity,
  uzegeometrytypes,
  uzcoimultiobjects,
  uzcoimultiproperties,
  uzcoimultipropertiesutil,
  uzsbVarmanDef,
  Varman,
  uzeTypes,
  uzclog, uzbLogIntf;

// Регистрирует свойства AcadTable в MultiPropertiesManager.
// Вызывается из секции initialization.
procedure RegisterAcadTableProperties;

implementation

// Процедура получения ширины AcadTable (только чтение)
procedure AcadTableWidthEntIterateProc(
  pdata: Pointer;
  ChangedData: TChangedData;
  mp: TMultiProperty;
  fistrun: boolean;
  ecp: TEntChangeProc;
  const f: TzeUnitsFormat);
var
  PVD: pvardesk;
  tableWidth: Double;
begin
  PVD := PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp = nil then
    ProcessVariableAttributes(PVD^.attrib, vda_RO, 0);
  tableWidth :=
    PGDBObjAcadTable(ChangedData.PEntity)^.Width;
  ChangedData.PGetDataInEtity := @tableWidth;
  GeneralEntIterateProc(
    pdata, ChangedData, mp, fistrun, ecp, f);
end;

// Процедура получения высоты AcadTable (только чтение)
procedure AcadTableHeightEntIterateProc(
  pdata: Pointer;
  ChangedData: TChangedData;
  mp: TMultiProperty;
  fistrun: boolean;
  ecp: TEntChangeProc;
  const f: TzeUnitsFormat);
var
  PVD: pvardesk;
  tableHeight: Double;
begin
  PVD := PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp = nil then
    ProcessVariableAttributes(PVD^.attrib, vda_RO, 0);
  tableHeight :=
    PGDBObjAcadTable(ChangedData.PEntity)^.Height;
  ChangedData.PGetDataInEtity := @tableHeight;
  GeneralEntIterateProc(
    pdata, ChangedData, mp, fistrun, ecp, f);
end;

// Процедура получения количества строк AcadTable
procedure AcadTableRowCountEntIterateProc(
  pdata: Pointer;
  ChangedData: TChangedData;
  mp: TMultiProperty;
  fistrun: boolean;
  ecp: TEntChangeProc;
  const f: TzeUnitsFormat);
var
  PVD: pvardesk;
  rowCount: TArrayIndex;
begin
  PVD := PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp = nil then
    ProcessVariableAttributes(PVD^.attrib, vda_RO, 0);
  rowCount :=
    PGDBObjAcadTable(ChangedData.PEntity)^.RowCount;
  if fistrun then begin
    ProcessVariableAttributes(
      PVD^.attrib, 0, vda_different);
    mp.MPType^.CopyValueToInstance(
      @rowCount, PVD^.data.Addr.Instance);
    PTOneVarData(pdata)^.StrValue :=
      mp.MPType^.GetDecoratedValueAsString(@rowCount, f);
  end else begin
    if mp.MPType^.Compare(
      @rowCount, PVD^.data.Addr.Instance) <> CREqual then
      ProcessVariableAttributes(
        PVD^.attrib, vda_approximately, 0);
    if PTOneVarData(pdata)^.StrValue <>
      mp.MPType^.GetDecoratedValueAsString(
        @rowCount, f) then
      ProcessVariableAttributes(
        PVD^.attrib, vda_different, vda_approximately);
  end;
end;

// Процедура получения количества столбцов AcadTable
procedure AcadTableColCountEntIterateProc(
  pdata: Pointer;
  ChangedData: TChangedData;
  mp: TMultiProperty;
  fistrun: boolean;
  ecp: TEntChangeProc;
  const f: TzeUnitsFormat);
var
  PVD: pvardesk;
  colCount: TArrayIndex;
begin
  PVD := PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp = nil then
    ProcessVariableAttributes(PVD^.attrib, vda_RO, 0);
  colCount :=
    PGDBObjAcadTable(ChangedData.PEntity)^.ColCount;
  if fistrun then begin
    ProcessVariableAttributes(
      PVD^.attrib, 0, vda_different);
    mp.MPType^.CopyValueToInstance(
      @colCount, PVD^.data.Addr.Instance);
    PTOneVarData(pdata)^.StrValue :=
      mp.MPType^.GetDecoratedValueAsString(@colCount, f);
  end else begin
    if mp.MPType^.Compare(
      @colCount, PVD^.data.Addr.Instance) <> CREqual then
      ProcessVariableAttributes(
        PVD^.attrib, vda_approximately, 0);
    if PTOneVarData(pdata)^.StrValue <>
      mp.MPType^.GetDecoratedValueAsString(
        @colCount, f) then
      ProcessVariableAttributes(
        PVD^.attrib, vda_different, vda_approximately);
  end;
end;

// Процедура получения имени стиля AcadTable
procedure AcadTableStyleNameEntIterateProc(
  pdata: Pointer;
  ChangedData: TChangedData;
  mp: TMultiProperty;
  fistrun: boolean;
  ecp: TEntChangeProc;
  const f: TzeUnitsFormat);
var
  PVD: pvardesk;
  styleName: AnsiString;
begin
  PVD := PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp = nil then
    ProcessVariableAttributes(PVD^.attrib, vda_RO, 0);
  styleName := PGDBObjAcadTable(
    ChangedData.PEntity)^.TableStyleName;
  if fistrun then begin
    ProcessVariableAttributes(
      PVD^.attrib, 0, vda_different);
    mp.MPType^.CopyValueToInstance(
      @styleName, PVD^.data.Addr.Instance);
    PTOneVarData(pdata)^.StrValue :=
      mp.MPType^.GetDecoratedValueAsString(@styleName, f);
  end else begin
    if (PVD^.attrib and vda_different) = 0 then begin
      if mp.MPType^.Compare(
        @styleName,
        PVD^.data.Addr.Instance) <> CREqual then
        ProcessVariableAttributes(
          PVD^.attrib, vda_different, 0);
    end;
  end;
end;

procedure RegisterAcadTableProperties;
const
  pacadtable: PGDBObjAcadTable = nil;
begin
  if sysunit = nil then Exit;

  programlog.LogOutStr(
    'AcadTable: inspector: RegisterAcadTableProperties',
    LM_Info);

  {AcadTable — геометрия}
  MultiPropertiesManager.RestartMultipropertySortID;
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'ACADTABLE_INSERT_X', 'X',
    sysunit^.TypeName2PTD('TzeXUnits'),
    MPCGeometry, GDBAcadTableID, nil,
    PtrInt(@pacadtable^.P_insert_in_WCS.x),
    PtrInt(@pacadtable^.Local.P_insert.x),
    OneVarDataMIPD, OneVarDataEIPD);
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'ACADTABLE_INSERT_Y', 'Y',
    sysunit^.TypeName2PTD('TzeYUnits'),
    MPCGeometry, GDBAcadTableID, nil,
    PtrInt(@pacadtable^.P_insert_in_WCS.y),
    PtrInt(@pacadtable^.Local.P_insert.y),
    OneVarDataMIPD, OneVarDataEIPD);
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'ACADTABLE_INSERT_Z', 'Z',
    sysunit^.TypeName2PTD('TzeZUnits'),
    MPCGeometry, GDBAcadTableID, nil,
    PtrInt(@pacadtable^.P_insert_in_WCS.z),
    PtrInt(@pacadtable^.Local.P_insert.z),
    OneVarDataMIPD, OneVarDataEIPD);
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableWidth', 'Width',
    sysunit^.TypeName2PTD('Double'),
    MPCGeometry, GDBAcadTableID, nil,
    0, 0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(
      nil, @AcadTableWidthEntIterateProc, nil));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableHeight', 'Height',
    sysunit^.TypeName2PTD('Double'),
    MPCGeometry, GDBAcadTableID, nil,
    0, 0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(
      nil, @AcadTableHeightEntIterateProc, nil));
  {AcadTable — основные свойства}
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableStyleName', 'Table style',
    sysunit^.TypeName2PTD('AnsiString'),
    MPCMisc, GDBAcadTableID, nil,
    0, 0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(
      nil, @AcadTableStyleNameEntIterateProc, nil));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableRowCount', 'Rows',
    sysunit^.TypeName2PTD('TArrayIndex'),
    MPCMisc, GDBAcadTableID, nil,
    0, 0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(
      nil, @AcadTableRowCountEntIterateProc, nil));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableColCount', 'Columns',
    sysunit^.TypeName2PTD('TArrayIndex'),
    MPCMisc, GDBAcadTableID, nil,
    0, 0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(
      nil, @AcadTableColCountEntIterateProc, nil));
end;

initialization
  RegisterAcadTableProperties;

end.
