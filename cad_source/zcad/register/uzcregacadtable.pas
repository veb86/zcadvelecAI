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
  Модуль: uzcregacadtable
  Назначение: регистрация свойств AcadTable в инспекторе объектов ZCAD.
  Вынесено в отдельный модуль register по образцу uzcregleader,
  чтобы не раздувать общие файлы инспектора. Свойства таблицы доступны
  только для чтения и применяются исключительно к сущности AcadTable
  (GDBAcadTableID), не затрагивая обычную сущность Table.

  Зависимости: uzcoimultiproperties, uzcoimultipropertiesutil,
               uzeacadtable_model, uzeacadtable_types, uzeconsts.
}

{$MODE OBJFPC}{$H+}
unit uzcregacadtable;
{$INCLUDE zengineconfig.inc}

interface

procedure RegisterAcadTableProperties;

implementation

uses
  uzcoimultiproperties,uzcoimultipropertiesutil,
  uzeacadtable_model,uzeacadtable_types,
  uzeconsts,uzegeometrytypes,
  uzcdrawings,
  uzsbVarmanDef,Varman,uzbUnits,uzetypes,gzctnrVectorTypes,uzctnrVectorStrings,
  uzclog,uzbLogIntf;

// Чтение ширины таблицы (только чтение)
procedure AcadTableWidthEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:Double;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.Width;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Чтение высоты таблицы (только чтение)
procedure AcadTableHeightEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:Double;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.Height;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Чтение имени стиля таблицы (только чтение)
procedure AcadTableStyleNameEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:AnsiString;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.TableStyleName;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Чтение количества строк (только чтение, не редактируется)
procedure AcadTableRowCountEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:TArrayIndex;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.RowCount;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Чтение количества столбцов (только чтение, не редактируется)
procedure AcadTableColCountEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:TArrayIndex;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.ColCount;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Чтение признака разбиения таблицы (только чтение)
procedure AcadTableBreakEnabledEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:Boolean;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.BreakEnabled;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Запись признака разбиения таблицы (issue #1305, часть 2b).
// Снятие флага (Break enabled = False) у разорванной таблицы объединяет
// сохранённые части-продолжения и перестраивает таблицу сверху вниз в один
// сплошной объект (вся работа выполняется в GDBObjAcadTable.SetBreakEnabled).
// Установка True самостоятельно таблицу по геометрии не разрывает — она лишь
// сохраняет признак, поэтому здесь специальной обработки этого случая нет.
// После изменения вызывается YouChanged, чтобы дерево чертежа пересчитало
// геометрию таблицы (FormatEntity перестроит её по обновлённой модели).
procedure AcadTableBreakEnabledEntChangeProc(var UMPlaced:boolean;
  pu:PTEntityUnit;pdata:PVarDesk;ChangedData:TChangedData;mp:TMultiProperty);
var
  Table:PGDBObjAcadTable;
  NewValue:Boolean;
begin
  Table:=PGDBObjAcadTable(ChangedData.PEntity);
  NewValue:=PBoolean(pvardesk(pdata)^.data.Addr.Instance)^;
  if Table^.BreakEnabled=NewValue then
    exit;

  PlaceUndoStartMarkerPropertyChangedIfNeed(UMPlaced);
  Table^.BreakEnabled:=NewValue;
  if drawings.GetCurrentDWG<>nil then
    Table^.YouChanged(drawings.GetCurrentDWG^);

  ProcessVariableAttributes(
    pvardesk(pdata)^.attrib,0,vda_approximately or vda_different);
end;

// Чтение признака повтора верхних подписей (только чтение)
procedure AcadTableBreakRepeatTopEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:Boolean;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.BreakRepeatTopLabels;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Запись признака повтора верхних подписей (issue #1309).
// Снятие флага (Repeat top labels = False) удаляет повторяющиеся верхние
// строки-метки из всех частей-продолжений разорванной таблицы; установка True
// добавляет их обратно (вся работа выполняется в
// GDBObjAcadTable.SetBreakRepeatTopLabels). После изменения вызывается
// YouChanged, чтобы дерево чертежа перестроило геометрию таблицы.
procedure AcadTableBreakRepeatTopEntChangeProc(var UMPlaced:boolean;
  pu:PTEntityUnit;pdata:PVarDesk;ChangedData:TChangedData;mp:TMultiProperty);
var
  Table:PGDBObjAcadTable;
  NewValue:Boolean;
begin
  Table:=PGDBObjAcadTable(ChangedData.PEntity);
  NewValue:=PBoolean(pvardesk(pdata)^.data.Addr.Instance)^;
  if Table^.BreakRepeatTopLabels=NewValue then
    exit;

  PlaceUndoStartMarkerPropertyChangedIfNeed(UMPlaced);
  Table^.BreakRepeatTopLabels:=NewValue;
  if drawings.GetCurrentDWG<>nil then
    Table^.YouChanged(drawings.GetCurrentDWG^);

  ProcessVariableAttributes(
    pvardesk(pdata)^.attrib,0,vda_approximately or vda_different);
end;

// Чтение признака повтора нижних подписей (только чтение)
procedure AcadTableBreakRepeatBottomEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:Boolean;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.BreakRepeatBottomLabels;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Чтение признака ручного позиционирования разбиения (только чтение)
procedure AcadTableBreakManualPosEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:Boolean;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.BreakManualPosition;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Чтение признака ручной высоты разбиения (только чтение)
procedure AcadTableBreakManualHeightEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:Boolean;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.BreakManualHeight;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Чтение интервала между частями разбиения (issue #1307, часть 1).
// Свойство редактируемое: при наличии change-процедуры (@ecp<>nil) флаг
// vda_RO не выставляется, поэтому значение можно менять в инспекторе.
procedure AcadTableBreakSpacingEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:Double;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.BreakSpacing;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Запись интервала между частями разбиения (issue #1307, часть 1).
// Изменение интервала смещает части-продолжения относительно главной части
// (вся геометрия пересчитывается в GDBObjAcadTable.SetBreakSpacing ->
// RepositionContinuationParts). После изменения вызывается YouChanged,
// чтобы дерево чертежа перестроило геометрию таблицы.
procedure AcadTableBreakSpacingEntChangeProc(var UMPlaced:boolean;
  pu:PTEntityUnit;pdata:PVarDesk;ChangedData:TChangedData;mp:TMultiProperty);
var
  Table:PGDBObjAcadTable;
  NewValue:Double;
begin
  Table:=PGDBObjAcadTable(ChangedData.PEntity);
  NewValue:=PDouble(pvardesk(pdata)^.data.Addr.Instance)^;
  if Table^.BreakSpacing=NewValue then
    exit;

  PlaceUndoStartMarkerPropertyChangedIfNeed(UMPlaced);
  Table^.BreakSpacing:=NewValue;
  if drawings.GetCurrentDWG<>nil then
    Table^.YouChanged(drawings.GetCurrentDWG^);

  ProcessVariableAttributes(
    pvardesk(pdata)^.attrib,0,vda_approximately or vda_different);
end;

// Чтение высоты разбиения (issue #1307, часть 2).
// Свойство редактируемое: при наличии change-процедуры флаг vda_RO не
// выставляется.
procedure AcadTableBreakHeightEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  v:Double;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  v:=PGDBObjAcadTable(ChangedData.PEntity)^.BreakHeight;
  ChangedData.PGetDataInEtity:=@v;
  GeneralEntIterateProc(pdata,ChangedData,mp,fistrun,ecp,f);
end;

// Запись высоты разбиения (issue #1307, часть 2).
// Изменение высоты пересегментирует таблицу: строки перераспределяются по
// частям так, чтобы суммарная высота строк в каждой части не превышала
// заданного порога, число частей определяется автоматически (вся работа
// выполняется в GDBObjAcadTable.SetBreakHeight). После изменения вызывается
// YouChanged для пересчёта геометрии.
procedure AcadTableBreakHeightEntChangeProc(var UMPlaced:boolean;
  pu:PTEntityUnit;pdata:PVarDesk;ChangedData:TChangedData;mp:TMultiProperty);
var
  Table:PGDBObjAcadTable;
  NewValue:Double;
begin
  Table:=PGDBObjAcadTable(ChangedData.PEntity);
  NewValue:=PDouble(pvardesk(pdata)^.data.Addr.Instance)^;
  if Table^.BreakHeight=NewValue then
    exit;

  PlaceUndoStartMarkerPropertyChangedIfNeed(UMPlaced);
  Table^.BreakHeight:=NewValue;
  if drawings.GetCurrentDWG<>nil then
    Table^.YouChanged(drawings.GetCurrentDWG^);

  ProcessVariableAttributes(
    pvardesk(pdata)^.attrib,0,vda_approximately or vda_different);
end;

// Подготовка списка значений направления разбиения для комбобокса.
// Enums.Clear обязателен: предотвращает повторное накопление строк
// при повторном выборе сущности (иначе возможен сбой / двойное освобождение).
function GetAcadTableBreakDirectionData(mp:TMultiProperty;pu:PTEntityUnit):Pointer;
const
  BreakDirNames:array[0..2] of string=('Right','Down','Left');
var
  PVD:pvardesk;
  t:PTEnumData;
  i:integer;
begin
  result:=GetTEnumData(mp,pu);
  PVD:=PTOneVarData(result)^.VDAddr.Instance;
  if PVD<>nil then begin
    t:=PVD^.data.Addr.Instance;
    t^.Enums.Clear;
    for i:=low(BreakDirNames) to high(BreakDirNames) do
      t^.Enums.PushBackData(BreakDirNames[i]);
    t^.Selected:=0;
  end;
end;

// Чтение направления разбиения (только чтение, отображается комбобоксом)
procedure AcadTableBreakDirectionEntIterateProc(pdata:Pointer;ChangedData:TChangedData;
  mp:TMultiProperty;fistrun:boolean;ecp:TEntChangeProc;
  const f:TzeUnitsFormat);
var
  PVD:pvardesk;
  enumindex:integer;
begin
  PVD:=PTOneVarData(pdata)^.VDAddr.Instance;
  if @ecp=nil then
    ProcessVariableAttributes(PVD^.attrib,vda_RO,0);
  enumindex:=ord(PGDBObjAcadTable(ChangedData.PEntity)^.BreakDirection);
  if fistrun then
    PTEnumData(PVD^.data.Addr.Instance)^.Selected:=enumindex
  else
    if PTEnumData(PVD^.data.Addr.Instance)^.Selected<>enumindex then
      ProcessVariableAttributes(PVD^.attrib,vda_different,0);
end;

procedure RegisterAcadTableProperties;
const
  pacadtable:PGDBObjAcadTable=nil;
begin
  if sysunit=nil then
    exit;

  programlog.LogOutStr(
    'AcadTable: uzcregacadtable: RegisterAcadTableProperties',LM_Info);

  MultiPropertiesManager.RestartMultipropertySortID;

  {AcadTable — геометрия (точка вставки)}
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'ACADTABLE_INSERT_X','X',sysunit^.TypeName2PTD('TzeXUnits'),
    MPCGeometry,GDBAcadTableID,nil,
    PtrInt(@pacadtable^.P_insert_in_WCS.x),
    PtrInt(@pacadtable^.Local.P_insert.x),
    OneVarDataMIPD,OneVarDataEIPD);
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'ACADTABLE_INSERT_Y','Y',sysunit^.TypeName2PTD('TzeYUnits'),
    MPCGeometry,GDBAcadTableID,nil,
    PtrInt(@pacadtable^.P_insert_in_WCS.y),
    PtrInt(@pacadtable^.Local.P_insert.y),
    OneVarDataMIPD,OneVarDataEIPD);
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'ACADTABLE_INSERT_Z','Z',sysunit^.TypeName2PTD('TzeZUnits'),
    MPCGeometry,GDBAcadTableID,nil,
    PtrInt(@pacadtable^.P_insert_in_WCS.z),
    PtrInt(@pacadtable^.Local.P_insert.z),
    OneVarDataMIPD,OneVarDataEIPD);

  {AcadTable — размеры (только чтение)}
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableWidth','Width',sysunit^.TypeName2PTD('Double'),
    MPCGeometry,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@AcadTableWidthEntIterateProc,nil));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableHeight','Height',sysunit^.TypeName2PTD('Double'),
    MPCGeometry,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@AcadTableHeightEntIterateProc,nil));

  {AcadTable — основные свойства (только чтение)}
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableStyleName','Table style',sysunit^.TypeName2PTD('AnsiString'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@AcadTableStyleNameEntIterateProc,nil));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableRowCount','Rows',sysunit^.TypeName2PTD('TArrayIndex'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@AcadTableRowCountEntIterateProc,nil));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableColCount','Columns',sysunit^.TypeName2PTD('TArrayIndex'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@AcadTableColCountEntIterateProc,nil));

  {AcadTable — разбиение таблицы}
  {Break enabled редактируется: снятие флага объединяет части (issue #1305).
   Остальные параметры разбиения — только чтение.}
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableBreakEnabled','Break enabled',sysunit^.TypeName2PTD('Boolean'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(
      nil,@AcadTableBreakEnabledEntIterateProc,
      @AcadTableBreakEnabledEntChangeProc));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableBreakDirection','Break direction',sysunit^.TypeName2PTD('TEnumData'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    TMainIterateProcsData.Create(
      @GetAcadTableBreakDirectionData,@FreeTEnumData),
    TEntIterateProcsData.Create(
      nil,@AcadTableBreakDirectionEntIterateProc,nil),
    MPUM_AtLeastOneEntMatched);
  {Repeat top labels редактируется (issue #1309): снятие флага удаляет
   повторяющиеся верхние строки-метки из всех частей-продолжений, установка —
   добавляет их обратно.}
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableBreakRepeatTop','Repeat top labels',
    sysunit^.TypeName2PTD('Boolean'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(
      nil,@AcadTableBreakRepeatTopEntIterateProc,
      @AcadTableBreakRepeatTopEntChangeProc));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableBreakRepeatBottom','Repeat bottom labels',
    sysunit^.TypeName2PTD('Boolean'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@AcadTableBreakRepeatBottomEntIterateProc,nil));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableBreakManualPosition','Manual position',
    sysunit^.TypeName2PTD('Boolean'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@AcadTableBreakManualPosEntIterateProc,nil));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableBreakManualHeight','Manual height',
    sysunit^.TypeName2PTD('Boolean'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(nil,@AcadTableBreakManualHeightEntIterateProc,nil));
  {Break spacing и Break height редактируются (issue #1307):
   изменение интервала смещает части-продолжения, изменение высоты
   пересегментирует таблицу с автоопределением числа частей.}
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableBreakSpacing','Break spacing',sysunit^.TypeName2PTD('Double'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(
      nil,@AcadTableBreakSpacingEntIterateProc,
      @AcadTableBreakSpacingEntChangeProc));
  MultiPropertiesManager.RegisterPhysMultiproperty(
    'AcadTableBreakHeight','Break height',sysunit^.TypeName2PTD('Double'),
    MPCMisc,GDBAcadTableID,nil,0,0,
    OneVarDataMIPD,
    TEntIterateProcsData.Create(
      nil,@AcadTableBreakHeightEntIterateProc,
      @AcadTableBreakHeightEntChangeProc));

  MultiPropertiesManager.sort;
end;

initialization
  RegisterAcadTableProperties;
finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization',[{$INCLUDE %FILE%}],
    LM_Info,UnitsFinalizeLMId);
end.
