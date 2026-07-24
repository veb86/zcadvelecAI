{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************
}
{$mode objfpc}{$H+}

{ Команда editacadtable: открывает единственную выделенную GDBObjAcadTable
  в uzvspreadsheet и запоминает её как цель последующего сохранения. }
unit uzvspreadsheet_cmdeditacadtable;

{$INCLUDE zengineconfig.inc}

interface

uses
  fpspreadsheet,
  fpsTypes,
  uzccommandsabstract,
  uzccommandsimpl,
  uzccommandsmanager,
  uzeacadtable_model;

function EditAcadTable_com(const Context: TZCADCommandContext;
  operands: TCommandOperands): TCommandResult;

function EditingAcadTable: PGDBObjAcadTable;
procedure ClearEditingAcadTable;

implementation

uses
  SysUtils,
  gzctnrVectorTypes,
  UGDBSelectedObjArray,
  uzeconsts,
  uzcdrawings,
  uzcinterface,
  uzclog,
  uzvspreadsheet_dimensions,
  uzvspreadsheet_cmdcellstyle,
  uzvspreadsheet_gui;

var
  FEditingAcadTable: PGDBObjAcadTable = nil;

function EditingAcadTable: PGDBObjAcadTable;
var
  Entity: PGDBObjEntity;
  Iter: itrec;
begin
  Result := nil;
  if FEditingAcadTable = nil then
    Exit;
  Entity := drawings.GetCurrentROOT^.ObjArray.beginiterate(Iter);
  while Entity <> nil do
  begin
    if Entity = PGDBObjEntity(FEditingAcadTable) then
    begin
      Result := FEditingAcadTable;
      Exit;
    end;
    Entity := drawings.GetCurrentROOT^.ObjArray.iterate(Iter);
  end;
  FEditingAcadTable := nil;
end;

procedure ClearEditingAcadTable;
begin
  FEditingAcadTable := nil;
end;

function SelectedAcadTable: PGDBObjAcadTable;
var
  Selected: PSelectedObjDesc;
  Iter: itrec;
begin
  Result := nil;
  if drawings.GetCurrentDWG^.SelObjArray.Count <> 1 then
    Exit;
  Selected := drawings.GetCurrentDWG^.SelObjArray.beginiterate(Iter);
  if (Selected = nil) or (Selected^.objaddr = nil) then
    Exit;
  if Selected^.objaddr^.GetObjType <> GDBAcadTableID then
    Exit;
  Result := PGDBObjAcadTable(Selected^.objaddr);
end;

procedure WriteRowType(AWorksheet: TsWorksheet; ARow, AColCount,
  AStyleType: Integer);
var
  Col: Integer;
  StyleKind: TZVCellStyleKind;
begin
  case AStyleType of
    0: StyleKind := zvskTitle;
    1: StyleKind := zvskHeader;
    else StyleKind := zvskData;
  end;
  for Col := 0 to AColCount - 1 do
    AWorksheet.WriteBackgroundColor(ARow, Col,
      CellStyleKindColor(StyleKind));
end;

function LoadAcadTableToWorksheet(ATable: PGDBObjAcadTable;
  AWorksheet: TsWorksheet): Boolean;
var
  Row, Col, Alignment: Integer;
  Hor: TsHorAlignment;
  Vert: TsVertAlignment;
begin
  Result := False;
  if (ATable = nil) or (AWorksheet = nil) or
     (ATable^.RowCount <= 0) or (ATable^.ColCount <= 0) then
    Exit;

  for Col := 0 to ATable^.ColCount - 1 do
    SetWorksheetColWidthMM(AWorksheet, Col,
      AcadToWorksheetSize(ATable^.ColWidthAt(Col)));
  for Row := 0 to ATable^.RowCount - 1 do
  begin
    SetWorksheetRowHeightMM(AWorksheet, Row,
      AcadToWorksheetSize(ATable^.RowHeightAt(Row)));
    WriteRowType(AWorksheet, Row, ATable^.ColCount,
      ATable^.RowStyleTypeAt(Row));
    for Col := 0 to ATable^.ColCount - 1 do
    begin
      AWorksheet.WriteText(Row, Col, ATable^.CellTextAt(Row, Col));
      Alignment := ATable^.CellAlignmentAt(Row, Col);
      AcadAlignmentToWorksheet(Alignment, Hor, Vert);
      AWorksheet.WriteHorAlignment(Row, Col, Hor);
      AWorksheet.WriteVertAlignment(Row, Col, Vert);
    end;
  end;
  Result := True;
end;

function EditAcadTable_com(const Context: TZCADCommandContext;
  operands: TCommandOperands): TCommandResult;
var
  Table: PGDBObjAcadTable;
  Worksheet: TsWorksheet;
begin
  Result := cmd_cancel;
  Table := SelectedAcadTable;
  if Table = nil then
  begin
    zcUI.TextMessage(
      'Для редактирования должна быть выделена ровно одна таблица ACAD_TABLE.',
      TMWOHistoryOut);
    Exit;
  end;

  zcUI.ShowForm('uzvspreadsheet_gui');
  if (uzvSpreadsheetForm = nil) or
     (uzvSpreadsheetForm.WorkbookSource = nil) then
  begin
    zcUI.TextMessage('Не удалось открыть редактор таблиц.',
      TMWOHistoryOut);
    Exit;
  end;

  uzvSpreadsheetForm.WorkbookSource.CreateNewWorkbook;
  Worksheet :=
    uzvSpreadsheetForm.WorkbookSource.Workbook.ActiveWorksheet;
  if not LoadAcadTableToWorksheet(Table, Worksheet) then
  begin
    zcUI.TextMessage('Не удалось загрузить ACAD_TABLE в редактор.',
      TMWOHistoryOut);
    Exit;
  end;

  FEditingAcadTable := Table;
  uzvSpreadsheetForm.WorksheetGrid.Invalidate;
  zcUI.TextMessage('ACAD_TABLE загружена в редактор таблиц.',
    TMWOHistoryOut);
  Result := cmd_ok;
end;

initialization
  CreateZCADCommand(@EditAcadTable_com, 'editacadtable', CADWG, 0);

end.
