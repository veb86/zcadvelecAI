{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************
}
{$mode objfpc}{$H+}

{ Кнопка сохранения изменений из uzvspreadsheet в открытую ACAD_TABLE. }
unit uzvspreadsheet_cmdsaveacadtable;

{$INCLUDE zengineconfig.inc}

interface

implementation

uses
  SysUtils,
  ActnList,
  fpspreadsheet,
  fpspreadsheetctrls,
  fpsTypes,
  Types,
  uzcutils,
  uzcdrawings,
  uzcinterface,
  uzeacadtable_types,
  uzeacadtable_model,
  uzgldrawcontext,
  uzvspreadsheet_dimensions,
  uzvspreadsheet_cmdcellstyle,
  uzvspreadsheet_cmdeditacadtable,
  uzvspreadsheet_cmdregistry;

function WorksheetUsedRange(AWorksheet: TsWorksheet;
  out ARowCount, AColCount: Integer): Boolean;
var
  Row, Col: Integer;
  Cell: PCell;
begin
  Result := False;
  ARowCount := 0;
  AColCount := 0;
  if AWorksheet = nil then
    Exit;
  // Совпадает с допустимым диапазоном команды создания ACAD_TABLE.
  for Row := 0 to 500 do
    for Col := 0 to 50 do
    begin
      Cell := AWorksheet.FindCell(Row, Col);
      if Cell = nil then
        Continue;
      if Row + 1 > ARowCount then
        ARowCount := Row + 1;
      if Col + 1 > AColCount then
        AColCount := Col + 1;
    end;
  Result := (ARowCount > 0) and (AColCount > 0);
end;

function SaveWorksheetToAcadTable(AWorksheet: TsWorksheet;
  ATable: PGDBObjAcadTable): Boolean;
var
  Row, Col, RowCount, ColCount: Integer;
  Cell: PCell;
  Texts: TTableTextArray;
  ColWidths, RowHeights: TTableSizeArray;
  Alignments: TTableAlignmentArray;
  RowTypes: TIntegerDynArray;
  DC: TDrawContext;
begin
  Result := False;
  if (ATable = nil) or
     not WorksheetUsedRange(AWorksheet, RowCount, ColCount) then
    Exit;

  SetLength(Texts, RowCount * ColCount);
  for Row := 0 to RowCount - 1 do
    for Col := 0 to ColCount - 1 do
    begin
      Cell := AWorksheet.FindCell(Row, Col);
      if Cell <> nil then
        Texts[Row * ColCount + Col] := AWorksheet.ReadAsText(Cell);
    end;
  ColWidths := CollectColWidths(AWorksheet, ColCount);
  RowHeights := CollectRowHeights(AWorksheet, RowCount);
  Alignments := CollectCellAlignments(AWorksheet, RowCount, ColCount);
  RowTypes := CollectRowStyleTypes(AWorksheet, RowCount, ColCount);

  if not ATable^.UpdateFromCellTextsWithSizesAndAlignments(
    RowCount, ColCount, Texts, ColWidths, RowHeights, Alignments) then
    Exit;
  ATable^.SetRowStyleTypes(RowTypes);
  DC := drawings.GetCurrentDWG^.CreateDrawingRC;
  ATable^.FormatEntity(drawings.GetCurrentDWG^, DC);
  drawings.GetCurrentROOT^.ObjArray.ObjTree.CorrectNodeBoundingBox(ATable^);
  Result := True;
end;

procedure CommandSaveAcadTable(const Context: TSpreadsheetCommandContext);
var
  Worksheet: TsWorksheet;
begin
  if EditingAcadTable = nil then
  begin
    zcUI.TextMessage(
      'Нет ACAD_TABLE, открытой командой editacadtable.',
      TMWOHistoryOut);
    Exit;
  end;
  if (Context.WorkbookSource = nil) or
     (Context.WorkbookSource.Workbook = nil) then
    Exit;
  Worksheet := Context.WorkbookSource.Workbook.ActiveWorksheet;
  if SaveWorksheetToAcadTable(Worksheet, EditingAcadTable) then
  begin
    zcRedrawCurrentDrawing;
    zcUI.TextMessage('Изменения сохранены в ACAD_TABLE.',
      TMWOHistoryOut);
  end
  else
    zcUI.TextMessage('Не удалось сохранить изменения в ACAD_TABLE.',
      TMWOHistoryOut);
end;

procedure UpdateSaveAcadTable(const Context: TSpreadsheetCommandContext;
  AnAction: TAction);
begin
  AnAction.Enabled := EditingAcadTable <> nil;
end;

initialization
  RegisterSpreadsheetCommand(
    'SaveAcadTable',
    'Сохранить изменения в ACAD_TABLE',
    'Сохранить изменения в таблице, открытой командой editacadtable',
    'velec/table_insert',
    @CommandSaveAcadTable,
    162,
    6,
    sbsButton,
    @UpdateSaveAcadTable
  );

end.
