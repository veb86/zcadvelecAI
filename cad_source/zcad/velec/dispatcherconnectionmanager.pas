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
@author(AI Assistant - implementing TLazVirtualStringTree instead of DBGrid)
}
unit dispatcherconnectionmanager;

{$mode objfpc}{$H+}
{$Codepage UTF8}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, Forms, Controls, Graphics, Dialogs,
  StdCtrls, laz.VirtualTrees, DB,
  uzclog, uzcinterface, uzbtypes, uzeconsts, uzcutils;

type

  PNodeData = ^TNodeData;
  TNodeData = record
    ID: Integer;
    T1, T2, T3, T4: string;
  end;

  { TDispatcherConnectionForm }
  TDispatcherConnectionForm = class(TForm)
    connDB: TSQLite3Connection;
    qryA: TSQLQuery;
    transDB: TSQLTransaction;
    VST: TLazVirtualStringTree;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; const NewText: string);
    procedure VSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
      Node: PVirtualNode; Column: TColumnIndex; const CellRect: TRect);
    procedure VSTMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure VSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; var Allowed: Boolean);
  private
    procedure LoadData;
    procedure SaveData(Node: PVirtualNode);
    function LoadSQLiteLibrary: Boolean;
  public

  end;

var
  DispatcherConnectionForm: TDispatcherConnectionForm;

implementation

{$R *.lfm}

{ TDispatcherConnectionForm }

function TDispatcherConnectionForm.LoadSQLiteLibrary: Boolean;
var
  LibPath: String;
begin
  LibPath := 'sqlite3.dll';

  if not FileExists(LibPath) then
    LibPath := ExtractFilePath(ParamStr(0)) + 'sqlite3.dll';

  if not FileExists(LibPath) then
    LibPath := 'C:\zcad\zcad\sqlite3.dll';

  SQLiteLibraryName := LibPath;
  Result := FileExists(LibPath);

  if not Result then
    zcUI.TextMessage('Не удалось найти sqlite3.dll по пути: ' + LibPath, TMWOHistoryOut);
end;

procedure TDispatcherConnectionForm.FormCreate(Sender: TObject);
begin
  if not LoadSQLiteLibrary then
    raise Exception.Create('SQLite3.dll not found!');

  connDB.DatabaseName := 'SampleDEV.db3';
  connDB.Connected := True;
  connDB.CharSet := 'UTF8';

  qryA.DataBase := connDB;
  qryA.Transaction := transDB;
  transDB.DataBase := connDB;
  transDB.StartTransaction;

  VST.NodeDataSize := SizeOf(TNodeData);
  VST.TreeOptions.MiscOptions := VST.TreeOptions.MiscOptions + [toEditable, toEditOnDblClick];

  VST.Header.Options := VST.Header.Options + [hoVisible];
  VST.Header.Columns.Clear;

  with VST.Header.Columns.Add do begin Text := 'Показать'; Width := 80; end;
  with VST.Header.Columns.Add do Text := 'ID';
  with VST.Header.Columns.Add do Text := 'T1';
  with VST.Header.Columns.Add do Text := 'T2';
  with VST.Header.Columns.Add do Text := 'T3';
  with VST.Header.Columns.Add do Text := 'T4';
  with VST.Header.Columns.Add do begin Text := 'Ред.'; Width := 70; end;

  VST.OnGetText := @VSTGetText;
  VST.OnNewText := @VSTNewText;
  VST.OnAfterCellPaint := @VSTAfterCellPaint;
  VST.OnEditing := @VSTEditing;
  VST.OnMouseDown := @VSTMouseDown;

  LoadData;
  VST.Invalidate;
end;

procedure TDispatcherConnectionForm.VSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const CellRect: TRect);
var
  Txt: string;
  W, H, X, Y: Integer;
  R: TRect;
begin
  if not (Column in [0, 6]) then Exit;

  case Column of
    0: Txt := 'Показать';
    6: Txt := 'Ред.';
  else
    Txt := '';
  end;

  R := CellRect;

  TargetCanvas.Brush.Color := clBtnFace;
  TargetCanvas.FillRect(R);

  W := TargetCanvas.TextWidth(Txt);
  H := TargetCanvas.TextHeight(Txt);

  X := R.Left + ((R.Right - R.Left - W) div 2);
  Y := R.Top + ((R.Bottom - R.Top - H) div 2);

  TargetCanvas.TextOut(X, Y, Txt);

  TargetCanvas.Pen.Color := clGray;
  TargetCanvas.Brush.Style := bsClear;
  TargetCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
  TargetCanvas.Brush.Style := bsSolid;
end;

procedure TDispatcherConnectionForm.VSTMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitInfo;
  Node: PVirtualNode;
  Data: PNodeData;
begin
  VST.GetHitTestInfoAt(X, Y, True, HitInfo);
  Node := HitInfo.HitNode;
  if not Assigned(Node) then Exit;

  Data := VST.GetNodeData(Node);
  if not Assigned(Data) then Exit;

  if HitInfo.HitColumn = 0 then
  begin
    ShowMessage('Показать ID = ' + IntToStr(Data^.ID));
    Exit;
  end
  else if HitInfo.HitColumn = 6 then
  begin
    ShowMessage('Редактировать: ' + Data^.T2);
    Exit;
  end;

  if (Button = mbLeft) and (HitInfo.HitColumn in [2,3,4,5]) then
  begin
    VST.FocusedNode := Node;
    VST.FocusedColumn := HitInfo.HitColumn;
    VST.EditNode(Node, HitInfo.HitColumn);
  end;
end;

procedure TDispatcherConnectionForm.FormDestroy(Sender: TObject);
begin
  try
    if connDB.Connected then connDB.Close;
  except end;
end;

procedure TDispatcherConnectionForm.LoadData;
var
  Node: PVirtualNode;
  Data: PNodeData;
begin
  VST.Clear;
  qryA.SQL.Text := 'SELECT ID, T1, T2, T3, T4 FROM A';
  qryA.Open;

  while not qryA.EOF do
  begin
    Node := VST.AddChild(nil);
    Data := VST.GetNodeData(Node);
    if Assigned(Data) then
    begin
      Data^.ID := qryA.FieldByName('ID').AsInteger;
      Data^.T1 := qryA.FieldByName('T1').AsString;
      Data^.T2 := qryA.FieldByName('T2').AsString;
      Data^.T3 := qryA.FieldByName('T3').AsString;
      Data^.T4 := qryA.FieldByName('T4').AsString;
    end;
    qryA.Next;
  end;
  qryA.Close;
end;

procedure TDispatcherConnectionForm.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then Exit;

  case Column of
    0: CellText := 'Показать';
    1: CellText := IntToStr(Data^.ID);
    2: CellText := Data^.T1;
    3: CellText := Data^.T2;
    4: CellText := Data^.T3;
    5: CellText := Data^.T4;
    6: CellText := 'Ред.';
  else
    CellText := '';
  end;
end;

procedure TDispatcherConnectionForm.VSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column in [2,3,4,5];
end;

procedure TDispatcherConnectionForm.VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const NewText: string);
var
  Data: PNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then Exit;

  case Column of
    2: Data^.T1 := NewText;
    3: Data^.T2 := NewText;
    4: Data^.T3 := NewText;
    5: Data^.T4 := NewText;
  end;

  SaveData(Node);
  Sender.InvalidateNode(Node);
end;

procedure TDispatcherConnectionForm.SaveData(Node: PVirtualNode);
var
  Data: PNodeData;
begin
  Data := VST.GetNodeData(Node);
  if not Assigned(Data) then Exit;

  qryA.SQL.Text := 'UPDATE A SET T1 = :T1, T2 = :T2, T3 = :T3, T4 = :T4 WHERE ID = :ID';
  qryA.Params.ParamByName('T1').AsString := Data^.T1;
  qryA.Params.ParamByName('T2').AsString := Data^.T2;
  qryA.Params.ParamByName('T3').AsString := Data^.T3;
  qryA.Params.ParamByName('T4').AsString := Data^.T4;
  qryA.Params.ParamByName('ID').AsInteger := Data^.ID;
  qryA.ExecSQL;
  transDB.CommitRetaining;
end;

end.