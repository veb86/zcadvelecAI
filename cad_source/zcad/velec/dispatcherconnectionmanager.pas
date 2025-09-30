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
    DeviceName: string;
    DeviceType: string;
    Connection: string;
    Status: string;
    Description: string;
  end;

  { TDispatcherConnectionForm }
  TDispatcherConnectionForm = class(TForm)
    connDB: TSQLite3Connection;
    qryA: TSQLQuery;
    transDB: TSQLTransaction;
    vstDev: TLazVirtualStringTree;
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

  vstDev.NodeDataSize := SizeOf(TNodeData);
  vstDev.TreeOptions.MiscOptions := vstDev.TreeOptions.MiscOptions + [toEditable, toEditOnDblClick];

  vstDev.Header.Options := vstDev.Header.Options + [hoVisible];
  vstDev.Header.Columns.Clear;

  with vstDev.Header.Columns.Add do begin Text := 'Показать'; Width := 80; end;
  with vstDev.Header.Columns.Add do Text := 'ID';
  with vstDev.Header.Columns.Add do Text := 'Имя устройства';
  with vstDev.Header.Columns.Add do Text := 'Тип устройства';
  with vstDev.Header.Columns.Add do Text := 'Подключение';
  with vstDev.Header.Columns.Add do Text := 'Статус';
  with vstDev.Header.Columns.Add do Text := 'Описание';
  with vstDev.Header.Columns.Add do begin Text := 'Ред.'; Width := 70; end;

  vstDev.OnGetText := @VSTGetText;
  vstDev.OnNewText := @VSTNewText;
  vstDev.OnAfterCellPaint := @VSTAfterCellPaint;
  vstDev.OnEditing := @VSTEditing;
  vstDev.OnMouseDown := @VSTMouseDown;

  LoadData;
  vstDev.Invalidate;
end;

procedure TDispatcherConnectionForm.VSTAfterCellPaint(Sender: TBaseVirtualTree; TargetCanvas: TCanvas;
  Node: PVirtualNode; Column: TColumnIndex; const CellRect: TRect);
var
  Txt: string;
  W, H, X, Y: Integer;
  R: TRect;
begin
  if not (Column in [0, 7]) then Exit;

  case Column of
    0: Txt := 'Показать';
    7: Txt := 'Ред.';
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
  vstDev.GetHitTestInfoAt(X, Y, True, HitInfo);
  Node := HitInfo.HitNode;
  if not Assigned(Node) then Exit;

  Data := vstDev.GetNodeData(Node);
  if not Assigned(Data) then Exit;

  if HitInfo.HitColumn = 0 then
  begin
    ShowMessage('Показать ID = ' + IntToStr(Data^.ID));
    Exit;
  end
  else if HitInfo.HitColumn = 7 then
  begin
    ShowMessage('Редактировать: ' + Data^.DeviceName);
    Exit;
  end;

  if (Button = mbLeft) and (HitInfo.HitColumn in [2,3,4,5,6]) then
  begin
    vstDev.FocusedNode := Node;
    vstDev.FocusedColumn := HitInfo.HitColumn;
    vstDev.EditNode(Node, HitInfo.HitColumn);
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
  vstDev.Clear;
  qryA.SQL.Text := 'SELECT ID, DeviceName, DeviceType, Connection, Status, Description FROM Devices';
  qryA.Open;

  while not qryA.EOF do
  begin
    Node := vstDev.AddChild(nil);
    Data := vstDev.GetNodeData(Node);
    if Assigned(Data) then
    begin
      Data^.ID := qryA.FieldByName('ID').AsInteger;
      Data^.DeviceName := qryA.FieldByName('DeviceName').AsString;
      Data^.DeviceType := qryA.FieldByName('DeviceType').AsString;
      Data^.Connection := qryA.FieldByName('Connection').AsString;
      Data^.Status := qryA.FieldByName('Status').AsString;
      Data^.Description := qryA.FieldByName('Description').AsString;
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
    2: CellText := Data^.DeviceName;
    3: CellText := Data^.DeviceType;
    4: CellText := Data^.Connection;
    5: CellText := Data^.Status;
    6: CellText := Data^.Description;
    7: CellText := 'Ред.';
  else
    CellText := '';
  end;
end;

procedure TDispatcherConnectionForm.VSTEditing(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; var Allowed: Boolean);
begin
  Allowed := Column in [2,3,4,5,6];
end;

procedure TDispatcherConnectionForm.VSTNewText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; const NewText: string);
var
  Data: PNodeData;
begin
  Data := Sender.GetNodeData(Node);
  if not Assigned(Data) then Exit;

  case Column of
    2: Data^.DeviceName := NewText;
    3: Data^.DeviceType := NewText;
    4: Data^.Connection := NewText;
    5: Data^.Status := NewText;
    6: Data^.Description := NewText;
  end;

  SaveData(Node);
  Sender.InvalidateNode(Node);
end;

procedure TDispatcherConnectionForm.SaveData(Node: PVirtualNode);
var
  Data: PNodeData;
begin
  Data := vstDev.GetNodeData(Node);
  if not Assigned(Data) then Exit;

  qryA.SQL.Text := 'UPDATE Devices SET DeviceName = :DeviceName, DeviceType = :DeviceType, ' +
                   'Connection = :Connection, Status = :Status, Description = :Description WHERE ID = :ID';
  qryA.Params.ParamByName('DeviceName').AsString := Data^.DeviceName;
  qryA.Params.ParamByName('DeviceType').AsString := Data^.DeviceType;
  qryA.Params.ParamByName('Connection').AsString := Data^.Connection;
  qryA.Params.ParamByName('Status').AsString := Data^.Status;
  qryA.Params.ParamByName('Description').AsString := Data^.Description;
  qryA.Params.ParamByName('ID').AsInteger := Data^.ID;
  qryA.ExecSQL;
  transDB.CommitRetaining;
end;

end.