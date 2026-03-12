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
{$mode objfpc}{$H+}

{**Модуль формы управления вращением объектов}
unit uzvrotate_form;

{$INCLUDE zengineconfig.inc}

interface
uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  ExtCtrls,
  ComCtrls,
  StdCtrls,
  ActnList,
  Dialogs,
  uzvrotate_struct,
  uzvrotate_logic,
  uzcinterface,
  uzcutils;

type
  {**Форма управления вращением объектов}
  TfrmRotateControl = class(TForm)
    ActionList1: TActionList;
    actResetRotation: TAction;
    actApplyRotation: TAction;
    actAutoCenter: TAction;
    TopPanel: TPanel;
    MainPanel: TPanel;
    ToolBar1: TToolBar;
    btnReset: TToolButton;
    btnApply: TToolButton;
    btnAutoCenter: TToolButton;

    {**Ползунок вращения по оси X}
    TrackBarX: TTrackBar;

    {**Ползунок вращения по оси Y}
    TrackBarY: TTrackBar;

    {**Ползунок вращения по оси Z}
    TrackBarZ: TTrackBar;

    {**Метки для осей}
    LabelX: TLabel;
    LabelY: TLabel;
    LabelZ: TLabel;

    {**Метки для отображения значений углов}
    LabelXValue: TLabel;
    LabelYValue: TLabel;
    LabelZValue: TLabel;

    {**Обработчик создания формы}
    procedure FormCreate(Sender: TObject);

    {**Обработчик уничтожения формы}
    procedure FormDestroy(Sender: TObject);

    {**Обработчик закрытия формы}
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);

    {**Обработчик изменения ползунка X}
    procedure TrackBarXChange(Sender: TObject);

    {**Обработчик изменения ползунка Y}
    procedure TrackBarYChange(Sender: TObject);

    {**Обработчик изменения ползунка Z}
    procedure TrackBarZChange(Sender: TObject);

    {**Обработчик кнопки сброса}
    procedure actResetRotationExecute(Sender: TObject);

    {**Обработчик кнопки применения}
    procedure actApplyRotationExecute(Sender: TObject);

    {**Обработчик кнопки центрирования}
    procedure actAutoCenterExecute(Sender: TObject);

  private
    {**Флаг блокировки обработки событий}
    FUpdating: Boolean;

    {**Инициализировать ползунки}
    procedure InitializeTrackBars;

    {**Обновить метки значений углов}
    procedure UpdateAngleLabels;

    {**Применить вращение на основе текущих позиций ползунков}
    procedure ApplyCurrentRotation;

  public
    {**Инициализировать форму для работы с выделенными объектами}
    function InitializeForSelectedObjects: Boolean;
  end;

var
  frmRotateControl: TfrmRotateControl;

implementation

{$R *.lfm}

{**Обработчик создания формы}
procedure TfrmRotateControl.FormCreate(Sender: TObject);
begin
  FUpdating := False;
  InitializeTrackBars;
end;

{**Обработчик уничтожения формы}
procedure TfrmRotateControl.FormDestroy(Sender: TObject);
begin
  FreeRotationData;
end;

{**Обработчик закрытия формы}
procedure TfrmRotateControl.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  // При закрытии без применения - сбрасываем вращение
  ResetRotation;
  FreeRotationData;
  CloseAction := caFree;
end;

{**Инициализировать ползунки}
procedure TfrmRotateControl.InitializeTrackBars;
begin
  // Настройка ползунка X
  TrackBarX.Min := TRACKBAR_MIN;
  TrackBarX.Max := TRACKBAR_MAX;
  TrackBarX.Position := TRACKBAR_CENTER;
  TrackBarX.Frequency := 10;

  // Настройка ползунка Y
  TrackBarY.Min := TRACKBAR_MIN;
  TrackBarY.Max := TRACKBAR_MAX;
  TrackBarY.Position := TRACKBAR_CENTER;
  TrackBarY.Frequency := 10;

  // Настройка ползунка Z
  TrackBarZ.Min := TRACKBAR_MIN;
  TrackBarZ.Max := TRACKBAR_MAX;
  TrackBarZ.Position := TRACKBAR_CENTER;
  TrackBarZ.Frequency := 10;

  UpdateAngleLabels;
end;

{**Обновить метки значений углов}
procedure TfrmRotateControl.UpdateAngleLabels;
var
  angleX, angleY, angleZ: Double;
begin
  // Получаем углы в градусах
  angleX := (TrackBarX.Position / TRACKBAR_MAX) * MAX_ROTATION_ANGLE;
  angleY := (TrackBarY.Position / TRACKBAR_MAX) * MAX_ROTATION_ANGLE;
  angleZ := (TrackBarZ.Position / TRACKBAR_MAX) * MAX_ROTATION_ANGLE;

  // Обновляем метки
  LabelXValue.Caption := Format('%.1f', [angleX]);
  LabelYValue.Caption := Format('%.1f', [angleY]);
  LabelZValue.Caption := Format('%.1f', [angleZ]);
end;

{**Применить вращение на основе текущих позиций ползунков}
procedure TfrmRotateControl.ApplyCurrentRotation;
var
  angleX, angleY, angleZ: Double;
begin
  if FUpdating then
    Exit;

  // Преобразуем позиции ползунков в углы
  angleX := TrackBarPositionToAngle(TrackBarX.Position);
  angleY := TrackBarPositionToAngle(TrackBarY.Position);
  angleZ := TrackBarPositionToAngle(TrackBarZ.Position);

  // Применяем вращение
  ApplyRotation(angleX, angleY, angleZ);

  // Запрашиваем перерисовку
  zcRedrawCurrentDrawing;
end;

{**Обработчик изменения ползунка X}
procedure TfrmRotateControl.TrackBarXChange(Sender: TObject);
begin
  UpdateAngleLabels;
  ApplyCurrentRotation;
end;

{**Обработчик изменения ползунка Y}
procedure TfrmRotateControl.TrackBarYChange(Sender: TObject);
begin
  UpdateAngleLabels;
  ApplyCurrentRotation;
end;

{**Обработчик изменения ползунка Z}
procedure TfrmRotateControl.TrackBarZChange(Sender: TObject);
begin
  UpdateAngleLabels;
  ApplyCurrentRotation;
end;

{**Обработчик кнопки сброса}
procedure TfrmRotateControl.actResetRotationExecute(Sender: TObject);
begin
  FUpdating := True;
  try
    // Сбрасываем позиции ползунков
    TrackBarX.Position := TRACKBAR_CENTER;
    TrackBarY.Position := TRACKBAR_CENTER;
    TrackBarZ.Position := TRACKBAR_CENTER;

    // Сбрасываем вращение объектов
    ResetRotation;

    UpdateAngleLabels;

    // Запрашиваем перерисовку
    zcRedrawCurrentDrawing;
  finally
    FUpdating := False;
  end;
end;

{**Обработчик кнопки применения}
procedure TfrmRotateControl.actApplyRotationExecute(Sender: TObject);
begin
  // Подтверждаем вращение
  CommitRotation;

  // Показываем сообщение
  ShowMessage('Вращение применено');

  // Закрываем форму
  Close;
end;

{**Обработчик кнопки центрирования}
procedure TfrmRotateControl.actAutoCenterExecute(Sender: TObject);
begin
  FUpdating := True;
  try
    // Центрируем ползунки
    TrackBarX.Position := TRACKBAR_CENTER;
    TrackBarY.Position := TRACKBAR_CENTER;
    TrackBarZ.Position := TRACKBAR_CENTER;

    UpdateAngleLabels;
  finally
    FUpdating := False;
  end;
end;

{**Инициализировать форму для работы с выделенными объектами}
function TfrmRotateControl.InitializeForSelectedObjects: Boolean;
begin
  Result := InitRotationData;

  if not Result then
  begin
    ShowMessage('Нет выделенных объектов для вращения');
    Exit;
  end;

  // Сбрасываем ползунки в начальное положение
  FUpdating := True;
  try
    TrackBarX.Position := TRACKBAR_CENTER;
    TrackBarY.Position := TRACKBAR_CENTER;
    TrackBarZ.Position := TRACKBAR_CENTER;
    UpdateAngleLabels;
  finally
    FUpdating := False;
  end;
end;

end.
