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
@author(AI Assistant for issue #354)
Панель статистики устройств для отображения агрегированной информации
}
{$mode objfpc}{$H+}

unit uzvdevstatspanel;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  uzvmcstruct, Math;

type
  // Статистика по фазам
  TPhaseStatistics = record
    PhaseA_Power: Double;
    PhaseB_Power: Double;
    PhaseC_Power: Double;
    Total_Power: Double;
    Imbalance: Double;        // Небаланс в кВт
    ImbalancePercent: Double; // Небаланс в процентах
  end;

  { TDeviceStatisticsPanel }

  TDeviceStatisticsPanel = class(TPanel)
  private
    FTitleLabel: TLabel;
    FContentPanel: TPanel;
    FDeviceCountLabel: TLabel;
    FTotalPowerLabel: TLabel;
    FVoltageLabel: TLabel;
    FPhaseHeaderLabel: TLabel;
    FPhaseALabel: TLabel;
    FPhaseBLabel: TLabel;
    FPhaseCLabel: TLabel;
    FImbalanceLabel: TLabel;

    procedure InitializeComponents;
    procedure ClearStatistics;
    function CalculatePhaseStatistics(devicesList: TListVElectrDevStruct): TPhaseStatistics;
    function GetVoltageInfo(devicesList: TListVElectrDevStruct): string;
    function FormatPhaseBar(power: Double; totalPower: Double): string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure UpdateStatistics(devicesList: TListVElectrDevStruct; const nodeName: string);
    procedure Hide;
    procedure Show;
  end;

implementation

{ TDeviceStatisticsPanel }

constructor TDeviceStatisticsPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // Настройка основной панели
  Height := 120;
  BevelOuter := bvNone;
  BorderWidth := 4;
  Color := clWhite;

  InitializeComponents;
end;

destructor TDeviceStatisticsPanel.Destroy;
begin
  inherited Destroy;
end;

procedure TDeviceStatisticsPanel.InitializeComponents;
begin
  // Заголовок панели
  FTitleLabel := TLabel.Create(Self);
  FTitleLabel.Parent := Self;
  FTitleLabel.Left := 8;
  FTitleLabel.Top := 8;
  FTitleLabel.Font.Style := [fsBold];
  FTitleLabel.Font.Size := 10;
  FTitleLabel.Caption := 'Статистика';

  // Панель для содержимого
  FContentPanel := TPanel.Create(Self);
  FContentPanel.Parent := Self;
  FContentPanel.Left := 8;
  FContentPanel.Top := 28;
  FContentPanel.Width := 800;
  FContentPanel.Height := 84;
  FContentPanel.BevelOuter := bvNone;
  FContentPanel.Color := clWhite;

  // Количество устройств
  FDeviceCountLabel := TLabel.Create(Self);
  FDeviceCountLabel.Parent := FContentPanel;
  FDeviceCountLabel.Left := 0;
  FDeviceCountLabel.Top := 0;
  FDeviceCountLabel.Caption := 'Устройств: 0';

  // Общая мощность
  FTotalPowerLabel := TLabel.Create(Self);
  FTotalPowerLabel.Parent := FContentPanel;
  FTotalPowerLabel.Left := 150;
  FTotalPowerLabel.Top := 0;
  FTotalPowerLabel.Caption := 'Общая мощность: 0 кВт';

  // Напряжение
  FVoltageLabel := TLabel.Create(Self);
  FVoltageLabel.Parent := FContentPanel;
  FVoltageLabel.Left := 350;
  FVoltageLabel.Top := 0;
  FVoltageLabel.Caption := 'Напряжение: -';

  // Заголовок раздела фаз
  FPhaseHeaderLabel := TLabel.Create(Self);
  FPhaseHeaderLabel.Parent := FContentPanel;
  FPhaseHeaderLabel.Left := 0;
  FPhaseHeaderLabel.Top := 24;
  FPhaseHeaderLabel.Caption := 'Распределение по фазам:';
  FPhaseHeaderLabel.Font.Style := [fsBold];

  // Фаза A
  FPhaseALabel := TLabel.Create(Self);
  FPhaseALabel.Parent := FContentPanel;
  FPhaseALabel.Left := 0;
  FPhaseALabel.Top := 42;
  FPhaseALabel.Caption := 'Фаза A: 0 кВт (0%)';
  FPhaseALabel.Font.Color := clBlue;

  // Фаза B
  FPhaseBLabel := TLabel.Create(Self);
  FPhaseBLabel.Parent := FContentPanel;
  FPhaseBLabel.Left := 0;
  FPhaseBLabel.Top := 56;
  FPhaseBLabel.Caption := 'Фаза B: 0 кВт (0%)';
  FPhaseBLabel.Font.Color := clGreen;

  // Фаза C
  FPhaseCLabel := TLabel.Create(Self);
  FPhaseCLabel.Parent := FContentPanel;
  FPhaseCLabel.Left := 0;
  FPhaseCLabel.Top := 70;
  FPhaseCLabel.Caption := 'Фаза C: 0 кВт (0%)';
  FPhaseCLabel.Font.Color := clMaroon;

  // Небаланс
  FImbalanceLabel := TLabel.Create(Self);
  FImbalanceLabel.Parent := FContentPanel;
  FImbalanceLabel.Left := 350;
  FImbalanceLabel.Top := 56;
  FImbalanceLabel.Caption := 'Небаланс: 0 кВт (0%)';
  FImbalanceLabel.Font.Color := clRed;
end;

procedure TDeviceStatisticsPanel.ClearStatistics;
begin
  FTitleLabel.Caption := 'Статистика';
  FDeviceCountLabel.Caption := 'Устройств: 0';
  FTotalPowerLabel.Caption := 'Общая мощность: 0 кВт';
  FVoltageLabel.Caption := 'Напряжение: -';
  FPhaseALabel.Caption := 'Фаза A: 0 кВт (0%)';
  FPhaseBLabel.Caption := 'Фаза B: 0 кВт (0%)';
  FPhaseCLabel.Caption := 'Фаза C: 0 кВт (0%)';
  FImbalanceLabel.Caption := 'Небаланс: 0 кВт (0%)';
end;

function TDeviceStatisticsPanel.CalculatePhaseStatistics(devicesList: TListVElectrDevStruct): TPhaseStatistics;
var
  i: Integer;
  device: TVElectrDevStruct;
  phaseStr: string;
  maxPower, minPower, avgPower: Double;
begin
  Result.PhaseA_Power := 0;
  Result.PhaseB_Power := 0;
  Result.PhaseC_Power := 0;
  Result.Total_Power := 0;
  Result.Imbalance := 0;
  Result.ImbalancePercent := 0;

  // Суммируем мощность по фазам
  for i := 0 to devicesList.Size - 1 do
  begin
    device := devicesList[i];
    phaseStr := UpperCase(Trim(device.phase));

    if phaseStr = 'A' then
      Result.PhaseA_Power := Result.PhaseA_Power + device.power
    else if phaseStr = 'B' then
      Result.PhaseB_Power := Result.PhaseB_Power + device.power
    else if phaseStr = 'C' then
      Result.PhaseC_Power := Result.PhaseC_Power + device.power;

    Result.Total_Power := Result.Total_Power + device.power;
  end;

  // Рассчитываем небаланс (разница между максимальной и минимальной нагрузкой)
  if Result.Total_Power > 0 then
  begin
    maxPower := Max(Result.PhaseA_Power, Max(Result.PhaseB_Power, Result.PhaseC_Power));
    minPower := Min(Result.PhaseA_Power, Min(Result.PhaseB_Power, Result.PhaseC_Power));
    avgPower := Result.Total_Power / 3;

    Result.Imbalance := maxPower - minPower;

    if avgPower > 0 then
      Result.ImbalancePercent := (Result.Imbalance / avgPower) * 100
    else
      Result.ImbalancePercent := 0;
  end;
end;

function TDeviceStatisticsPanel.GetVoltageInfo(devicesList: TListVElectrDevStruct): string;
var
  i: Integer;
  voltages: array of Integer;
  voltage: Integer;
  found: Boolean;
  j: Integer;
begin
  Result := '-';
  SetLength(voltages, 0);

  // Собираем уникальные значения напряжений
  for i := 0 to devicesList.Size - 1 do
  begin
    voltage := devicesList[i].voltage;
    if voltage > 0 then
    begin
      found := False;
      for j := 0 to High(voltages) do
      begin
        if voltages[j] = voltage then
        begin
          found := True;
          Break;
        end;
      end;

      if not found then
      begin
        SetLength(voltages, Length(voltages) + 1);
        voltages[High(voltages)] := voltage;
      end;
    end;
  end;

  // Формируем строку с напряжениями
  if Length(voltages) = 0 then
    Result := '-'
  else if Length(voltages) = 1 then
    Result := IntToStr(voltages[0]) + ' В'
  else
    Result := 'Разные';
end;

function TDeviceStatisticsPanel.FormatPhaseBar(power: Double; totalPower: Double): string;
var
  percent: Double;
  barLength: Integer;
  i: Integer;
begin
  Result := '';

  if totalPower > 0 then
    percent := (power / totalPower) * 100
  else
    percent := 0;

  barLength := Round(percent / 10); // 10% = 1 символ

  for i := 1 to barLength do
    Result := Result + '█';

  for i := barLength + 1 to 10 do
    Result := Result + '░';
end;

procedure TDeviceStatisticsPanel.UpdateStatistics(devicesList: TListVElectrDevStruct; const nodeName: string);
var
  phaseStats: TPhaseStatistics;
  voltageInfo: string;
  deviceCount: Integer;
begin
  if devicesList.Size = 0 then
  begin
    ClearStatistics;
    if nodeName <> '' then
      FTitleLabel.Caption := 'Статистика: ' + nodeName;
    Exit;
  end;

  // Обновляем заголовок
  if nodeName <> '' then
    FTitleLabel.Caption := 'Статистика: ' + nodeName
  else
    FTitleLabel.Caption := 'Статистика: Все устройства';

  // Подсчитываем устройства
  deviceCount := devicesList.Size;
  FDeviceCountLabel.Caption := 'Устройств: ' + IntToStr(deviceCount);

  // Рассчитываем статистику по фазам
  phaseStats := CalculatePhaseStatistics(devicesList);

  // Обновляем общую мощность
  FTotalPowerLabel.Caption := Format('Общая мощность: %.2f кВт', [phaseStats.Total_Power]);

  // Обновляем информацию о напряжении
  voltageInfo := GetVoltageInfo(devicesList);
  FVoltageLabel.Caption := 'Напряжение: ' + voltageInfo;

  // Обновляем информацию по фазам
  if phaseStats.Total_Power > 0 then
  begin
    FPhaseALabel.Caption := Format('Фаза A: %.2f кВт (%.1f%%) %s',
      [phaseStats.PhaseA_Power,
       (phaseStats.PhaseA_Power / phaseStats.Total_Power) * 100,
       FormatPhaseBar(phaseStats.PhaseA_Power, phaseStats.Total_Power)]);

    FPhaseBLabel.Caption := Format('Фаза B: %.2f кВт (%.1f%%) %s',
      [phaseStats.PhaseB_Power,
       (phaseStats.PhaseB_Power / phaseStats.Total_Power) * 100,
       FormatPhaseBar(phaseStats.PhaseB_Power, phaseStats.Total_Power)]);

    FPhaseCLabel.Caption := Format('Фаза C: %.2f кВт (%.1f%%) %s',
      [phaseStats.PhaseC_Power,
       (phaseStats.PhaseC_Power / phaseStats.Total_Power) * 100,
       FormatPhaseBar(phaseStats.PhaseC_Power, phaseStats.Total_Power)]);

    FImbalanceLabel.Caption := Format('Небаланс: %.2f кВт (%.1f%%)',
      [phaseStats.Imbalance, phaseStats.ImbalancePercent]);
  end
  else
  begin
    FPhaseALabel.Caption := 'Фаза A: 0 кВт (0%)';
    FPhaseBLabel.Caption := 'Фаза B: 0 кВт (0%)';
    FPhaseCLabel.Caption := 'Фаза C: 0 кВт (0%)';
    FImbalanceLabel.Caption := 'Небаланс: 0 кВт (0%)';
  end;

  // Делаем панель видимой
  Self.Visible := True;
end;

procedure TDeviceStatisticsPanel.Hide;
begin
  Self.Visible := False;
end;

procedure TDeviceStatisticsPanel.Show;
begin
  Self.Visible := True;
end;

end.
