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

{**Модуль главной формы навигатора спецификационных данных}
unit uzvspecificationnav;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  Forms,
  Controls,
  Graphics,
  ExtCtrls,
  ActnList,
  ComCtrls,
  laz.VirtualTrees,
  uzvspecificationnav_types,
  uzvspecificationnav_loader,
  uzvspecificationnav_tree,
  uzclog;

type
  // Главная форма навигатора спецификаций
  TSpecificationNavFrame = class(TFrame)
    ActionList1: TActionList;
    PanelTop: TPanel;
    PanelMain: TPanel;
    ToolBar1: TToolBar;
    SpecTree: TLazVirtualStringTree;

  private
    FSpecList: TSpecificationList;
    FTreeManager: TSpecificationTreeManager;
    FLoader: TSpecificationLoader;

    // Инициализация панелей и компонентов
    procedure InitializePanels;

    // Инициализация действий и кнопок
    procedure InitializeActions;

    // Обработчики действий
    procedure OnReadDataAction(Sender: TObject);
    procedure OnClearDataAction(Sender: TObject);
    procedure OnRefreshAction(Sender: TObject);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    // Загрузка данных из выделенных устройств
    procedure LoadDataFromSelection;

    // Очистка данных
    procedure ClearData;

    // Обновление отображения
    procedure RefreshDisplay;

    // Получение списка спецификаций
    property SpecificationList: TSpecificationList read FSpecList;
  end;

implementation

uses
  uzcinterface,
  uzcdrawings,
  uzeentity,
  uzeentdevice,
  gzctnrVectorTypes;

{$R *.lfm}

{ TSpecificationNavFrame }

constructor TSpecificationNavFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  Name := 'SpecificationNavFrame';
  Caption := 'Навигатор спецификаций';

  // Создаем список для хранения спецификационных данных
  FSpecList := TSpecificationList.Create(True);

  // Создаем загрузчик данных
  FLoader := TSpecificationLoader.Create;

  // Создаем менеджер дерева
  FTreeManager := TSpecificationTreeManager.Create(SpecTree);

  // Инициализация компонентов
  try
    InitializePanels;
    InitializeActions;
    FTreeManager.Initialize;

    programlog.LogOutFormatStr(
      'Форма навигатора спецификаций инициализирована',
      [],
      LM_Info
    );
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'Ошибка инициализации формы навигатора: %s',
        [E.Message],
        LM_Info
      );
    end;
  end;
end;

destructor TSpecificationNavFrame.Destroy;
begin
  // Освобождаем ресурсы
  if FTreeManager <> nil then
    FTreeManager.Free;

  if FLoader <> nil then
    FLoader.Free;

  if FSpecList <> nil then
    FSpecList.Free;

  inherited Destroy;
end;

// Инициализация панелей
procedure TSpecificationNavFrame.InitializePanels;
begin
  // Настройка панели кнопок
  PanelTop.Caption := '';
  PanelTop.BevelOuter := bvNone;

  // Настройка основной панели
  PanelMain.Caption := '';
  PanelMain.BevelOuter := bvNone;
end;

// Инициализация действий и кнопок
procedure TSpecificationNavFrame.InitializeActions;
var
  Action: TAction;
  ToolButton: TToolButton;
begin
  // Действие: Считать данные
  Action := TAction.Create(Self);
  Action.Caption := 'Считать';
  Action.Hint := 'Считать спецификационные данные из выделенных устройств';
  Action.OnExecute := @OnReadDataAction;
  ActionList1.AddAction(Action);

  ToolButton := TToolButton.Create(ToolBar1);
  ToolButton.Parent := ToolBar1;
  ToolButton.Action := Action;
  ToolButton.Caption := 'Считать';

  // Действие: Очистить данные
  Action := TAction.Create(Self);
  Action.Caption := 'Очистить';
  Action.Hint := 'Очистить все спецификационные данные';
  Action.OnExecute := @OnClearDataAction;
  ActionList1.AddAction(Action);

  ToolButton := TToolButton.Create(ToolBar1);
  ToolButton.Parent := ToolBar1;
  ToolButton.Action := Action;
  ToolButton.Caption := 'Очистить';

  // Действие: Обновить отображение
  Action := TAction.Create(Self);
  Action.Caption := 'Обновить';
  Action.Hint := 'Обновить отображение спецификационных данных';
  Action.OnExecute := @OnRefreshAction;
  ActionList1.AddAction(Action);

  ToolButton := TToolButton.Create(ToolBar1);
  ToolButton.Parent := ToolBar1;
  ToolButton.Action := Action;
  ToolButton.Caption := 'Обновить';
end;

// Обработчик действия: Считать данные
procedure TSpecificationNavFrame.OnReadDataAction(Sender: TObject);
begin
  LoadDataFromSelection;
end;

// Обработчик действия: Очистить данные
procedure TSpecificationNavFrame.OnClearDataAction(Sender: TObject);
begin
  ClearData;
end;

// Обработчик действия: Обновить
procedure TSpecificationNavFrame.OnRefreshAction(Sender: TObject);
begin
  RefreshDisplay;
end;

// Загрузка данных из выделенных устройств
procedure TSpecificationNavFrame.LoadDataFromSelection;
var
  pEntity: PGDBObjEntity;
  pDevice: PGDBObjDevice;
  DeviceList: TSpecificationList;
  selectedCount: Integer;
  processedCount: Integer;
  i: Integer;
begin
  if drawings.GetCurrentDWG = nil then
  begin
    zcUI.TextMessage('Нет активного чертежа', TMWOHistoryOut);
    Exit;
  end;

  // Получаем количество выделенных объектов
  selectedCount := drawings.GetCurrentDWG^.wa.param.seldesc.Selectedobjcount;

  if selectedCount = 0 then
  begin
    zcUI.TextMessage('Не выделено ни одного объекта', TMWOHistoryOut);
    zcUI.TextMessage('Выделите устройства для анализа', TMWOHistoryOut);
    Exit;
  end;

  programlog.LogOutFormatStr(
    'Начало загрузки спецификаций из %d выделенных объектов',
    [selectedCount],
    LM_Info
  );

  // Очищаем предыдущие данные
  FSpecList.Clear;
  processedCount := 0;

  // Проходим по всем выделенным объектам
  pEntity := drawings.GetCurrentDWG^.wa.param.seldesc.Selectedobjarray.beginiterate;
  while pEntity <> nil do
  begin
    // Проверяем, является ли объект устройством
    if pEntity^.GetObjType = GDBDeviceID then
    begin
      pDevice := PGDBObjDevice(pEntity);

      // Проверяем наличие спецификационных данных
      if FLoader.HasSpecificationData(pDevice) then
      begin
        // Загружаем данные из устройства
        DeviceList := FLoader.LoadFromDevice(pDevice, 'Device_' + IntToStr(processedCount + 1));

        // Добавляем в общий список
        for i := 0 to DeviceList.Count - 1 do
        begin
          FSpecList.Add(DeviceList.Items[i]);
        end;

        // Освобождаем временный список (но не элементы)
        DeviceList.OwnsObjects := False;
        DeviceList.Free;

        Inc(processedCount);
      end;
    end;

    pEntity := drawings.GetCurrentDWG^.wa.param.seldesc.Selectedobjarray.iterate;
  end;

  programlog.LogOutFormatStr(
    'Загрузка завершена: обработано %d устройств, загружено %d записей',
    [processedCount, FSpecList.Count],
    LM_Info
  );

  zcUI.TextMessage(
    Format('Загружено %d спецификационных записей из %d устройств',
           [FSpecList.Count, processedCount]),
    TMWOHistoryOut
  );

  // Обновляем отображение
  RefreshDisplay;
end;

// Очистка данных
procedure TSpecificationNavFrame.ClearData;
begin
  FSpecList.Clear;
  FTreeManager.ClearTree;

  programlog.LogOutFormatStr(
    'Спецификационные данные очищены',
    [],
    LM_Info
  );

  zcUI.TextMessage('Данные очищены', TMWOHistoryOut);
end;

// Обновление отображения
procedure TSpecificationNavFrame.RefreshDisplay;
begin
  programlog.LogOutFormatStr(
    'Обновление отображения: %d записей',
    [FSpecList.Count],
    LM_Info
  );

  FTreeManager.PopulateTree(FSpecList);

  zcUI.TextMessage(
    Format('Отображено %d записей', [FTreeManager.GetNodeCount]),
    TMWOHistoryOut
  );
end;

end.
