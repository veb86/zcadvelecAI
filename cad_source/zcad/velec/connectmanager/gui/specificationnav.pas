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
  Модуль навигации по спецификации
  Предназначен для отображения и группировки спецификационных данных
}
{$mode objfpc}{$H+}

unit SpecificationNav;

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ComCtrls, ActnList, Menus,
  laz.VirtualTrees,
  uzcLog,
  uzvgetentity,
  uzeentity,
  uzcenitiesvariablesextender,
  uzcvariablesutils,
  varmandef,
  gvector;

const
  // Константы для группировки
  GROUP_BY_NONE = 0;
  GROUP_BY_NAME = 1;
  GROUP_BY_BRAND = 2;
  GROUP_BY_NAME_AND_BRAND = 3;

  // Константы для сортировки
  SORT_BY_POSITION = 0;
  SORT_BY_NAME = 1;
  SORT_BY_BRAND = 2;
  SORT_BY_COUNT = 3;

type
  // Структура данных одной спецификационной единицы
  TSpecificationItem = class
  public
    Position: String;        // Позиция
    Name: String;            // Наименование
    Brand: String;           // Марка
    Article: String;         // Код изделия
    FactoryName: String;     // Завод-изготовитель
    UnitMeasure: String;     // Единица измерения
    Count: Double;           // Количество
    Weight: String;          // Масса
    Note: String;            // Примечание
    Grouping: String;        // Группирование
    Belong: String;          // Принадлежит
  end;

  TSpecificationItemList = specialize TVector<TSpecificationItem>;

  // Данные узла VST
  PSpecNodeData = ^TSpecNodeData;
  TSpecNodeData = record
    Position: String;
    Name: String;
    Brand: String;
    Article: String;
    FactoryName: String;
    UnitMeasure: String;
    Count: Double;
    Weight: String;
    Note: String;
    Grouping: String;
    Belong: String;
  end;

  { TSpecificationNav }

  TSpecificationNav = class(TFrame)
    ActionList1: TActionList;
    actRefresh: TAction;
    actGroupSettings: TAction;
    actExport: TAction;
    actClear: TAction;
    PanelTop: TPanel;
    PanelMain: TPanel;
    ToolBar1: TToolBar;
    vstSpec: TLazVirtualStringTree;

    procedure actRefreshExecute(Sender: TObject);
    procedure actGroupSettingsExecute(Sender: TObject);
    procedure actExportExecute(Sender: TObject);
    procedure actClearExecute(Sender: TObject);
    procedure vstSpecGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: String);
    procedure vstSpecHeaderClick(Sender: TVTHeader; HitInfo: TVTHeaderHitInfo);

  private
    FSpecItems: TSpecificationItemList;
    FGroupBy: Integer;
    FSortBy: Integer;
    FSortAscending: Boolean;

    procedure InitializeToolbar;
    procedure InitializeVST;
    procedure AddToolbarButton(AAction: TAction);
    procedure LoadSpecificationData;
    procedure ExtractSpecFromDevice(ADevice: PGDBObjEntity);
    procedure GroupAndSortData;
    procedure PopulateVST;
    procedure ClearData;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    procedure RefreshData;
  end;

implementation

{$R *.lfm}

{ TSpecificationNav }

{
  Конструктор формы навигации по спецификации
}
constructor TSpecificationNav.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  programlog.LogOutFormatStr(
    'SpecificationNav: Инициализация модуля навигации по спецификации',
    [],
    LM_Info
  );

  Name := 'SpecificationNav';
  Caption := 'Specification Navigation';

  // Инициализация данных
  FSpecItems := TSpecificationItemList.Create;
  FGroupBy := GROUP_BY_NAME_AND_BRAND;
  FSortBy := SORT_BY_POSITION;
  FSortAscending := True;

  // Инициализация интерфейса
  InitializeToolbar;
  InitializeVST;

  programlog.LogOutFormatStr(
    'SpecificationNav: Модуль успешно инициализирован',
    [],
    LM_Info
  );
end;

{
  Деструктор формы
}
destructor TSpecificationNav.Destroy;
begin
  ClearData;
  FSpecItems.Free;

  programlog.LogOutFormatStr(
    'SpecificationNav: Модуль завершил работу',
    [],
    LM_Info
  );

  inherited Destroy;
end;

{
  Инициализация панели инструментов с кнопками
}
procedure TSpecificationNav.InitializeToolbar;
begin
  try
    // Настройка toolbar
    ToolBar1.Parent := PanelTop;
    ToolBar1.Height := 50;
    ToolBar1.AutoSize := False;
    ToolBar1.ShowCaptions := True;
    ToolBar1.ButtonWidth := 80;
    ToolBar1.ButtonHeight := 45;
    ToolBar1.Images := nil;

    // Создание действий
    actRefresh.Caption := 'Обновить';
    actRefresh.Hint := 'Обновить данные спецификации из чертежа';

    actGroupSettings.Caption := 'Настройки';
    actGroupSettings.Hint := 'Настройки группировки и сортировки';

    actExport.Caption := 'Экспорт';
    actExport.Hint := 'Экспорт данных в файл';

    actClear.Caption := 'Очистить';
    actClear.Hint := 'Очистить таблицу';

    // Создание кнопок
    AddToolbarButton(actRefresh);
    AddToolbarButton(actGroupSettings);
    AddToolbarButton(actExport);
    AddToolbarButton(actClear);

    ToolBar1.Realign;

    programlog.LogOutFormatStr(
      'SpecificationNav: Панель инструментов создана',
      [],
      LM_Info
    );
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'SpecificationNav: Ошибка создания панели инструментов: %s',
        [E.Message],
        LM_Info
      );
      ShowMessage('Ошибка создания панели инструментов: ' + E.Message);
    end;
  end;
end;

{
  Добавление кнопки на панель инструментов
}
procedure TSpecificationNav.AddToolbarButton(AAction: TAction);
var
  btn: TToolButton;
begin
  btn := TToolButton.Create(ToolBar1);
  btn.Parent := ToolBar1;
  btn.Action := AAction;
  btn.ShowHint := True;
  btn.AutoSize := False;
  btn.Width := 80;
  btn.Height := 45;
end;

{
  Инициализация виртуального дерева (VST)
}
procedure TSpecificationNav.InitializeVST;
begin
  try
    vstSpec.BeginUpdate;
    try
      vstSpec.Header.Columns.Clear;
      vstSpec.Clear;

      // Настройка опций VST
      vstSpec.TreeOptions.PaintOptions :=
        vstSpec.TreeOptions.PaintOptions + [toShowRoot, toShowTreeLines];
      vstSpec.TreeOptions.SelectionOptions :=
        vstSpec.TreeOptions.SelectionOptions + [toFullRowSelect];
      vstSpec.TreeOptions.MiscOptions :=
        vstSpec.TreeOptions.MiscOptions + [toGridExtensions];
      vstSpec.Header.Options :=
        vstSpec.Header.Options + [hoVisible, hoColumnResize, hoShowSortGlyphs];
      vstSpec.Header.AutoSizeIndex := -1;
      vstSpec.Header.MainColumn := 1;
      vstSpec.NodeDataSize := SizeOf(TSpecNodeData);

      // Создание колонок
      with vstSpec.Header.Columns.Add do
      begin
        Text := 'Позиция';
        Width := 80;
      end;

      with vstSpec.Header.Columns.Add do
      begin
        Text := 'Наименование';
        Width := 250;
      end;

      with vstSpec.Header.Columns.Add do
      begin
        Text := 'Марка';
        Width := 150;
      end;

      with vstSpec.Header.Columns.Add do
      begin
        Text := 'Код изделия';
        Width := 120;
      end;

      with vstSpec.Header.Columns.Add do
      begin
        Text := 'Завод-изготовитель';
        Width := 200;
      end;

      with vstSpec.Header.Columns.Add do
      begin
        Text := 'Ед. изм.';
        Width := 60;
      end;

      with vstSpec.Header.Columns.Add do
      begin
        Text := 'Количество';
        Width := 90;
      end;

      with vstSpec.Header.Columns.Add do
      begin
        Text := 'Масса';
        Width := 80;
      end;

      with vstSpec.Header.Columns.Add do
      begin
        Text := 'Примечание';
        Width := 200;
      end;

      programlog.LogOutFormatStr(
        'SpecificationNav: VST инициализирован, создано колонок: %d',
        [vstSpec.Header.Columns.Count],
        LM_Info
      );
    finally
      vstSpec.EndUpdate;
    end;
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'SpecificationNav: Ошибка инициализации VST: %s',
        [E.Message],
        LM_Info
      );
      ShowMessage('Ошибка инициализации таблицы: ' + E.Message);
    end;
  end;
end;

{
  Получение текста для ячеек VST
}
procedure TSpecificationNav.vstSpecGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
var
  data: PSpecNodeData;
begin
  data := vstSpec.GetNodeData(Node);
  if data = nil then
    Exit;

  case Column of
    0: CellText := data^.Position;
    1: CellText := data^.Name;
    2: CellText := data^.Brand;
    3: CellText := data^.Article;
    4: CellText := data^.FactoryName;
    5: CellText := data^.UnitMeasure;
    6: CellText := FormatFloat('0.###', data^.Count);
    7: CellText := data^.Weight;
    8: CellText := data^.Note;
  end;
end;

{
  Обработка клика по заголовку колонки (для сортировки)
}
procedure TSpecificationNav.vstSpecHeaderClick(Sender: TVTHeader;
  HitInfo: TVTHeaderHitInfo);
begin
  if HitInfo.Button = mbLeft then
  begin
    // Изменение направления сортировки
    if FSortBy = HitInfo.Column then
      FSortAscending := not FSortAscending
    else
    begin
      FSortBy := HitInfo.Column;
      FSortAscending := True;
    end;

    // Применение сортировки
    GroupAndSortData;
    PopulateVST;

    programlog.LogOutFormatStr(
      'SpecificationNav: Сортировка по колонке %d, направление: %s',
      [FSortBy, BoolToStr(FSortAscending, True)],
      LM_Info
    );
  end;
end;

{
  Очистка всех данных
}
procedure TSpecificationNav.ClearData;
var
  i: Integer;
begin
  for i := 0 to FSpecItems.Size - 1 do
    FSpecItems[i].Free;
  FSpecItems.Clear;

  vstSpec.Clear;

  programlog.LogOutFormatStr(
    'SpecificationNav: Данные очищены',
    [],
    LM_Info
  );
end;

{
  Действие: Обновить данные
}
procedure TSpecificationNav.actRefreshExecute(Sender: TObject);
begin
  RefreshData;
end;

{
  Действие: Настройки группировки и сортировки
}
procedure TSpecificationNav.actGroupSettingsExecute(Sender: TObject);
var
  newGroupBy: Integer;
begin
  // Временное решение - циклическое переключение режимов группировки
  newGroupBy := (FGroupBy + 1) mod 4;
  FGroupBy := newGroupBy;

  case FGroupBy of
    GROUP_BY_NONE:
      ShowMessage('Группировка отключена');
    GROUP_BY_NAME:
      ShowMessage('Группировка по наименованию');
    GROUP_BY_BRAND:
      ShowMessage('Группировка по марке');
    GROUP_BY_NAME_AND_BRAND:
      ShowMessage('Группировка по наименованию и марке');
  end;

  // Перегруппировка данных
  GroupAndSortData;
  PopulateVST;

  programlog.LogOutFormatStr(
    'SpecificationNav: Режим группировки изменен на %d',
    [FGroupBy],
    LM_Info
  );
end;

{
  Действие: Экспорт данных
}
procedure TSpecificationNav.actExportExecute(Sender: TObject);
begin
  ShowMessage('Функция экспорта будет реализована в следующей версии');

  programlog.LogOutFormatStr(
    'SpecificationNav: Запрошен экспорт данных',
    [],
    LM_Info
  );
end;

{
  Действие: Очистить таблицу
}
procedure TSpecificationNav.actClearExecute(Sender: TObject);
begin
  ClearData;

  ShowMessage('Таблица очищена');
end;

{
  Обновление данных из чертежа
}
procedure TSpecificationNav.RefreshData;
begin
  programlog.LogOutFormatStr(
    'SpecificationNav: Начато обновление данных',
    [],
    LM_Info
  );

  try
    // Очистка старых данных
    ClearData;

    // Загрузка данных
    LoadSpecificationData;

    // Группировка и сортировка
    GroupAndSortData;

    // Заполнение VST
    PopulateVST;

    programlog.LogOutFormatStr(
      'SpecificationNav: Обновление завершено, загружено записей: %d',
      [FSpecItems.Size],
      LM_Info
    );

    ShowMessage(Format('Загружено спецификационных позиций: %d', [FSpecItems.Size]));
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'SpecificationNav: Ошибка обновления данных: %s',
        [E.Message],
        LM_Info
      );
      ShowMessage('Ошибка обновления данных: ' + E.Message);
    end;
  end;
end;

{
  Загрузка спецификационных данных из устройств на чертеже
}
procedure TSpecificationNav.LoadSpecificationData;
var
  devices: TEntityVector;
  i: Integer;
begin
  programlog.LogOutFormatStr(
    'SpecificationNav: Начата загрузка спецификационных данных',
    [],
    LM_Info
  );

  try
    // Получение всех устройств с чертежа
    devices := uzvGetEntity(0, '');

    programlog.LogOutFormatStr(
      'SpecificationNav: Найдено устройств: %d',
      [devices.Size],
      LM_Info
    );

    // Извлечение спецификационных данных из каждого устройства
    for i := 0 to devices.Size - 1 do
    begin
      ExtractSpecFromDevice(devices[i]);
    end;

    devices.Free;
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'SpecificationNav: Ошибка загрузки данных: %s',
        [E.Message],
        LM_Info
      );
      raise;
    end;
  end;
end;

{
  Извлечение спецификационных данных из одного устройства
}
procedure TSpecificationNav.ExtractSpecFromDevice(ADevice: PGDBObjEntity);
var
  specIndex: Integer;
  varName: String;
  pvd: pvardesk;
  item: TSpecificationItem;
begin
  if ADevice = nil then
    Exit;

  specIndex := 1;

  // Проход по всем возможным спецификационным единицам
  while specIndex <= 100 do
  begin
    // Проверка наличия переменной Position для данного индекса
    varName := Format('VSPECIFICATION%d_Position', [specIndex]);
    pvd := FindVariableInEnt(ADevice, varName);

    if pvd = nil then
      Break; // Больше нет спецификационных единиц

    // Создание новой записи спецификации
    item := TSpecificationItem.Create;

    // Заполнение полей
    item.Position := GetVariableValueAsString(ADevice,
      Format('VSPECIFICATION%d_Position', [specIndex]));
    item.Name := GetVariableValueAsString(ADevice,
      Format('VSPECIFICATION%d_Name', [specIndex]));
    item.Brand := GetVariableValueAsString(ADevice,
      Format('VSPECIFICATION%d_Brand', [specIndex]));
    item.Article := GetVariableValueAsString(ADevice,
      Format('VSPECIFICATION%d_Article', [specIndex]));
    item.FactoryName := GetVariableValueAsString(ADevice,
      Format('VSPECIFICATION%d_Factoryname', [specIndex]));
    item.UnitMeasure := GetVariableValueAsString(ADevice,
      Format('VSPECIFICATION%d_Unit', [specIndex]));
    item.Weight := GetVariableValueAsString(ADevice,
      Format('VSPECIFICATION%d_Weight', [specIndex]));
    item.Note := GetVariableValueAsString(ADevice,
      Format('VSPECIFICATION%d_Note', [specIndex]));
    item.Grouping := GetVariableValueAsString(ADevice,
      Format('VSPECIFICATION%d_Grouping', [specIndex]));
    item.Belong := GetVariableValueAsString(ADevice,
      Format('VSPECIFICATION%d_Belong', [specIndex]));

    // Получение количества
    item.Count := GetVariableValueAsDouble(ADevice,
      Format('VSPECIFICATION%d_Count', [specIndex]));

    // Добавление в список
    FSpecItems.PushBack(item);

    Inc(specIndex);
  end;
end;

{
  Группировка и сортировка данных
}
procedure TSpecificationNav.GroupAndSortData;
var
  i, j: Integer;
  grouped: TSpecificationItemList;
  currentItem, existingItem: TSpecificationItem;
  found: Boolean;
  canGroup: Boolean;
begin
  if FSpecItems.Size = 0 then
    Exit;

  programlog.LogOutFormatStr(
    'SpecificationNav: Начата группировка данных, режим: %d',
    [FGroupBy],
    LM_Info
  );

  // Группировка данных
  if FGroupBy = GROUP_BY_NONE then
  begin
    // Группировка не требуется
    Exit;
  end;

  grouped := TSpecificationItemList.Create;
  try
    for i := 0 to FSpecItems.Size - 1 do
    begin
      currentItem := FSpecItems[i];
      found := False;

      // Поиск существующей группы
      for j := 0 to grouped.Size - 1 do
      begin
        existingItem := grouped[j];
        canGroup := False;

        case FGroupBy of
          GROUP_BY_NAME:
            canGroup := (currentItem.Name = existingItem.Name);
          GROUP_BY_BRAND:
            canGroup := (currentItem.Brand = existingItem.Brand);
          GROUP_BY_NAME_AND_BRAND:
            canGroup := (currentItem.Name = existingItem.Name) and
                       (currentItem.Brand = existingItem.Brand);
        end;

        if canGroup then
        begin
          // Объединение количества
          existingItem.Count := existingItem.Count + currentItem.Count;
          found := True;
          Break;
        end;
      end;

      // Создание новой группы
      if not found then
      begin
        existingItem := TSpecificationItem.Create;
        existingItem.Position := currentItem.Position;
        existingItem.Name := currentItem.Name;
        existingItem.Brand := currentItem.Brand;
        existingItem.Article := currentItem.Article;
        existingItem.FactoryName := currentItem.FactoryName;
        existingItem.UnitMeasure := currentItem.UnitMeasure;
        existingItem.Count := currentItem.Count;
        existingItem.Weight := currentItem.Weight;
        existingItem.Note := currentItem.Note;
        existingItem.Grouping := currentItem.Grouping;
        existingItem.Belong := currentItem.Belong;
        grouped.PushBack(existingItem);
      end;
    end;

    // Замена старых данных сгруппированными
    ClearData;
    for i := 0 to grouped.Size - 1 do
      FSpecItems.PushBack(grouped[i]);

    grouped.Clear;
  finally
    grouped.Free;
  end;

  programlog.LogOutFormatStr(
    'SpecificationNav: Группировка завершена, итого записей: %d',
    [FSpecItems.Size],
    LM_Info
  );
end;

{
  Заполнение VST данными
}
procedure TSpecificationNav.PopulateVST;
var
  i: Integer;
  node: PVirtualNode;
  data: PSpecNodeData;
  item: TSpecificationItem;
begin
  programlog.LogOutFormatStr(
    'SpecificationNav: Начато заполнение VST',
    [],
    LM_Info
  );

  vstSpec.BeginUpdate;
  try
    vstSpec.Clear;

    for i := 0 to FSpecItems.Size - 1 do
    begin
      item := FSpecItems[i];

      node := vstSpec.AddChild(nil);
      data := vstSpec.GetNodeData(node);

      data^.Position := item.Position;
      data^.Name := item.Name;
      data^.Brand := item.Brand;
      data^.Article := item.Article;
      data^.FactoryName := item.FactoryName;
      data^.UnitMeasure := item.UnitMeasure;
      data^.Count := item.Count;
      data^.Weight := item.Weight;
      data^.Note := item.Note;
      data^.Grouping := item.Grouping;
      data^.Belong := item.Belong;
    end;

    programlog.LogOutFormatStr(
      'SpecificationNav: VST заполнен, узлов: %d',
      [vstSpec.RootNodeCount],
      LM_Info
    );
  finally
    vstSpec.EndUpdate;
  end;
end;

{
  Получение значения переменной как строки
}
function GetVariableValueAsString(AEntity: PGDBObjEntity;
  const VarName: String): String;
var
  pvd: pvardesk;
begin
  Result := '';
  if AEntity = nil then
    Exit;

  pvd := FindVariableInEnt(AEntity, VarName);
  if pvd <> nil then
    Result := Trim(pString(pvd^.data.Addr.Instance)^);
end;

{
  Получение значения переменной как числа
}
function GetVariableValueAsDouble(AEntity: PGDBObjEntity;
  const VarName: String): Double;
var
  pvd: pvardesk;
begin
  Result := 0.0;
  if AEntity = nil then
    Exit;

  pvd := FindVariableInEnt(AEntity, VarName);
  if pvd <> nil then
  begin
    if pvd^.data.PTD^.TypeName = 'Double' then
      Result := PDouble(pvd^.data.Addr.Instance)^
    else if pvd^.data.PTD^.TypeName = 'Integer' then
      Result := PInteger(pvd^.data.Addr.Instance)^;
  end;
end;

end.
