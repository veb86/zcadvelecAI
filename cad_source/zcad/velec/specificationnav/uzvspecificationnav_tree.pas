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

{**Модуль обертки для работы с TLazVirtualStringTree для отображения спецификаций}
unit uzvspecificationnav_tree;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  Graphics,
  laz.VirtualTrees,
  uzvspecificationnav_types,
  uzclog;

type
  // Данные узла дерева
  PSpecTreeNodeData = ^TSpecTreeNodeData;
  TSpecTreeNodeData = record
    SpecItem: TSpecificationItem;  // Ссылка на элемент спецификации
  end;

  // Класс-обертка для работы с деревом спецификаций
  TSpecificationTreeManager = class
  private
    FTree: TLazVirtualStringTree;
    FSpecList: TSpecificationList;

    // Обработчик получения текста для ячейки
    procedure OnGetText(
      Sender: TBaseVirtualTree;
      Node: PVirtualNode;
      Column: TColumnIndex;
      TextType: TVSTTextType;
      var CellText: String
    );

    // Обработчик получения размера данных узла
    procedure OnGetNodeDataSize(
      Sender: TBaseVirtualTree;
      var NodeDataSize: Integer
    );

    // Обработчик инициализации узла
    procedure OnInitNode(
      Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates
    );

    // Обработчик освобождения узла
    procedure OnFreeNode(
      Sender: TBaseVirtualTree;
      Node: PVirtualNode
    );

  public
    constructor Create(ATree: TLazVirtualStringTree);
    destructor Destroy; override;

    // Инициализация дерева и настройка колонок
    procedure Initialize;

    // Заполнение дерева данными из списка спецификаций
    procedure PopulateTree(ASpecList: TSpecificationList);

    // Очистка дерева
    procedure ClearTree;

    // Получение количества узлов
    function GetNodeCount: Integer;

    // Экспорт данных в строковый список (для отладки)
    procedure ExportToStringList(AList: TStringList);
  end;

implementation

{ TSpecificationTreeManager }

constructor TSpecificationTreeManager.Create(
  ATree: TLazVirtualStringTree
);
begin
  inherited Create;
  FTree := ATree;
  FSpecList := nil;
end;

destructor TSpecificationTreeManager.Destroy;
begin
  inherited Destroy;
end;

// Инициализация дерева и настройка колонок
procedure TSpecificationTreeManager.Initialize;
var
  Column: TVirtualTreeColumn;
begin
  if FTree = nil then
    Exit;

  // Очищаем дерево
  FTree.Clear;
  FTree.Header.Columns.Clear;

  // Настройка дерева
  FTree.NodeDataSize := SizeOf(TSpecTreeNodeData);
  FTree.TreeOptions.SelectionOptions := FTree.TreeOptions.SelectionOptions + [toFullRowSelect];
  FTree.TreeOptions.MiscOptions := FTree.TreeOptions.MiscOptions + [toGridExtensions];
  FTree.Header.Options := FTree.Header.Options + [hoVisible, hoColumnResize, hoShowSortGlyphs];

  // Подключаем обработчики событий
  FTree.OnGetText := @OnGetText;
  FTree.OnGetNodeDataSize := @OnGetNodeDataSize;
  FTree.OnInitNode := @OnInitNode;
  FTree.OnFreeNode := @OnFreeNode;

  // Создаем колонки
  // Колонка: Устройство
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Устройство';
  Column.Width := 150;

  // Колонка: Позиция
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Позиция';
  Column.Width := 80;

  // Колонка: Наименование
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Наименование';
  Column.Width := 250;

  // Колонка: Марка
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Марка';
  Column.Width := 150;

  // Колонка: Код изделия
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Код изделия';
  Column.Width := 120;

  // Колонка: Завод-изготовитель
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Завод';
  Column.Width := 150;

  // Колонка: Единица измерения
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Ед. изм.';
  Column.Width := 70;

  // Колонка: Количество
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Количество';
  Column.Width := 80;

  // Колонка: Масса
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Масса';
  Column.Width := 80;

  // Колонка: Примечание
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Примечание';
  Column.Width := 200;

  // Колонка: Группирование
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Группа';
  Column.Width := 100;

  // Колонка: Принадлежит
  Column := FTree.Header.Columns.Add;
  Column.Text := 'Принадлежит';
  Column.Width := 100;

  programlog.LogOutFormatStr(
    'Дерево спецификаций инициализировано с %d колонками',
    [FTree.Header.Columns.Count],
    LM_Info
  );
end;

// Обработчик получения размера данных узла
procedure TSpecificationTreeManager.OnGetNodeDataSize(
  Sender: TBaseVirtualTree;
  var NodeDataSize: Integer
);
begin
  NodeDataSize := SizeOf(TSpecTreeNodeData);
end;

// Обработчик инициализации узла
procedure TSpecificationTreeManager.OnInitNode(
  Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates
);
begin
  // Инициализация выполняется в PopulateTree
end;

// Обработчик освобождения узла
procedure TSpecificationTreeManager.OnFreeNode(
  Sender: TBaseVirtualTree;
  Node: PVirtualNode
);
var
  NodeData: PSpecTreeNodeData;
begin
  NodeData := Sender.GetNodeData(Node);
  if NodeData <> nil then
  begin
    NodeData^.SpecItem := nil;  // Не освобождаем, т.к. владеет TSpecificationList
  end;
end;

// Обработчик получения текста для ячейки
procedure TSpecificationTreeManager.OnGetText(
  Sender: TBaseVirtualTree;
  Node: PVirtualNode;
  Column: TColumnIndex;
  TextType: TVSTTextType;
  var CellText: String
);
var
  NodeData: PSpecTreeNodeData;
  Item: TSpecificationItem;
begin
  CellText := '';
  NodeData := Sender.GetNodeData(Node);

  if (NodeData = nil) or (NodeData^.SpecItem = nil) then
    Exit;

  Item := NodeData^.SpecItem;

  case Column of
    0: CellText := Item.DeviceName;
    1: CellText := Item.Position;
    2: CellText := Item.Name;
    3: CellText := Item.Brand;
    4: CellText := Item.Article;
    5: CellText := Item.FactoryName;
    6: CellText := Item.UnitMeasure;
    7: CellText := Format('%.2f', [Item.Count]);
    8: CellText := Item.Weight;
    9: CellText := Item.Note;
    10: CellText := Item.Grouping;
    11: CellText := Item.Belong;
  else
    CellText := '';
  end;
end;

// Заполнение дерева данными из списка спецификаций
procedure TSpecificationTreeManager.PopulateTree(
  ASpecList: TSpecificationList
);
var
  i: Integer;
  Item: TSpecificationItem;
  Node: PVirtualNode;
  NodeData: PSpecTreeNodeData;
begin
  if FTree = nil then
    Exit;

  if ASpecList = nil then
  begin
    programlog.LogOutFormatStr(
      'Попытка заполнить дерево пустым списком',
      [],
      LM_Info
    );
    Exit;
  end;

  FSpecList := ASpecList;

  // Очищаем дерево перед заполнением
  FTree.BeginUpdate;
  try
    FTree.Clear;

    programlog.LogOutFormatStr(
      'Заполнение дерева: %d элементов спецификации',
      [ASpecList.Count],
      LM_Info
    );

    // Добавляем каждый элемент в дерево
    for i := 0 to ASpecList.Count - 1 do
    begin
      Item := ASpecList.Items[i];

      // Создаем узел
      Node := FTree.AddChild(nil);
      NodeData := FTree.GetNodeData(Node);

      if NodeData <> nil then
      begin
        NodeData^.SpecItem := Item;
      end;
    end;

    programlog.LogOutFormatStr(
      'Дерево заполнено: %d узлов',
      [FTree.TotalCount],
      LM_Info
    );

  finally
    FTree.EndUpdate;
  end;
end;

// Очистка дерева
procedure TSpecificationTreeManager.ClearTree;
begin
  if FTree <> nil then
  begin
    FTree.Clear;
    FSpecList := nil;
  end;
end;

// Получение количества узлов
function TSpecificationTreeManager.GetNodeCount: Integer;
begin
  Result := 0;
  if FTree <> nil then
    Result := FTree.TotalCount;
end;

// Экспорт данных в строковый список (для отладки)
procedure TSpecificationTreeManager.ExportToStringList(AList: TStringList);
var
  i: Integer;
  Item: TSpecificationItem;
begin
  if (AList = nil) or (FSpecList = nil) then
    Exit;

  AList.Clear;
  AList.Add('Спецификационные данные:');
  AList.Add('');

  for i := 0 to FSpecList.Count - 1 do
  begin
    Item := FSpecList.Items[i];
    AList.Add(Format(
      '%d. Устройство: %s, Позиция: %s, Наименование: %s, Кол-во: %.2f',
      [i + 1, Item.DeviceName, Item.Position, Item.Name, Item.Count]
    ));
  end;
end;

end.
