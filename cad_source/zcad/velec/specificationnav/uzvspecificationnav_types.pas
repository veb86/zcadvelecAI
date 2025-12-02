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

{**Модуль описания типов данных для работы со спецификационными данными устройств}
unit uzvspecificationnav_types;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fgl;

type
  // Структура данных для одной спецификационной единицы
  TSpecificationItem = class
  private
    FPosition: String;        // Позиция
    FName: String;            // Наименование
    FBrand: String;           // Марка
    FArticle: String;         // Код изделия
    FFactoryName: String;     // Завод-изготовитель
    FUnit: String;            // Единица измерения
    FCount: Double;           // Количество
    FWeight: String;          // Масса (текст)
    FNote: String;            // Примечание
    FGrouping: String;        // Группирование
    FBelong: String;          // Принадлежит
    FDeviceName: String;      // Имя устройства, которому принадлежит эта запись
    FIndex: Integer;          // Индекс спецификации внутри устройства (1, 2, 3...)
  public
    constructor Create;
    destructor Destroy; override;

    // Свойства для доступа к данным
    property Position: String read FPosition write FPosition;
    property Name: String read FName write FName;
    property Brand: String read FBrand write FBrand;
    property Article: String read FArticle write FArticle;
    property FactoryName: String read FFactoryName write FFactoryName;
    property UnitMeasure: String read FUnit write FUnit;
    property Count: Double read FCount write FCount;
    property Weight: String read FWeight write FWeight;
    property Note: String read FNote write FNote;
    property Grouping: String read FGrouping write FGrouping;
    property Belong: String read FBelong write FBelong;
    property DeviceName: String read FDeviceName write FDeviceName;
    property SpecIndex: Integer read FIndex write FIndex;

    // Проверка на пустоту записи
    function IsEmpty: Boolean;

    // Получение строкового представления для отладки
    function ToString: String;
  end;

  // Список спецификационных элементов
  TSpecificationList = class(specialize TFPGObjectList<TSpecificationItem>)
  public
    // Добавление нового элемента в список
    function AddItem(const AItem: TSpecificationItem): Integer;

    // Очистка списка
    procedure ClearAll;

    // Получение количества элементов
    function GetCount: Integer;

    // Фильтрация по группе
    function FilterByGrouping(const AGrouping: String): TSpecificationList;

    // Фильтрация по принадлежности
    function FilterByBelong(const ABelong: String): TSpecificationList;
  end;

  // Константы для имен переменных спецификации
const
  SPEC_VAR_PREFIX = 'VSPECIFICATION';
  SPEC_VAR_POSITION = '_Position';
  SPEC_VAR_NAME = '_Name';
  SPEC_VAR_BRAND = '_Brand';
  SPEC_VAR_ARTICLE = '_Article';
  SPEC_VAR_FACTORYNAME = '_Factoryname';
  SPEC_VAR_UNIT = '_Unit';
  SPEC_VAR_COUNT = '_Count';
  SPEC_VAR_WEIGHT = '_Weight';
  SPEC_VAR_NOTE = '_Note';
  SPEC_VAR_GROUPING = '_Grouping';
  SPEC_VAR_BELONG = '_Belong';

implementation

{ TSpecificationItem }

constructor TSpecificationItem.Create;
begin
  inherited Create;
  FPosition := '';
  FName := '';
  FBrand := '';
  FArticle := '';
  FFactoryName := '';
  FUnit := '';
  FCount := 0.0;
  FWeight := '';
  FNote := '';
  FGrouping := '';
  FBelong := '';
  FDeviceName := '';
  FIndex := 0;
end;

destructor TSpecificationItem.Destroy;
begin
  inherited Destroy;
end;

// Проверка, пустая ли запись (все основные поля пусты)
function TSpecificationItem.IsEmpty: Boolean;
begin
  Result := (Trim(FName) = '') and
            (Trim(FBrand) = '') and
            (Trim(FArticle) = '') and
            (FCount = 0.0);
end;

// Строковое представление для отладки
function TSpecificationItem.ToString: String;
begin
  Result := Format(
    'Spec[%d]: Pos=%s, Name=%s, Brand=%s, Article=%s, Count=%.2f, Device=%s',
    [FIndex, FPosition, FName, FBrand, FArticle, FCount, FDeviceName]
  );
end;

{ TSpecificationList }

// Добавление элемента в список
function TSpecificationList.AddItem(const AItem: TSpecificationItem): Integer;
begin
  Result := Self.Add(AItem);
end;

// Полная очистка списка
procedure TSpecificationList.ClearAll;
begin
  Self.Clear;
end;

// Получение количества элементов
function TSpecificationList.GetCount: Integer;
begin
  Result := Self.Count;
end;

// Фильтрация по группе
function TSpecificationList.FilterByGrouping(const AGrouping: String): TSpecificationList;
var
  i: Integer;
  Item: TSpecificationItem;
  NewItem: TSpecificationItem;
begin
  Result := TSpecificationList.Create(False);

  for i := 0 to Self.Count - 1 do
  begin
    Item := Self.Items[i];
    if (Item.Grouping = AGrouping) or (AGrouping = '') then
    begin
      NewItem := TSpecificationItem.Create;
      NewItem.Position := Item.Position;
      NewItem.Name := Item.Name;
      NewItem.Brand := Item.Brand;
      NewItem.Article := Item.Article;
      NewItem.FactoryName := Item.FactoryName;
      NewItem.UnitMeasure := Item.UnitMeasure;
      NewItem.Count := Item.Count;
      NewItem.Weight := Item.Weight;
      NewItem.Note := Item.Note;
      NewItem.Grouping := Item.Grouping;
      NewItem.Belong := Item.Belong;
      NewItem.DeviceName := Item.DeviceName;
      NewItem.SpecIndex := Item.SpecIndex;
      Result.Add(NewItem);
    end;
  end;
end;

// Фильтрация по принадлежности
function TSpecificationList.FilterByBelong(const ABelong: String): TSpecificationList;
var
  i: Integer;
  Item: TSpecificationItem;
  NewItem: TSpecificationItem;
begin
  Result := TSpecificationList.Create(False);

  for i := 0 to Self.Count - 1 do
  begin
    Item := Self.Items[i];
    if (Item.Belong = ABelong) or (ABelong = '') then
    begin
      NewItem := TSpecificationItem.Create;
      NewItem.Position := Item.Position;
      NewItem.Name := Item.Name;
      NewItem.Brand := Item.Brand;
      NewItem.Article := Item.Article;
      NewItem.FactoryName := Item.FactoryName;
      NewItem.UnitMeasure := Item.UnitMeasure;
      NewItem.Count := Item.Count;
      NewItem.Weight := Item.Weight;
      NewItem.Note := Item.Note;
      NewItem.Grouping := Item.Grouping;
      NewItem.Belong := Item.Belong;
      NewItem.DeviceName := Item.DeviceName;
      NewItem.SpecIndex := Item.SpecIndex;
      Result.Add(NewItem);
    end;
  end;
end;

end.
