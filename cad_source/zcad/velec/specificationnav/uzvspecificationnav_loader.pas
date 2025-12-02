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

{**Модуль загрузки спецификационных данных из устройств на чертеже}
unit uzvspecificationnav_loader;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzvspecificationnav_types,
  uzeentdevice,
  uzcentitiesvariablesextender,
  varmandef,
  uzclog;

type
  // Класс для загрузки спецификационных данных из устройств
  TSpecificationLoader = class
  private
    // Получение строковой переменной из расширения устройства
    function GetStringVariable(
      VarExt: TVariablesExtender;
      const VarName: String
    ): String;

    // Получение числовой переменной из расширения устройства
    function GetDoubleVariable(
      VarExt: TVariablesExtender;
      const VarName: String;
      DefaultValue: Double
    ): Double;

    // Построение имени переменной для конкретного индекса спецификации
    function BuildVarName(Index: Integer; const Suffix: String): String;

  public
    constructor Create;
    destructor Destroy; override;

    // Загрузка всех спецификационных данных из одного устройства
    function LoadFromDevice(
      pDevice: PGDBObjDevice;
      const DeviceName: String
    ): TSpecificationList;

    // Загрузка одной спецификационной единицы по индексу
    function LoadSpecificationItem(
      VarExt: TVariablesExtender;
      Index: Integer;
      const DeviceName: String
    ): TSpecificationItem;

    // Проверка наличия спецификационных данных в устройстве
    function HasSpecificationData(pDevice: PGDBObjDevice): Boolean;

    // Подсчет количества спецификационных единиц в устройстве
    function CountSpecificationItems(pDevice: PGDBObjDevice): Integer;
  end;

implementation

{ TSpecificationLoader }

constructor TSpecificationLoader.Create;
begin
  inherited Create;
end;

destructor TSpecificationLoader.Destroy;
begin
  inherited Destroy;
end;

// Получение строковой переменной из расширения
function TSpecificationLoader.GetStringVariable(
  VarExt: TVariablesExtender;
  const VarName: String
): String;
var
  pvd: pvardesk;
begin
  Result := '';

  if VarExt = nil then
    Exit;

  pvd := VarExt.entityunit.FindVariable(VarName);
  if pvd = nil then
    Exit;

  Result := Trim(PString(pvd^.data.Addr.Instance)^);
end;

// Получение числовой переменной из расширения
function TSpecificationLoader.GetDoubleVariable(
  VarExt: TVariablesExtender;
  const VarName: String;
  DefaultValue: Double
): Double;
var
  pvd: pvardesk;
begin
  Result := DefaultValue;

  if VarExt = nil then
    Exit;

  pvd := VarExt.entityunit.FindVariable(VarName);
  if pvd = nil then
    Exit;

  Result := PDouble(pvd^.data.Addr.Instance)^;
end;

// Построение имени переменной для конкретного индекса
function TSpecificationLoader.BuildVarName(
  Index: Integer;
  const Suffix: String
): String;
begin
  Result := SPEC_VAR_PREFIX + IntToStr(Index) + Suffix;
end;

// Загрузка одной спецификационной единицы по индексу
function TSpecificationLoader.LoadSpecificationItem(
  VarExt: TVariablesExtender;
  Index: Integer;
  const DeviceName: String
): TSpecificationItem;
var
  Item: TSpecificationItem;
begin
  Item := TSpecificationItem.Create;

  try
    // Заполняем все поля из переменных устройства
    Item.Position := GetStringVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_POSITION)
    );

    Item.Name := GetStringVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_NAME)
    );

    Item.Brand := GetStringVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_BRAND)
    );

    Item.Article := GetStringVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_ARTICLE)
    );

    Item.FactoryName := GetStringVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_FACTORYNAME)
    );

    Item.UnitMeasure := GetStringVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_UNIT)
    );

    Item.Count := GetDoubleVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_COUNT),
      0.0
    );

    Item.Weight := GetStringVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_WEIGHT)
    );

    Item.Note := GetStringVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_NOTE)
    );

    Item.Grouping := GetStringVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_GROUPING)
    );

    Item.Belong := GetStringVariable(
      VarExt,
      BuildVarName(Index, SPEC_VAR_BELONG)
    );

    Item.DeviceName := DeviceName;
    Item.SpecIndex := Index;

    Result := Item;

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'Ошибка загрузки спецификационной единицы %d из устройства %s: %s',
        [Index, DeviceName, E.Message],
        LM_Info
      );
      Item.Free;
      Result := nil;
    end;
  end;
end;

// Подсчет количества спецификационных единиц в устройстве
function TSpecificationLoader.CountSpecificationItems(
  pDevice: PGDBObjDevice
): Integer;
var
  VarExt: TVariablesExtender;
  Index: Integer;
  VarName: String;
  pvd: pvardesk;
begin
  Result := 0;

  if pDevice = nil then
    Exit;

  // Получаем расширение переменных устройства
  VarExt := pDevice^.specialize GetExtension<TVariablesExtender>;

  if VarExt = nil then
    Exit;

  // Проверяем наличие переменных с индексами 1, 2, 3...
  // пока находим переменную _Name
  Index := 1;
  while Index <= 100 do  // Ограничиваем максимум 100 спецификациями
  begin
    VarName := BuildVarName(Index, SPEC_VAR_NAME);
    pvd := VarExt.entityunit.FindVariable(VarName);

    if pvd = nil then
      Break;

    Inc(Result);
    Inc(Index);
  end;
end;

// Проверка наличия спецификационных данных в устройстве
function TSpecificationLoader.HasSpecificationData(
  pDevice: PGDBObjDevice
): Boolean;
begin
  Result := CountSpecificationItems(pDevice) > 0;
end;

// Загрузка всех спецификационных данных из устройства
function TSpecificationLoader.LoadFromDevice(
  pDevice: PGDBObjDevice;
  const DeviceName: String
): TSpecificationList;
var
  VarExt: TVariablesExtender;
  SpecList: TSpecificationList;
  Item: TSpecificationItem;
  Index: Integer;
  ItemCount: Integer;
begin
  SpecList := TSpecificationList.Create(True);

  if pDevice = nil then
  begin
    Result := SpecList;
    Exit;
  end;

  // Получаем расширение переменных устройства
  VarExt := pDevice^.specialize GetExtension<TVariablesExtender>;

  if VarExt = nil then
  begin
    Result := SpecList;
    Exit;
  end;

  try
    // Подсчитываем количество спецификационных единиц
    ItemCount := CountSpecificationItems(pDevice);

    if ItemCount = 0 then
    begin
      programlog.LogOutFormatStr(
        'Устройство %s не содержит спецификационных данных',
        [DeviceName],
        LM_Info
      );
    end
    else
    begin
      programlog.LogOutFormatStr(
        'Загрузка %d спецификационных единиц из устройства %s',
        [ItemCount, DeviceName],
        LM_Info
      );
    end;

    // Загружаем каждую единицу
    for Index := 1 to ItemCount do
    begin
      Item := LoadSpecificationItem(VarExt, Index, DeviceName);

      if Item <> nil then
      begin
        // Проверяем, не пустая ли запись
        if not Item.IsEmpty then
        begin
          SpecList.Add(Item);
        end
        else
        begin
          // Пустая запись, освобождаем память
          Item.Free;
        end;
      end;
    end;

    programlog.LogOutFormatStr(
      'Загружено %d непустых спецификационных записей',
      [SpecList.Count],
      LM_Info
    );

  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr(
        'Ошибка при загрузке спецификации из устройства %s: %s',
        [DeviceName, E.Message],
        LM_Info
      );
    end;
  end;

  Result := SpecList;
end;

end.
