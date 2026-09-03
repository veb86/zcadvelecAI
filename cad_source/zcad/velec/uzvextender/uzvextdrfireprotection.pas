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
unit uzvExtdrFireProtection;
{$Mode objfpc}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzedrawingdef,
  uzeExtdrAbstractEntityExtender,
  uzeExtdrBaseEntityExtender,
  uzeentdevice,
  uzeentcircle,
  uzeentity,
  uzbtypes,
  uzctnrVectorBytes,
  uzeffdxfsupport,
  usimplegenerics,
  uzeBaseExtender,
  uzgldrawcontext,
  uzclog,
  uzegeometrytypes,
  uzegeometry,
  varmandef,
  Varman,
  TypeDescriptors,
  UBaseTypeDescriptor;

const
  FireProtectionExtenderName = 'extdrFireProtection';
  DefaultExtederLayer = 'EXTEDER';

type
  // Тип пожарного извещателя
  TFireDetectorType = (
    fdtUnknown,   // Неизвестный тип
    fdtSmoke,     // Дымовой извещатель
    fdtHeat       // Тепловой извещатель
  );

  // Расширение для отображения кругов защиты пожарных извещателей
  TFireProtectionExtender = class(TBaseEntityExtender)
  private
    FDetectorType: TFireDetectorType;
    FInstallHeight: Double;
    FProtectionCircle: PGDBObjCircle;

    // Получить радиус защиты на основе типа и высоты установки
    function CalculateProtectionRadius: Double;

    // Получить тип извещателя из параметров устройства
    function GetDetectorTypeFromDevice: TFireDetectorType;

    // Получить высоту установки из параметров устройства
    function GetInstallHeightFromDevice: Double;

    // Создать круг защиты
    procedure CreateProtectionCircle(const Drawing: TDrawingDef);

    // Удалить круг защиты
    procedure DestroyProtectionCircle(const Drawing: TDrawingDef);

  public
    constructor Create(pEntity: Pointer); override;
    destructor Destroy; override;

    class function getExtenderName: string; override;

    procedure Assign(Source: TBaseExtender); override;
    procedure onEntityClone(pSourceEntity, pDestEntity: Pointer); override;
    procedure CopyExt2Ent(pSourceEntity, pDestEntity: Pointer); override;

    procedure onEntityBuildVarGeometry(pEntity: Pointer; const Drawing: TDrawingDef); override;
    procedure onBeforeEntityFormat(pEntity: Pointer; const Drawing: TDrawingDef; var DC: TDrawContext); override;
    procedure onAfterEntityFormat(pEntity: Pointer; const Drawing: TDrawingDef; var DC: TDrawContext); override;
    procedure onRemoveFromArray(pEntity: Pointer; const Drawing: TDrawingDef); override;
    procedure onEntitySupportOldVersions(pEntity: Pointer; const Drawing: TDrawingDef); override;

    procedure ReorganizeEnts(OldEnts2NewEntsMap: TMapPointerToPointer); override;
    procedure PostLoad(var Context: TIODXFLoadContext); override;
    procedure SaveToDxfObjXData(var outStream: TZctnrVectorBytes; pEntity: Pointer; var IODXFContext: TIODXFSaveContext); override;

    class function EntIOLoadFireProtection(_Name, _Value: String; ptu: PExtensionData; const Drawing: TDrawingDef; pEntity: Pointer): Boolean;

    property DetectorType: TFireDetectorType read FDetectorType write FDetectorType;
    property InstallHeight: Double read FInstallHeight write FInstallHeight;
  end;

// Добавить расширение защиты к устройству
function AddFireProtectionExtenderToEntity(pEntity: PGDBObjEntity): TFireProtectionExtender;

implementation

// Добавить расширение защиты к устройству
function AddFireProtectionExtenderToEntity(pEntity: PGDBObjEntity): TFireProtectionExtender;
begin
  Result := TFireProtectionExtender.Create(pEntity);
  pEntity^.AddExtension(Result);
  programlog.LogOutFormatStr('uzvextdrfireprotection: extender added to entity', [], LM_Info);
end;

// Получить имя расширения
class function TFireProtectionExtender.getExtenderName: string;
begin
  Result := FireProtectionExtenderName;
end;

// Конструктор
constructor TFireProtectionExtender.Create(pEntity: Pointer);
begin
  inherited Create(pEntity);
  FDetectorType := fdtUnknown;
  FInstallHeight := 0.0;
  FProtectionCircle := nil;
  programlog.LogOutFormatStr('uzvextdrfireprotection: extender created', [], LM_Info);
end;

// Деструктор
destructor TFireProtectionExtender.Destroy;
begin
  FProtectionCircle := nil;
  inherited Destroy;
end;

// Копирование параметров расширения
procedure TFireProtectionExtender.Assign(Source: TBaseExtender);
begin
  if Source is TFireProtectionExtender then
  begin
    FDetectorType := TFireProtectionExtender(Source).FDetectorType;
    FInstallHeight := TFireProtectionExtender(Source).FInstallHeight;
  end;
end;

// Клонирование расширения при копировании объекта
procedure TFireProtectionExtender.onEntityClone(pSourceEntity, pDestEntity: Pointer);
var
  pDestExtender: TFireProtectionExtender;
begin
  pDestExtender := PGDBObjEntity(pDestEntity)^.EntExtensions.GetExtensionOf<TFireProtectionExtender>;
  if pDestExtender = nil then
    pDestExtender := AddFireProtectionExtenderToEntity(pDestEntity);
  pDestExtender.Assign(Self);
end;

// Копирование расширения в другую сущность
procedure TFireProtectionExtender.CopyExt2Ent(pSourceEntity, pDestEntity: Pointer);
begin
  onEntityClone(pSourceEntity, pDestEntity);
end;

// Получить тип извещателя из параметров устройства
function TFireProtectionExtender.GetDetectorTypeFromDevice: TFireDetectorType;
var
  pDevice: PGDBObjDevice;
  detectorTypeVar: Pointer;
  varName: String;
  varValue: String;
begin
  Result := fdtUnknown;

  if pThisEntity <> nil then
  begin
    pDevice := PGDBObjDevice(pThisEntity);

    // Попытка найти переменную типа извещателя
    detectorTypeVar := pDevice^.ou.FindVariable('DETECTORTYPE');
    if detectorTypeVar = nil then
      detectorTypeVar := pDevice^.ou.FindVariable('NMO_Name');

    if detectorTypeVar <> nil then
    begin
      varValue := pvardesk(detectorTypeVar)^.data.PTD^.GetValueAsString(pvardesk(detectorTypeVar)^.data.Instance);
      varValue := UpperCase(varValue);

      // Определение типа по названию
      if (Pos('ДЫМ', varValue) > 0) or (Pos('SMOKE', varValue) > 0) then
        Result := fdtSmoke
      else if (Pos('ТЕПЛ', varValue) > 0) or (Pos('HEAT', varValue) > 0) then
        Result := fdtHeat;

      programlog.LogOutFormatStr('uzvextdrfireprotection: detector type = %s', [varValue], LM_Info);
    end;
  end;
end;

// Получить высоту установки из параметров устройства
function TFireProtectionExtender.GetInstallHeightFromDevice: Double;
var
  pDevice: PGDBObjDevice;
  heightVar: Pointer;
  varValue: String;
begin
  Result := 3.5; // Значение по умолчанию

  if pThisEntity <> nil then
  begin
    pDevice := PGDBObjDevice(pThisEntity);

    // Попытка найти переменную высоты установки
    heightVar := pDevice^.ou.FindVariable('INSTALLHEIGHT');
    if heightVar = nil then
      heightVar := pDevice^.ou.FindVariable('HEIGHT');
    if heightVar = nil then
      heightVar := pDevice^.ou.FindVariable('H');

    if heightVar <> nil then
    begin
      if pvardesk(heightVar)^.data.PTD = @FundamentalDoubleDescriptorObj then
        Result := PDouble(pvardesk(heightVar)^.data.Instance)^
      else if pvardesk(heightVar)^.data.PTD = @FundamentalSingleDescriptorObj then
        Result := PSingle(pvardesk(heightVar)^.data.Instance)^
      else
      begin
        varValue := pvardesk(heightVar)^.data.PTD^.GetValueAsString(pvardesk(heightVar)^.data.Instance);
        Result := StrToFloatDef(varValue, 3.5);
      end;

      programlog.LogOutFormatStr('uzvextdrfireprotection: install height = %.2f', [Result], LM_Info);
    end;
  end;
end;

// Расчет радиуса защиты на основе типа и высоты установки
// По СП 5.13130.2009 (нормы пожарной безопасности РФ)
function TFireProtectionExtender.CalculateProtectionRadius: Double;
begin
  Result := 0.0;

  case FDetectorType of
    fdtSmoke:
    begin
      // Для дымовых извещателей
      if FInstallHeight <= 3.5 then
        Result := 9.0
      else if FInstallHeight <= 6.0 then
        Result := 8.5
      else if FInstallHeight <= 10.0 then
        Result := 8.0
      else if FInstallHeight <= 12.0 then
        Result := 7.0
      else
        Result := 6.0;
    end;

    fdtHeat:
    begin
      // Для тепловых извещателей
      if FInstallHeight <= 3.5 then
        Result := 5.0
      else if FInstallHeight <= 6.0 then
        Result := 4.5
      else if FInstallHeight <= 9.0 then
        Result := 4.0
      else
        Result := 3.5;
    end;

    fdtUnknown:
    begin
      // Для неизвестного типа используем минимальный радиус
      Result := 3.5;
    end;
  end;

  programlog.LogOutFormatStr('uzvextdrfireprotection: protection radius = %.2f m', [Result], LM_Info);
end;

// Создание круга защиты на слое EXTEDER
procedure TFireProtectionExtender.CreateProtectionCircle(const Drawing: TDrawingDef);
var
  pDevice: PGDBObjDevice;
  pLayer: PGDBLayerProp;
  radius: Double;
  centerPoint: TzePoint3d;
begin
  if pThisEntity = nil then
    Exit;

  if FProtectionCircle <> nil then
    Exit;

  pDevice := PGDBObjDevice(pThisEntity);

  // Получение или создание слоя EXTEDER
  pLayer := Drawing.GetLayerTable^.GetOrCreateLayer(DefaultExtederLayer);
  if pLayer = nil then
  begin
    programlog.LogOutFormatStr('uzvextdrfireprotection: failed to get/create layer %s', [DefaultExtederLayer], LM_Info);
    Exit;
  end;

  // Расчет радиуса защиты
  radius := CalculateProtectionRadius;
  if radius <= 0.0 then
    Exit;

  // Центр круга - точка вставки устройства
  centerPoint := pDevice^.P_insert_in_WCS;

  // Создание круга
  FProtectionCircle := GDBObjCircle.CreateInstance;
  FProtectionCircle^.init(@pDevice^.vp.owner, pLayer, 0, centerPoint, radius);

  // Добавление круга в массив переменных устройства
  pDevice^.VarObjArray.AddPEntity(FProtectionCircle^);
  FProtectionCircle^.bp.ListPos.Owner := pDevice;

  programlog.LogOutFormatStr('uzvextdrfireprotection: circle created with radius %.2f at layer %s', [radius, DefaultExtederLayer], LM_Info);
end;

// Удаление круга защиты
procedure TFireProtectionExtender.DestroyProtectionCircle(const Drawing: TDrawingDef);
var
  pDevice: PGDBObjDevice;
  i: Integer;
  pEnt: PGDBObjEntity;
  ir: itrec;
begin
  if FProtectionCircle = nil then
    Exit;

  if pThisEntity = nil then
    Exit;

  pDevice := PGDBObjDevice(pThisEntity);

  // Поиск и удаление круга из массива
  pEnt := pDevice^.VarObjArray.beginiterate(ir);
  if pEnt <> nil then
  repeat
    if pEnt = PGDBObjEntity(FProtectionCircle) then
    begin
      pDevice^.VarObjArray.DeleteElement(ir.itc);
      FProtectionCircle^.done;
      Freemem(Pointer(FProtectionCircle));
      FProtectionCircle := nil;
      programlog.LogOutFormatStr('uzvextdrfireprotection: circle destroyed', [], LM_Info);
      Exit;
    end;
    pEnt := pDevice^.VarObjArray.iterate(ir);
  until pEnt = nil;

  FProtectionCircle := nil;
end;

// Построение геометрии переменных
procedure TFireProtectionExtender.onEntityBuildVarGeometry(pEntity: Pointer; const Drawing: TDrawingDef);
begin
  // Получение параметров из устройства
  FDetectorType := GetDetectorTypeFromDevice;
  FInstallHeight := GetInstallHeightFromDevice;

  // Пересоздание круга защиты с новыми параметрами
  DestroyProtectionCircle(Drawing);
  CreateProtectionCircle(Drawing);
end;

// До форматирования сущности
procedure TFireProtectionExtender.onBeforeEntityFormat(pEntity: Pointer; const Drawing: TDrawingDef; var DC: TDrawContext);
begin
  // Ничего не делаем
end;

// После форматирования сущности
procedure TFireProtectionExtender.onAfterEntityFormat(pEntity: Pointer; const Drawing: TDrawingDef; var DC: TDrawContext);
begin
  // Ничего не делаем
end;

// Удаление из массива
procedure TFireProtectionExtender.onRemoveFromArray(pEntity: Pointer; const Drawing: TDrawingDef);
begin
  DestroyProtectionCircle(Drawing);
end;

// Поддержка старых версий
procedure TFireProtectionExtender.onEntitySupportOldVersions(pEntity: Pointer; const Drawing: TDrawingDef);
begin
  // Ничего не делаем
end;

// Реорганизация сущностей
procedure TFireProtectionExtender.ReorganizeEnts(OldEnts2NewEntsMap: TMapPointerToPointer);
begin
  // Ничего не делаем
end;

// Постзагрузка
procedure TFireProtectionExtender.PostLoad(var Context: TIODXFLoadContext);
begin
  // Ничего не делаем
end;

// Сохранение в DXF
procedure TFireProtectionExtender.SaveToDxfObjXData(var outStream: TZctnrVectorBytes; pEntity: Pointer; var IODXFContext: TIODXFSaveContext);
begin
  dxfStringout(outStream, 1000, 'FIREPROTECTIONEXTENDER=');
  dxfIntegerout(outStream, 1071, Ord(FDetectorType));
  dxfFloatout(outStream, 1040, FInstallHeight);
end;

// Загрузка из DXF
class function TFireProtectionExtender.EntIOLoadFireProtection(_Name, _Value: String; ptu: PExtensionData; const Drawing: TDrawingDef; pEntity: Pointer): Boolean;
var
  fireExtender: TFireProtectionExtender;
  detectorTypeCode: Integer;
  heightValue: Double;
begin
  Result := True;

  fireExtender := PGDBObjEntity(pEntity)^.GetExtension<TFireProtectionExtender>;
  if fireExtender = nil then
  begin
    fireExtender := AddFireProtectionExtenderToEntity(pEntity);
  end;

  // Загрузка типа извещателя
  if ptu^.xTypedData.CurrentDataType = 1071 then
  begin
    detectorTypeCode := ptu^.xTypedData.CurrentData.i;
    if (detectorTypeCode >= Ord(Low(TFireDetectorType))) and
       (detectorTypeCode <= Ord(High(TFireDetectorType))) then
      fireExtender.FDetectorType := TFireDetectorType(detectorTypeCode);
  end;

  // Загрузка высоты установки
  if ptu^.xTypedData.CurrentDataType = 1040 then
  begin
    heightValue := ptu^.xTypedData.CurrentData.d;
    fireExtender.FInstallHeight := heightValue;
  end;

  programlog.LogOutFormatStr('uzvextdrfireprotection: extender loaded from DXF', [], LM_Info);
end;

initialization
  // Регистрация расширения в системе
  EntityExtenders.RegisterKey(uppercase(FireProtectionExtenderName), TFireProtectionExtender);
  GDBObjEntity.GetDXFIOFeatures.RegisterNamedLoadFeature('FIREPROTECTIONEXTENDER', TFireProtectionExtender.EntIOLoadFireProtection);
  programlog.LogOutFormatStr('uzvextdrfireprotection: module initialized', [], LM_Info);

finalization
  programlog.LogOutFormatStr('uzvextdrfireprotection: module finalized', [], LM_Info);

end.
