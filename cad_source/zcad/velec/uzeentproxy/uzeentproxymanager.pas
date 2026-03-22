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
  Модуль: uzeentproxymanager
  Назначение: Менеджер регистрации Proxy примитивов
  
  Архитектура по аналогии с uzeentityfactory.pas:
  - Каждый примитив регистрируется при инициализации модуля
  - Менеджер хранит фабрики для создания парсеров
  - Легко отключить примитив - исключить файл из проекта
}

unit uzeentproxymanager;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzeentproxytypes,
  uzeentity,
  uzegeometrytypes,
  uzedrawingdef;

type
  { Интерфейс парсера Proxy примитива }
  IProxyPrimitiveParser = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    
    { Чтение данных из потока (должен вызвать TProxyByteStream.Read...) }
    function ParseFromStream(Stream: TObject; CommandSize: Integer): Boolean;
    
    { Проверка валидности данных }
    function IsValid: Boolean;
    
    { Получение ошибки }
    function GetErrorMsg: string;
    
    { Создание сущности ZCAD для отрисовки }
    function CreateZCDEntity(const Drawing: TDrawingDef; const State: TProxyGraphicState): PGDBObjEntity;
    
    { Расширение BBox (для вычисления габаритов) }
    procedure ExpandBoundingBox(var MinPt, MaxPt: TzePoint3d);
    
    { Тип примитива }
    function GetPrimitiveType: TProxyPrimitiveType;
  end;

  { Функция создания парсера }
  TCreateProxyParserFunc = function: IProxyPrimitiveParser;

  { Информация о зарегистрированном примитиве }
  TProxyPrimitiveInfo = record
    PrimitiveType: TProxyPrimitiveType;
    DXFName: string;
    CreateParserFunc: TCreateProxyParserFunc;
  end;

  { Менеджер Proxy примитивов }
  TProxyPrimitiveManager = class
  private
    class var
      FPrimitives: array[TProxyPrimitiveType] of TProxyPrimitiveInfo;
      FInitialized: Boolean;
      
    class procedure InitializeManager;
    class function GetPrimitiveInfo(PrimitiveType: TProxyPrimitiveType): TProxyPrimitiveInfo;
    
  public
    { Регистрация примитива (вызывается в initialization модуля) }
    class procedure RegisterPrimitive(
      const PrimitiveType: TProxyPrimitiveType;
      const DXFName: string;
      const CreateParserFunc: TCreateProxyParserFunc
    );
    
    { Создание парсера для примитива }
    class function CreateParser(PrimitiveType: TProxyPrimitiveType): IProxyPrimitiveParser;
    
    { Проверка регистрации примитива }
    class function IsPrimitiveRegistered(PrimitiveType: TProxyPrimitiveType): Boolean;
    
    { Получение количества зарегистрированных примитивов }
    class function GetRegisteredCount: Integer;
    
    { Очистка (для финализации) }
    class procedure FinalizeManager;
  end;

{ Вспомогательная функция для расширения BBox }
procedure ExpandBBoxWithPoint(const Pt: TzePoint3d; var MinPt, MaxPt: TzePoint3d);

implementation

uses
  uzcLog;

{ === TProxyPrimitiveManager === }

class procedure TProxyPrimitiveManager.InitializeManager;
var
  P: TProxyPrimitiveType;
begin
  if not FInitialized then
  begin
    for P := Low(TProxyPrimitiveType) to High(TProxyPrimitiveType) do
    begin
      FPrimitives[P].PrimitiveType := P;
      FPrimitives[P].DXFName := '';
      FPrimitives[P].CreateParserFunc := nil;
    end;
    FInitialized := True;
  end;
end;

class procedure TProxyPrimitiveManager.RegisterPrimitive(
  const PrimitiveType: TProxyPrimitiveType;
  const DXFName: string;
  const CreateParserFunc: TCreateProxyParserFunc
);
begin
  InitializeManager;
  
  if PrimitiveType in [Low(TProxyPrimitiveType)..High(TProxyPrimitiveType)] then
  begin
    FPrimitives[PrimitiveType].PrimitiveType := PrimitiveType;
    FPrimitives[PrimitiveType].DXFName := DXFName;
    FPrimitives[PrimitiveType].CreateParserFunc := CreateParserFunc;
    
    programlog.LogOutFormatStr('uzeentproxymanager: Registered primitive %s (%d)', 
      [DXFName, Ord(PrimitiveType)], LM_Info);
  end
  else
  begin
    programlog.LogOutFormatStr('uzeentproxymanager: Invalid PrimitiveType %d', 
      [Ord(PrimitiveType)], LM_Error);
  end;
end;

class function TProxyPrimitiveManager.GetPrimitiveInfo(
  PrimitiveType: TProxyPrimitiveType
): TProxyPrimitiveInfo;
begin
  InitializeManager;
  
  if PrimitiveType in [Low(TProxyPrimitiveType)..High(TProxyPrimitiveType)] then
    Result := FPrimitives[PrimitiveType]
  else
  begin
    Result.PrimitiveType := pptUnknown;
    Result.DXFName := '';
    Result.CreateParserFunc := nil;
  end;
end;

class function TProxyPrimitiveManager.CreateParser(
  PrimitiveType: TProxyPrimitiveType
): IProxyPrimitiveParser;
var
  Info: TProxyPrimitiveInfo;
begin
  Result := nil;
  
  InitializeManager;
  Info := GetPrimitiveInfo(PrimitiveType);
  
  if Assigned(Info.CreateParserFunc) then
  begin
    try
      Result := Info.CreateParserFunc();
      programlog.LogOutFormatStr('uzeentproxymanager: Created parser for %s (%d)', 
        [Info.DXFName, Ord(PrimitiveType)], LM_Info);
    except
      on E: Exception do
      begin
        programlog.LogOutFormatStr('uzeentproxymanager: Failed to create parser for %s: %s', 
          [Info.DXFName, E.Message], LM_Error);
        Result := nil;
      end;
    end;
  end
  else
  begin
    programlog.LogOutFormatStr('uzeentproxymanager: No parser registered for %d', 
      [Ord(PrimitiveType)], LM_Warning);
  end;
end;

class function TProxyPrimitiveManager.IsPrimitiveRegistered(
  PrimitiveType: TProxyPrimitiveType
): Boolean;
var
  Info: TProxyPrimitiveInfo;
begin
  InitializeManager;
  Info := GetPrimitiveInfo(PrimitiveType);
  Result := Assigned(Info.CreateParserFunc);
end;

class function TProxyPrimitiveManager.GetRegisteredCount: Integer;
var
  P: TProxyPrimitiveType;
begin
  InitializeManager;
  Result := 0;
  
  for P := Low(TProxyPrimitiveType) to High(TProxyPrimitiveType) do
  begin
    if Assigned(FPrimitives[P].CreateParserFunc) then
      Inc(Result);
  end;
end;

class procedure TProxyPrimitiveManager.FinalizeManager;
var
  P: TProxyPrimitiveType;
begin
  if FInitialized then
  begin
    for P := Low(TProxyPrimitiveType) to High(TProxyPrimitiveType) do
    begin
      FPrimitives[P].CreateParserFunc := nil;
    end;
    FInitialized := False;
    
    programlog.LogOutFormatStr('uzeentproxymanager: Finalized (%d primitives unregistered)', 
      [GetRegisteredCount], LM_Info);
  end;
end;

{ === Вспомогательные функции === }

procedure ExpandBBoxWithPoint(const Pt: TzePoint3d; var MinPt, MaxPt: TzePoint3d);
begin
  if Pt.x < MinPt.x then MinPt.x := Pt.x;
  if Pt.y < MinPt.y then MinPt.y := Pt.y;
  if Pt.z < MinPt.z then MinPt.z := Pt.z;
  if Pt.x > MaxPt.x then MaxPt.x := Pt.x;
  if Pt.y > MaxPt.y then MaxPt.y := Pt.y;
  if Pt.z > MaxPt.z then MaxPt.z := Pt.z;
end;

initialization
  TProxyPrimitiveManager.InitializeManager;
  
finalization
  TProxyPrimitiveManager.FinalizeManager;
  
end.
