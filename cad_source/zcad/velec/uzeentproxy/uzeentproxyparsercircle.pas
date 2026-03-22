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
  Модуль: uzeentproxyparsercircle
  Назначение: Парсер круга из Proxy Graphic (OPCODE=2)
  
  Формат данных (AcGiWorldDraw):
  - Center (3 doubles) - центр круга
  - Radius (1 double) - радиус
  - Normal (3 doubles) - нормаль (ось Z локальной СК)
  
  На основе анализа ezdxf/proxygraphic.py и AutoCAD DevBlog
}

unit uzeentproxyparsercircle;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzeentproxytypes,
  uzeentproxymanager,
  uzeentproxybaseparser,
  uzeentity,
  uzedrawingdef,
  uzeTypes,
  uzestyleslayers,
  uzeGeometryTypes,
  uzegeometry;

type
  { Парсер круга (OPCODE=2) }
  TProxyCircleParser = class(TProxyBaseParser)
  private
    FCenter: TzePoint3d;
    FRadius: Double;
    FNormal: TzePoint3d;
    
  protected
    { Чтение данных из потока }
    function DoParseFromStream(Stream: TObject; CommandSize: Integer): Boolean; override;
    
    { Создание сущности ZCAD }
    function DoCreateZCDEntity(const Drawing: TDrawingDef; const State: TProxyGraphicState): PGDBObjEntity; override;
    
    { Расширение BBox }
    procedure DoExpandBoundingBox(var MinPt, MaxPt: TzePoint3d); override;
    
  public
    constructor Create;
    function GetPrimitiveType: TProxyPrimitiveType; override;
  end;

{ Функция создания для регистрации в менеджере }
function CreateCircleParser: IProxyPrimitiveParser;

implementation

uses
  uzcLog,
  uzeentcircle,
  uzeconsts,
  uzeentproxyparser;

constructor TProxyCircleParser.Create;
begin
  inherited Create;
  FPrimitiveType := pptCircle;
  FValid := False;
  FErrorMsg := '';
end;

function TProxyCircleParser.GetPrimitiveType: TProxyPrimitiveType;
begin
  Result := pptCircle;
end;

function TProxyCircleParser.DoParseFromStream(Stream: TObject; CommandSize: Integer): Boolean;
var
  Center: TzePoint3d;
  Radius: Double;
  Normal: TzePoint3d;
  StartPos: Integer;
begin
  Result := False;
  
  try
    StartPos := TProxyByteStream(Stream).Index;
    programlog.LogOutFormatStr('uzeentproxyparsercircle: DoParseFromStream START - Index=%d, CommandSize=%d', [StartPos, CommandSize], LM_Info);
    
    // Приводим TObject к TProxyByteStream
    // Формат: Center (3d) + Radius (d) + Normal (3d)
    Center := TProxyByteStream(Stream).ReadVertex;
    programlog.LogOutFormatStr('uzeentproxyparsercircle: Read Center = (%.6f, %.6f, %.6f)', [Center.x, Center.y, Center.z], LM_Info);
    
    Radius := TProxyByteStream(Stream).ReadDouble;
    programlog.LogOutFormatStr('uzeentproxyparsercircle: Read Radius = %.6f', [Radius], LM_Info);
    
    Normal := TProxyByteStream(Stream).ReadVector;
    programlog.LogOutFormatStr('uzeentproxyparsercircle: Read Normal = (%.6f, %.6f, %.6f)', [Normal.x, Normal.y, Normal.z], LM_Info);
    
    programlog.LogOutFormatStr('uzeentproxyparsercircle: Bytes read: %d', [TProxyByteStream(Stream).Index - StartPos], LM_Info);
    
    // Преобразуем в OCS если нормаль не совпадает с Z
    if not VectorIsClose(Normal, PROXY_Z_AXIS, 1e-9) then
      FCenter := TransformToOCS(Center, Normal)
    else
      FCenter := Center;
      
    FRadius := Radius;
    FNormal := Normal;
    FValid := True;
    Result := True;
    
    programlog.LogOutFormatStr('uzeentproxyparsercircle: Parsed CIRCLE center=(%.3f,%.3f,%.3f) radius=%.3f', 
      [FCenter.x, FCenter.y, FCenter.z, FRadius], LM_Info);
  except
    on E: Exception do
    begin
      FValid := False;
      FErrorMsg := 'Circle parse error: ' + E.Message;
      Result := False;
      
      programlog.LogOutFormatStr('uzeentproxyparsercircle: Parse error: %s', [E.Message], LM_Error);
    end;
  end;
end;

function TProxyCircleParser.DoCreateZCDEntity(const Drawing: TDrawingDef; const State: TProxyGraphicState): PGDBObjEntity;
var
  CircleObj: PGDBObjCircle;
  LayerProp: PGDBLayerProp;
begin
  try
    CircleObj := GDBObjCircle.CreateInstance;
    CircleObj^.initnul;
    
    { Копируем свойства из состояния }
    if State.Color >= 0 then
      CircleObj^.vp.Color := State.Color;
    
    { Получаем слой из drawing по имени }
    if State.Layer <> '' then
      LayerProp := Drawing.GetLayerTable.getaddres(State.Layer)
    else
      LayerProp := Drawing.GetLayerTable.getaddres('0');
    
    if LayerProp <> nil then
      CircleObj^.vp.Layer := LayerProp;
    
    { Устанавливаем геометрию }
    CircleObj^.Local.p_insert := FCenter;
    CircleObj^.Radius := FRadius;
    
    { Вычисляем матрицу для отрисовки }
    CircleObj^.CalcObjMatrix(@Drawing);
    
    Result := CircleObj;
    
    programlog.LogOutFormatStr('uzeentproxyparsercircle: Created ZCAD circle entity', [], LM_Info);
  except
    on E: Exception do
    begin
      programlog.LogOutFormatStr('uzeentproxyparsercircle: Failed to create circle entity: %s', [E.Message], LM_Error);
      Result := nil;
    end;
  end;
end;

procedure TProxyCircleParser.DoExpandBoundingBox(var MinPt, MaxPt: TzePoint3d);
begin
  if not FValid then
    Exit;
    
  // Круг: центр ± радиус в плоскости XY
  ExpandBBoxWithPoint(CreateVertex(FCenter.x - FRadius, FCenter.y - FRadius, FCenter.z), MinPt, MaxPt);
  ExpandBBoxWithPoint(CreateVertex(FCenter.x + FRadius, FCenter.y + FRadius, FCenter.z), MinPt, MaxPt);
end;

{ Функция создания для регистрации }
function CreateCircleParser: IProxyPrimitiveParser;
begin
  Result := TProxyCircleParser.Create;
end;

initialization
  { Регистрация парсера при загрузке модуля }
  TProxyPrimitiveManager.RegisterPrimitive(pptCircle, 'CIRCLE', @CreateCircleParser);
  
finalization
  { Модуль выгружается }
  
end.
