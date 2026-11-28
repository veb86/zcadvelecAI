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
{$mode delphi}

{**Модуль реализации команды spaceadd для добавления пространств с расширениями}
unit uzvcommand_spaceadd;

{$INCLUDE zengineconfig.inc}

interface
uses
  SysUtils,               // String utilities / Утилиты для работы со строками
  uzcLog,
  uzccommandsabstract,
  uzccommandsimpl,
  uzcstrconsts,
  uzccommandsmanager,
  uzeentity,
  uzcdrawings,
  uzcEnitiesVariablesExtender,
  uzcExtdrIncludingVolume,
  uzccommand_rectangle,
  uzcinterface,           // Interface utilities / Утилиты интерфейса
  
  uzsbVarmanDef,              // Variable manager definitions / Определения менеджера переменных
  uzestyleslayers,
  uzeconsts,
  uzeentlwpolyline,
  uzeentpolyline,
  uzvcommand_spaceutils;  // Space utilities with shared structures / Утилиты для команд пространств


function AddExtdrToRectangle(const AStage:TEntitySetupStage;const APEnt:PGDBObjEntity;
  const AESD:TEntitySetupData):boolean;

{**Процедура применения к примитиву расширений пространства
   (extdrVariables, extdrIncludingVolume), переменных из структуры операндов,
   слоя, цвета, толщины линии «поСлою» и замыкания полилинии.
   Используется интерактивными командами spaceadd/SpaceAddPoly (стадия
   ESSSetEntity) и HTTP-командами SPACE_ADD_ROOM/SPACE_ADD_POLY, поэтому
   зона, начерченная вручную и через HTTP, получается одинаковой.
   @param(APEnt - указатель на примитив, nil игнорируется)
   @param(AStruct - разобранные операнды команды)}
procedure ApplySpaceExtensions(const APEnt:PGDBObjEntity;
  const AStruct:TOperandsStruct);

implementation

{**Процедура применения к примитиву расширений пространства}
procedure ApplySpaceExtensions(const APEnt:PGDBObjEntity;
  const AStruct:TOperandsStruct);
var
  pLayer: PGDBLayerProp;
begin
  if APEnt = nil then
    exit;

  // Добавляем расширение extdrVariables для хранения переменных
  // Add extdrVariables extension for storing variables
  AddVariablesToEntity(APEnt);

  // Добавляем расширение extdrIncludingVolume для работы с объемом
  // Add extdrIncludingVolume extension for volume operations
  AddVolumeExtenderToEntity(APEnt);

  // Добавляем переменные из структуры операндов
  // Add variables from operands structure
  AddVariablesFromStruct(APEnt, AStruct);

  // Устанавливаем слой если указан в структуре
  // Set layer if specified in structure
  if AStruct.namelayer <> '' then begin
    pLayer := GetOrCreateLayer(
      AStruct.namelayer,
      AStruct.indexColor
    );

    if pLayer <> nil then
      APEnt^.vp.Layer := pLayer;
  end;

  // Устанавливаем цвет примитива из структуры
  // Set entity color from structure
  APEnt^.vp.Color := AStruct.indexColor;
  APEnt^.vp.LineWeight := LnWtByLayer;
  if  APEnt^.GetObjType = GDBPolyLineID then
  PGDBObjPolyline(APEnt).Closed:=True;
  //if  APEnt^.GetObjType = GDBLWPolyLineID then
  //PGDBObjLWPolyline(APEnt).Closed:=True;
end;

{**Функция добавления расширений к прямоугольнику на разных стадиях настройки
   Использует глобальную структуру gOperandsStruct для получения параметров.
   @param(AStage - стадия настройки примитива)
   @param(APEnt - указатель на примитив)
   @return(true если обработка успешна)}
function AddExtdrToRectangle(const AStage:TEntitySetupStage;const APEnt:PGDBObjEntity;
  const AESD:TEntitySetupData):boolean;
begin
  case AStage of
    ESSSuppressCommandParams:
      result := false;

    ESSSetEntity: begin
      if APEnt <> nil then begin
        // Расширения, переменные, слой, цвет и замыкание из gOperandsStruct
        // Extensions, variables, layer, color and closing from gOperandsStruct
        ApplySpaceExtensions(APEnt, gOperandsStruct);

        result := true;
      end else
        result := False;
    end;
    
    ESSSetConstructEntity:
      begin
        APEnt^.vp.Color := gOperandsStruct.indexColor;
        APEnt^.vp.LineWeight := LnWt200;
        if  APEnt^.GetObjType = GDBPolyLineID then
        PGDBObjPolyline(APEnt).Closed:=True;
        //APEnt^.Closed:=True;
        result:=False;

      end;
    ESSCommandEnd: begin
      // Очищаем структуру операндов после завершения команды
      // Clear operands structure after command ends
      if gOperandsStruct.listParam <> nil then
        gOperandsStruct.listParam.Clear;
      gOperandsStruct.indexColor := 256;  // ByLayer
      gOperandsStruct.namelayer := '';
      //APEnt^.vp.LineWeight := LnWtByLayer;


      // Выделяем созданную полилинию для последующего редактирования в инспекторе
      // Select created polyline for subsequent editing in inspector
      if APEnt <> nil then begin
         zcUI.Do_GUIaction(drawings.GetCurrentDWG.wa,zcMsgUIReturnToDefaultObject);
         APEnt^.Select(drawings.GetCurrentDWG^.wa.param.SelDesc.Selectedobjcount, drawings.GetCurrentDWG^.Selector);
         zcUI.Do_GUIaction(drawings.GetCurrentDWG.wa,zcMsgUIActionSelectionChanged);
      end;

      result := False;
    end;
  end;
end;

{**Команда добавления пространства с расширениями
   Разбирает операнды в структуру и вызывает интерактивное черчение.
   @param(Context - контекст команды ZCAD)
   @param(operands - операнды команды)
   @return(результат выполнения команды)}
function SpaceAdd_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands): TCommandResult;
begin
  // Вывод сообщения о запуске команды
  // Output message about command launch
  zcUI.TextMessage('Запущена команда spaceadd', TMWOHistoryOut);

  // Инициализируем структуру операндов
  // Initialize operands structure
  gOperandsStruct.listParam := TParamInfoList.Create;  // Создаем экземпляр TVector / Create TVector instance
  gOperandsStruct.indexColor := 256;  // ByLayer
  gOperandsStruct.namelayer := '';

  // Разбираем операнды и заполняем структуру
  // Parse operands and fill structure
  ParseOperandsToStruct(operands, gOperandsStruct);

  // Вызываем интерактивное черчение прямоугольника
  // Call interactive rectangle drawing
  Result := InteractiveDrawRectangle(
    Context,
    rscmSpecifyFirstPoint,
    rscmSpecifySecondPoint,
    @AddExtdrToRectangle,
    nil
  );
end;

initialization
  programlog.LogOutFormatStr(clUInit,[{$INCLUDE %FILE%}],LM_Info,UnitsInitializeLMId);

  CreateZCADCommand(@SpaceAdd_com,'spaceadd',CADWG,0)^.CEndActionAttr:=[CEGUIRePrepare];

finalization
  ProgramLog.LogOutFormatStr(clUFin,[{$INCLUDE %FILE%}],LM_Info,UnitsFinalizeLMId);

  // Освобождаем список параметров
  // Free parameters list
  if gOperandsStruct.listParam <> nil then
    FreeAndNil(gOperandsStruct.listParam);
end.
