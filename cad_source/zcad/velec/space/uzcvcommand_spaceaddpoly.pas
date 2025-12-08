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
@author(Andrey Zubarev <zamtmn@yandex.ru>) 
}
{$MODE OBJFPC}{$H+}
unit uzCVCommand_SpaceAddPoly;
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzccommandsabstract,
  uzccommandsimpl,
  uzcLog,
  uzcinterface,
  uzeentity,
  uzcEnitiesVariablesExtender,
  uzcExtdrIncludingVolume,
  uzestyleslayers,
  uzeconsts,
  uzbtypes,
  uzvcommand_spaceadd,
  uzvcommand_spaceutils,
  uzccommand_3dpoly;

var
  // Структура уровня модуля для хранения разобранных операндов команды
  // Module-level structure to store parsed command operands
  gOperandsStruct: TOperandsStruct;

implementation

{**Функция-обёртка для добавления расширений к полилинии
   Использует локальную структуру gOperandsStruct для получения параметров.
   @param(AStage - стадия настройки примитива)
   @param(APEnt - указатель на примитив)
   @return(true если обработка успешна)}
function AddExtdrToPolyline(
  const AStage: TEntitySetupStage;
  const APEnt: PGDBObjEntity): boolean;
var
  pLayer: PGDBLayerProp;
begin
  case AStage of
    ESSSuppressCommandParams:
      result := false;

    ESSSetEntity: begin
      if APEnt <> nil then begin
        // Добавляем расширение extdrVariables для хранения переменных
        // Add extdrVariables extension for storing variables
        AddVariablesToEntity(APEnt);

        // Добавляем расширение extdrIncludingVolume для работы с объемом
        // Add extdrIncludingVolume extension for volume operations
        AddVolumeExtenderToEntity(APEnt);

        // Добавляем переменные из структуры операндов
        // Add variables from operands structure
        AddVariablesFromStruct(APEnt, gOperandsStruct);

        // Устанавливаем слой если указан в структуре
        // Set layer if specified in structure
        if gOperandsStruct.namelayer <> '' then begin
          pLayer := GetOrCreateLayer(
            gOperandsStruct.namelayer,
            gOperandsStruct.indexColor
          );

          if pLayer <> nil then
            APEnt^.vp.Layer := pLayer;
        end;

        // Устанавливаем цвет примитива из структуры
        // Set entity color from structure
        APEnt^.vp.Color := gOperandsStruct.indexColor;
        APEnt^.vp.LineWeight := LnWtByLayer;

        result := true;
      end else
        result := False;
    end;

    ESSSetConstructEntity:
      begin
        APEnt^.vp.Color := gOperandsStruct.indexColor;
        APEnt^.vp.LineWeight := LnWt200;
        result:=False;
      end;

    ESSCommandEnd: begin
      // Очищаем структуру операндов после завершения команды
      // Clear operands structure after command ends
      if gOperandsStruct.listParam <> nil then
        gOperandsStruct.listParam.Clear;
      gOperandsStruct.indexColor := 256;  // ByLayer
      gOperandsStruct.namelayer := '';

      result := False;
    end;
  end;
end;

function _SpaceAddPoly_com_CommandStart(const Context:TZCADCommandContext;
  operands:TCommandOperands):TCommandResult;
begin
  // Вывод сообщения о запуске команды
  // Output message about command launch
  zcUI.TextMessage('запущена команда spaceaddpoly', TMWOHistoryOut);

  // Разбираем операнды и заполняем структуру
  // Parse operands and fill structure
  ParseOperandsToStruct(operands, gOperandsStruct);

  result:=_3DPoly_com_CommandStart(Context,operands);
  p3dplESP:=@AddExtdrToPolyline;
  if assigned(p3dplESP) then
    p3dplESP(ESSSuppressCommandParams,nil);
end;


procedure startup;
begin
  CreateCommandRTEdObjectPlugin(@_SpaceAddPoly_com_CommandStart,@_3DPoly_com_CommandEnd,
    @_3DPoly_com_CommandEnd,nil,@_3DPoly_com_BeforeClick,@_3DPoly_com_AfterClick,
  nil,nil,'SpaceAddPoly',0,0);
end;

procedure Finalize;
begin
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization',[{$INCLUDE %FILE%}],
    LM_Info,UnitsInitializeLMId);

  // Инициализируем структуру операндов
  // Initialize operands structure
  gOperandsStruct.listParam := TParamInfoList.Create;
  gOperandsStruct.indexColor := 256;  // ByLayer
  gOperandsStruct.namelayer := '';

  startup;

finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization',[{$INCLUDE %FILE%}],
    LM_Info,UnitsFinalizeLMId);

  // Освобождаем список параметров
  // Free parameters list
  if gOperandsStruct.listParam <> nil then
    FreeAndNil(gOperandsStruct.listParam);

  finalize;
end.
