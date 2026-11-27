{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************
}

{
@author(HTTP Space commands for ZCAD)
@author(Vladimir Bobrov)
}

{$mode objfpc}{$H+}

{**
  HTTP-команды черчения зон (пространств):

    SPACE_ADD_ROOM  - зона помещения с номером (Space_RoomPos) и, при
                      желании, наименованием, площадью, категорией и т.д.
    SPACE_ADD_POLY  - любая зона из панели «Пространства» (room, floor,
                      section, building) или зона с произвольными операндами
                      в формате команды SpaceAddPoly.

  Оба вызова принимают один аргумент - JSON-объект (или строку с ним), см.
  SPACE_ADD_ROOM.md. Разбор и проверка запроса выполняются в чистом модуле
  uzvhttpspaceroom (покрыт тестами experiments/issue1480), а сама зона
  создаётся функцией SpaceAddPolyFromPoints модуля uzCVCommand_SpaceAddPoly,
  которая применяет те же расширения, что и интерактивная команда SpaceAddPoly.
}
unit uzvhttpcmdspace;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  uzclog,
  uzvhttpipc;

{** Регистрация HTTP-команд SPACE_ADD_ROOM и SPACE_ADD_POLY}
procedure HTTPSpaceRegisterCommands;

implementation

uses
  uzcdrawings,
  uzegeometrytypes,
  uzeentity,
  uzvhttpcmdbatch,
  uzvhttpspaceroom,
  uzCVCommand_SpaceAddPoly;


{=============================================================================
  ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
=============================================================================}

{** Перевод вершин проверенного запроса в точки ZCAD}
function ToZCADPoints(const APoints: TSpacePoints): TSpaceAddPolyPoints;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, Length(APoints));
  for i := 0 to High(APoints) do
  begin
    Result[i].x := APoints[i].X;
    Result[i].y := APoints[i].Y;
    Result[i].z := APoints[i].Z;
  end;
end;

{** Перевод значений переменных запроса в формат SpaceAddPolyFromPoints}
function ToZCADValues(const AValues: TSpaceValues): TSpaceAddPolyValues;
var
  i: Integer;
begin
  Result := nil;
  SetLength(Result, Length(AValues));
  for i := 0 to High(AValues) do
  begin
    Result[i].VarName := AValues[i].VarName;
    Result[i].Value := AValues[i].Value;
  end;
end;

{** Создание зоны в текущем чертеже по проверенному запросу.
    Любая ошибка ZCAD превращается в ESpaceRoom с кодом CREATE_FAILED}
procedure CreateSpaceZone(const ARequest: TSpaceRequest; ABatch: Boolean);
var
  LEnt: PGDBObjEntity;
begin
  if (drawings.GetCurrentDWG = nil) or (drawings.GetCurrentROOT = nil) then
    raise ESpaceRoom.CreateCode(SPACE_ERR_NO_DOCUMENT, 'No drawing is open');

  try
    // В BEGIN_BATCH зона попадает в ConstructRoot, иначе - в чертёж с Undo
    LEnt := SpaceAddPolyFromPoints(ARequest.Operands,
      ToZCADPoints(ARequest.Points), ToZCADValues(ARequest.Values),
      ABatch, ARequest.Select);
  except
    on E: Exception do
      raise ESpaceRoom.CreateCode(SPACE_ERR_CREATE_FAILED, E.Message);
  end;

  if LEnt = nil then
    raise ESpaceRoom.CreateCode(SPACE_ERR_CREATE_FAILED,
      'Zone entity was not created');
  if ABatch then
    HTTPBatchIncCount;
end;


{=============================================================================
  ОБЩИЙ ОБРАБОТЧИК
=============================================================================}

{** Выполнение SPACE_ADD_ROOM / SPACE_ADD_POLY: разбор запроса чистым
    модулем, создание зоны и формирование JSON-ответа или JSON-ошибки}
function HTTPSpaceExecute(
  const AIPCCommand: string;
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
var
  LRequest: TSpaceRequest;
  LBatch: Boolean;
begin
  AResult := '';
  AError := '';
  try
    LRequest := SpaceRequestParse(AIPCCommand, AArgs);
    LBatch := HTTPBatchMode;
    CreateSpaceZone(LRequest, LBatch);
    AResult := SpaceSuccessJSON(LRequest, LBatch);
    ProgramLog.LogOutFormatStr('HTTP %s: %s zone, %d vertices, area %s',
      [AIPCCommand, LRequest.Kind, Length(LRequest.Points),
       SpaceFloatToStr(LRequest.Geometry.Area)], LM_Info);
  except
    on E: ESpaceRoom do
      AError := SpaceErrorJSON(SpaceCommandName(AIPCCommand), E.Code,
        E.Message);
    on E: Exception do
      AError := SpaceErrorJSON(SpaceCommandName(AIPCCommand),
        SPACE_ERR_INTERNAL_ERROR, E.ClassName + ': ' + E.Message);
  end;
  if AError <> '' then
    ProgramLog.LogOutFormatStr('HTTP %s failed: %s', [AIPCCommand, AError],
      LM_Info);
  Result := AError = '';
end;


{=============================================================================
  SPACE_ADD_ROOM
=============================================================================}

{** Обработчик HTTP-команды SPACE_ADD_ROOM}
function HTTPCommandSpaceAddRoom(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
begin
  Result := HTTPSpaceExecute(SPACE_IPC_ADD_ROOM, AArgs, AResult, AError);
end;


{=============================================================================
  SPACE_ADD_POLY
=============================================================================}

{** Обработчик HTTP-команды SPACE_ADD_POLY}
function HTTPCommandSpaceAddPoly(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
begin
  Result := HTTPSpaceExecute(SPACE_IPC_ADD_POLY, AArgs, AResult, AError);
end;


{=============================================================================
  РЕГИСТРАЦИЯ КОМАНД
=============================================================================}

procedure HTTPSpaceRegisterCommands;
begin
  RegisterHTTPIPCCommand(
    SPACE_IPC_ADD_ROOM,
    @HTTPCommandSpaceAddRoom
  );

  RegisterHTTPIPCCommand(
    SPACE_IPC_ADD_POLY,
    @HTTPCommandSpaceAddPoly
  );
end;


{=============================================================================
  INITIALIZATION
=============================================================================}

initialization

  ProgramLog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsInitializeLMId
  );

  HTTPSpaceRegisterCommands;


{=============================================================================
  FINALIZATION
=============================================================================}

finalization

  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsFinalizeLMId
  );

end.
