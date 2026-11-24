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
  REST-маршрут POST /api/export_selection (issue #1478).

  HTTP-поток только разбирает тело и передаёт команду EXPORT_SELECTION
  в очередь HTTP IPC; сама выгрузка выполняется главным потоком ZCAD
  (uzvhttpcmdexportselection). Ответ всегда JSON-объект
  с полями ok и command="export_selection".
}

{$mode objfpc}{$H+}

unit uzvhttpexportroute;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, fpjson,
  uzvhttpipc, uzvhttpexportselection;

const
  { HTTP-коды маршрута: ошибки выгрузки возвращаются со статусом 200 и ok=false }
  EXPORT_HTTP_OK = 200;
  EXPORT_HTTP_METHOD_NOT_ALLOWED = 405;

{ Проверяет, что URI запроса относится к маршруту /api/export_selection }
function HTTPExportSelectionIsRoute(const AURI: string): Boolean;

{ Обрабатывает запрос к /api/export_selection и формирует JSON-ответ }
procedure HTTPExportSelectionHandleREST(const AMethod, ABody: string;
  out ACode: Integer; out AContent: string);

implementation

{ Проверяет, что URI запроса относится к маршруту /api/export_selection }
function HTTPExportSelectionIsRoute(const AURI: string): Boolean;
var
  Path: string;
  QueryPos: Integer;
begin
  Path := AURI;
  QueryPos := Pos('?', Path);
  if QueryPos > 0 then
    Path := Copy(Path, 1, QueryPos - 1);
  Result := (Path = EXPORT_REST_ROUTE) or (Path = EXPORT_REST_ROUTE + '/');
end;

{ Возвращает текст ошибки таймаута с фактическим лимитом в секундах }
function TimeoutMessage: string;
begin
  Result := Format('ZCAD did not finish the export within %d s (%s)',
    [HTTPIPCCommandTimeout(EXPORT_IPC_COMMAND) div EXPORT_MS_PER_SECOND,
    ENV_EXPORT_TIMEOUT]);
end;

{ Возвращает JSON ошибки обработчика или оборачивает текст в INTERNAL_ERROR }
function FailureJSON(const AError: string): string;
begin
  if ExportErrorCodeOf(AError) <> '' then
    Exit(AError);
  if AError = '' then
    Result := ExportErrorJSON(ERR_INTERNAL_ERROR, 'Export command failed')
  else
    Result := ExportErrorJSON(ERR_INTERNAL_ERROR, AError);
end;

{ Переводит статус диспетчера HTTP IPC в JSON ответа export_selection }
function StatusToJSON(AStatus: THTTPIPCExecStatus;
  const AResult, AError: string): string;
begin
  case AStatus of
    hesOK:
      Result := AResult;
    hesFailed:
      Result := FailureJSON(AError);
    hesTimeout:
      Result := ExportErrorJSON(ERR_TIMEOUT, TimeoutMessage);
    hesUnknownCommand:
      Result := ExportErrorJSON(ERR_INTERNAL_ERROR,
        EXPORT_IPC_COMMAND + ' command is not registered');
  else
    Result := FailureJSON(AError);
  end;
end;

{ Выполняет EXPORT_SELECTION через очередь главного потока }
function ExecuteExport(const ABody: string): string;
var
  Args: TJSONArray;
  ResultText, ErrorText: string;
  Status: THTTPIPCExecStatus;
begin
  Args := ExportArgsFromBody(ABody);
  try
    Status := HTTPIPCExecuteCommand(EXPORT_IPC_COMMAND, Args, ResultText, ErrorText);
  finally
    Args.Free;
  end;
  Result := StatusToJSON(Status, ResultText, ErrorText);
end;

{ Обрабатывает запрос к /api/export_selection и формирует JSON-ответ }
procedure HTTPExportSelectionHandleREST(const AMethod, ABody: string;
  out ACode: Integer; out AContent: string);
begin
  ACode := EXPORT_HTTP_OK;
  if UpperCase(AMethod) <> 'POST' then
  begin
    ACode := EXPORT_HTTP_METHOD_NOT_ALLOWED;
    AContent := ExportErrorJSON(ERR_INVALID_REQUEST, 'Use POST ' + EXPORT_REST_ROUTE);
    Exit;
  end;
  try
    AContent := ExecuteExport(ABody);
  except
    on E: EExportSelection do
      AContent := ExportErrorJSON(E.Code, E.Message);
    on E: Exception do
      AContent := ExportErrorJSON(ERR_INTERNAL_ERROR, E.Message);
  end;
end;

end.
