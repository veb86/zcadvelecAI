{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  HTTP IPC System Commands for ZCAD                                       *
*                                                                           *
*****************************************************************************
}
{
@author(HTTP IPC System Commands for ZCAD)
@author(Vladimir Bobrov)
}

{$mode objfpc}{$H+}

unit uzvhttpcmdsystem;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  uzclog,
  uzvhttpipc;

{**
  PING

  Проверка доступности HTTP IPC dispatcher.

  Ожидаемый запрос:

    {
      "cmd": "PING",
      "args": []
    }

  Ответ:

    {
      "status": "ok",
      "result": "pong"
    }
*}
function HTTPCommandPing(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;

implementation

{============================================================================}
{ HTTPCommandPing                                                             }
{============================================================================}

function HTTPCommandPing(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
begin
  AResult := '';
  AError := '';

  try
    AResult := 'pong';

    ProgramLog.LogOutFormatStr(
      'HTTP PING',
      [],
      LM_Debug,
      0
    );

    Result := True;

  except
    on E: Exception do
    begin
      AError := Format(
        'PING error: %s',
        [E.Message]
      );

      ProgramLog.LogOutFormatStr(
        'HTTP PING error: %s',
        [E.Message],
        LM_Error,
        0
      );

      Result := False;
    end;
  end;
end;


{============================================================================}
{ Initialization                                                              }
{============================================================================}

initialization

  ProgramLog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsInitializeLMId
  );

  HTTPIPCRegisterCommand(
    'PING',
    @HTTPCommandPing
  );


finalization

  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsFinalizeLMId
  );

end.