{ Печать примеров ответов SPACE_ADD_ROOM / SPACE_ADD_POLY без ZCAD.
  Используется для документации SPACE_ADD_ROOM.md (issue #1480). }
program print_sample;

{$mode objfpc}{$H+}

uses
  SysUtils, fpjson, jsonparser, uzvhttpspaceroom;

{ Разбор запроса и печать JSON-ответа (успех или ошибка) }
procedure PrintReply(const ACommand, ABody: string);
var
  Args: TJSONArray;
  Request: TSpaceRequest;
begin
  Args := TJSONArray.Create;
  try
    Args.Add(GetJSON(ABody));
    WriteLn(ACommand, ' ', ABody);
    try
      Request := SpaceRequestParse(ACommand, Args);
      WriteLn('  -> ', SpaceSuccessJSON(Request, False));
    except
      on E: ESpaceRoom do
        WriteLn('  -> ', SpaceErrorJSON(SpaceCommandName(ACommand), E.Code, E.Message));
    end;
  finally
    Args.Free;
  end;
end;

begin
  // Как в ZCAD: строки AnsiString в UTF-8
  SetMultiByteConversionCodePage(CP_UTF8);
  DefaultSystemCodePage := CP_UTF8;
  PrintReply(SPACE_IPC_ADD_ROOM,
    '{"points":[[0,0],[6000,0],[6000,4000],[0,4000]],"number":"101",' +
    '"name":"Кабинет","category":"Д","illumination":300}');
  PrintReply(SPACE_IPC_ADD_POLY,
    '{"kind":"floor","points":[[0,0],[30000,0],[30000,12000],[0,12000]],' +
    '"number":"1","height":3.3}');
  PrintReply(SPACE_IPC_ADD_ROOM,
    '{"points":[[0,0],[10,10],[10,0],[0,10]],"number":"1"}');
  PrintReply(SPACE_IPC_ADD_ROOM, '{"points":[[0,0],[10,0],[10,10]]}');
end.
