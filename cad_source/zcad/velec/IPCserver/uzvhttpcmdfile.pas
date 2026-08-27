{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  HTTP IPC File Commands for ZCAD                                         *
*                                                                           *
*****************************************************************************
}
{
@author(HTTP IPC File Commands for ZCAD)
@author(Vladimir Bobrov)
}

{$mode objfpc}{$H+}

unit uzvhttpcmdfile;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  uzclog,
  uzvhttpipc;

{**
  SAVE

  Сохраняет текущий чертёж.

  args[0] может содержать имя файла, однако для обычного
  QSave оно фактически не требуется.

  Пример:

    {
      "cmd": "SAVE",
      "args": []
    }
*}
function HTTPCommandSave(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;

{**
  EXPORT

  Экспортирует текущий чертёж.

  args[0] = имя файла.

  Пример:

    {
      "cmd": "EXPORT",
      "args": [
        "C:\\temp\\drawing.dxf"
      ]
    }

  В текущей реализации поддерживается DXF.
*}
function HTTPCommandExport(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;

implementation

uses
  uzcdrawings,
  uzccommandsmanager;

{============================================================================}
{ Вспомогательные функции                                                     }
{============================================================================}

function GetStringArg(
  AArgs: TJSONArray;
  AIndex: Integer;
  const ADefault: string = ''
): string;
begin
  Result := ADefault;

  if AArgs = nil then
    Exit;

  if (AIndex < 0) or (AIndex >= AArgs.Count) then
    Exit;

  if AArgs.Items[AIndex] = nil then
    Exit;

  try
    Result := AArgs.Items[AIndex].AsString;
  except
    on E: Exception do
      Result := ADefault;
  end;
end;


{============================================================================}
{ HTTPCommandSave                                                             }
{============================================================================}

function HTTPCommandSave(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
var
  PDWG: Pointer;
  FileName: string;
begin
  AResult := '';
  AError := '';

  try
    PDWG := drawings.GetCurrentDWG;

    if PDWG = nil then
    begin
      AError := 'No current drawing';
      Result := False;
      Exit;
    end;

    {**
      Для совместимости с прежней реализацией разрешаем
      передавать имя файла, но QSave сохраняет текущий файл.
    }
    FileName := GetStringArg(AArgs, 0);

    commandmanager.ExecuteCommandSilent(
      'QSave',
      drawings.GetCurrentDWG,
      drawings.GetCurrentOGLWParam
    );

    if FileName <> '' then
      AResult := Format(
        'File saved: %s',
        [FileName]
      )
    else
      AResult := 'File saved';

    ProgramLog.LogOutFormatStr(
      'HTTP SAVE: %s',
      [AResult],
      LM_Info,
      0
    );

    Result := True;

  except
    on E: Exception do
    begin
      AError := Format(
        'Save error: %s',
        [E.Message]
      );

      ProgramLog.LogOutFormatStr(
        'HTTP SAVE error: %s',
        [E.Message],
        LM_Error,
        0
      );

      Result := False;
    end;
  end;
end;


{============================================================================}
{ HTTPCommandExport                                                           }
{============================================================================}

function HTTPCommandExport(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
var
  FileName: string;
  FileExt: string;
  Command: string;
begin
  AResult := '';
  AError := '';

  try
    FileName := GetStringArg(AArgs, 0);

    if FileName = '' then
    begin
      AError := 'No filename specified';
      Result := False;
      Exit;
    end;

    FileExt := UpperCase(
      ExtractFileExt(FileName)
    );

    if FileExt = '.DXF' then
    begin
      Command :=
        'SaveAs(' + FileName + ')';

      commandmanager.ExecuteCommandSilent(
        Command,
        drawings.GetCurrentDWG,
        drawings.GetCurrentOGLWParam
      );

      AResult := Format(
        'Exported to: %s',
        [FileName]
      );

      ProgramLog.LogOutFormatStr(
        'HTTP EXPORT: %s',
        [AResult],
        LM_Info,
        0
      );

      Result := True;
      Exit;
    end;

    AError := Format(
      'Unsupported export format: %s',
      [FileExt]
    );

    ProgramLog.LogOutFormatStr(
      'HTTP EXPORT error: unsupported format %s',
      [FileExt],
      LM_Error,
      0
    );

    Result := False;

  except
    on E: Exception do
    begin
      AError := Format(
        'Export error: %s',
        [E.Message]
      );

      ProgramLog.LogOutFormatStr(
        'HTTP EXPORT error: %s',
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
    'SAVE',
    @HTTPCommandSave
  );

  HTTPIPCRegisterCommand(
    'EXPORT',
    @HTTPCommandExport
  );


finalization

  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsFinalizeLMId
  );

end.