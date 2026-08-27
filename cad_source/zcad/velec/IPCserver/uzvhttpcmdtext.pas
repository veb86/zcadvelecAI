{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  HTTP IPC Text Commands for ZCAD                                        *
*                                                                           *
*****************************************************************************
}
{
@author(HTTP IPC Text Commands for ZCAD)
@author(Vladimir Bobrov)
}

{$mode objfpc}{$H+}

unit uzvhttpcmdtext;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  uzclog,
  uzvhttpipc;

{**
  TEXT

  Создаёт текстовый примитив.

  Формат:

    {
      "cmd": "TEXT",
      "args": [
        x,
        y,
        "text",
        height
      ]
    }

  height необязателен.
  По умолчанию height = 2.5.
*}
function HTTPCommandText(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;

implementation

uses
  uzcdrawings,
  uzeTypes,
  uzeentity,
  uzeenttext,
  uzcutils,
  uzcgeometry,
  uzccommandsmanager,
  uzeffdxf;

{============================================================================}
{ Вспомогательные функции                                                     }
{============================================================================}

function GetFloatArg(
  AArgs: TJSONArray;
  AIndex: Integer;
  ADefault: Double
): Double;
begin
  Result := ADefault;

  if AArgs = nil then
    Exit;

  if (AIndex < 0) or (AIndex >= AArgs.Count) then
    Exit;

  if AArgs.Items[AIndex] = nil then
    Exit;

  try
    Result := AArgs.Items[AIndex].AsFloat;
  except
    on E: Exception do
      Result := ADefault;
  end;
end;


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
{ HTTPCommandText                                                             }
{============================================================================}

function HTTPCommandText(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
var
  PText: PGDBObjText;
  InsertPoint: TzePoint3d;
  TextContent: string;
  Height: Double;
  X: Double;
  Y: Double;
begin
  AResult := '';
  AError := '';

  {==========================================================================
    Проверка аргументов
  ==========================================================================}

  if AArgs = nil then
  begin
    AError :=
      'TEXT requires arguments: x y text [height]';

    Result := False;
    Exit;
  end;

  if AArgs.Count < 3 then
  begin
    AError :=
      'TEXT requires at least 3 arguments: x y text [height]';

    Result := False;
    Exit;
  end;

  {==========================================================================
    Читаем параметры
  ==========================================================================}

  try

    X := GetFloatArg(AArgs, 0, 0);
    Y := GetFloatArg(AArgs, 1, 0);

    TextContent := GetStringArg(
      AArgs,
      2,
      ''
    );

    Height := GetFloatArg(
      AArgs,
      3,
      2.5
    );

    if TextContent = '' then
    begin
      AError := 'TEXT content cannot be empty';
      Result := False;
      Exit;
    end;

    if Height <= 0 then
    begin
      AError := 'TEXT height must be greater than zero';
      Result := False;
      Exit;
    end;

    {========================================================================
      Формируем точку вставки.
    =========================================================================}

    InsertPoint.x := X;
    InsertPoint.y := Y;
    InsertPoint.z := 0;

    {========================================================================
      Создаём текстовый примитив.
    =========================================================================}

    PText := GDBObjText.CreateInstance;

    if PText = nil then
    begin
      AError := 'Failed to create TEXT entity';
      Result := False;
      Exit;
    end;

    zcSetEntPropFromCurrentDrawingProp(PText);

    PText^.TXTStyle :=
      drawings.GetCurrentDWG^.GetCurrentTextStyle;

    PText^.Local.P_insert :=
      InsertPoint;

    PText^.Template :=
      TDXFEntsInternalStringType(TextContent);

    PText^.obj_height :=
      Height;

    {========================================================================
      Добавляем объект в чертёж одним Undo-действием.
    =========================================================================}

    zcAddEntToCurrentDrawingWithUndo(PText);

    {========================================================================
      Перерисовываем чертёж.
    =========================================================================}

    zcRedrawCurrentDrawing;

    AResult := Format(
      'Text created: "%s" at (%.2f,%.2f)',
      [
        TextContent,
        X,
        Y
      ]
    );

    ProgramLog.LogOutFormatStr(
      'HTTP TEXT: %s',
      [AResult],
      LM_Info,
      0
    );

    Result := True;

  except
    on E: Exception do
    begin
      AError := Format(
        'Failed to create text: %s',
        [E.Message]
      );

      ProgramLog.LogOutFormatStr(
        'HTTP TEXT error: %s',
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
    'TEXT',
    @HTTPCommandText
  );


finalization

  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsFinalizeLMId
  );

end.