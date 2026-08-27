{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************
}

{
@author(HTTP Geometry commands for ZCAD)
@author(Vladimir Bobrov)
}

{$mode objfpc}{$H+}

unit uzvhttpcmdgeometry;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  uzclog,
  uzbLogTypes,
  uzvhttpipc;

{**
  Регистрация HTTP-команд геометрии:

    LINE
    CIRCLE
    ARC
    POLYLINE
}
procedure HTTPGeometryRegisterCommands;

implementation

uses
  uzcdrawings,
  uzcinterface,
  uzeTypes,
  uzeentline,
  uzeentity,
  uzeentityfactory,
  uzegeometrytypes,
  uzegeometry,
  uzeconsts,
  uzcutils,
  uzeentcircle,
  uzvhttpcmdbatch;


{=============================================================================
  ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
=============================================================================}

function GetFloatArg(
  AArgs: TJSONArray;
  AIndex: Integer;
  ADefault: Double = 0
): Double;
begin

  if
    (AArgs <> nil) and
    (AIndex >= 0) and
    (AIndex < AArgs.Count)
  then
    Result := AArgs.Items[AIndex].AsFloat
  else
    Result := ADefault;

end;


{=============================================================================
  LINE
=============================================================================}

function HTTPCommandLine(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
var
  PLine: PGDBObjLine;
  P1, P2: TzePoint3d;
begin

  Result := False;

  AResult := '';
  AError := '';


  {---------------------------------------------------------------------------
    Проверка аргументов

      LINE x1 y1 x2 y2
  ---------------------------------------------------------------------------}

  if
    (AArgs = nil) or
    (AArgs.Count < 4)
  then
  begin

    AError :=
      'LINE requires 4 arguments: x1 y1 x2 y2';

    Exit;

  end;


  {---------------------------------------------------------------------------
    Первая точка
  ---------------------------------------------------------------------------}

  P1.x :=
    GetFloatArg(
      AArgs,
      0
    );

  P1.y :=
    GetFloatArg(
      AArgs,
      1
    );

  P1.z := 0;


  {---------------------------------------------------------------------------
    Вторая точка
  ---------------------------------------------------------------------------}

  P2.x :=
    GetFloatArg(
      AArgs,
      2
    );

  P2.y :=
    GetFloatArg(
      AArgs,
      3
    );

  P2.z := 0;


  try

    {-----------------------------------------------------------------------
      Создаём LINE
    -----------------------------------------------------------------------}

    PLine :=
      AllocEnt(
        GDBLineID
      );


    if PLine = nil then
    begin

      AError :=
        'Failed to allocate LINE entity';

      Exit;

    end;


    PLine^.init(
      nil,
      nil,
      LnWtByLayer,
      P1,
      P2
    );


    {-----------------------------------------------------------------------
      Наследуем свойства текущего чертежа
    -----------------------------------------------------------------------}

    zcSetEntPropFromCurrentDrawingProp(
      PLine
    );


    {-----------------------------------------------------------------------
      Если активен BEGIN_BATCH:

        ConstructRoot

      иначе:

        обычный drawing + Undo + Redraw
    -----------------------------------------------------------------------}

    if HTTPBatchMode then
    begin

      zcAddEntToCurrentDrawingConstructRoot(
        PLine
      );


      HTTPBatchIncCount;


      AResult :=
        Format(
          'Line queued: (%.2f,%.2f)-(%.2f,%.2f)',
          [
            P1.x,
            P1.y,
            P2.x,
            P2.y
          ]
        );

    end
    else
    begin

      zcAddEntToCurrentDrawingWithUndo(
        PLine
      );


      zcRedrawCurrentDrawing;


      AResult :=
        Format(
          'Line created: (%.2f,%.2f)-(%.2f,%.2f)',
          [
            P1.x,
            P1.y,
            P2.x,
            P2.y
          ]
        );

    end;


    Result := True;


    ProgramLog.LogOutFormatStr(
      'HTTP LINE: %s',
      [AResult],
      LM_Debug,
      0
    );


  except

    on E: Exception do
    begin

      AError :=
        Format(
          'Failed to create line: %s',
          [E.Message]
        );


      ProgramLog.LogOutFormatStr(
        'HTTP LINE error: %s',
        [E.Message],
        LM_Error,
        0
      );

    end;

  end;

end;


{=============================================================================
  CIRCLE
=============================================================================}

function HTTPCommandCircle(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
var
  PCircle: PGDBObjCircle;
  Center: TzePoint3d;
  Radius: Double;
begin

  Result := False;

  AResult := '';
  AError := '';


  {---------------------------------------------------------------------------
    Проверка аргументов

      CIRCLE x y radius
  ---------------------------------------------------------------------------}

  if
    (AArgs = nil) or
    (AArgs.Count < 3)
  then
  begin

    AError :=
      'CIRCLE requires 3 arguments: x y radius';

    Exit;

  end;


  {---------------------------------------------------------------------------
    Центр
  ---------------------------------------------------------------------------}

  Center.x :=
    GetFloatArg(
      AArgs,
      0
    );

  Center.y :=
    GetFloatArg(
      AArgs,
      1
    );

  Center.z := 0;


  {---------------------------------------------------------------------------
    Радиус
  ---------------------------------------------------------------------------}

  Radius :=
    GetFloatArg(
      AArgs,
      2
    );


  if Radius <= 0 then
  begin

    AError :=
      'CIRCLE radius must be greater than zero';

    Exit;

  end;


  try

    {-----------------------------------------------------------------------
      Создаём CIRCLE
    -----------------------------------------------------------------------}

    PCircle :=
      AllocEnt(
        GDBCircleID
      );


    if PCircle = nil then
    begin

      AError :=
        'Failed to allocate CIRCLE entity';

      Exit;

    end;


    PCircle^.init(
      nil,
      nil,
      LnWtByLayer,
      Center,
      Radius
    );


    {-----------------------------------------------------------------------
      Наследуем свойства текущего чертежа
    -----------------------------------------------------------------------}

    zcSetEntPropFromCurrentDrawingProp(
      PCircle
    );


    {-----------------------------------------------------------------------
      Batch / обычный режим
    -----------------------------------------------------------------------}

    if HTTPBatchMode then
    begin

      zcAddEntToCurrentDrawingConstructRoot(
        PCircle
      );


      HTTPBatchIncCount;


      AResult :=
        Format(
          'Circle queued: center (%.2f,%.2f), radius %.2f',
          [
            Center.x,
            Center.y,
            Radius
          ]
        );

    end
    else
    begin

      zcAddEntToCurrentDrawingWithUndo(
        PCircle
      );


      zcRedrawCurrentDrawing;


      AResult :=
        Format(
          'Circle created: center (%.2f,%.2f), radius %.2f',
          [
            Center.x,
            Center.y,
            Radius
          ]
        );

    end;


    Result := True;


    ProgramLog.LogOutFormatStr(
      'HTTP CIRCLE: %s',
      [AResult],
      LM_Debug,
      0
    );


  except

    on E: Exception do
    begin

      AError :=
        Format(
          'Failed to create circle: %s',
          [E.Message]
        );


      ProgramLog.LogOutFormatStr(
        'HTTP CIRCLE error: %s',
        [E.Message],
        LM_Error,
        0
      );

    end;

  end;

end;


{=============================================================================
  ARC
=============================================================================}

function HTTPCommandArc(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
begin

  Result := False;

  AResult := '';

  AError :=
    'ARC command is not implemented yet';


  ProgramLog.LogOutFormatStr(
    'HTTP ARC: command not implemented',
    [],
    LM_Debug,
    0
  );

end;


{=============================================================================
  POLYLINE
=============================================================================}

function HTTPCommandPolyline(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
begin

  Result := False;

  AResult := '';

  AError :=
    'POLYLINE command is not implemented yet';


  ProgramLog.LogOutFormatStr(
    'HTTP POLYLINE: command not implemented',
    [],
    LM_Debug,
    0
  );

end;


{=============================================================================
  РЕГИСТРАЦИЯ КОМАНД
=============================================================================}

procedure HTTPGeometryRegisterCommands;
begin

  { LINE }

  RegisterHTTPIPCCommand(
    'LINE',
    @HTTPCommandLine
  );


  { CIRCLE }

  RegisterHTTPIPCCommand(
    'CIRCLE',
    @HTTPCommandCircle
  );


  { ARC }

  RegisterHTTPIPCCommand(
    'ARC',
    @HTTPCommandArc
  );


  { POLYLINE }

  RegisterHTTPIPCCommand(
    'POLYLINE',
    @HTTPCommandPolyline
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


  HTTPGeometryRegisterCommands;


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
