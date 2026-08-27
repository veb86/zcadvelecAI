{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************
}

{
@author(HTTP IPC Batch Commands for ZCAD)
@author(Vladimir Bobrov)

HTTP commands:

  BEGIN_BATCH
  END_BATCH
  BATCH_LINES

Batch state хранится непосредственно здесь.

Этот unit НЕ зависит от:

  uzvipcserver
  uzvipcintegration
  PIPCCommand
  TIPCCommandType
  IPCCommandQueue
}

{$mode objfpc}{$H+}

unit uzvhttpcmdbatch;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  uzclog,
  uzbLogTypes,
  uzegeometrytypes,
  uzeentline,
  uzeconsts,
  uzvhttpipc;

function HTTPCommandBeginBatch(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;

function HTTPCommandEndBatch(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;

function HTTPCommandBatchLines(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;

function HTTPBatchMode: Boolean;

procedure HTTPBatchIncCount;

implementation

uses
  uzcdrawings,
  uzeTypes,
  uzeentityfactory,
  uzcutils;


var
  FBatchMode: Boolean;
  FBatchCount: Integer;


  procedure HTTPBatchIncCount;
begin
  if FBatchMode then
    Inc(FBatchCount);
end;

{=============================================================================
  ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
=============================================================================}

function GetFloatFromJSON(
  AArray: TJSONArray;
  AIndex: Integer;
  out AValue: Double
): Boolean;
begin
  Result := False;
  AValue := 0;

  if AArray = nil then
    Exit;

  if
    (AIndex < 0) or
    (AIndex >= AArray.Count)
  then
    Exit;

  if AArray.Items[AIndex] = nil then
    Exit;

  try

    AValue :=
      AArray.Items[AIndex].AsFloat;

    Result := True;

  except

    on E: Exception do
    begin
      Result := False;
    end;

  end;

end;


function JSONItemIsArray(
  AJSON: TJSONData
): Boolean;
begin

  Result :=
    (AJSON <> nil) and
    (AJSON.JSONType = jtArray);

end;


{=============================================================================
  BEGIN_BATCH
=============================================================================}

function HTTPCommandBeginBatch(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
begin

  AResult := '';
  AError := '';


  try

    if FBatchMode then
    begin

      AError :=
        'Batch mode is already active';

      Result := False;
      Exit;

    end;


    FBatchMode := True;
    FBatchCount := 0;


    AResult :=
      'Batch mode started';


    ProgramLog.LogOutFormatStr(
      'HTTP BEGIN_BATCH: batch mode started',
      [],
      LM_Info,
      0
    );


    Result := True;


  except

    on E: Exception do
    begin

      AError :=
        Format(
          'BEGIN_BATCH error: %s',
          [E.Message]
        );


      ProgramLog.LogOutFormatStr(
        'HTTP BEGIN_BATCH error: %s',
        [E.Message],
        LM_Error,
        0
      );


      Result := False;

    end;

  end;

end;


{=============================================================================
  END_BATCH
=============================================================================}

function HTTPCommandEndBatch(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
var
  BatchCount: Integer;
begin

  AResult := '';
  AError := '';

  Result := False;


  if not FBatchMode then
  begin

    AError :=
      'END_BATCH called without BEGIN_BATCH';

    Exit;

  end;


  BatchCount :=
    FBatchCount;


  try

    {-----------------------------------------------------------------------
      Переносим все объекты из ConstructRoot в текущий чертёж.

      Это одно Undo-действие.
    -----------------------------------------------------------------------}

    zcMoveEntsFromConstructRootToCurrentDrawingWithUndo(
      'HTTP_BATCH_IMPORT'
    );


    {-----------------------------------------------------------------------
      Одна перерисовка после всего batch.
    -----------------------------------------------------------------------}

    zcRedrawCurrentDrawing;


    AResult :=
      Format(
        'Batch mode completed: %d primitives imported',
        [BatchCount]
      );


    ProgramLog.LogOutFormatStr(
      'HTTP END_BATCH: batch completed: %d primitives',
      [BatchCount],
      LM_Info,
      0
    );


    Result := True;


  except

    on E: Exception do
    begin

      AError :=
        Format(
          'Batch commit error: %s',
          [E.Message]
        );


      ProgramLog.LogOutFormatStr(
        'HTTP END_BATCH error: %s',
        [E.Message],
        LM_Error,
        0
      );


      Result := False;

    end;

  end;


  {-----------------------------------------------------------------------
    В любом случае завершаем batch-состояние.
  -----------------------------------------------------------------------}

  FBatchMode := False;
  FBatchCount := 0;

end;


{=============================================================================
  BATCH_LINES
=============================================================================}

function HTTPCommandBatchLines(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;
var
  Lines: TJSONArray;
  LineData: TJSONArray;

  PLine: PGDBObjLine;

  P1, P2: TzePoint3d;

  X1, Y1, X2, Y2: Double;

  I: Integer;
  CreatedCount: Integer;

begin

  AResult := '';
  AError := '';

  CreatedCount := 0;

  Result := False;


  {=========================================================================
    Проверка аргументов
  =========================================================================}

  if AArgs = nil then
  begin

    AError :=
      'BATCH_LINES requires an array of lines';

    Exit;

  end;


  if AArgs.Count < 1 then
  begin

    AError :=
      'BATCH_LINES requires an array of lines';

    Exit;

  end;


  {=========================================================================
    Args[0] должен быть массивом
  =========================================================================}

  if not JSONItemIsArray(
    AArgs.Items[0]
  ) then
  begin

    AError :=
      'BATCH_LINES: args[0] must be an array of lines';

    Exit;

  end;


  Lines :=
    TJSONArray(
      AArgs.Items[0]
    );


  if Lines.Count = 0 then
  begin

    AError :=
      'BATCH_LINES: empty lines array';

    Exit;

  end;


  {=========================================================================
    BEGIN_BATCH должен быть вызван заранее
  =========================================================================}

  if not FBatchMode then
  begin

    AError :=
      'BATCH_LINES called without BEGIN_BATCH';

    Exit;

  end;


  {=========================================================================
    Создание линий
  =========================================================================}

  try

    for I := 0 to Lines.Count - 1 do
    begin

      {=====================================================================
        Проверяем, что элемент действительно массив.

        Именно здесь раньше возникала проблема:

          LineData := Lines.Items[I] as TJSONArray;

        Теперь приведение выполняется только после проверки.
      =====================================================================}

      if not JSONItemIsArray(
        Lines.Items[I]
      ) then
      begin

        raise Exception.CreateFmt(
          'BATCH_LINES: item %d must be an array [x1,y1,x2,y2]',
          [I]
        );

      end;


      LineData :=
        TJSONArray(
          Lines.Items[I]
        );


      {=====================================================================
        Проверяем количество координат
      =====================================================================}

      if LineData.Count < 4 then
      begin

        raise Exception.CreateFmt(
          'BATCH_LINES: line %d requires 4 coordinates [x1,y1,x2,y2]',
          [I]
        );

      end;


      {=====================================================================
        Читаем координаты
      =====================================================================}

      if not GetFloatFromJSON(
        LineData,
        0,
        X1
      ) then
      begin

        raise Exception.CreateFmt(
          'BATCH_LINES: invalid x1 at line %d',
          [I]
        );

      end;


      if not GetFloatFromJSON(
        LineData,
        1,
        Y1
      ) then
      begin

        raise Exception.CreateFmt(
          'BATCH_LINES: invalid y1 at line %d',
          [I]
        );

      end;


      if not GetFloatFromJSON(
        LineData,
        2,
        X2
      ) then
      begin

        raise Exception.CreateFmt(
          'BATCH_LINES: invalid x2 at line %d',
          [I]
        );

      end;


      if not GetFloatFromJSON(
        LineData,
        3,
        Y2
      ) then
      begin

        raise Exception.CreateFmt(
          'BATCH_LINES: invalid y2 at line %d',
          [I]
        );

      end;


      {=====================================================================
        Формируем точки
      =====================================================================}

      P1.x := X1;
      P1.y := Y1;
      P1.z := 0;


      P2.x := X2;
      P2.y := Y2;
      P2.z := 0;


      {=====================================================================
        Создаём LINE
      =====================================================================}

      PLine :=
        AllocEnt(
          GDBLineID
        );


      if PLine = nil then
      begin

        raise Exception.CreateFmt(
          'BATCH_LINES: failed to allocate line %d',
          [I]
        );

      end;


      PLine^.init(
        nil,
        nil,
        LnWtByLayer,
        P1,
        P2
      );


      zcSetEntPropFromCurrentDrawingProp(
        PLine
      );


      {=====================================================================
        Добавляем в ConstructRoot.

        Undo и Redraw здесь НЕ выполняются.
      =====================================================================}

      zcAddEntToCurrentDrawingConstructRoot(
        PLine
      );


      Inc(FBatchCount);
      Inc(CreatedCount);

    end;


    {=======================================================================
      Все линии успешно созданы.
    =======================================================================}

    AResult :=
      Format(
        'Created %d lines',
        [CreatedCount]
      );


    ProgramLog.LogOutFormatStr(
      'HTTP BATCH_LINES: created %d lines, batch total: %d',
      [
        CreatedCount,
        FBatchCount
      ],
      LM_Info,
      0
    );


    Result := True;


  except

    on E: Exception do
    begin

      AError :=
        Format(
          'BATCH_LINES error: %s',
          [E.Message]
        );


      ProgramLog.LogOutFormatStr(
        'HTTP BATCH_LINES error: %s',
        [E.Message],
        LM_Error,
        0
      );


      Result := False;

    end;

  end;

end;


{=============================================================================
  HTTPBatchMode
=============================================================================}

function HTTPBatchMode: Boolean;
begin

  Result :=
    FBatchMode;

end;


{=============================================================================
  INITIALIZATION
=============================================================================}

initialization

  FBatchMode := False;
  FBatchCount := 0;


  ProgramLog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsInitializeLMId
  );


  RegisterHTTPIPCCommand(
    'BEGIN_BATCH',
    @HTTPCommandBeginBatch
  );


  RegisterHTTPIPCCommand(
    'END_BATCH',
    @HTTPCommandEndBatch
  );


  RegisterHTTPIPCCommand(
    'BATCH_LINES',
    @HTTPCommandBatchLines
  );


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
