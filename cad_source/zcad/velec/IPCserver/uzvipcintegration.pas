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
@author(IPC Integration for ZCAD Main Loop)
@author(Vladimir Bobrov)
}
{$mode objfpc}{$H+}

{**Модуль интеграции IPC-сервера с главным циклом ZCAD}
unit uzvipcintegration;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes, SysUtils, ExtCtrls, Forms, fpjson,
  uzclog, uzcinterface, uzvipcserver, uzeTypes;

type
  {** Обработчик команд IPC в главном потоке }
  TIPCCommandHandler = class(TComponent)
  private
    FTimer: TTimer;
    FProcessing: Boolean;
    FBatchMode: Boolean;
    FBatchCount: Integer;
    procedure OnTimer(Sender: TObject);
    procedure ProcessQueue;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
  end;

  {** Расширенная запись команды с контекстом выполнения }
  TIPCCommandContext = record
    Drawing: Pointer;
    ViewArea: Pointer;
  end;

var
  {** Глобальный обработчик команд IPC }
  IPCCommandHandler: TIPCCommandHandler;

{** Инициализация интеграции IPC }
procedure IPCIntegrationInit;
{** Завершение интеграции IPC }
procedure IPCIntegrationDone;
{** Обработка очереди команд IPC - вызывать из главного цикла }
procedure IPCProcessPendingCommands;

implementation

uses
  uzcdrawings, uzglviewareadata, uzccommandsmanager, uzeentline, uzeentity,
  uzeentityfactory, uzegeometrytypes, uzegeometry, uzcutils, uzeconsts,
  uzeentcircle, uzeenttext, uzeffdxf, uzeffdxfsupport,
  uzbpaths, uzcFileStructure;

{=== Вспомогательные функции ===}

function CreateJSONResponse(const AID, AStatus, AResult, AError: string): TJSONObject;
begin
  Result := TJSONObject.Create;
  Result.Add('id', AID);
  Result.Add('status', AStatus);
  if AResult <> '' then
    Result.Add('result', AResult);
  if AError <> '' then
    Result.Add('error', AError);
end;

{=== TIPCCommandHandler ===}

constructor TIPCCommandHandler.Create(AOwner: TComponent);
begin
  inherited;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 10; {** Проверка очереди каждые 10 мс для быстрой обработки }
  FTimer.OnTimer := @OnTimer;
  FProcessing := False;
  FBatchMode := False;
  FBatchCount := 0;
end;

destructor TIPCCommandHandler.Destroy;
begin
  Stop;
  inherited;
end;

procedure TIPCCommandHandler.Start;
begin
  FTimer.Enabled := True;
  ProgramLog.LogOutFormatStr('IPC command handler started', [], LM_Info, 0);
end;

procedure TIPCCommandHandler.Stop;
begin
  FTimer.Enabled := False;
  ProgramLog.LogOutFormatStr('IPC command handler stopped', [], LM_Info, 0);
end;

procedure TIPCCommandHandler.OnTimer(Sender: TObject);
begin
  if FProcessing then
    Exit;
  
  FProcessing := True;
  try
    ProcessQueue;
  finally
    FProcessing := False;
  end;
end;

procedure TIPCCommandHandler.ProcessQueue;
var
  Cmd: PIPCCommand;
  CmdResult: TIPCCommandResult;
  Response: TJSONObject;
  
  function GetStringArg(AIndex: Integer; const ADefault: string = ''): string;
  begin
    if (Cmd^.Args <> nil) and (AIndex < Cmd^.Args.Count) then
      Result := Cmd^.Args.Items[AIndex].AsString
    else
      Result := ADefault;
  end;
  
  function GetFloatArg(AIndex: Integer; ADefault: Double = 0): Double;
  begin
    if (Cmd^.Args <> nil) and (AIndex < Cmd^.Args.Count) then
      Result := Cmd^.Args.Items[AIndex].AsFloat
    else
      Result := ADefault;
  end;
  
  procedure ExecutePing;
  begin
    CmdResult.Status := 'ok';
    CmdResult.Result := 'pong';
    CmdResult.Error := '';
  end;
  
  procedure ExecuteSave;
  var
    FileName: string;
  begin
    FileName := GetStringArg(0);
    if FileName = '' then
      FileName := drawings.GetCurrentDWG^.GetFileName;
    
    if FileName = '' then
    begin
      CmdResult.Status := 'error';
      CmdResult.Error := 'No filename specified';
      Exit;
    end;
    
    try
      commandmanager.ExecuteCommandSilent('QSave',
        drawings.GetCurrentDWG, drawings.GetCurrentOGLWParam);
      CmdResult.Status := 'ok';
      CmdResult.Result := Format('File saved: %s', [FileName]);
    except
      on E: Exception do
      begin
        CmdResult.Status := 'error';
        CmdResult.Error := Format('Save error: %s', [E.Message]);
      end;
    end;
  end;
  
  procedure ExecuteExport;
  var
    FileName: string;
    FileExt: string;
  begin
    FileName := GetStringArg(0);
    if FileName = '' then
    begin
      CmdResult.Status := 'error';
      CmdResult.Error := 'No filename specified';
      Exit;
    end;
    
    FileExt := UpperCase(ExtractFileExt(FileName));
    
    if FileExt = '.DXF' then
    begin
      try
        commandmanager.ExecuteCommandSilent('SaveAs(' + FileName + ')',
          drawings.GetCurrentDWG, drawings.GetCurrentOGLWParam);
        CmdResult.Status := 'ok';
        CmdResult.Result := Format('Exported to: %s', [FileName]);
      except
        on E: Exception do
        begin
          CmdResult.Status := 'error';
          CmdResult.Error := Format('Export error: %s', [E.Message]);
        end;
      end;
    end
    else
    begin
      CmdResult.Status := 'error';
      CmdResult.Error := Format('Unsupported export format: %s', [FileExt]);
    end;
  end;
  
  procedure ExecuteBeginBatch;
  begin
    FBatchMode := True;
    FBatchCount := 0;
    CmdResult.Status := 'ok';
    CmdResult.Result := 'Batch mode started';
    ProgramLog.LogOutFormatStr('IPC batch mode started', [], LM_Info, 0);
  end;

  procedure ExecuteEndBatch;
  begin
    if FBatchMode then
    begin
      try
        {** Перемещаем все примитивы из ConstructRoot в чертёж с одним Undo }
        zcMoveEntsFromConstructRootToCurrentDrawingWithUndo('IPC_BATCH_IMPORT');
        {** Одна перерисовка после всех примитивов }
        zcRedrawCurrentDrawing;

        CmdResult.Status := 'ok';
        CmdResult.Result := Format('Batch mode completed: %d primitives imported', [FBatchCount]);
        ProgramLog.LogOutFormatStr('IPC batch mode completed: %d primitives', [FBatchCount], LM_Info, 0);
      except
        on E: Exception do
        begin
          CmdResult.Status := 'error';
          CmdResult.Error := Format('Batch commit error: %s', [E.Message]);
        end;
      end;
    end
    else
    begin
      CmdResult.Status := 'error';
      CmdResult.Error := 'EndBatch called without BeginBatch';
    end;
    FBatchMode := False;
    FBatchCount := 0;
  end;

  procedure ExecuteLine;
  var
    PLine: PGDBObjLine;
    P1, P2: TzePoint3d;
  begin
    if (Cmd^.Args = nil) or (Cmd^.Args.Count < 4) then
    begin
      CmdResult.Status := 'error';
      CmdResult.Error := 'LINE requires 4 arguments: x1 y1 x2 y2';
      Exit;
    end;

    P1.x := GetFloatArg(0);
    P1.y := GetFloatArg(1);
    P1.z := 0;

    P2.x := GetFloatArg(2);
    P2.y := GetFloatArg(3);
    P2.z := 0;

    try
      PLine := AllocEnt(GDBLineID);
      PLine^.init(nil, nil, LnWtByLayer, P1, P2);
      zcSetEntPropFromCurrentDrawingProp(PLine);
      
      if FBatchMode then
      begin
        {** В пакетном режиме добавляем в ConstructRoot без Undo и Redraw }
        zcAddEntToCurrentDrawingConstructRoot(PLine);
        Inc(FBatchCount);
        CmdResult.Result := Format('Line queued: (%.2f,%.2f)-(%.2f,%.2f)',
          [P1.x, P1.y, P2.x, P2.y]);
      end
      else
      begin
        {** В обычном режиме добавляем с Undo и Redraw }
        zcAddEntToCurrentDrawingWithUndo(PLine);
        zcRedrawCurrentDrawing;
        CmdResult.Result := Format('Line created: (%.2f,%.2f)-(%.2f,%.2f)',
          [P1.x, P1.y, P2.x, P2.y]);
      end;
      
      CmdResult.Status := 'ok';
    except
      on E: Exception do
      begin
        CmdResult.Status := 'error';
        CmdResult.Error := Format('Failed to create line: %s', [E.Message]);
      end;
    end;
  end;
  
  procedure ExecuteCircle;
  var
    PCircle: PGDBObjCircle;
    Center: TzePoint3d;
    Radius: Double;
  begin
    if (Cmd^.Args = nil) or (Cmd^.Args.Count < 3) then
    begin
      CmdResult.Status := 'error';
      CmdResult.Error := 'CIRCLE requires 3 arguments: x y radius';
      Exit;
    end;

    Center.x := GetFloatArg(0);
    Center.y := GetFloatArg(1);
    Center.z := 0;
    Radius := GetFloatArg(2);

    try
      PCircle := AllocEnt(GDBCircleID);
      PCircle^.init(nil, nil, LnWtByLayer, Center, Radius);
      zcSetEntPropFromCurrentDrawingProp(PCircle);
      
      if FBatchMode then
      begin
        zcAddEntToCurrentDrawingConstructRoot(PCircle);
        Inc(FBatchCount);
        CmdResult.Result := Format('Circle queued: center (%.2f,%.2f), radius %.2f',
          [Center.x, Center.y, Radius]);
      end
      else
      begin
        zcAddEntToCurrentDrawingWithUndo(PCircle);
        zcRedrawCurrentDrawing;
        CmdResult.Result := Format('Circle created: center (%.2f,%.2f), radius %.2f',
          [Center.x, Center.y, Radius]);
      end;
      
      CmdResult.Status := 'ok';
    except
      on E: Exception do
      begin
        CmdResult.Status := 'error';
        CmdResult.Error := Format('Failed to create circle: %s', [E.Message]);
      end;
    end;
  end;
  
  procedure ExecuteText;
  var
    PText: PGDBObjText;
    InsertPoint: TzePoint3d;
    TextContent: string;
    Height: Double;
  begin
    if (Cmd^.Args = nil) or (Cmd^.Args.Count < 3) then
    begin
      CmdResult.Status := 'error';
      CmdResult.Error := 'TEXT requires at least 3 arguments: x y text [height]';
      Exit;
    end;

    InsertPoint.x := GetFloatArg(0);
    InsertPoint.y := GetFloatArg(1);
    InsertPoint.z := 0;
    TextContent := GetStringArg(2);
    Height := GetFloatArg(3, 2.5);

    try
      PText := GDBObjText.CreateInstance;
      zcSetEntPropFromCurrentDrawingProp(PText);
      PText^.TXTStyle := drawings.GetCurrentDWG^.GetCurrentTextStyle;
      PText^.Local.P_insert := InsertPoint;
      PText^.Template := TDXFEntsInternalStringType(TextContent);
      PText^.obj_height := Height;
      
      if FBatchMode then
      begin
        zcAddEntToCurrentDrawingConstructRoot(PText);
        Inc(FBatchCount);
        CmdResult.Result := Format('Text queued: "%s" at (%.2f,%.2f)',
          [TextContent, InsertPoint.x, InsertPoint.y]);
      end
      else
      begin
        zcAddEntToCurrentDrawingWithUndo(PText);
        zcRedrawCurrentDrawing;
        CmdResult.Result := Format('Text created: "%s" at (%.2f,%.2f)',
          [TextContent, InsertPoint.x, InsertPoint.y]);
      end;
      
      CmdResult.Status := 'ok';
    except
      on E: Exception do
      begin
        CmdResult.Status := 'error';
        CmdResult.Error := Format('Failed to create text: %s', [E.Message]);
      end;
    end;
  end;

begin
  {** Обрабатываем несколько команд за один вызов для производительности }
  while (IPCCommandQueue <> nil) and (not IPCCommandQueue.IsEmpty) do
  begin
    {** Получаем команду из очереди }
    Cmd := IPCCommandQueue.Dequeue;
    if Cmd = nil then
      Exit;

    {** Устанавливаем статус busy }
    IPCCommandQueue.SetStatus(csBusy);
  
  try
    {** Инициализация результата }
    CmdResult.Status := 'error';
    CmdResult.Result := '';
    CmdResult.Error := '';
    
    {** Выполняем команду }
    try
      case Cmd^.CmdType of
        ictPing: ExecutePing;
        ictSave: ExecuteSave;
        ictExport: ExecuteExport;
        ictLine: ExecuteLine;
        ictCircle: ExecuteCircle;
        ictText: ExecuteText;
        ictBeginBatch: ExecuteBeginBatch;
        ictEndBatch: ExecuteEndBatch;
        ictUnknown:
          begin
            CmdResult.Status := 'error';
            CmdResult.Error := 'Unknown command';
          end;
      else
        begin
          CmdResult.Status := 'error';
          CmdResult.Error := 'Command not implemented';
        end;
      end;
    except
      on E: Exception do
      begin
        CmdResult.Status := 'error';
        CmdResult.Error := Format('Exception: %s', [E.Message]);
        ProgramLog.LogOutFormatStr('IPC command error: %s', [E.Message], LM_Error, 0);
      end;
    end;
    
    {** Формируем ответ }
    Response := CreateJSONResponse(Cmd^.ID, CmdResult.Status, 
      CmdResult.Result, CmdResult.Error);
    Cmd^.Response := Response;
    
    {** Сигнализируем о завершении }
    Cmd^.Completed.SetEvent;
    
    ProgramLog.LogOutFormatStr('IPC command executed: %s - %s', 
      [Cmd^.ID, CmdResult.Status], LM_Debug, 0);
    
  finally
    {** Очистка ресурсов команды (кроме Response и Completed) }
    if Cmd^.Args <> nil then
    begin
      Cmd^.Args.Free;
      Cmd^.Args := nil;
    end;
    
    {** Устанавливаем статус idle }
    IPCCommandQueue.SetStatus(csIdle);
  end;
  end;
end;

{=== Глобальные функции ===}

procedure IPCIntegrationInit;
begin
  if IPCCommandHandler = nil then
  begin
    IPCCommandHandler := TIPCCommandHandler.Create(Application);
    IPCCommandHandler.Start;
    ProgramLog.LogOutFormatStr('IPC integration initialized', [], LM_Info, 0);
  end;
end;

procedure IPCIntegrationDone;
begin
  if IPCCommandHandler <> nil then
  begin
    IPCCommandHandler.Free;
    IPCCommandHandler := nil;
    ProgramLog.LogOutFormatStr('IPC integration finalized', [], LM_Info, 0);
  end;
end;

procedure IPCProcessPendingCommands;
begin
  if IPCCommandHandler <> nil then
    IPCCommandHandler.ProcessQueue;
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsInitializeLMId);
  
  {** Инициализация интеграции при загрузке модуля }
  IPCIntegrationInit;

finalization
  {** Завершение интеграции }
  IPCIntegrationDone;
  
  ProgramLog.LogOutFormatStr('Unit "%s" finalization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsFinalizeLMId);

end.
