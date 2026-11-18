unit uzvhttpwallcommands;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

implementation

uses
  SysUtils, Math, fpjson, uzbLogTypes, uzclog, uzcinterface,
  uzccommandsabstract, uzccommandsimpl, uzccommandsmanager, uzcdrawings,
  uzedrawingsimple, uzegeometrytypes, UGDBSelectedObjArray, uzglviewareaabstract,
  uzvhttpipc, uzvhttpwallgeometry, uzvhttpwallcollector, uzvhttpwallpreview,
  uzvhttpwallai, uzvhttpwallcreator, uzvhttpwallxdata;

type
  TWallPhase = (wpCandidates, wpApproved, wpSent, wpRecognized);
  TWallSession = class(TWallPreview)
  public
    Drawing: PTSimpleDrawing;
    ID: string;
    Phase: TWallPhase;
    Minimum, Maximum: Double;
    Sources: TWallSources;
    Segments: TWallSegments;
    Candidates: TWallCandidates;
    Recognized: TWallRecognitions;
    Stats: TWallStatistics;
    Rejected, NonWalls, Unknown, Existing: Integer;
    destructor Destroy; override;
  end;

const
  PhaseNames: array[TWallPhase] of string =
    ('candidate_preview', 'approved_to_send', 'awaiting_ai', 'recognition_preview');

var
  Active: TWallSession;
  LastID, LastStatus: string;
  LocalInput: Boolean;

procedure Tell(const Msg: string);
begin
  zcUI.TextMessage(Msg, TMWOHistoryOut);
end;

destructor TWallSession.Destroy;
begin
  if Active = Self then Active := nil;
  inherited Destroy;
end;

procedure EndSession(const Phase: string; Created: Integer = 0; Redraw: Boolean = True);
var
  View: TAbstractViewArea;
  Data: TJSONObject;
begin
  Data := TJSONObject.Create;
  try
    LastID := Active.ID;
    Data.Add('session_id', LastID);
    Data.Add('phase', Phase);
    Data.Add('created_count', Created);
    LastStatus := Data.AsJSON;
  finally
    Data.Free;
  end;
  View := Active.Drawing^.wa;
  ProgramLog.LogOutFormatStr('WALL %s: %s (%d)', [Active.ID, Phase, Created], LM_Debug, 0);
  FreeAndNil(Active);
  if Redraw then View.DrawOrInvalidate;
end;

function CurrentSession(const ID: string): TWallSession;
begin
  if (Active = nil) or ((ID <> '') and (Active.ID <> ID)) then
    raise Exception.Create('Нет активной сессии с этим идентификатором.');
  if drawings.GetCurrentDWG <> Active.Drawing then
    raise Exception.Create('Сессия принадлежит другому чертежу. Переключитесь на него.');
  Result := Active;
end;

procedure CheckSources(Session: TWallSession);
begin
  if WallSourcesUnchanged(Session.Drawing^.GetCurrentROOT, Session.Sources,
    Session.Segments, Session.Candidates) then Exit;
  EndSession('invalidated');
  raise Exception.Create('Исходная геометрия изменилась. Запустите анализ снова.');
end;

procedure ConfidenceStats(const Walls: TWallRecognitions; out Lo, Hi, Avg: Double);
var
  Wall: TWallRecognition;
begin
  Lo := 0;
  Hi := 0;
  Avg := 0;
  if Length(Walls) = 0 then Exit;
  Lo := 1;
  for Wall in Walls do begin
    Lo := Min(Lo, Wall.Confidence);
    Hi := Max(Hi, Wall.Confidence);
    Avg := Avg + Wall.Confidence;
  end;
  Avg := Avg / Length(Walls);
end;

function SessionStatus(Session: TWallSession): string;
var
  Data: TJSONObject;
  Lo, Hi, Avg: Double;
begin
  Data := TJSONObject.Create;
  try
    Data.Add('session_id', Session.ID);
    Data.Add('phase', PhaseNames[Session.Phase]);
    Data.Add('min_thickness', Session.Minimum);
    Data.Add('max_thickness', Session.Maximum);
    Data.Add('segment_count', Length(Session.Segments));
    Data.Add('candidate_count', Length(Session.Candidates));
    Data.Add('rejected_pair_count', Session.Stats.Rejected);
    Data.Add('outside_thickness_count', Session.Stats.OutsideThickness);
    Data.Add('found_min_thickness', Session.Stats.MinFound);
    Data.Add('found_max_thickness', Session.Stats.MaxFound);
    Data.Add('existing_wall_count', Session.Existing);
    if Session.Phase = wpRecognized then begin
      Data.Add('recognized_count', Length(Session.Recognized));
      Data.Add('rejected_result_count', Session.Rejected);
      Data.Add('unknown_candidate_count', Session.Unknown);
      Data.Add('non_wall_count', Session.NonWalls);
      if Length(Session.Recognized) > 0 then begin
        ConfidenceStats(Session.Recognized, Lo, Hi, Avg);
        Data.Add('confidence_min', Lo);
        Data.Add('confidence_max', Hi);
        Data.Add('confidence_mean', Avg);
      end;
    end;
    Result := Data.AsJSON;
  finally
    Data.Free;
  end;
end;

function StartAnalysis(const Scope: TWallScope; Minimum, Maximum: Double): string;
var
  Drawing: PTSimpleDrawing;
  Session: TWallSession;
  Sources: TWallSources;
  Segments: TWallSegments;
  Candidates: TWallCandidates;
  Stats: TWallStatistics;
  ID: TGUID;
  I, Count, Existing: Integer;
  Msg: string;
begin
  if Active <> nil then
    raise Exception.Create('Сначала завершите или отмените текущую сессию (WallCancel).');
  Drawing := drawings.GetCurrentDWG;
  if (Drawing = nil) or (Drawing^.wa = nil) then
    raise Exception.Create('Сначала откройте чертёж.');
  if not ValidWallThickness(Minimum, Maximum) then
    raise Exception.Create('Задайте конечные значения толщины: 0 < минимум < максимум.');
  CollectWallSegments(Drawing^.GetCurrentROOT, Scope, @IsRecognizedWall, Sources, Segments);
  if Length(Segments) = 0 then
    raise Exception.Create('В выбранной области нет LINE или прямых сегментов LWPOLYLINE.');
  Candidates := FindWallCandidates(Segments, Minimum, Maximum, Stats);
  if Length(Candidates) = 0 then begin
    if Stats.OutsideThickness > 0 then
      raise Exception.Create('Параллельные пары с перекрытием выходят за диапазон толщин.');
    raise Exception.Create('Не найдено ни одной пары параллельных границ с перекрытием.');
  end;
  Count := 0;
  Existing := 0;
  for I := 0 to High(Candidates) do
    if AlreadyHasWall(Drawing^.GetCurrentROOT, Candidates[I]) then
      Inc(Existing)
    else begin
      Candidates[Count] := Candidates[I];
      Inc(Count);
    end;
  SetLength(Candidates, Count);
  if Count = 0 then
    raise Exception.Create('Оси найденных стен уже существуют в чертеже.');
  Stats.MinFound := Candidates[0].Thickness;
  Stats.MaxFound := Stats.MinFound;
  for I := 1 to High(Candidates) do begin
    Stats.MinFound := Min(Stats.MinFound, Candidates[I].Thickness);
    Stats.MaxFound := Max(Stats.MaxFound, Candidates[I].Thickness);
  end;
  if CreateGUID(ID) <> 0 then
    raise Exception.Create('Не удалось создать идентификатор сессии.');
  Session := TWallSession.Create(Drawing);
  try
    Session.Drawing := Drawing;
    Session.ID := GUIDToString(ID);
    Session.Phase := wpCandidates;
    Session.Minimum := Minimum;
    Session.Maximum := Maximum;
    Session.Sources := Sources;
    Session.Segments := Segments;
    Session.Candidates := Candidates;
    Session.Existing := Existing;
    Session.Stats := Stats;
    Session.ShowCandidates(Candidates);
    Result := SessionStatus(Session);
    Active := Session;
  except
    Session.Free;
    Drawing^.wa.DrawOrInvalidate;
    raise;
  end;
  Msg := Format('Найдено %d пар. Отклонено %d, уже построено %d. Толщина %g–%g ед. чертежа.',
    [Count, Stats.Rejected, Existing, Stats.MinFound, Stats.MaxFound]);
  Tell(Msg);
  Tell('Отправить показанную геометрию в ИИ? WallSend — отправить, WallCancel — отменить.');
  ProgramLog.LogOutFormatStr('WALL %s: %s', [Active.ID, Msg], LM_Debug, 0);
end;

function RequiredID(Args: TJSONArray): string;
begin
  if (Args = nil) or (Args.Count <> 1) or (Args.Items[0].JSONType <> jtString) or
    (Args.Items[0].AsString = '') then
    raise Exception.Create('Передайте session_id как единственный аргумент.');
  Result := Args.Items[0].AsString;
end;

function Options(Args: TJSONArray): TJSONObject;
begin
  if (Args = nil) or (Args.Count <> 1) or (Args.Items[0].JSONType <> jtObject) then
    raise Exception.Create('Передайте один объект JSON в args.');
  Result := TJSONObject(Args.Items[0]);
end;

function Analyze(Args: TJSONArray; out Reply, Error: string): Boolean;
var
  Obj: TJSONObject;
  Scope: TWallScope;
  MinThickness, MaxThickness: Double;
  Window, Handles: TJSONData;
  I: Integer;
begin
  if LocalInput then raise Exception.Create('Завершите выбор области в ZCAD.');
  Obj := Options(Args);
  if not JSONWallNumber(Obj.Find('min_thickness'), MinThickness) or
    not JSONWallNumber(Obj.Find('max_thickness'), MaxThickness) then
    raise Exception.Create('Требуются числовые min_thickness и max_thickness.');
  Window := Obj.Find('window');
  Handles := Obj.Find('handles');
  Scope.Kind := wskSelection;
  if Window <> nil then begin
    if (Handles <> nil) or (Window.JSONType <> jtArray) or (Window.Count <> 2) or
      not JSONWallPoint(Window.Items[0], 0, Scope.Window.First) or
      not JSONWallPoint(Window.Items[1], 0, Scope.Window.Last) then
      raise Exception.Create('window должен содержать два угла в WCS.');
    Scope.Kind := wskWindow;
  end else if Handles <> nil then begin
    if (Handles.JSONType <> jtArray) or (Handles.Count = 0) then
      raise Exception.Create('handles должен быть непустым массивом DXF HANDLE.');
    Scope.Kind := wskHandles;
    SetLength(Scope.Handles, Handles.Count);
    for I := 0 to Handles.Count - 1 do begin
      if (Handles.Items[I].JSONType <> jtString) or (Handles.Items[I].AsString = '') then
        raise Exception.Create('Каждый элемент handles должен быть непустой строкой.');
      Scope.Handles[I] := Handles.Items[I].AsString;
    end;
  end;
  Reply := StartAnalysis(Scope, MinThickness, MaxThickness);
  Result := True;
end;

function Preview(Args: TJSONArray; out Reply, Error: string): Boolean;
var
  Session: TWallSession;
  ID: string;
begin
  ID := '';
  if (Args <> nil) and (Args.Count > 0) then ID := RequiredID(Args);
  if (ID <> '') and (ID = LastID) then Reply := LastStatus
  else if (ID = '') and (Active = nil) then Reply := '{"phase":"idle"}'
  else begin
    Session := CurrentSession(ID);
    if not Session.IsDisplayed then Session.Refresh;
    Reply := SessionStatus(Session);
  end;
  Result := True;
end;

function SendAI(Args: TJSONArray; out Reply, Error: string): Boolean;
var
  Session: TWallSession;
begin
  Session := CurrentSession(RequiredID(Args));
  if not (Session.Phase in [wpApproved, wpSent]) then
    raise Exception.Create('Для передачи геометрии подтвердите отправку в ZCAD (WallSend).');
  CheckSources(Session);
  if not Session.IsDisplayed then
    raise Exception.Create('Включите слой предпросмотра и дождитесь его отображения.');
  Reply := CandidatePayload(Session.ID, Session.Minimum, Session.Maximum,
    Session.Segments, Session.Candidates);
  Session.Phase := wpSent;
  Result := True;
end;

function AIResult(Args: TJSONArray; out Reply, Error: string): Boolean;
var
  Session: TWallSession;
  Obj: TJSONObject;
  ID: TJSONData;
  Lo, Hi, Avg: Double;
  Msg: string;
begin
  Obj := Options(Args);
  ID := Obj.Find('session_id', jtString);
  if (ID = nil) or (ID.AsString = '') then
    raise Exception.Create('Ответ должен содержать session_id.');
  Session := CurrentSession(ID.AsString);
  if Session.Phase <> wpSent then
    raise Exception.Create('Сессия не ожидает ответа ИИ.');
  CheckSources(Session);
  Session.Recognized := ReadRecognition(Obj, Session.ID, Session.Minimum, Session.Maximum,
    Session.Segments, Session.Candidates, Session.Rejected, Session.NonWalls, Session.Unknown);
  try
    Session.ShowRecognized(Session.Recognized);
  except
    EndSession('preview_failed');
    raise;
  end;
  Session.Phase := wpRecognized;
  ConfidenceStats(Session.Recognized, Lo, Hi, Avg);
  Msg := Format('ИИ распознал %d стен. Отклонено %d (неизвестных %d), прочих объектов %d.',
    [Length(Session.Recognized), Session.Rejected, Session.Unknown, Session.NonWalls]);
  Tell(Msg);
  if Length(Session.Recognized) > 0 then begin
    Tell(Format('Уверенность (мин./сред./макс.) %.2f / %.2f / %.2f.', [Lo, Avg, Hi]));
    Tell('Создать полилинии по показанным осям? WallCommit — создать, WallCancel — отменить.');
  end else
    Tell('Осей для создания нет. WallCancel — закрыть предпросмотр.');
  ProgramLog.LogOutFormatStr('WALL %s: %s', [Session.ID, Msg], LM_Debug, 0);
  Reply := SessionStatus(Session);
  Result := True;
end;

function Commit(Args: TJSONArray; out Reply, Error: string): Boolean;
var
  Session: TWallSession;
  ID: string;
begin
  ID := RequiredID(Args);
  if ID = LastID then Reply := LastStatus
  else begin
    Session := CurrentSession(ID);
    if Session.Phase <> wpRecognized then
      raise Exception.Create('Сначала получите результат распознавания.');
    Tell('Для создания полилиний подтвердите результат в ZCAD (WallCommit).');
    Reply := SessionStatus(Session);
  end;
  Result := True;
end;

function Cancel(Args: TJSONArray; out Reply, Error: string): Boolean;
var
  ID: string;
begin
  ID := RequiredID(Args);
  if ID <> LastID then begin
    CurrentSession(ID);
    EndSession('cancelled');
    Tell('Анализ стен отменён.');
  end;
  Reply := LastStatus;
  Result := True;
end;

function DrawPoint(const Prompt: string; out P: TWallPoint): Boolean;
var
  V: TzePoint3d;
begin
  Result := CommandManager.Get3DPoint(Prompt, V) = IRNormal;
  if Result then P := ToWallPoint(V);
end;

function Thickness(const Prompt: string; out Value: Double): Boolean;
var
  S: string;
  Settings: TFormatSettings;
begin
  Result := CommandManager.GetInput(Prompt, S) = IRNormal;
  if not Result then Exit;
  Settings := DefaultFormatSettings;
  if not TryStrToFloat(Trim(S), Value, Settings) then begin
    Settings.DecimalSeparator := '.';
    if not TryStrToFloat(Trim(S), Value, Settings) then
      raise Exception.Create('Введите число в единицах чертежа.');
  end;
end;

function WallAnalyze(const Context: TZCADCommandContext; Operands: TCommandOperands): TCommandResult;
var
  Scope: TWallScope;
  Minimum, Maximum: Double;
begin
  Result := cmd_cancel;
  try
    if Active <> nil then raise Exception.Create('Сначала завершите текущую сессию (WallCancel).');
    LocalInput := True;
    try
      if not Thickness('Минимальная толщина стены (ед. чертежа)', Minimum) or
        not Thickness('Максимальная толщина стены (ед. чертежа)', Maximum) then Exit;
      if not ValidWallThickness(Minimum, Maximum) then
        raise Exception.Create('Задайте 0 < минимум < максимум.');
      Scope.Kind := wskSelection;
      if drawings.GetCurrentDWG^.GetSelObjArray^.Count = 0 then begin
        Scope.Kind := wskWindow;
        if not DrawPoint('Первый угол области анализа', Scope.Window.First) or
          not DrawPoint('Противоположный угол области анализа', Scope.Window.Last) then Exit;
      end;
      StartAnalysis(Scope, Minimum, Maximum);
      Result := cmd_ok;
    finally
      LocalInput := False;
    end;
  except
    on E: Exception do begin
      Tell(E.Message);
      ProgramLog.LogOutFormatStr('WALL ANALYZE: %s', [E.Message], LM_Debug, 0);
      Result := cmd_error;
    end;
  end;
end;

function WallSend(const Context: TZCADCommandContext; Operands: TCommandOperands): TCommandResult;
var
  Session: TWallSession;
begin
  Result := cmd_error;
  try
    Session := CurrentSession('');
    if Session.Phase <> wpCandidates then
      raise Exception.Create('Нет кандидатов, ожидающих разрешения на отправку.');
    CheckSources(Session);
    if not Session.IsDisplayed then
      raise Exception.Create('Включите слой предпросмотра и дождитесь его отображения.');
    Session.Phase := wpApproved;
    Tell('Отправка разрешена. Внешний клиент может получить кандидатов.');
    Result := cmd_ok;
  except
    on E: Exception do Tell(E.Message);
  end;
end;

function WallCommit(const Context: TZCADCommandContext; Operands: TCommandOperands): TCommandResult;
var
  Session: TWallSession;
  Count: Integer;
begin
  Result := cmd_error;
  try
    Session := CurrentSession('');
    if (Session.Phase <> wpRecognized) or (Length(Session.Recognized) = 0) then
      raise Exception.Create('Нет распознанных осей, ожидающих подтверждения.');
    CheckSources(Session);
    if not Session.IsDisplayed then
      raise Exception.Create('Включите слой предпросмотра и дождитесь его отображения.');
    Count := CreateRecognizedWalls(Session.ID, Session.Sources, Session.Segments,
      Session.Candidates, Session.Recognized);
    EndSession('committed', Count, False);
    Tell(Format('Создано полилиний стен: %d.', [Count]));
    Result := cmd_ok;
  except
    on E: Exception do begin
      Tell(E.Message);
      ProgramLog.LogOutFormatStr('WALL COMMIT: %s', [E.Message], LM_Debug, 0);
    end;
  end;
end;

function WallCancel(const Context: TZCADCommandContext; Operands: TCommandOperands): TCommandResult;
begin
  Result := cmd_error;
  try
    CurrentSession('');
    EndSession('cancelled', 0, False);
    Tell('Анализ стен отменён.');
    Result := cmd_ok;
  except
    on E: Exception do Tell(E.Message);
  end;
end;

initialization
  CreateZCADCommand(@WallAnalyze, 'WallAnalyze', CADWG, 0)^.CEndActionAttr := [];
  CreateZCADCommand(@WallSend, 'WallSend', CADWG, 0)^.CEndActionAttr := [];
  CreateZCADCommand(@WallCommit, 'WallCommit', CADWG, 0)^.CEndActionAttr := [];
  CreateZCADCommand(@WallCancel, 'WallCancel', CADWG, 0)^.CEndActionAttr := [];
  RegisterHTTPIPCCommand('WALL_ANALYZE', @Analyze);
  RegisterHTTPIPCCommand('WALL_PREVIEW', @Preview);
  RegisterHTTPIPCCommand('WALL_SEND_AI', @SendAI);
  RegisterHTTPIPCCommand('WALL_AI_RESULT', @AIResult);
  RegisterHTTPIPCCommand('WALL_COMMIT', @Commit);
  RegisterHTTPIPCCommand('WALL_CANCEL', @Cancel);

finalization
  FreeAndNil(Active);
end.
