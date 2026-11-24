{
  Заглушка uzclog для регрессии #1478: без пакетов zbaseutils и LCL.
  Сохраняет интерфейс, который использует uzvhttpipc (ProgramLog и LM_*),
  и печатает сообщения в stdout, чтобы тест видел журнал очереди.
}
unit uzclog;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, SyncObjs, uzbLogTypes;

type
  { Минимальный журнал с той же сигнатурой LogOutFormatStr, что и TLog }
  TStubLog = object
    procedure LogOutFormatStr(const Fmt: TLogMsg; const Args: array of const;
      LogMode: TLogLevel; LMDI: TModuleDesk = 1; MsgOptions: TMsgOpt = 0);
  end;

const
  LM_Debug = 1;
  LM_Info = 2;
  LM_Warning = 3;
  LM_Error = 4;

var
  ProgramLog: TStubLog;

implementation

var
  LogLock: TCriticalSection;

{ Печатает отформатированное сообщение журнала в stdout }
procedure TStubLog.LogOutFormatStr(const Fmt: TLogMsg; const Args: array of const;
  LogMode: TLogLevel; LMDI: TModuleDesk; MsgOptions: TMsgOpt);
begin
  if LogMode = LM_Debug then
    Exit;
  LogLock.Enter;
  try
    WriteLn('LOG ', Format(Fmt, Args));
    Flush(Output);
  finally
    LogLock.Leave;
  end;
end;

initialization
  LogLock := TCriticalSection.Create;
finalization
  LogLock.Free;
end.
