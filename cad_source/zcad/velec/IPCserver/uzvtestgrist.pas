unit uzvtestgrist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, uzclog, uzvhttpgrist;

type
  { Класс команды тестирования }
  TTestGristCommand = class
  public
    class procedure Execute(const Params: string);
  end;

implementation

{ TTestGristCommand }

class procedure TTestGristCommand.Execute(const Params: string);
var
  Args: TJSONObject;
  CmdID: Int64;
  LogMsg: string;
begin
  { Создаём JSON-аргументы команды }
  Args := TJSONObject.Create;
  try
    Args.Add('table', 'Devices');
    Args.Add('recordId', 25);
    Args.Add('field', 'Name');
    Args.Add('value', 'Светильник');
    
    { Добавляем команду в очередь ZCAD → Grist }
    { QueueGristCommand создаёт копию Args, поэтому после вызова }
    { очередь владеет данными, и мы можем безопасно освободить Args }
    CmdID := QueueGristCommand('SET_GRIST_VALUE', Args);
    
    { Логируем результат }
    LogMsg := Format(
      'testGrist: команда добавлена в очередь. ID=%d, command=SET_GRIST_VALUE, table=Devices, recordId=25',
      [CmdID]
    );
    
    LM_Info(LogMsg);
    
    WriteLn(LogMsg);
    WriteLn(Format('Аргументы: table=Devices, recordId=25, field=Name, value=Светильник'));
    WriteLn('Команда ожидает обработки через POST /grist/poll от managerGRIST');
    
  finally
    { Освобождаем локальный объект Args }
    { Примечание: QueueGristCommand internally clones the JSON object, }
    { so the queue owns its own copy and we can safely free Args here }
    Args.Free;
  end;
end;

initialization
  { Регистрация команды в системе команд ZCAD }
  { Формат: RegisterCommand('имя_команды', @обработчик) }
  { Если в проекте есть реестр команд, добавить сюда: }
  { RegisterCADCommand('testGrist', @TTestGristCommand.Execute); }
  
end.
