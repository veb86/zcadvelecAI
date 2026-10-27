program polyfacemeshdisplay;

// Консольный запуск регрессий отображения сетки без IDE Lazarus.
{$mode objfpc}{$H+}

uses
  Interfaces, consoletestrunner, uzctentpolyfacemesh;

var
  Runner: TTestRunner;
begin
  DefaultRunAllTests := True;
  DefaultFormat := fPlain;
  Runner := TTestRunner.Create(nil);
  try
    Runner.Initialize;
    Runner.Run;
  finally
    Runner.Free;
  end;
end.
