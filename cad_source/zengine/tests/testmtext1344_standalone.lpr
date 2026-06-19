program testmtext1344_standalone;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, uzctmtextcontentsave;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
