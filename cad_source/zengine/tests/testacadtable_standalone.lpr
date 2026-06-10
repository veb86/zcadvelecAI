program testacadtable_standalone;

{$mode objfpc}{$H+}

uses
  Classes, consoletestrunner, uzctacadtable;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
