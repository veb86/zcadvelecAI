program fpdwg_tests;

{$mode objfpc}{$H+}

uses
  Classes,
  consoletestrunner,
  fpdwg_test_handles;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
