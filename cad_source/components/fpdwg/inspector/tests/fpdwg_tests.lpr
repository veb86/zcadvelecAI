program fpdwg_tests;

{$mode objfpc}{$H+}

uses
  Classes,
  consoletestrunner,
  fpdwg_test_cli,
  fpdwg_test_dwgproc,
  fpdwg_test_factory,
  fpdwg_test_handles,
  fpdwg_test_reader,
  fpdwg_test_reporter,
  fpdwg_test_registry,
  fpdwg_test_resolver,
  fpdwg_test_smoke,
  fpdwg_test_utils,
  fpdwg_test_validator;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
