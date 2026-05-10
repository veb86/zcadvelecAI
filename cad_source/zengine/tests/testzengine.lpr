program testzengine;

{$mode objfpc}{$H+}

uses
  //MemCheck,
  Classes, consoletestrunner,
  Logtest,BoundaryPathSimpletest,
  uzctEntityArc, uzctmtextwrap, uzctenttextjustify, uzctacadtable, uzctentproxy,
  uzctentpolyfacemesh, uzctdwgblockreserve;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
