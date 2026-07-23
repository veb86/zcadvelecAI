program testzengine;

{$mode objfpc}{$H+}

uses
  //MemCheck,
  Classes, consoletestrunner,
  Logtest,BoundaryPathSimpletest, uzctdwgdimstylelinetype,
  uzctEntityArc, uzctmtextwrap, uzctenttextjustify, uzctacadtable, uzctentproxy,
  uzctentpolyfacemesh, uzctentleader, uzctdwgblockreserve, uzctdwgentleader,
  uzctenttextnilstyle,
  uzctmtextcontentsave,
  uzctenttransformscalars,
  uzctentityvisibility;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Run;
  Application.Free;
end.
