{ BUILD-COPY ONLY stub for Linux E2E of issue #1478; not part of the repository }
unit comobj;
{$mode objfpc}{$H+}
interface
uses SysUtils;
type
  EOleException = class(Exception);
function CreateOleObject(const ClassName: string): IDispatch;
function GetActiveOleObject(const ClassName: string): IDispatch;
implementation
function CreateOleObject(const ClassName: string): IDispatch;
begin
  raise EOleException.Create('OLE is not available on this platform');
end;
function GetActiveOleObject(const ClassName: string): IDispatch;
begin
  raise EOleException.Create('OLE is not available on this platform');
end;
end.
