program fpdwginspect;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

uses
  SysUtils,
  fpdwg_cli;

type
  TStringArray = array of string;

function CommandLineArgs: TStringArray;
var
  I: Integer;
begin
  Result := nil;
  SetLength(Result, ParamCount);
  for I := 1 to ParamCount do
    Result[I - 1] := ParamStr(I);
end;

var
  ParseResult: TDWGInspectorParseResult;
  OutputText: string;

begin
  ParseResult := ParseDWGInspectorArgs(CommandLineArgs);
  if ParseResult.Command = icHelp then
  begin
    Write(DWGInspectorHelpText);
    Halt(ParseResult.ExitCode);
  end;

  if not ParseResult.Success then
  begin
    WriteLn(ErrOutput, ParseResult.Message);
    WriteLn(ErrOutput);
    Write(ErrOutput, DWGInspectorHelpText);
    Halt(ParseResult.ExitCode);
  end;

  ExitCode := RunDWGInspector(ParseResult.Options, OutputText);
  if OutputText <> '' then
    Write(OutputText);
end.
