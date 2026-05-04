unit fpdwg_test_cli;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGCLITest = class(TTestCase)
  published
    procedure HelpDoesNotRequireInputFile;
    procedure DefaultsUseTextTolerantAndAllReportSections;
    procedure ParsesFormatModeLibraryAndLineFilter;
    procedure ParsesArcFilter;
    procedure FormatSelectsDefaultOutputFileExtension;
    procedure OutputDashKeepsReportOnStdout;
    procedure SectionFlagsSwitchToExplicitSelection;
    procedure ShowOptionParsesHexHandle;
    procedure InvalidFormatFailsWithUsageExitCode;
    procedure MissingFileReturnsFileExitCode;
  end;

implementation

uses
  SysUtils,
  fpdwg_types,
  fpdwg_logger,
  fpdwg_reader,
  fpdwg_reporter,
  fpdwg_cli;

procedure TFPDWGCLITest.HelpDoesNotRequireInputFile;
var
  ResultInfo: TDWGInspectorParseResult;
begin
  ResultInfo := ParseDWGInspectorArgs(['--help']);

  AssertTrue(ResultInfo.Success);
  AssertEquals(Ord(icHelp), Ord(ResultInfo.Command));
  AssertEquals(DWG_INSPECT_EXIT_SUCCESS, ResultInfo.ExitCode);
end;

procedure TFPDWGCLITest.DefaultsUseTextTolerantAndAllReportSections;
var
  ResultInfo: TDWGInspectorParseResult;
begin
  ResultInfo := ParseDWGInspectorArgs(['sample.dwg']);

  AssertTrue(ResultInfo.Success);
  AssertEquals(Ord(icRun), Ord(ResultInfo.Command));
  AssertEquals('sample.dwg', ResultInfo.Options.InputFile);
  AssertEquals(Ord(drfText), Ord(ResultInfo.Options.Format));
  AssertEquals('sample.txt', ResultInfo.Options.OutputFile);
  AssertFalse(ResultInfo.Options.OutputToStdout);
  AssertEquals(Ord(lmTolerant), Ord(ResultInfo.Options.Mode));
  AssertEquals(Ord(iefAll), Ord(ResultInfo.Options.EntityFilter));
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeSummary);
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeLayers);
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeLinetypes);
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeLines);
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeArcs);
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeUnknown);
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeObjects);
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeWarnings);
  AssertFalse(ResultInfo.Options.Verbose);
end;

procedure TFPDWGCLITest.ParsesFormatModeLibraryAndLineFilter;
var
  ResultInfo: TDWGInspectorParseResult;
begin
  ResultInfo := ParseDWGInspectorArgs([
    'sample.dwg',
    '--format=json',
    '--mode=strict',
    '--lib=/opt/libredwg.so',
    '--entities=line',
    '--dump-unknown',
    '--verbose'
  ]);

  AssertTrue(ResultInfo.Success);
  AssertEquals(Ord(drfJSON), Ord(ResultInfo.Options.Format));
  AssertEquals(Ord(lmStrict), Ord(ResultInfo.Options.Mode));
  AssertEquals('/opt/libredwg.so', ResultInfo.Options.ReaderOptions.LibraryPath);
  AssertTrue(ResultInfo.Options.ReaderOptions.ReloadLibrary);
  AssertEquals(Ord(iefLine), Ord(ResultInfo.Options.EntityFilter));
  AssertTrue(ResultInfo.Options.DumpUnknown);
  AssertTrue(ResultInfo.Options.Verbose);
end;

procedure TFPDWGCLITest.ParsesArcFilter;
var
  ResultInfo: TDWGInspectorParseResult;
begin
  ResultInfo := ParseDWGInspectorArgs(['sample.dwg', '--entities=arc']);

  AssertTrue(ResultInfo.Success);
  AssertEquals(Ord(iefArc), Ord(ResultInfo.Options.EntityFilter));
end;

procedure TFPDWGCLITest.FormatSelectsDefaultOutputFileExtension;
var
  ResultInfo: TDWGInspectorParseResult;
begin
  ResultInfo := ParseDWGInspectorArgs(['sample.dwg', '--format=json']);

  AssertTrue(ResultInfo.Success);
  AssertEquals(Ord(drfJSON), Ord(ResultInfo.Options.Format));
  AssertEquals('sample.json', ResultInfo.Options.OutputFile);
  AssertFalse(ResultInfo.Options.OutputToStdout);
end;

procedure TFPDWGCLITest.OutputDashKeepsReportOnStdout;
var
  ResultInfo: TDWGInspectorParseResult;
begin
  ResultInfo := ParseDWGInspectorArgs([
    'sample.dwg',
    '--format=json',
    '--output=-'
  ]);

  AssertTrue(ResultInfo.Success);
  AssertTrue(ResultInfo.Options.OutputToStdout);
  AssertEquals('', ResultInfo.Options.OutputFile);
end;

procedure TFPDWGCLITest.SectionFlagsSwitchToExplicitSelection;
var
  ResultInfo: TDWGInspectorParseResult;
begin
  ResultInfo := ParseDWGInspectorArgs([
    'sample.dwg',
    '--summary',
    '--layers',
    '--warnings'
  ]);

  AssertTrue(ResultInfo.Success);
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeSummary);
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeLayers);
  AssertFalse(ResultInfo.Options.ReportOptions.IncludeLinetypes);
  AssertFalse(ResultInfo.Options.ReportOptions.IncludeLines);
  AssertFalse(ResultInfo.Options.ReportOptions.IncludeArcs);
  AssertFalse(ResultInfo.Options.ReportOptions.IncludeUnknown);
  AssertFalse(ResultInfo.Options.ReportOptions.IncludeObjects);
  AssertTrue(ResultInfo.Options.ReportOptions.IncludeWarnings);
end;

procedure TFPDWGCLITest.ShowOptionParsesHexHandle;
var
  ResultInfo: TDWGInspectorParseResult;
begin
  ResultInfo := ParseDWGInspectorArgs(['sample.dwg', '--show=2A']);

  AssertTrue(ResultInfo.Success);
  AssertTrue(ResultInfo.Options.HasObjectDetailHandle);
  AssertEquals(Int64($2A), Int64(ResultInfo.Options.ObjectDetailHandle));
end;

procedure TFPDWGCLITest.InvalidFormatFailsWithUsageExitCode;
var
  ResultInfo: TDWGInspectorParseResult;
begin
  ResultInfo := ParseDWGInspectorArgs(['sample.dwg', '--format=xml']);

  AssertFalse(ResultInfo.Success);
  AssertEquals(DWG_INSPECT_EXIT_USAGE, ResultInfo.ExitCode);
  AssertTrue(Pos('Unsupported format', ResultInfo.Message) > 0);
end;

procedure TFPDWGCLITest.MissingFileReturnsFileExitCode;
var
  Options: TDWGInspectorOptions;
  Logger: IDWGLogger;
  OutputText: string;
  ExitCode: Integer;
begin
  Options := TDWGInspectorOptions.Default;
  Options.InputFile := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'fpdwg-cli-missing-file.dwg';
  Logger := TDWGMemoryLogger.Create;

  ExitCode := RunDWGInspector(Options, OutputText, Logger);

  AssertEquals(DWG_INSPECT_EXIT_FILE, ExitCode);
  AssertEquals('', OutputText);
  Logger := nil;
end;

begin
  RegisterTests([TFPDWGCLITest]);
end.
