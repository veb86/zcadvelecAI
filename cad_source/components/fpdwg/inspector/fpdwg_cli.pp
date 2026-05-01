unit fpdwg_cli;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  dwg,
  fpdwg_types,
  fpdwg_logger,
  fpdwg_reader,
  fpdwg_reporter;

const
  DWG_INSPECT_EXIT_SUCCESS = 0;
  DWG_INSPECT_EXIT_USAGE = 1;
  DWG_INSPECT_EXIT_FILE = 2;
  DWG_INSPECT_EXIT_LIBRARY = 3;
  DWG_INSPECT_EXIT_PARSE = 4;
  DWG_INSPECT_EXIT_STRICT_VALIDATION = 5;
  DWG_INSPECT_EXIT_INTERNAL = 10;

type
  TDWGInspectorCommand = (
    icRun,
    icHelp
  );

  TDWGInspectorEntityFilter = (
    iefAll,
    iefLine
  );

  TDWGInspectorOptions = record
    InputFile: string;
    Format: TDWGReportFormat;
    Mode: TDWGLoadMode;
    ReportOptions: TDWGReportOptions;
    ReaderOptions: TDWGReaderOptions;
    Verbose: Boolean;
    DumpUnknown: Boolean;
    EntityFilter: TDWGInspectorEntityFilter;
    HasObjectDetailHandle: Boolean;
    ObjectDetailHandle: TDWGHandle;
    ResolveDepth: Integer;
    class function Default: TDWGInspectorOptions; static;
  end;

  TDWGInspectorParseResult = record
    Success: Boolean;
    ExitCode: Integer;
    Message: string;
    Command: TDWGInspectorCommand;
    Options: TDWGInspectorOptions;
    class function Ok(const AOptions: TDWGInspectorOptions
      ): TDWGInspectorParseResult; static;
    class function Help: TDWGInspectorParseResult; static;
    class function Failure(AExitCode: Integer; const AMessage: string
      ): TDWGInspectorParseResult; static;
  end;

function ParseDWGInspectorArgs(
  const Args: array of string): TDWGInspectorParseResult;
function DWGInspectorHelpText: string;
function RunDWGInspector(const Options: TDWGInspectorOptions;
  out OutputText: string; Logger: IDWGLogger = nil): Integer;
function RunDWGInspectorWithApi(const Options: TDWGInspectorOptions;
  out OutputText: string; Logger: IDWGLogger;
  Api: IDWGLibreDWGApi): Integer;

implementation

uses
  Classes,
  fpdwg_document,
  fpdwg_factory,
  fpdwg_filter,
  fpdwg_libredwg_utils,
  fpdwg_registry,
  fpdwg_resolver,
  fpdwg_validator,
  fpdwg_model_base;

class function TDWGInspectorOptions.Default: TDWGInspectorOptions;
begin
  Result.InputFile := '';
  Result.Format := drfText;
  Result.Mode := lmTolerant;
  Result.ReportOptions := TDWGReportOptions.Default;
  Result.ReaderOptions := TDWGReaderOptions.Default;
  Result.Verbose := False;
  Result.DumpUnknown := False;
  Result.EntityFilter := iefAll;
  Result.HasObjectDetailHandle := False;
  Result.ObjectDetailHandle := 0;
  Result.ResolveDepth := 1;
end;

class function TDWGInspectorParseResult.Ok(
  const AOptions: TDWGInspectorOptions): TDWGInspectorParseResult;
begin
  Result.Success := True;
  Result.ExitCode := DWG_INSPECT_EXIT_SUCCESS;
  Result.Message := '';
  Result.Command := icRun;
  Result.Options := AOptions;
end;

class function TDWGInspectorParseResult.Help: TDWGInspectorParseResult;
begin
  Result.Success := True;
  Result.ExitCode := DWG_INSPECT_EXIT_SUCCESS;
  Result.Message := '';
  Result.Command := icHelp;
  Result.Options := TDWGInspectorOptions.Default;
end;

class function TDWGInspectorParseResult.Failure(AExitCode: Integer;
  const AMessage: string): TDWGInspectorParseResult;
begin
  Result.Success := False;
  Result.ExitCode := AExitCode;
  Result.Message := AMessage;
  Result.Command := icRun;
  Result.Options := TDWGInspectorOptions.Default;
end;

function StartsWithText(const Value, Prefix: string): Boolean;
begin
  Result := Copy(Value, 1, Length(Prefix)) = Prefix;
end;

function SameOption(const Value, LongName, ShortName: string): Boolean;
begin
  Result := (Value = LongName) or ((ShortName <> '') and (Value = ShortName));
end;

function ConsumeValue(const Args: array of string; var Index: Integer;
  const Arg, Name: string; out Value: string;
  out ErrorMessage: string): Boolean;
var
  Prefix: string;
begin
  Prefix := Name + '=';
  if StartsWithText(Arg, Prefix) then
  begin
    Value := Copy(Arg, Length(Prefix) + 1, MaxInt);
    Exit(True);
  end;

  if Arg <> Name then
    Exit(False);

  if Index >= High(Args) then
  begin
    ErrorMessage := Format('Missing value for %s', [Name]);
    Exit(True);
  end;

  Inc(Index);
  Value := Args[Index];
  Result := True;
end;

function TryParseUInt(const Value: string; out Parsed: Integer): Boolean;
var
  Code: Integer;
begin
  Val(Value, Parsed, Code);
  Result := (Code = 0) and (Parsed >= 0);
end;

function HexDigitValue(C: Char; out Digit: Byte): Boolean;
begin
  case C of
    '0'..'9':
      Digit := Ord(C) - Ord('0');
    'a'..'f':
      Digit := Ord(C) - Ord('a') + 10;
    'A'..'F':
      Digit := Ord(C) - Ord('A') + 10;
  else
    Exit(False);
  end;
  Result := True;
end;

function TryParseHandleText(Value: string; out Handle: TDWGHandle): Boolean;
var
  I: Integer;
  Digit: Byte;
begin
  Handle := 0;
  if StartsWithText(Value, '0x') or StartsWithText(Value, '0X') then
    Delete(Value, 1, 2);
  if Value = '' then
    Exit(False);

  for I := 1 to Length(Value) do
  begin
    if not HexDigitValue(Value[I], Digit) then
      Exit(False);
    Handle := (Handle shl 4) or Digit;
  end;

  Result := True;
end;

procedure ClearReportOptions(var Options: TDWGReportOptions);
begin
  Options.IncludeSummary := False;
  Options.IncludeLayers := False;
  Options.IncludeLinetypes := False;
  Options.IncludeLines := False;
  Options.IncludeUnknown := False;
  Options.IncludeObjects := False;
  Options.IncludeWarnings := False;
end;

procedure EnsureExplicitReportSelection(var Options: TDWGInspectorOptions;
  var SelectionExplicit: Boolean);
begin
  if SelectionExplicit then
    Exit;
  ClearReportOptions(Options.ReportOptions);
  SelectionExplicit := True;
end;

function ParseFormat(const Value: string; var Options: TDWGInspectorOptions;
  out ErrorMessage: string): Boolean;
begin
  Result := True;
  if SameText(Value, 'text') then
    Options.Format := drfText
  else if SameText(Value, 'json') then
    Options.Format := drfJSON
  else
  begin
    ErrorMessage := Format('Unsupported format "%s"; expected text or json',
      [Value]);
    Result := False;
  end;
end;

function ParseMode(const Value: string; var Options: TDWGInspectorOptions;
  out ErrorMessage: string): Boolean;
begin
  Result := True;
  if SameText(Value, 'tolerant') then
    Options.Mode := lmTolerant
  else if SameText(Value, 'strict') then
    Options.Mode := lmStrict
  else
  begin
    ErrorMessage := Format('Unsupported mode "%s"; expected tolerant or strict',
      [Value]);
    Result := False;
  end;
end;

function ParseEntities(const Value: string; var Options: TDWGInspectorOptions;
  out ErrorMessage: string): Boolean;
begin
  Result := True;
  if SameText(Value, 'all') then
    Options.EntityFilter := iefAll
  else if SameText(Value, 'line') or SameText(Value, 'lines') then
    Options.EntityFilter := iefLine
  else
  begin
    ErrorMessage := Format('Unsupported entity filter "%s"; expected all or line',
      [Value]);
    Result := False;
  end;
end;

function ParseShowHandle(const Value: string;
  var Options: TDWGInspectorOptions; out ErrorMessage: string): Boolean;
var
  Handle: TDWGHandle;
begin
  if TryParseHandleText(Value, Handle) then
  begin
    Options.HasObjectDetailHandle := True;
    Options.ObjectDetailHandle := Handle;
    Exit(True);
  end;

  ErrorMessage := Format('Invalid handle "%s"', [Value]);
  Result := False;
end;

function ParseDWGInspectorArgs(
  const Args: array of string): TDWGInspectorParseResult;
var
  Options: TDWGInspectorOptions;
  SelectionExplicit: Boolean;
  I: Integer;
  Arg, Value, ErrorMessage: string;
  ParsedDepth: Integer;
begin
  Options := TDWGInspectorOptions.Default;
  SelectionExplicit := False;
  I := Low(Args);
  while I <= High(Args) do
  begin
    Arg := Args[I];
    Value := '';
    ErrorMessage := '';

    if SameOption(Arg, '--help', '-h') then
      Exit(TDWGInspectorParseResult.Help);

    if ConsumeValue(Args, I, Arg, '--format', Value, ErrorMessage) then
    begin
      if ErrorMessage <> '' then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          ErrorMessage));
      if not ParseFormat(Value, Options, ErrorMessage) then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          ErrorMessage));
    end
    else if ConsumeValue(Args, I, Arg, '--mode', Value, ErrorMessage) then
    begin
      if ErrorMessage <> '' then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          ErrorMessage));
      if not ParseMode(Value, Options, ErrorMessage) then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          ErrorMessage));
    end
    else if ConsumeValue(Args, I, Arg, '--lib', Value, ErrorMessage) then
    begin
      if ErrorMessage <> '' then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          ErrorMessage));
      Options.ReaderOptions.LibraryPath := Value;
      Options.ReaderOptions.ReloadLibrary := True;
    end
    else if ConsumeValue(Args, I, Arg, '--entities', Value, ErrorMessage) then
    begin
      if ErrorMessage <> '' then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          ErrorMessage));
      if not ParseEntities(Value, Options, ErrorMessage) then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          ErrorMessage));
    end
    else if ConsumeValue(Args, I, Arg, '--show', Value, ErrorMessage) or
            ConsumeValue(Args, I, Arg, '--object', Value, ErrorMessage) then
    begin
      if ErrorMessage <> '' then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          ErrorMessage));
      if not ParseShowHandle(Value, Options, ErrorMessage) then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          ErrorMessage));
    end
    else if ConsumeValue(Args, I, Arg, '--resolve-depth', Value,
            ErrorMessage) then
    begin
      if ErrorMessage <> '' then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          ErrorMessage));
      if not TryParseUInt(Value, ParsedDepth) then
        Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
          Format('Invalid resolve depth "%s"', [Value])));
      Options.ResolveDepth := ParsedDepth;
    end
    else if Arg = '--summary' then
    begin
      EnsureExplicitReportSelection(Options, SelectionExplicit);
      Options.ReportOptions.IncludeSummary := True;
    end
    else if Arg = '--stats' then
    begin
      EnsureExplicitReportSelection(Options, SelectionExplicit);
      Options.ReportOptions.IncludeSummary := True;
    end
    else if Arg = '--layers' then
    begin
      EnsureExplicitReportSelection(Options, SelectionExplicit);
      Options.ReportOptions.IncludeLayers := True;
    end
    else if Arg = '--linetypes' then
    begin
      EnsureExplicitReportSelection(Options, SelectionExplicit);
      Options.ReportOptions.IncludeLinetypes := True;
    end
    else if Arg = '--lines' then
    begin
      EnsureExplicitReportSelection(Options, SelectionExplicit);
      Options.ReportOptions.IncludeLines := True;
    end
    else if Arg = '--unknown' then
    begin
      EnsureExplicitReportSelection(Options, SelectionExplicit);
      Options.ReportOptions.IncludeUnknown := True;
    end
    else if Arg = '--objects' then
    begin
      EnsureExplicitReportSelection(Options, SelectionExplicit);
      Options.ReportOptions.IncludeObjects := True;
    end
    else if Arg = '--warnings' then
    begin
      EnsureExplicitReportSelection(Options, SelectionExplicit);
      Options.ReportOptions.IncludeWarnings := True;
    end
    else if Arg = '--all' then
    begin
      Options.ReportOptions := TDWGReportOptions.Default;
      SelectionExplicit := False;
    end
    else if Arg = '--verbose' then
      Options.Verbose := True
    else if Arg = '--dump-unknown' then
      Options.DumpUnknown := True
    else if Arg = '--shell' then
      Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
        'Interactive shell is not implemented in the MVP inspector CLI'))
    else if StartsWithText(Arg, '-') then
      Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
        Format('Unknown option "%s"', [Arg])))
    else if Options.InputFile = '' then
      Options.InputFile := Arg
    else
      Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
        Format('Unexpected positional argument "%s"', [Arg])));

    Inc(I);
  end;

  if Options.InputFile = '' then
    Exit(TDWGInspectorParseResult.Failure(DWG_INSPECT_EXIT_USAGE,
      'Input DWG file is required'));

  Result := TDWGInspectorParseResult.Ok(Options);
end;

function DWGInspectorHelpText: string;
begin
  Result :=
    'Usage: fpdwginspect <file.dwg> [options]' + LineEnding +
    LineEnding +
    'Output:' + LineEnding +
    '  --format=text|json       Report format, default text' + LineEnding +
    '  --summary                Include summary/statistics' + LineEnding +
    '  --layers                 Include layer list' + LineEnding +
    '  --linetypes              Include linetype list' + LineEnding +
    '  --lines                  Include LINE entities' + LineEnding +
    '  --unknown                Include unknown object list' + LineEnding +
    '  --objects                Include all object details in JSON' + LineEnding +
    '  --warnings               Include validation warnings' + LineEnding +
    '  --show=<handle>          Render one object by hex handle' + LineEnding +
    LineEnding +
    'Filtering and loading:' + LineEnding +
    '  --entities=all|line      Materialize all objects or LINE-focused model' + LineEnding +
    '  --mode=strict|tolerant   Duplicate/broken-ref policy, default tolerant' + LineEnding +
    '  --dump-unknown           Copy unknown raw bytes into diagnostics' + LineEnding +
    '  --lib=<path>             Explicit LibreDWG library path' + LineEnding +
    '  --resolve-depth=<n>      Accepted for script compatibility' + LineEnding +
    '  --verbose                Print info logs to stderr' + LineEnding +
    '  --help                   Show help' + LineEnding;
end;

function CreateFilter(const Options: TDWGInspectorOptions): TFilterStrategy;
begin
  case Options.EntityFilter of
    iefLine:
      Result := TFilterByDomainType.Create([
        dotLayer,
        dotLinetype,
        dotBlockHeader,
        dotLine
      ]);
  else
    Result := TFilterAll.Create;
  end;
end;

function BuildDocumentFromRaw(const Raw: Dwg_Data;
  const Options: TDWGInspectorOptions; Logger: IDWGLogger;
  Filter: TFilterStrategy): TDWGDocument;
var
  Factory: TDWGObjectFactory;
  Ctx: TDWGBuilderContext;
  Obj: TDWGObject;
  I: UInt32;
begin
  Result := TDWGDocument.Create(Options.Mode, Logger);
  try
    Result.FileName := Options.InputFile;
    Result.Version := DWGVersionFromLibre(Raw.header.version,
      Raw.header.from_version);
    Result.Codepage := Raw.header.codepage;
    Result.RawObjectCount := Raw.num_objects;
    Result.RawClassCount := Raw.num_classes;
    Result.RegisterSyntheticTables;

    if (Raw.num_objects > 0) and (Raw.&object = nil) then
      raise EDWGFactoryError.Create(
        'LibreDWG returned object count without object array');

    Factory := TDWGObjectFactory.CreateDefault;
    try
      I := 0;
      while I < Raw.num_objects do
      begin
        Ctx := TDWGBuilderContext.Default;
        Ctx.Version := Result.Version;
        Ctx.RawIndex := I;
        Ctx.Codepage := Result.Codepage;
        Ctx.Mode := Options.Mode;
        Ctx.DumpUnknown := Options.DumpUnknown;
        Ctx.Logger := Logger;

        Obj := Factory.CreateObject(Raw.&object[I], Ctx, Filter);
        if Obj <> nil then
        begin
          try
            Result.AddObject(Obj);
            Obj := nil;
          finally
            Obj.Free;
          end;
        end;

        Inc(I);
      end;
    finally
      Factory.Free;
    end;
  except
    Result.Free;
    raise;
  end;
end;

function CreateReporter(Document: TDWGDocument;
  Validation: TDWGValidationResult; const Options: TDWGInspectorOptions
  ): TDWGBaseReporter;
begin
  case Options.Format of
    drfJSON:
      Result := TDWGJSONReporter.Create(Document, Validation);
  else
    Result := TDWGTextReporter.Create(Document, Validation);
  end;
  Result.Options := Options.ReportOptions;
end;

function ValidationHasIssues(Validation: TDWGValidationResult): Boolean;
begin
  Result := (Validation <> nil) and
    ((Validation.BrokenRefCount > 0) or
     (Validation.OrphanCount > 0) or
     (Validation.CycleCount > 0));
end;

function ReadFailureExitCode(const ReadResult: TDWGReadResult): Integer;
begin
  case ReadResult.Status of
    drsFileNotFound:
      Result := DWG_INSPECT_EXIT_FILE;
    drsLibraryLoadFailed:
      Result := DWG_INSPECT_EXIT_LIBRARY;
    drsReadFailed:
      Result := DWG_INSPECT_EXIT_PARSE;
  else
    Result := DWG_INSPECT_EXIT_INTERNAL;
  end;
end;

function RunDWGInspectorWithApi(const Options: TDWGInspectorOptions;
  out OutputText: string; Logger: IDWGLogger;
  Api: IDWGLibreDWGApi): Integer;
var
  LocalLogger: IDWGLogger;
  Reader: TDWGReader;
  Raw: Dwg_Data;
  ReadResult: TDWGReadResult;
  Filter: TFilterStrategy;
  Document: TDWGDocument;
  Resolver: TDWGResolver;
  Validator: TDWGValidator;
  Validation: TDWGValidationResult;
  Reporter: TDWGBaseReporter;
begin
  OutputText := '';
  if Logger <> nil then
    LocalLogger := Logger
  else
    LocalLogger := TDWGConsoleLogger.Create(Options.Verbose);

  Reader := TDWGReader.Create(LocalLogger, Api);
  FillChar(Raw, SizeOf(Raw), 0);
  try
    try
      ReadResult := Reader.ReadFile(Options.InputFile, Raw,
        Options.ReaderOptions);
      if not ReadResult.Success then
        Exit(ReadFailureExitCode(ReadResult));

      Filter := nil;
      Document := nil;
      Validation := nil;
      try
        Filter := CreateFilter(Options);
        Document := BuildDocumentFromRaw(Raw, Options, LocalLogger, Filter);

        Resolver := TDWGResolver.Create(Document.Registry, LocalLogger);
        try
          Resolver.ResolveAll;
        finally
          Resolver.Free;
        end;

        Validator := TDWGValidator.Create(Document, LocalLogger);
        try
          Validation := Validator.Validate;
        finally
          Validator.Free;
        end;

        Reporter := CreateReporter(Document, Validation, Options);
        try
          if Options.HasObjectDetailHandle then
            OutputText := Reporter.RenderObjectDetail(
              Options.ObjectDetailHandle)
          else
            OutputText := Reporter.Render;
        finally
          Reporter.Free;
        end;

        if (Options.Mode = lmStrict) and ValidationHasIssues(Validation) then
          Result := DWG_INSPECT_EXIT_STRICT_VALIDATION
        else
          Result := DWG_INSPECT_EXIT_SUCCESS;
      finally
        Validation.Free;
        Document.Free;
        Filter.Free;
        Reader.FreeData(Raw);
      end;
    except
      on E: EDWGDuplicateHandleError do
      begin
        if LocalLogger <> nil then
          LocalLogger.Error(1201, E.Message);
        Result := DWG_INSPECT_EXIT_STRICT_VALIDATION;
      end;
      on E: Exception do
      begin
        if LocalLogger <> nil then
          LocalLogger.Error(1900, E.Message);
        Result := DWG_INSPECT_EXIT_INTERNAL;
      end;
    end;
  finally
    Reader.Free;
    LocalLogger := nil;
  end;
end;

function RunDWGInspector(const Options: TDWGInspectorOptions;
  out OutputText: string; Logger: IDWGLogger): Integer;
begin
  Result := RunDWGInspectorWithApi(Options, OutputText, Logger, nil);
end;

end.
