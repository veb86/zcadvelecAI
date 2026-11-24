{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
}
{
  Чистая логика HTTP-команды export_selection (issue #1478).

  Модуль не зависит от ZCAD: здесь разбор запроса, конфигурация,
  проверка пути по allowlist, формирование JSON-ответов и удаление
  XDATA из готового DXF. Поэтому он проверяется обычным fpc в CI,
  а работа с чертежом вынесена в uzvhttpcmdexportselection.
}

{$mode objfpc}{$H+}

unit uzvhttpexportselection;

interface

uses
  SysUtils, Classes, Math, fpjson, jsonparser;

const
  { Имя операции в JSON-ответе и имя команды HTTP IPC }
  EXPORT_COMMAND_NAME = 'export_selection';
  EXPORT_IPC_COMMAND = 'EXPORT_SELECTION';
  EXPORT_REST_ROUTE = '/api/export_selection';

  { Коды ошибок из ТЗ (раздел 8) и код некорректного тела запроса }
  ERR_NO_DOCUMENT = 'NO_DOCUMENT';
  ERR_EMPTY_SELECTION = 'EMPTY_SELECTION';
  ERR_INVALID_PATH = 'INVALID_PATH';
  ERR_PATH_NOT_WRITABLE = 'PATH_NOT_WRITABLE';
  ERR_FILE_EXISTS = 'FILE_EXISTS';
  ERR_EXPORT_FAILED = 'EXPORT_FAILED';
  ERR_TIMEOUT = 'TIMEOUT';
  ERR_INTERNAL_ERROR = 'INTERNAL_ERROR';
  ERR_INVALID_REQUEST = 'INVALID_REQUEST';

  { Области выгрузки }
  EXPORT_SCOPE_SELECTION = 'selection';
  EXPORT_SCOPE_ALL = 'all';

  { Переменные окружения конфигурации Bridge }
  ENV_EXPORT_DIR = 'ZCAD_EXPORT_DIR';
  ENV_MCP_WORKSPACE = 'ZCAD_MCP_WORKSPACE';
  ENV_EXPORT_FILENAME = 'ZCAD_EXPORT_FILENAME';
  ENV_EXPORT_TIMEOUT = 'ZCAD_EXPORT_TIMEOUT';
  ENV_EXPORT_ROOT = 'ZCAD_EXPORT_ROOT';

  { Значения по умолчанию }
  EXPORT_DEFAULT_FILE_NAME = 'selection.dxf';
  EXPORT_DEFAULT_DIR_NAME = 'zcad_export';
  EXPORT_DXF_EXT = '.dxf';
  EXPORT_ANY_ROOT = '*';
  EXPORT_DEFAULT_TIMEOUT_SEC = 60;
  EXPORT_MIN_TIMEOUT_SEC = 1;
  EXPORT_MAX_TIMEOUT_SEC = 600;
  EXPORT_MS_PER_SECOND = 1000;

  { Диапазон групповых кодов DXF, занятых расширенными данными (XDATA) }
  DXF_XDATA_FIRST_CODE = 1000;
  DXF_XDATA_LAST_CODE = 1071;

type
  { Ошибка экспорта с машинно-читаемым кодом для JSON-ответа }
  EExportSelection = class(Exception)
  public
    Code: string;
    constructor CreateCode(const ACode, AMessage: string);
  end;

  { Разобранное тело запроса }
  TExportRequest = record
    OutPath: string;
    HasOutPath: Boolean;
    Scope: string;
    Overwrite: Boolean;
    IncludeXData: Boolean;
  end;

  { Конфигурация выгрузки, читаемая из окружения при каждом запросе }
  TExportConfig = record
    ExportDir: string;
    FileName: string;
    TimeoutSec: Integer;
    Roots: TStringArray;
  end;

  { Габарит выгруженных сущностей в WCS чертежа }
  TExportBBox = record
    MinX, MinY, MaxX, MaxY: Double;
  end;

  { Итог успешной выгрузки }
  TExportResult = record
    Path: string;
    Scope: string;
    Units: string;
    EntityCount: Integer;
    BBox: TExportBBox;
    HasBBox: Boolean;
    IncludeXData: Boolean;
    FileSize: Int64;
    DurationMs: Int64;
  end;

  { Источник переменных окружения (подменяется в тестах) }
  TExportEnvReader = function(const AName: string): string;

function ExportConfigLoad(AReader: TExportEnvReader;
  const ATempDir: string): TExportConfig;
function ExportTimeoutMs(const AConfig: TExportConfig): Cardinal;
function ExportRequestParse(AArgs: TJSONArray): TExportRequest;
function ExportArgsFromBody(const ABody: string): TJSONArray;
function ExportResolvePath(const ARequest: TExportRequest;
  const AConfig: TExportConfig): string;
function ExportPathInRoots(const APath: string;
  const ARoots: TStringArray): Boolean;
function ExportUnitsName(AInsUnits: Integer): string;
function ExportSuccessJSON(const AResult: TExportResult): string;
function ExportErrorJSON(const ACode, AMessage: string): string;
function ExportErrorCodeOf(const AJSON: string): string;
function ExportStripXData(const ADXF: string): string;

implementation

const
  { Обозначения единиц по порядку $INSUNITS (TInsUnits в uzbunits) }
  INS_UNIT_NAMES: array[0..20] of string = (
    '', 'in', 'ft', 'mi', 'mm', 'cm', 'm', 'km', 'uin', 'mil', 'yd',
    'angstrom', 'nm', 'um', 'dm', 'dam', 'hm', 'Gm', 'au', 'ly', 'pc');

  { Символы, запрещённые в путях Windows (кроме разделителя диска) }
  WINDOWS_BAD_PATH_CHARS = ['<', '>', '"', '|', '?', '*'];
  FIRST_PRINTABLE_CHAR = #32;
  JSON_ROUND_DIGITS = 15;
  JSON_FLOAT_SUFFIX = '.0';
  JSON_NULL = 'null';

type
  { Число JSON в коротком виде (12000.0 вместо 1.2000000000000000E+004) }
  TExportJSONFloat = class(TJSONFloatNumber)
  protected
    function GetAsString: TJSONStringType; override;
    function GetAsJSON: TJSONStringType; override;
  end;

{ Форматирует координату с 15 значащими цифрами и точкой как разделителем }
function FormatCoord(AValue: Double): string;
var
  Settings: TFormatSettings;
begin
  if IsNan(AValue) or IsInfinite(AValue) then
    Exit(JSON_NULL);
  Settings := DefaultFormatSettings;
  Settings.DecimalSeparator := '.';
  Settings.ThousandSeparator := #0;
  Result := FloatToStrF(AValue, ffGeneral, JSON_ROUND_DIGITS, 0, Settings);
  if (Pos('.', Result) = 0) and (Pos('E', Result) = 0) then
    Result := Result + JSON_FLOAT_SUFFIX;
end;

{ Возвращает текстовое представление числа без двоичного шума }
function TExportJSONFloat.GetAsString: TJSONStringType;
begin
  Result := FormatCoord(AsFloat);
end;

{ Возвращает представление числа внутри JSON-документа }
function TExportJSONFloat.GetAsJSON: TJSONStringType;
begin
  Result := FormatCoord(AsFloat);
end;

{ Создаёт исключение с кодом ошибки из ТЗ }
constructor EExportSelection.CreateCode(const ACode, AMessage: string);
begin
  inherited Create(AMessage);
  Code := ACode;
end;

{ Возвращает первое непустое значение из пары переменных окружения }
function FirstValue(AReader: TExportEnvReader; const AFirst, ASecond: string): string;
begin
  Result := Trim(AReader(AFirst));
  if Result = '' then
    Result := Trim(AReader(ASecond));
end;

{ Делит список корней по разделителю PATH и разворачивает каждый корень }
function SplitRoots(const AValue: string): TStringArray;
var
  Parts: TStringArray;
  Part: string;
begin
  Result := nil;
  Parts := AValue.Split([PathSeparator]);
  for Part in Parts do
  begin
    if Trim(Part) = '' then
      Continue;
    SetLength(Result, Length(Result) + 1);
    if Trim(Part) = EXPORT_ANY_ROOT then
      Result[High(Result)] := EXPORT_ANY_ROOT
    else
      Result[High(Result)] := ExpandFileName(Trim(Part));
  end;
end;

{ Ограничивает таймаут разумным диапазоном секунд }
function ClampTimeout(const AValue: string): Integer;
begin
  Result := StrToIntDef(Trim(AValue), EXPORT_DEFAULT_TIMEOUT_SEC);
  if Result < EXPORT_MIN_TIMEOUT_SEC then
    Result := EXPORT_MIN_TIMEOUT_SEC;
  if Result > EXPORT_MAX_TIMEOUT_SEC then
    Result := EXPORT_MAX_TIMEOUT_SEC;
end;

{ Собирает конфигурацию: каталог, имя файла, таймаут и allowlist корней }
function ExportConfigLoad(AReader: TExportEnvReader;
  const ATempDir: string): TExportConfig;
var
  Workspace: string;
begin
  Result.ExportDir := FirstValue(AReader, ENV_EXPORT_DIR, ENV_MCP_WORKSPACE);
  if Result.ExportDir = '' then
    Result.ExportDir := ConcatPaths([ATempDir, EXPORT_DEFAULT_DIR_NAME]);
  Result.ExportDir := ExcludeTrailingPathDelimiter(ExpandFileName(Result.ExportDir));
  Result.FileName := Trim(AReader(ENV_EXPORT_FILENAME));
  if Result.FileName = '' then
    Result.FileName := EXPORT_DEFAULT_FILE_NAME;
  Result.TimeoutSec := ClampTimeout(AReader(ENV_EXPORT_TIMEOUT));
  Result.Roots := SplitRoots(AReader(ENV_EXPORT_ROOT));
  if Length(Result.Roots) > 0 then
    Exit;
  Result.Roots := SplitRoots(Result.ExportDir);
  Workspace := Trim(AReader(ENV_MCP_WORKSPACE));
  if Workspace <> '' then
    Result.Roots := Concat(Result.Roots, SplitRoots(Workspace));
end;

{ Переводит таймаут конфигурации в миллисекунды для диспетчера IPC }
function ExportTimeoutMs(const AConfig: TExportConfig): Cardinal;
begin
  Result := Cardinal(AConfig.TimeoutSec) * EXPORT_MS_PER_SECOND;
end;

{ Проверяет, что поле отсутствует или имеет ожидаемый тип JSON }
function FieldOfType(AObject: TJSONObject; const AName: string;
  AType: TJSONtype; out AData: TJSONData): Boolean;
begin
  AData := AObject.Find(AName);
  Result := AData <> nil;
  if Result and (AData.JSONType = jtNull) then
    Exit(False);
  if Result and (AData.JSONType <> AType) then
    raise EExportSelection.CreateCode(ERR_INVALID_REQUEST,
      Format('Field "%s" has wrong type', [AName]));
end;

{ Возвращает объект параметров из args; пустой args означает пустой объект }
function ParamsObject(AArgs: TJSONArray): TJSONObject;
begin
  Result := nil;
  if (AArgs = nil) or (AArgs.Count = 0) then
    Exit;
  if (AArgs.Count > 1) or not (AArgs.Items[0].JSONType in [jtObject, jtNull]) then
    raise EExportSelection.CreateCode(ERR_INVALID_REQUEST,
      'args must contain one JSON object with export parameters');
  if AArgs.Items[0].JSONType = jtObject then
    Result := TJSONObject(AArgs.Items[0]);
end;

{ Разбирает параметры запроса; все поля необязательны }
function ExportRequestParse(AArgs: TJSONArray): TExportRequest;
var
  Params: TJSONObject;
  Data: TJSONData;
begin
  Result := Default(TExportRequest);
  Result.Scope := EXPORT_SCOPE_SELECTION;
  Result.Overwrite := True;
  Result.IncludeXData := True;
  Params := ParamsObject(AArgs);
  if Params = nil then
    Exit;
  Result.HasOutPath := FieldOfType(Params, 'out_path', jtString, Data);
  if Result.HasOutPath then
    Result.OutPath := Data.AsString;
  if FieldOfType(Params, 'scope', jtString, Data) then
    Result.Scope := LowerCase(Trim(Data.AsString));
  if FieldOfType(Params, 'overwrite', jtBoolean, Data) then
    Result.Overwrite := Data.AsBoolean;
  if FieldOfType(Params, 'include_xdata', jtBoolean, Data) then
    Result.IncludeXData := Data.AsBoolean;
  if (Result.Scope <> EXPORT_SCOPE_SELECTION) and (Result.Scope <> EXPORT_SCOPE_ALL) then
    raise EExportSelection.CreateCode(ERR_INVALID_REQUEST,
      'scope must be "selection" or "all"');
end;

{ Превращает тело REST-запроса в args; пустое тело равно пустому объекту }
function ExportArgsFromBody(const ABody: string): TJSONArray;
var
  Data: TJSONData;
begin
  Result := TJSONArray.Create;
  if Trim(ABody) = '' then
    Exit;
  try
    Data := GetJSON(ABody);
  except
    on E: Exception do
    begin
      Result.Free;
      raise EExportSelection.CreateCode(ERR_INVALID_REQUEST, 'Invalid JSON: ' + E.Message);
    end;
  end;
  Result.Add(Data);
end;

{ Определяет абсолютный путь без обращения к текущему каталогу процесса }
function IsAbsolutePath(const APath: string): Boolean;
begin
  {$IFDEF WINDOWS}
  Result := ((Length(APath) >= 3) and (APath[2] = ':') and
    (APath[3] in AllowDirectorySeparators)) or
    ((Length(APath) >= 2) and (APath[1] in AllowDirectorySeparators) and
    (APath[2] in AllowDirectorySeparators));
  {$ELSE}
  Result := (APath <> '') and (APath[1] = '/');
  {$ENDIF}
end;

{ Проверяет отсутствие управляющих и запрещённых в Windows символов }
function HasBadChars(const APath: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 1 to Length(APath) do
  begin
    if APath[I] < FIRST_PRINTABLE_CHAR then
      Exit(True);
    {$IFDEF WINDOWS}
    if (APath[I] in WINDOWS_BAD_PATH_CHARS) or ((APath[I] = ':') and (I <> 2)) then
      Exit(True);
    {$ENDIF}
  end;
end;

{ Сравнивает пути с учётом регистра ОС }
function SamePathPrefix(const APath, ARoot: string): Boolean;
var
  Root: string;
begin
  Root := IncludeTrailingPathDelimiter(ARoot);
  {$IFDEF WINDOWS}
  Result := SameText(Copy(APath, 1, Length(Root)), Root);
  {$ELSE}
  Result := Copy(APath, 1, Length(Root)) = Root;
  {$ENDIF}
end;

{ Проверяет, что путь лежит внутри одного из разрешённых корней }
function ExportPathInRoots(const APath: string;
  const ARoots: TStringArray): Boolean;
var
  Root: string;
begin
  Result := False;
  for Root in ARoots do
  begin
    if Root = EXPORT_ANY_ROOT then
      Exit(True);
    if SamePathPrefix(APath, Root) then
      Exit(True);
  end;
end;

{ Возвращает исходный путь из запроса или путь по умолчанию }
function RequestedPath(const ARequest: TExportRequest;
  const AConfig: TExportConfig): string;
begin
  if not ARequest.HasOutPath then
    Exit(AConfig.FileName);
  Result := Trim(ARequest.OutPath);
  if Result = '' then
    raise EExportSelection.CreateCode(ERR_INVALID_PATH, 'out_path is empty');
end;

{ Нормализует путь к абсолютному и проверяет имя, расширение и allowlist }
function ExportResolvePath(const ARequest: TExportRequest;
  const AConfig: TExportConfig): string;
begin
  Result := RequestedPath(ARequest, AConfig);
  if HasBadChars(Result) then
    raise EExportSelection.CreateCode(ERR_INVALID_PATH, 'out_path contains invalid characters');
  if not IsAbsolutePath(Result) then
    Result := ConcatPaths([AConfig.ExportDir, Result]);
  Result := ExpandFileName(Result);
  if ExtractFileName(Result) = '' then
    raise EExportSelection.CreateCode(ERR_INVALID_PATH, 'out_path has no file name');
  if not SameText(ExtractFileExt(Result), EXPORT_DXF_EXT) then
    raise EExportSelection.CreateCode(ERR_INVALID_PATH, 'out_path must have .dxf extension');
  if not ExportPathInRoots(Result, AConfig.Roots) then
    raise EExportSelection.CreateCode(ERR_INVALID_PATH,
      'out_path is outside the allowed export root (' + ENV_EXPORT_ROOT + ')');
end;

{ Возвращает краткое обозначение единиц чертежа или пустую строку }
function ExportUnitsName(AInsUnits: Integer): string;
begin
  Result := '';
  if (AInsUnits >= Low(INS_UNIT_NAMES)) and (AInsUnits <= High(INS_UNIT_NAMES)) then
    Result := INS_UNIT_NAMES[AInsUnits];
end;

{ Формирует массив bbox [minx, miny, maxx, maxy] }
function BBoxJSON(const ABox: TExportBBox): TJSONArray;
begin
  Result := TJSONArray.Create;
  Result.Add(TExportJSONFloat.Create(ABox.MinX));
  Result.Add(TExportJSONFloat.Create(ABox.MinY));
  Result.Add(TExportJSONFloat.Create(ABox.MaxX));
  Result.Add(TExportJSONFloat.Create(ABox.MaxY));
end;

{ Возвращает человекочитаемое сообщение для области выгрузки }
function SuccessMessage(const AScope: string): string;
begin
  if AScope = EXPORT_SCOPE_ALL then
    Result := 'drawing exported'
  else
    Result := 'selection exported';
end;

{ Формирует JSON успешного ответа по контракту ТЗ (раздел 7.3) }
function ExportSuccessJSON(const AResult: TExportResult): string;
var
  Response: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Response.Add('ok', True);
    Response.Add('command', EXPORT_COMMAND_NAME);
    Response.Add('path', AResult.Path);
    Response.Add('scope', AResult.Scope);
    Response.Add('entity_count', AResult.EntityCount);
    if AResult.HasBBox then
      Response.Add('bbox', BBoxJSON(AResult.BBox));
    if AResult.Units <> '' then
      Response.Add('units', AResult.Units);
    Response.Add('include_xdata', AResult.IncludeXData);
    Response.Add('file_size', AResult.FileSize);
    Response.Add('duration_ms', AResult.DurationMs);
    Response.Add('message', SuccessMessage(AResult.Scope));
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;

{ Формирует JSON ошибки по контракту ТЗ (раздел 7.4) }
function ExportErrorJSON(const ACode, AMessage: string): string;
var
  Response, Error: TJSONObject;
begin
  Response := TJSONObject.Create;
  try
    Error := TJSONObject.Create;
    Error.Add('code', ACode);
    Error.Add('message', AMessage);
    Response.Add('ok', False);
    Response.Add('command', EXPORT_COMMAND_NAME);
    Response.Add('error', Error);
    Result := Response.AsJSON;
  finally
    Response.Free;
  end;
end;

{ Извлекает код ошибки из JSON ответа; для журнала и тестов }
function ExportErrorCodeOf(const AJSON: string): string;
var
  Data, Code: TJSONData;
begin
  Result := '';
  try
    Data := GetJSON(AJSON);
  except
    Exit;
  end;
  try
    Code := Data.FindPath('error.code');
    if (Code <> nil) and (Code.JSONType = jtString) then
      Result := Code.AsString;
  finally
    Data.Free;
  end;
end;

{ Возвращает True, если групповой код DXF относится к XDATA }
function IsXDataCode(const ACode: string): Boolean;
var
  Code: Integer;
begin
  Code := StrToIntDef(Trim(ACode), -1);
  Result := (Code >= DXF_XDATA_FIRST_CODE) and (Code <= DXF_XDATA_LAST_CODE);
end;

{ Обновляет имя текущей секции DXF по паре "код/значение" }
procedure TrackSection(const ACode, AValue, APrevValue: string; var ASection: string);
begin
  if Trim(ACode) = '0' then
  begin
    if Trim(AValue) = 'ENDSEC' then
      ASection := '';
    Exit;
  end;
  if (Trim(ACode) = '2') and (Trim(APrevValue) = 'SECTION') then
    ASection := Trim(AValue);
end;

{ Удаляет пары XDATA (коды 1000-1071) из секций BLOCKS и ENTITIES }
function ExportStripXData(const ADXF: string): string;
var
  Lines, Output: TStringList;
  I: Integer;
  Section, PrevValue: string;
begin
  Lines := TStringList.Create;
  Output := TStringList.Create;
  try
    Lines.Text := ADXF;
    Output.TextLineBreakStyle := Lines.TextLineBreakStyle;
    if Pos(#13#10, ADXF) > 0 then
      Output.TextLineBreakStyle := tlbsCRLF;
    Section := '';
    PrevValue := '';
    I := 0;
    while I + 1 < Lines.Count do
    begin
      TrackSection(Lines[I], Lines[I + 1], PrevValue, Section);
      PrevValue := Lines[I + 1];
      if not (((Section = 'ENTITIES') or (Section = 'BLOCKS')) and IsXDataCode(Lines[I])) then
        Output.AddStrings([Lines[I], Lines[I + 1]]);
      Inc(I, 2);
    end;
    if I < Lines.Count then
      Output.Add(Lines[I]);
    Result := Output.Text;
  finally
    Output.Free;
    Lines.Free;
  end;
end;

end.
