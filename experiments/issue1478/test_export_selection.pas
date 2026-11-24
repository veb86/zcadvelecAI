{
  Регрессия #1478: чистая логика команды export_selection.
  Проверяет разбор запроса, конфигурацию из окружения, allowlist путей
  (включая кириллицу), контракты JSON-ответов и удаление XDATA.
  Исходник в UTF-8 без $codepage: литералы остаются байтами UTF-8, как в ZCAD.
  Запуск: fpc -Fu../../cad_source/zcad/velec/IPCserver test_export_selection.pas
}
program test_export_selection;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Math, fpjson, jsonparser, uzvhttpexportselection;

var
  Failures: Integer = 0;
  Checks: Integer = 0;
  EnvValues: TStringList;

{ Фиксирует результат одной проверки }
procedure Check(ACondition: Boolean; const AName: string);
begin
  Inc(Checks);
  if ACondition then
    Exit;
  Inc(Failures);
  WriteLn('FAIL: ', AName);
end;

{ Подменённое окружение: значения берутся из EnvValues }
function FakeEnv(const AName: string): string;
begin
  Result := EnvValues.Values[AName];
end;

{ Загружает конфигурацию из пар NAME=VALUE }
function ConfigFrom(const APairs: array of string): TExportConfig;
var
  Pair: string;
begin
  EnvValues.Clear;
  for Pair in APairs do
    EnvValues.Add(Pair);
  Result := ExportConfigLoad(@FakeEnv, '/tmp/');
end;

{ Разбирает тело REST-запроса так же, как маршрут /api/export_selection }
function ParseBody(const ABody: string): TExportRequest;
var
  Args: TJSONArray;
begin
  Args := ExportArgsFromBody(ABody);
  try
    Result := ExportRequestParse(Args);
  finally
    Args.Free;
  end;
end;

{ Возвращает код ошибки, с которым падает разбор тела, или '' }
function ParseErrorCode(const ABody: string): string;
begin
  Result := '';
  try
    ParseBody(ABody);
  except
    on E: EExportSelection do
      Result := E.Code;
  end;
end;

{ Возвращает код ошибки разрешения пути или '' }
function PathErrorCode(const ABody: string; const AConfig: TExportConfig): string;
begin
  Result := '';
  try
    ExportResolvePath(ParseBody(ABody), AConfig);
  except
    on E: EExportSelection do
      Result := E.Code;
  end;
end;

{ Разрешает путь из тела запроса }
function ResolveBody(const ABody: string; const AConfig: TExportConfig): string;
begin
  Result := ExportResolvePath(ParseBody(ABody), AConfig);
end;

{ Разбор запроса: значения по умолчанию, типы полей и scope }
procedure TestRequestParsing;
var
  Req: TExportRequest;
begin
  Req := ParseBody('');
  Check(not Req.HasOutPath and Req.Overwrite and Req.IncludeXData, 'empty body defaults');
  Check(Req.Scope = EXPORT_SCOPE_SELECTION, 'default scope is selection');
  Req := ParseBody('{}');
  Check(not Req.HasOutPath and (Req.Scope = 'selection'), '{} defaults');
  Req := ParseBody('{"out_path":"/x/a.dxf","scope":"ALL","overwrite":false,' +
    '"include_xdata":false}');
  Check(Req.HasOutPath and (Req.OutPath = '/x/a.dxf'), 'out_path parsed');
  Check((Req.Scope = 'all') and not Req.Overwrite and not Req.IncludeXData, 'flags parsed');
  Req := ParseBody('{"out_path":null,"overwrite":null}');
  Check(not Req.HasOutPath and Req.Overwrite, 'null means default');
  Check(ParseErrorCode('{"scope":"layer"}') = ERR_INVALID_REQUEST, 'unknown scope');
  Check(ParseErrorCode('{"overwrite":"yes"}') = ERR_INVALID_REQUEST, 'bool type check');
  Check(ParseErrorCode('{"out_path":5}') = ERR_INVALID_REQUEST, 'string type check');
  Check(ParseErrorCode('{bad json') = ERR_INVALID_REQUEST, 'invalid JSON');
  Check(ParseErrorCode('[1,2]') = ERR_INVALID_REQUEST, 'array body rejected');
end;

{ Конфигурация: приоритет переменных, таймаут и корни allowlist }
procedure TestConfig;
var
  Cfg: TExportConfig;
begin
  Cfg := ConfigFrom([]);
  Check(Cfg.ExportDir = '/tmp/zcad_export', 'default dir in temp: ' + Cfg.ExportDir);
  Check(Cfg.FileName = 'selection.dxf', 'default file name');
  Check(Cfg.TimeoutSec = EXPORT_DEFAULT_TIMEOUT_SEC, 'default timeout');
  Check(ExportTimeoutMs(Cfg) = 60000, 'timeout in ms');
  Check((Length(Cfg.Roots) = 1) and (Cfg.Roots[0] = '/tmp/zcad_export'), 'root = dir');
  Cfg := ConfigFrom(['ZCAD_MCP_WORKSPACE=/w/ws/', 'ZCAD_EXPORT_TIMEOUT=90']);
  Check(Cfg.ExportDir = '/w/ws', 'workspace used as export dir');
  Check(Cfg.TimeoutSec = 90, 'timeout from env');
  Cfg := ConfigFrom(['ZCAD_EXPORT_DIR=/e', 'ZCAD_MCP_WORKSPACE=/w',
    'ZCAD_EXPORT_FILENAME=frag.dxf', 'ZCAD_EXPORT_TIMEOUT=0']);
  Check(Cfg.ExportDir = '/e', 'ZCAD_EXPORT_DIR wins');
  Check(Cfg.FileName = 'frag.dxf', 'file name from env');
  Check(Cfg.TimeoutSec = EXPORT_MIN_TIMEOUT_SEC, 'timeout clamped up');
  Check((Length(Cfg.Roots) = 2) and (Cfg.Roots[1] = '/w'), 'dir and workspace roots');
  Cfg := ConfigFrom(['ZCAD_EXPORT_ROOT=/r1:/r2', 'ZCAD_EXPORT_TIMEOUT=100000']);
  Check((Length(Cfg.Roots) = 2) and (Cfg.Roots[0] = '/r1'), 'explicit roots');
  Check(Cfg.TimeoutSec = EXPORT_MAX_TIMEOUT_SEC, 'timeout clamped down');
  Cfg := ConfigFrom(['ZCAD_EXPORT_TIMEOUT=abc']);
  Check(Cfg.TimeoutSec = EXPORT_DEFAULT_TIMEOUT_SEC, 'bad timeout uses default');
end;

{ Пути: default, относительные, кириллица, traversal и allowlist }
procedure TestPaths;
var
  Cfg: TExportConfig;
begin
  Cfg := ConfigFrom(['ZCAD_MCP_WORKSPACE=/home/u/проект/workspace']);
  Check(ResolveBody('{}', Cfg) = '/home/u/проект/workspace/selection.dxf', 'default path');
  Check(ResolveBody('{"out_path":"sub/фрагмент.dxf"}', Cfg) =
    '/home/u/проект/workspace/sub/фрагмент.dxf', 'relative cyrillic path');
  Check(ResolveBody('{"out_path":"/home/u/проект/workspace/a/../b.DXF"}', Cfg) =
    '/home/u/проект/workspace/b.DXF', 'normalized absolute path');
  Check(ResolveBody('{"out_path":"ф.dxf"}', Cfg) =
    '/home/u/проект/workspace/ф.dxf', 'escaped unicode path');
  Check(PathErrorCode('{"out_path":"../evil.dxf"}', Cfg) = ERR_INVALID_PATH, 'traversal');
  Check(PathErrorCode('{"out_path":"/etc/passwd.dxf"}', Cfg) = ERR_INVALID_PATH, 'outside');
  Check(PathErrorCode('{"out_path":"/home/u/проект/workspace2/a.dxf"}', Cfg) =
    ERR_INVALID_PATH, 'sibling prefix is not inside root');
  Check(PathErrorCode('{"out_path":"  "}', Cfg) = ERR_INVALID_PATH, 'empty out_path');
  Check(PathErrorCode('{"out_path":"a.txt"}', Cfg) = ERR_INVALID_PATH, 'extension');
  Check(PathErrorCode('{"out_path":"sub/"}', Cfg) = ERR_INVALID_PATH, 'no file name');
  Check(PathErrorCode('{"out_path":"a\u0001.dxf"}', Cfg) = ERR_INVALID_PATH, 'control char');
  Cfg := ConfigFrom(['ZCAD_EXPORT_ROOT=*']);
  Check(ResolveBody('{"out_path":"/srv/x.dxf"}', Cfg) = '/srv/x.dxf', 'any root allowed');
  Check(ExportPathInRoots('/a/b/c.dxf', ['/x', '/a/b']), 'second root matches');
  Check(not ExportPathInRoots('/a/bc.dxf', ['/a/b']), 'prefix needs delimiter');
end;

{ Возвращает разобранный JSON-ответ (вызывающий освобождает) }
function ParseResponse(const AText: string): TJSONObject;
begin
  Result := GetJSON(AText) as TJSONObject;
end;

{ JSON успешного ответа по контракту ТЗ 7.3 }
procedure TestSuccessJSON;
var
  Res: TExportResult;
  Obj: TJSONObject;
  Box: TJSONArray;
begin
  Res := Default(TExportResult);
  Res.Path := '/w/проект/selection.dxf';
  Res.Scope := 'selection';
  Res.Units := ExportUnitsName(4);
  Res.EntityCount := 1523;
  Res.HasBBox := True;
  Res.BBox.MaxX := 12000.000000000002;
  Res.BBox.MaxY := 8000;
  Res.BBox.MinX := -0.1;
  Res.IncludeXData := True;
  Res.FileSize := 2048;
  Obj := ParseResponse(ExportSuccessJSON(Res));
  try
    Check(Obj.Booleans['ok'] and (Obj.Strings['command'] = 'export_selection'), 'ok/command');
    Check(string(Obj.Strings['path']) = Res.Path, 'path roundtrip utf-8');
    Check((Obj.Strings['scope'] = 'selection') and (Obj.Integers['entity_count'] = 1523),
      'scope/entity_count');
    Box := Obj.Arrays['bbox'];
    Check((Box.Count = 4) and SameValue(Box.Floats[2], 12000), 'bbox values');
    Check(Pos('[-0.1, 0.0, 12000.0, 8000.0]', ExportSuccessJSON(Res)) > 0,
      'bbox compact: ' + ExportSuccessJSON(Res));
    Check(Obj.Strings['units'] = 'mm', 'units mm');
    Check(Obj.Strings['message'] = 'selection exported', 'message');
    Check(Obj.Int64s['file_size'] = 2048, 'file_size');
  finally
    Obj.Free;
  end;
  Res.Units := ExportUnitsName(0);
  Res.Scope := 'all';
  Obj := ParseResponse(ExportSuccessJSON(Res));
  try
    Check(Obj.Find('units') = nil, 'unspecified units omitted');
    Check(Obj.Strings['message'] = 'drawing exported', 'scope all message');
  finally
    Obj.Free;
  end;
end;

{ JSON ошибки по контракту ТЗ 7.4 и извлечение кода }
procedure TestErrorJSON;
var
  Obj: TJSONObject;
  Text: string;
begin
  Text := ExportErrorJSON(ERR_EMPTY_SELECTION, 'No entities selected');
  Obj := ParseResponse(Text);
  try
    Check(not Obj.Booleans['ok'] and (Obj.Strings['command'] = 'export_selection'), 'error ok');
    Check(Obj.Objects['error'].Strings['code'] = 'EMPTY_SELECTION', 'error code');
    Check(Obj.Objects['error'].Strings['message'] = 'No entities selected', 'error message');
  finally
    Obj.Free;
  end;
  Check(ExportErrorCodeOf(Text) = ERR_EMPTY_SELECTION, 'code extracted');
  Check(ExportErrorCodeOf('plain text') = '', 'non-JSON has no code');
  Check(ExportErrorCodeOf('{"error":"x"}') = '', 'string error has no code');
  Check(ExportUnitsName(99) = '', 'unknown units');
end;

{ Собирает DXF из пар строк с заданным переводом строки }
function MakeDXF(const ALines: array of string; const AEol: string): string;
var
  Line: string;
begin
  Result := '';
  for Line in ALines do
    Result := Result + Line + AEol;
end;

{ Удаление XDATA только в BLOCKS/ENTITIES, CRLF сохраняется }
procedure TestStripXData;
const
  SRC: array[0..27] of string = (
    '0', 'SECTION', '2', 'TABLES', '1001', 'KEEP_TABLE_APP', '0', 'ENDSEC',
    '0', 'SECTION', '2', 'ENTITIES', '0', 'LINE', '8', 'Слой',
    '1001', 'DSTP_XDATA', '1002', '{', '1000', 'NMO_Name=Щит', '1002', '}',
    '0', 'ENDSEC', '0', 'EOF');
var
  Output: string;
begin
  Output := ExportStripXData(MakeDXF(SRC, #13#10));
  Check(Pos('KEEP_TABLE_APP', Output) > 0, 'tables untouched');
  Check(Pos('DSTP_XDATA', Output) = 0, 'entity xdata removed');
  Check(Pos('NMO_Name', Output) = 0, 'xdata strings removed');
  Check(Pos('Слой', Output) > 0, 'layer kept');
  Check(Pos(#13#10, Output) > 0, 'CRLF preserved');
  Check(Pos('EOF', Output) > 0, 'EOF kept');
  Output := ExportStripXData(MakeDXF(SRC, #10));
  Check((Pos(#13, Output) = 0) and (Pos('1002', Output) = 0), 'LF input stays LF');
end;

begin
  { ZCAD работает с UTF-8 строками RTL (LazUTF8); повторяем это в тесте }
  SetMultiByteConversionCodePage(CP_UTF8);
  DefaultSystemCodePage := CP_UTF8;
  EnvValues := TStringList.Create;
  try
    TestRequestParsing;
    TestConfig;
    TestPaths;
    TestSuccessJSON;
    TestErrorJSON;
    TestStripXData;
  finally
    EnvValues.Free;
  end;
  WriteLn(Format('%d checks, %d failures', [Checks, Failures]));
  if Failures > 0 then
    Halt(1);
end.
