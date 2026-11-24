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
  HTTP IPC команда EXPORT_SELECTION (issue #1478).

  Выполняется главным потоком ZCAD через очередь HTTP IPC.
  Выгрузка повторяет штатный механизм CopyClip: выбранные сущности
  копируются во временный чертёж (CopyEnt переносит нужные слои,
  стили и определения блоков), после чего чертёж записывается
  штатным DXF-писателем savedxf20XX вместе с XDATA.

  Файл сначала пишется рядом с целевым под временным именем и только
  после проверки размера переименовывается в out_path, поэтому клиент
  никогда не получает успех для недописанного файла.
}

{$mode objfpc}{$H+}

unit uzvhttpcmdexportselection;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, fpjson;

{ Обработчик команды EXPORT_SELECTION (args пустой или из одного объекта параметров) }
function ExportSelectionCmd(AArgs: TJSONArray; out AResult, AError: string): Boolean;

implementation

uses
  Classes, LazUTF8, LazFileUtils,
  uzclog,
  uzcdrawings, uzedrawingsimple, uzeentity, uzeentgenericsubentry,
  gzctnrVectorTypes, uzgldrawcontext, uzegeometrytypes,
  uzeffDxfOut, uzeTypes, uzccommand_saveas,
  uzvhttpipc, uzvhttpexportselection;

const
  { Файл переменных, с которым ZCAD создаёт служебные чертежи (как в CopyClip) }
  SCRATCH_DRAWING_VARS = '$(DistribPath)/rtl/dwg/DrawingVars.pas';
  { Суффикс временного файла, который заменяет целевой после записи }
  EXPORT_TEMP_SUFFIX = '.tmp';
  EXPORT_LOG_PREFIX = '[HTTP-IPC] export_selection: ';
  EXPORT_LOG_OK = 'OK';

type
  { Состояние одной выгрузки }
  TExportJob = record
    Request: TExportRequest;
    Config: TExportConfig;
    Path: string;
    TempPath: string;
    Drawing: PTSimpleDrawing;
    Root: PGDBObjGenericSubEntry;
    Count: Integer;
    DXFCount: Integer;
    BBox: TBoundingBox;
    FileSize: Int64;
  end;

{ Читает переменную окружения в UTF-8 (пути могут содержать кириллицу) }
function ReadEnv(const AName: string): string;
begin
  Result := GetEnvironmentVariableUTF8(AName);
end;

{ Загружает актуальную конфигурацию выгрузки из окружения }
function LoadConfig: TExportConfig;
begin
  Result := ExportConfigLoad(@ReadEnv, GetTempDir(False));
end;

{ Таймаут команды для диспетчера HTTP IPC в миллисекундах }
function ExportTimeout: Cardinal;
begin
  Result := ExportTimeoutMs(LoadConfig);
end;

{ Создаёт исключение выгрузки с кодом ошибки из ТЗ }
function Fail(const ACode, AMessage: string): EExportSelection;
begin
  Result := EExportSelection.CreateCode(ACode, AMessage);
end;

{ Проверяет, входит ли сущность в область выгрузки }
function IsExported(AEntity: PGDBObjEntity; const AScope: string): Boolean;
begin
  Result := (AScope = EXPORT_SCOPE_ALL) or AEntity^.selected;
end;

{ Считает сущности области выгрузки и их общий габарит }
procedure MeasureEntities(var AJob: TExportJob);
var
  Entity: PGDBObjEntity;
  Iter: itrec;
begin
  AJob.Count := 0;
  Entity := AJob.Root^.ObjArray.beginiterate(Iter);
  while Entity <> nil do
  begin
    if IsExported(Entity, AJob.Request.Scope) then
    begin
      if AJob.Count = 0 then
        AJob.BBox := Entity^.vp.BoundingBox
      else
        AJob.BBox.CheckAndConcat(Entity^.vp.BoundingBox);
      Inc(AJob.Count);
    end;
    Entity := AJob.Root^.ObjArray.iterate(Iter);
  end;
end;

{ Находит активный чертёж и проверяет, что выгружать есть что }
procedure ResolveDocument(var AJob: TExportJob);
begin
  AJob.Drawing := drawings.GetCurrentDWG;
  if AJob.Drawing = nil then
    raise Fail(ERR_NO_DOCUMENT, 'No active drawing in ZCAD');
  AJob.Root := drawings.GetCurrentROOT;
  if AJob.Root = nil then
    raise Fail(ERR_NO_DOCUMENT, 'Active drawing has no model space');
  MeasureEntities(AJob);
  if AJob.Count > 0 then
    Exit;
  if AJob.Request.Scope = EXPORT_SCOPE_ALL then
    raise Fail(ERR_EMPTY_SELECTION, 'Drawing contains no entities');
  raise Fail(ERR_EMPTY_SELECTION, 'No entities selected');
end;

{ Проверяет целевой путь и создаёт родительский каталог }
procedure PrepareTarget(const AJob: TExportJob);
var
  Dir: string;
begin
  if DirectoryExistsUTF8(AJob.Path) then
    raise Fail(ERR_INVALID_PATH, 'out_path points to a directory: ' + AJob.Path);
  if FileExistsUTF8(AJob.Path) and not AJob.Request.Overwrite then
    raise Fail(ERR_FILE_EXISTS, 'File already exists and overwrite=false: ' + AJob.Path);
  Dir := ExtractFileDir(AJob.Path);
  if not ForceDirectoriesUTF8(Dir) then
    raise Fail(ERR_PATH_NOT_WRITABLE, 'Cannot create directory: ' + Dir);
end;

{ Возвращает уникальное имя временного файла рядом с целевым }
function TempPathFor(const APath: string): string;
begin
  Result := Format('%s.%d-%d%s', [APath, GetProcessID, GetTickCount64,
    EXPORT_TEMP_SUFFIX]);
end;

{ Копирует сущности области выгрузки в служебный чертёж (как CopyClip) }
function CopyEntities(const AJob: TExportJob; AScratch: PTSimpleDrawing): Integer;
var
  Entity, EntCopy: PGDBObjEntity;
  Iter: itrec;
  DC: TDrawContext;
begin
  Result := 0;
  DC := AJob.Drawing^.CreateDrawingRC(False, [DCODrawable]);
  Entity := AJob.Root^.ObjArray.beginiterate(Iter);
  while Entity <> nil do
  begin
    if IsExported(Entity, AJob.Request.Scope) then
    begin
      EntCopy := drawings.CopyEnt(AJob.Drawing, AScratch, Entity);
      if EntCopy <> nil then
        EntCopy^.FormatEntity(AJob.Drawing^, DC);
      if EntCopy <> nil then
        Inc(Result);
    end;
    Entity := AJob.Root^.ObjArray.iterate(Iter);
  end;
end;

{ Записывает служебный чертёж во временный файл штатным DXF-писателем }
function SaveScratch(var AJob: TExportJob): Boolean;
var
  Scratch: PTSimpleDrawing;
begin
  Scratch := PTSimpleDrawing(drawings.CreateDWG(SCRATCH_DRAWING_VARS, ''));
  try
    Scratch^.DXFCodePage := AJob.Drawing^.DXFCodePage;
    Scratch^.InsUnits := AJob.Drawing^.InsUnits;
    AJob.Count := CopyEntities(AJob, Scratch);
    Result := savedxf20XX(AJob.TempPath, getDxfTemplateFile(cDefaultSaveDxfVefsion),
      Scratch^, cDefaultSaveDxfVefsion);
  finally
    Scratch^.done;
    Freemem(Scratch);
  end;
end;

{ Выполняет запись DXF, переводя сбои писателя в код EXPORT_FAILED }
procedure WriteDXF(var AJob: TExportJob);
var
  Saved: Boolean;
begin
  try
    Saved := SaveScratch(AJob);
  except
    on E: Exception do
      raise Fail(ERR_EXPORT_FAILED, 'DXF export failed: ' + E.Message);
  end;
  if not Saved then
    raise Fail(ERR_PATH_NOT_WRITABLE, 'Cannot write file: ' + AJob.TempPath);
end;

{ Читает файл целиком без перекодирования }
function ReadFileBytes(const AFileName: string): string;
var
  Stream: TFileStream;
begin
  Result := '';
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Result[1], Stream.Size);
  finally
    Stream.Free;
  end;
end;

{ Записывает строку в файл без перекодирования }
procedure WriteFileBytes(const AFileName, AData: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    if AData <> '' then
      Stream.WriteBuffer(AData[1], Length(AData));
  finally
    Stream.Free;
  end;
end;

{ Удаляет XDATA из записанного файла, если клиент попросил include_xdata=false }
procedure ApplyXDataOption(const AJob: TExportJob);
begin
  if AJob.Request.IncludeXData then
    Exit;
  try
    WriteFileBytes(AJob.TempPath, ExportStripXData(ReadFileBytes(AJob.TempPath)));
  except
    on E: Exception do
      raise Fail(ERR_EXPORT_FAILED, 'Cannot remove XDATA: ' + E.Message);
  end;
end;

{ Считает объекты пространства модели в записанном файле (как ezdxf) }
procedure CountFileEntities(var AJob: TExportJob);
begin
  try
    AJob.DXFCount := ExportCountDXFEntities(ReadFileBytes(AJob.TempPath));
  except
    on E: Exception do
      raise Fail(ERR_EXPORT_FAILED, 'Cannot read exported DXF: ' + E.Message);
  end;
end;

{ Проверяет записанный файл и атомарно ставит его на место out_path }
procedure CommitFile(var AJob: TExportJob);
begin
  AJob.FileSize := FileSizeUtf8(AJob.TempPath);
  if AJob.FileSize <= 0 then
    raise Fail(ERR_EXPORT_FAILED, 'DXF writer produced an empty file');
  if FileExistsUTF8(AJob.Path) and not DeleteFileUTF8(AJob.Path) then
    raise Fail(ERR_PATH_NOT_WRITABLE, 'Cannot replace existing file: ' + AJob.Path);
  if not RenameFileUTF8(AJob.TempPath, AJob.Path) then
    raise Fail(ERR_PATH_NOT_WRITABLE, 'Cannot move exported file to ' + AJob.Path);
end;

{ Полный цикл выгрузки: запрос, путь, чертёж, запись, замена файла }
procedure RunExport(AArgs: TJSONArray; var AJob: TExportJob);
begin
  AJob.Config := LoadConfig;
  AJob.Request := ExportRequestParse(AArgs);
  AJob.Path := ExportResolvePath(AJob.Request, AJob.Config);
  ResolveDocument(AJob);
  PrepareTarget(AJob);
  AJob.TempPath := TempPathFor(AJob.Path);
  try
    WriteDXF(AJob);
    ApplyXDataOption(AJob);
    CountFileEntities(AJob);
    CommitFile(AJob);
  finally
    if FileExistsUTF8(AJob.TempPath) then
      DeleteFileUTF8(AJob.TempPath);
  end;
end;

{ Переносит итоги выгрузки в структуру JSON-ответа }
function BuildResult(const AJob: TExportJob; ADurationMs: Int64): TExportResult;
begin
  Result := Default(TExportResult);
  Result.Path := AJob.Path;
  Result.Scope := AJob.Request.Scope;
  Result.Units := ExportUnitsName(Ord(AJob.Drawing^.InsUnits));
  Result.EntityCount := AJob.Count;
  Result.DXFEntityCount := AJob.DXFCount;
  Result.HasBBox := True;
  Result.BBox.MinX := AJob.BBox.pMin.x;
  Result.BBox.MinY := AJob.BBox.pMin.y;
  Result.BBox.MaxX := AJob.BBox.pMax.x;
  Result.BBox.MaxY := AJob.BBox.pMax.y;
  Result.IncludeXData := AJob.Request.IncludeXData;
  Result.FileSize := AJob.FileSize;
  Result.DurationMs := ADurationMs;
end;

{ Пишет в журнал Bridge путь, число сущностей, длительность и код ошибки }
procedure LogOutcome(const AJob: TExportJob; ADurationMs: Int64; const AError: string);
var
  Code: string;
begin
  Code := ExportErrorCodeOf(AError);
  if Code = '' then
    Code := EXPORT_LOG_OK;
  ProgramLog.LogOutFormatStr(
    EXPORT_LOG_PREFIX + 'path="%s" entity_count=%d dxf_entity_count=%d ' +
    'duration_ms=%d code=%s',
    [AJob.Path, AJob.Count, AJob.DXFCount, ADurationMs, Code], LM_Info, 0);
end;

{ Обработчик команды EXPORT_SELECTION (args пустой или из одного объекта параметров) }
function ExportSelectionCmd(AArgs: TJSONArray; out AResult, AError: string): Boolean;
var
  Job: TExportJob;
  Started: QWord;
begin
  AResult := '';
  AError := '';
  Started := GetTickCount64;
  Job := Default(TExportJob);
  try
    RunExport(AArgs, Job);
    AResult := ExportSuccessJSON(BuildResult(Job, GetTickCount64 - Started));
  except
    on E: EExportSelection do
      AError := ExportErrorJSON(E.Code, E.Message);
    on E: Exception do
      AError := ExportErrorJSON(ERR_INTERNAL_ERROR, E.ClassName + ': ' + E.Message);
  end;
  Result := AError = '';
  LogOutcome(Job, GetTickCount64 - Started, AError);
end;

initialization
  RegisterHTTPIPCCommand(EXPORT_IPC_COMMAND, @ExportSelectionCmd);
  RegisterHTTPIPCCommandTimeout(EXPORT_IPC_COMMAND, @ExportTimeout);
end.
