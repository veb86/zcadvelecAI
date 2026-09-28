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
  Модуль: uzeffdxfnodregistry
  Назначение: реестр NOD-обработчиков (этап 3 ТЗ
  cad_source/zengine/TZ_NOD_NamedObjectDictionary.md, контракт — раздел 4.3).

  Обработчик регистрируется по ключу корневого словаря (NOD), например
  ACAD_TABLESTYLE, в initialization своего модуля (как
  RegisterObjectsSaveDxfProc в uzeacadtable_dxf_write) и получает:

  * чтение (AddFromDXF, DXF 2000+, до ENTITIES) — LoadProc со словарём-веткой
    своего ключа, если ключ есть в NOD загружаемого файла и ссылается на
    словарь. Хэндл словаря-ветки реестр помечает «забранным», хэндлы
    объектов ветки помечает сам обработчик. Незарегистрированные ключи
    пропускаются (до этапа 7). Исключение в LoadProc загрузку не прерывает;
  * запись (savedxf20XX) — через TZNODSaveSession:
    ReserveHandlesProc — до ENTITIES (там же, где
    PreallocateTableStyleHandles), не более одного раза за сохранение;
    ClassesProc — перед ENDSEC секции CLASSES;
    SaveProc — перед ENDSEC секции OBJECTS (до RunObjectsSaveDxfProcs).
    Обработчики, у которых MinVersion выше версии сохраняемого файла, при
    записи не вызываются, в лог пишется предупреждение.

  Обработчики вызываются в порядке регистрации — хэндлы в файле
  детерминированы. Реестр сам хэндлы не выделяет: все хэндлы обработчик
  берёт из IODXFContext.handle.

  Детальная трасса — модуль лога NOD (lem NOD), выключен по умолчанию.
}
unit uzeffdxfnodregistry;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}
interface

uses
  SysUtils,
  uzeTypes,
  uzctnrVectorBytesStream,
  uzedrawingsimple,
  uzeffdxfsupport,
  uzeffdxfnod;

const
  { Минимальная версия DXF обработчика по умолчанию: DXF 2000 }
  CNODHandlerDefaultMinVersion = AC1015;

type
  { Чтение: перенос данных ветки ADict (словарь по ключу обработчика в NOD)
    в чертёж. Вызывается до разбора ENTITIES. }
  TNODLoadProc = procedure(const AModel: TZNODModel;
    const ADict: TZDXFDictionary; var ADrawing: TSimpleDrawing);
  { Запись, до ENTITIES: выделяет хэндлы словаря-ветки и её объектов через
    AIODXFContext.handle и заполняет карты имя → хэндл. Возвращает хэндл
    словаря-ветки (0 — ветка не пишется). Реестр вызывает её не более
    одного раза за сохранение. }
  TNODReserveHandlesProc = function(var ADrawing: TSimpleDrawing;
    var AIODXFContext: TIODXFSaveContext): TDWGHandle;
  { Запись, секция OBJECTS: словарь-ветка и её объекты.
    ADictHandle — результат ReserveHandlesProc, ANODHandle — новый хэндл
    корневого словаря (NOD) в сохраняемом файле (0 — не найден). }
  TNODSaveProc = procedure(var AOutStream: TZctnrVectorBytes;
    var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext;
    ADictHandle, ANODHandle: TDWGHandle);
  { Запись, секция CLASSES (необязательная): объявление классов. }
  TNODClassesProc = procedure(var AOutStream: TZctnrVectorBytes;
    var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext);

  { Описание NOD-обработчика. Любая процедура может быть nil. }
  TZNODHandler = record
    { Ключ записи в NOD ('ACAD_TABLESTYLE'); сравнивается без учёта
      регистра, как ключи словарей }
    Key: string;
    { Тип объектов ветки ('TABLESTYLE') — для лога и проверок обработчика }
    ObjectType: string;
    { Минимальная версия DXF, в которой ветка пишется }
    MinVersion: TACDWGVer;
    { Имя обязательной записи ('Standard'), '' — нет }
    DefaultName: string;
    LoadProc: TNODLoadProc;
    ReserveHandlesProc: TNODReserveHandlesProc;
    SaveProc: TNODSaveProc;
    ClassesProc: TNODClassesProc;
  end;
  TZNODHandlers = array of TZNODHandler;

  { Состояние NOD-обработчиков на время одного сохранения DXF.
    Список обработчиков фиксируется при создании (только подходящие по
    версии), поэтому регистрация во время сохранения на него не влияет. }
  TZNODSaveSession = class
  private
    FVersion: TACDWGVer;
    FHandlers: TZNODHandlers;
    FDictHandles: array of TDWGHandle;
    FHandlesReserved: Boolean;
    FObjectsWritten: Boolean;
    FTemplateNODHandle: TDWGHandle;
    function GetCount: Integer;
    function GetHandler(AIndex: Integer): TZNODHandler;
    function GetDictHandle(AIndex: Integer): TDWGHandle;
  public
    constructor Create(AVersion: TACDWGVer);
    { Разбирает секцию OBJECTS шаблона моделью этапа 1 и запоминает
      исходный хэндл его NOD (TemplateNODHandle). Если обработчиков нет,
      шаблон не читается. False — NOD в шаблоне не найден. }
    function LoadTemplate(const ATemplateFileName: string): Boolean;
    { Вызывает ReserveHandlesProc всех обработчиков. Идемпотентна: вызовы
      после первого ничего не делают. }
    procedure ReserveHandles(var ADrawing: TSimpleDrawing;
      var AIODXFContext: TIODXFSaveContext);
    { Вызывает ClassesProc всех обработчиков. }
    procedure WriteClasses(var AOutStream: TZctnrVectorBytes;
      var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext);
    { Вызывает SaveProc всех обработчиков (при необходимости сначала
      ReserveHandles). ANODHandle — новый хэндл NOD в сохраняемом файле.
      Идемпотентна. }
    procedure WriteObjects(var AOutStream: TZctnrVectorBytes;
      var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext;
      ANODHandle: TDWGHandle);

    property Version: TACDWGVer read FVersion;
    { Количество обработчиков, участвующих в сохранении }
    property Count: Integer read GetCount;
    property Handlers[AIndex: Integer]: TZNODHandler read GetHandler;
    { Хэндл словаря-ветки, выделенный ReserveHandlesProc (0 — нет) }
    property DictHandles[AIndex: Integer]: TDWGHandle read GetDictHandle;
    property HandlesReserved: Boolean read FHandlesReserved;
    property ObjectsWritten: Boolean read FObjectsWritten;
    { Исходный хэндл NOD шаблона (0 — шаблон не читался или NOD нет) }
    property TemplateNODHandle: TDWGHandle read FTemplateNODHandle;
  end;

{ Регистрирует обработчик. False (и предупреждение в лог) — пустой ключ или
  обработчик с таким ключом уже зарегистрирован. }
function RegisterNODHandler(const AHandler: TZNODHandler): Boolean; overload;
function RegisterNODHandler(const AKey, AObjectType: string;
  ALoadProc: TNODLoadProc; AReserveHandlesProc: TNODReserveHandlesProc;
  ASaveProc: TNODSaveProc; AClassesProc: TNODClassesProc = nil;
  AMinVersion: TACDWGVer = CNODHandlerDefaultMinVersion;
  const ADefaultName: string = ''): Boolean; overload;
{ Удаляет обработчик (для тестов и выгрузки модулей). False — не найден. }
function UnregisterNODHandler(const AKey: string): Boolean;
function NODHandlerCount: Integer;
{ Обработчик по индексу в порядке регистрации }
function GetNODHandler(AIndex: Integer): TZNODHandler;
function FindNODHandler(const AKey: string; out AHandler: TZNODHandler): Boolean;

{ Чтение: для каждого зарегистрированного ключа, найденного в NOD модели и
  ссылающегося на словарь, вызывает LoadProc (в порядке регистрации).
  Возвращает количество вызванных LoadProc. AModel = nil или модель без
  NOD — ничего не вызывается. }
function RunNODLoadHandlers(AModel: TZNODModel;
  var ADrawing: TSimpleDrawing): Integer;

{ Имя версии DXF для лога ('AC1015') }
function ACDWGVerName(AVersion: TACDWGVer): string;

implementation

uses
  Classes,
  TypInfo,
  uzeffdxfobjects,
  uzeffdxfnodlog;

var
  NODHandlers: TZNODHandlers;

function ACDWGVerName(AVersion: TACDWGVer): string;
begin
  Result := GetEnumName(TypeInfo(TACDWGVer), Ord(AVersion));
end;

function IndexOfNODHandler(const AKey: string): Integer;
var
  I: Integer;
begin
  for I := 0 to High(NODHandlers) do
    if SameText(NODHandlers[I].Key, AKey) then
      Exit(I);
  Result := -1;
end;

function RegisterNODHandler(const AHandler: TZNODHandler): Boolean;
var
  I: Integer;
begin
  if Trim(AHandler.Key) = '' then begin
    NODLogWarningFormatStr(
      'uzeffdxfnodregistry: NOD handler with empty key is not registered', []);
    Exit(False);
  end;
  if IndexOfNODHandler(AHandler.Key) >= 0 then begin
    NODLogWarningFormatStr(
      'uzeffdxfnodregistry: NOD handler for key "%s" is already registered',
      [AHandler.Key]);
    Exit(False);
  end;
  I := Length(NODHandlers);
  SetLength(NODHandlers, I + 1);
  NODHandlers[I] := AHandler;
  NODLogTraceFormatStr(
    'uzeffdxfnodregistry: registered NOD handler #%d "%s" (%s, min %s)',
    [I, AHandler.Key, AHandler.ObjectType, ACDWGVerName(AHandler.MinVersion)]);
  Result := True;
end;

function RegisterNODHandler(const AKey, AObjectType: string;
  ALoadProc: TNODLoadProc; AReserveHandlesProc: TNODReserveHandlesProc;
  ASaveProc: TNODSaveProc; AClassesProc: TNODClassesProc;
  AMinVersion: TACDWGVer; const ADefaultName: string): Boolean;
var
  H: TZNODHandler;
begin
  H.Key := AKey;
  H.ObjectType := AObjectType;
  H.MinVersion := AMinVersion;
  H.DefaultName := ADefaultName;
  H.LoadProc := ALoadProc;
  H.ReserveHandlesProc := AReserveHandlesProc;
  H.SaveProc := ASaveProc;
  H.ClassesProc := AClassesProc;
  Result := RegisterNODHandler(H);
end;

function UnregisterNODHandler(const AKey: string): Boolean;
var
  I, J: Integer;
begin
  I := IndexOfNODHandler(AKey);
  if I < 0 then
    Exit(False);
  for J := I to High(NODHandlers) - 1 do
    NODHandlers[J] := NODHandlers[J + 1];
  SetLength(NODHandlers, Length(NODHandlers) - 1);
  Result := True;
end;

function NODHandlerCount: Integer;
begin
  Result := Length(NODHandlers);
end;

function GetNODHandler(AIndex: Integer): TZNODHandler;
begin
  Result := NODHandlers[AIndex];
end;

function FindNODHandler(const AKey: string; out AHandler: TZNODHandler): Boolean;
var
  I: Integer;
begin
  I := IndexOfNODHandler(AKey);
  Result := I >= 0;
  if Result then
    AHandler := NODHandlers[I]
  else
    AHandler := Default(TZNODHandler);
end;

function RunNODLoadHandlers(AModel: TZNODModel;
  var ADrawing: TSimpleDrawing): Integer;
var
  I: Integer;
  Handlers: TZNODHandlers;
  Entry: TZDXFDictEntry;
  Dict: TZDXFDictionary;
begin
  Result := 0;
  if (AModel = nil) or (AModel.NOD = nil) then begin
    NODLogTraceFormatStr('uzeffdxfnodregistry: load: no NOD, handlers skipped', []);
    Exit;
  end;
  { Копия списка: обработчик может (от)регистрировать обработчики }
  Handlers := Copy(NODHandlers);
  for I := 0 to AModel.NOD.Count - 1 do
    if IndexOfNODHandler(AModel.NOD[I].Key) < 0 then
      NODLogTraceFormatStr(
        'uzeffdxfnodregistry: load: NOD key "%s" has no handler, skipped',
        [AModel.NOD[I].Key]);
  for I := 0 to High(Handlers) do begin
    if not AModel.NOD.FindEntry(Handlers[I].Key, Entry) then begin
      NODLogTraceFormatStr(
        'uzeffdxfnodregistry: load: key "%s" not found in NOD',
        [Handlers[I].Key]);
      Continue;
    end;
    Dict := AModel.FindDictionary(Entry.TargetHandle);
    if Dict = nil then begin
      NODLogWarningFormatStr(
        'uzeffdxfnodregistry: load: NOD key "%s" refers to %s, which is not a dictionary; skipped',
        [Handlers[I].Key, DXFHandleToStr(Entry.TargetHandle)]);
      Continue;
    end;
    AModel.ClaimHandle(Dict.Handle);
    if not Assigned(Handlers[I].LoadProc) then
      Continue;
    NODLogTraceFormatStr(
      'uzeffdxfnodregistry: load: key "%s", dictionary %s, %d entries',
      [Handlers[I].Key, Dict.RawObject.HandleStr, Dict.Count]);
    try
      Handlers[I].LoadProc(AModel, Dict, ADrawing);
      Inc(Result);
    except
      on E: Exception do
        NODLogWarningFormatStr(
          'uzeffdxfnodregistry: load: handler "%s" failed: %s: %s',
          [Handlers[I].Key, E.ClassName, E.Message]);
    end;
  end;
end;

{ Текст секции OBJECTS DXF-файла (от 0/SECTION до конца файла — разбор
  остановится на ENDSEC). '' — секции нет. }
function ReadDXFObjectsSectionText(const AFileName: string): string;
var
  Lines, Section: TStringList;
  I, Start: Integer;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    Start := -1;
    for I := 0 to Lines.Count - 4 do
      if (Trim(Lines[I]) = '0') and (Trim(Lines[I + 1]) = 'SECTION') and
         (Trim(Lines[I + 2]) = '2') and (Trim(Lines[I + 3]) = 'OBJECTS') then begin
        Start := I;
        Break;
      end;
    if Start < 0 then
      Exit;
    Section := TStringList.Create;
    try
      for I := Start to Lines.Count - 1 do
        Section.Add(Lines[I]);
      Result := Section.Text;
    finally
      Section.Free;
    end;
  finally
    Lines.Free;
  end;
end;

{ TZNODSaveSession }

constructor TZNODSaveSession.Create(AVersion: TACDWGVer);
var
  I, N: Integer;
begin
  inherited Create;
  FVersion := AVersion;
  SetLength(FHandlers, Length(NODHandlers));
  N := 0;
  for I := 0 to High(NODHandlers) do
    if NODHandlers[I].MinVersion <= AVersion then begin
      FHandlers[N] := NODHandlers[I];
      Inc(N);
    end else
      NODLogWarningFormatStr(
        'uzeffdxfnodregistry: save: NOD key "%s" is not written to DXF %s (requires %s or newer)',
        [NODHandlers[I].Key, ACDWGVerName(AVersion),
         ACDWGVerName(NODHandlers[I].MinVersion)]);
  SetLength(FHandlers, N);
  SetLength(FDictHandles, N);
  for I := 0 to N - 1 do
    FDictHandles[I] := 0;
end;

function TZNODSaveSession.GetCount: Integer;
begin
  Result := Length(FHandlers);
end;

function TZNODSaveSession.GetHandler(AIndex: Integer): TZNODHandler;
begin
  Result := FHandlers[AIndex];
end;

function TZNODSaveSession.GetDictHandle(AIndex: Integer): TDWGHandle;
begin
  Result := FDictHandles[AIndex];
end;

function TZNODSaveSession.LoadTemplate(const ATemplateFileName: string): Boolean;
var
  Model: TZNODModel;
begin
  FTemplateNODHandle := 0;
  if Count = 0 then
    Exit(False);
  Model := TZNODModel.Create;
  try
    try
      if not Model.LoadFromText(ReadDXFObjectsSectionText(ATemplateFileName)) then
        NODLogWarningFormatStr(
          'uzeffdxfnodregistry: save: template "%s": OBJECTS parse error: %s',
          [ATemplateFileName, Model.ParseError]);
      if Model.NOD <> nil then
        FTemplateNODHandle := Model.NOD.Handle;
    except
      on E: Exception do
        NODLogWarningFormatStr(
          'uzeffdxfnodregistry: save: template "%s": %s: %s',
          [ATemplateFileName, E.ClassName, E.Message]);
    end;
  finally
    Model.Free;
  end;
  Result := FTemplateNODHandle <> 0;
  if Result then
    NODLogTraceFormatStr(
      'uzeffdxfnodregistry: save: template NOD %s',
      [DXFHandleToStr(FTemplateNODHandle)])
  else
    NODLogWarningFormatStr(
      'uzeffdxfnodregistry: save: template "%s" has no NOD',
      [ATemplateFileName]);
end;

procedure TZNODSaveSession.ReserveHandles(var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext);
var
  I: Integer;
begin
  if FHandlesReserved then
    Exit;
  FHandlesReserved := True;
  for I := 0 to High(FHandlers) do
    if Assigned(FHandlers[I].ReserveHandlesProc) then begin
      FDictHandles[I] := FHandlers[I].ReserveHandlesProc(ADrawing, AIODXFContext);
      NODLogTraceFormatStr(
        'uzeffdxfnodregistry: save: key "%s": dictionary handle %s',
        [FHandlers[I].Key, DXFHandleToStr(FDictHandles[I])]);
    end;
end;

procedure TZNODSaveSession.WriteClasses(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext);
var
  I: Integer;
begin
  for I := 0 to High(FHandlers) do
    if Assigned(FHandlers[I].ClassesProc) then
      FHandlers[I].ClassesProc(AOutStream, ADrawing, AIODXFContext);
end;

procedure TZNODSaveSession.WriteObjects(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext;
  ANODHandle: TDWGHandle);
var
  I: Integer;
begin
  if FObjectsWritten then
    Exit;
  FObjectsWritten := True;
  ReserveHandles(ADrawing, AIODXFContext);
  for I := 0 to High(FHandlers) do
    if Assigned(FHandlers[I].SaveProc) then begin
      NODLogTraceFormatStr(
        'uzeffdxfnodregistry: save: key "%s": objects (dictionary %s, NOD %s)',
        [FHandlers[I].Key, DXFHandleToStr(FDictHandles[I]),
         DXFHandleToStr(ANODHandle)]);
      FHandlers[I].SaveProc(AOutStream, ADrawing, AIODXFContext,
        FDictHandles[I], ANODHandle);
    end;
end;

end.
