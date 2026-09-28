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
    ReserveHandlesProc — до ENTITIES, не более одного раза за сохранение;
    ClassesProc — перед ENDSEC секции CLASSES;
    SaveProc — перед ENDSEC секции OBJECTS (до RunObjectsSaveDxfProcs).
    Обработчики, у которых MinVersion выше версии сохраняемого файла, при
    записи не вызываются, в лог пишется предупреждение.
    Этап 4: если ReserveHandlesProc вернул хэндл словаря-ветки (не 0), то
    ветка шаблона для ключа обработчика (словарь и все его объекты)
    пропускается при копировании шаблона, запись 3/350 в NOD шаблона
    ссылается на новый словарь, а при отсутствии ключа в NOD шаблона
    запись добавляется (в алфавитном порядке ключей). 0 — обработчик ключ
    не берёт: ветка шаблона копируется как есть.

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
  Generics.Collections,
  uzeTypes,
  uzctnrVectorBytesStream,
  uzedrawingsimple,
  usimplegenerics,
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
  { Запись, секция CLASSES (необязательная): объявление классов.
    Имена классов шаблона — AIODXFContext.TemplateClassNames. }
  TNODClassesProc = procedure(var AOutStream: TZctnrVectorBytes;
    var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext);
  { Запись (необязательная, после ReserveHandlesProc): новый хэндл объекта
    ветки с именем AName (0 — нет). Нужна, чтобы перенаправить ссылки
    других объектов шаблона на пропущенные объекты его ветки. }
  TNODFindObjectHandleProc = function(const AName: string;
    var AIODXFContext: TIODXFSaveContext): TDWGHandle;

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
    FindObjectHandleProc: TNODFindObjectHandleProc;
  end;
  TZNODHandlers = array of TZNODHandler;

  { Ссылка объекта шаблона (вне ветки) на объект ветки обработчика }
  TZNODTemplateRef = record
    { Хэндл объекта ветки в шаблоне }
    Handle: TDWGHandle;
    { Индекс обработчика в сессии }
    HandlerIndex: Integer;
    { Ключ объекта в словаре ветки ('' — объект не запись словаря) }
    Name: string;
  end;
  TZNODTemplateRefs = array of TZNODTemplateRef;

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
    { Хэндл словаря-ветки ключа обработчика в NOD шаблона (0 — ключа нет) }
    FTemplateDictHandles: array of TDWGHandle;
    { Хэндл объекта ветки шаблона → индекс обработчика }
    FTemplateBranch: TDictionary<TDWGHandle, Integer>;
    { Ссылки на объекты веток из остальных объектов шаблона }
    FTemplateRefs: TZNODTemplateRefs;
    FTemplateHandlesMapped: Boolean;
    { Записи NOD, добавленные в вывод (по индексу обработчика) }
    FNODEntryInserted: array of Boolean;
    function GetCount: Integer;
    function GetHandler(AIndex: Integer): TZNODHandler;
    function GetDictHandle(AIndex: Integer): TDWGHandle;
    function GetTemplateDictHandle(AIndex: Integer): TDWGHandle;
    procedure BuildTemplateBranches(AModel: TZNODModel);
  public
    constructor Create(AVersion: TACDWGVer);
    destructor Destroy; override;
    { Разбирает секцию OBJECTS шаблона моделью этапа 1 и запоминает
      исходный хэндл его NOD (TemplateNODHandle), словари-ветки ключей
      обработчиков, множество хэндлов их объектов и ссылки на них из
      остальных объектов шаблона. Если обработчиков нет, шаблон не
      читается. False — NOD в шаблоне не найден. }
    function LoadTemplate(const ATemplateFileName: string): Boolean;
    { Разбирает текст секции OBJECTS шаблона (для тестов; LoadTemplate —
      то же для файла). }
    function LoadTemplateText(const AObjectsSection: string): Boolean;
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
    { После ReserveHandles: регистрирует в AMap (старый хэндл шаблона →
      новый) словари-ветки шаблона, которые заменяет обработчик (→ хэндл
      из ReserveHandlesProc, так перемапится пара 350 в NOD шаблона), и
      объекты веток, на которые ссылаются другие объекты шаблона (→ объект
      чертежа с тем же именем через FindObjectHandleProc, иначе — объект
      с именем DefaultName). Идемпотентна. }
    procedure MapTemplateHandles(AMap: TMapHandleToHandle;
      var AIODXFContext: TIODXFSaveContext);
    { Пропускается ли объект шаблона с хэндлом AHandle: объект ветки ключа,
      для которого обработчик выделил свой словарь (DictHandle > 0). }
    function IsTemplateHandleSkipped(AHandle: TDWGHandle): Boolean;
    { Есть ли что пропускать в шаблоне (после ReserveHandles) }
    function HasTemplateSkips: Boolean;
    { Записи 3/350, которые нужно добавить в NOD шаблона перед его записью
      с ключом ANextKey (ключи обработчиков, которых нет в NOD шаблона, у
      которых DictHandle > 0 и которые по алфавиту меньше ANextKey).
      ANextKey = '' — все оставшиеся (конец NOD). Каждая запись выдаётся
      один раз; результат упорядочен по ключу. }
    function TakeNODInsertions(const ANextKey: string): TZDXFDictEntries;

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
    { Хэндл словаря-ветки обработчика в шаблоне (0 — ключа в NOD шаблона нет) }
    property TemplateDictHandles[AIndex: Integer]: TDWGHandle
      read GetTemplateDictHandle;
    { Ссылки остальных объектов шаблона на объекты веток }
    property TemplateRefs: TZNODTemplateRefs read FTemplateRefs;
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
  H.FindObjectHandleProc := nil;
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
  SetLength(FTemplateDictHandles, N);
  SetLength(FNODEntryInserted, N);
  for I := 0 to N - 1 do begin
    FDictHandles[I] := 0;
    FTemplateDictHandles[I] := 0;
    FNODEntryInserted[I] := False;
  end;
  FTemplateBranch := TDictionary<TDWGHandle, Integer>.Create;
end;

destructor TZNODSaveSession.Destroy;
begin
  FTemplateBranch.Free;
  inherited Destroy;
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

function TZNODSaveSession.GetTemplateDictHandle(AIndex: Integer): TDWGHandle;
begin
  Result := FTemplateDictHandles[AIndex];
end;

{ Является ли код группы ссылкой на хэндл (те же коды, что перемапливает
  savedxf20XX, кроме собственного хэндла 5) }
function IsHandleRefGroupCode(ACode: Integer): Boolean;
begin
  case ACode of
    320, 330, 340, 350, 360, 390, 105, 1005:
      Result := True;
  else
    Result := False;
  end;
end;

procedure TZNODSaveSession.BuildTemplateBranches(AModel: TZNODModel);
var
  I, J, K, Owner: Integer;
  Entry: TZDXFDictEntry;
  Obj: TZDXFRawObject;
  Dict: TZDXFDictionary;
  Changed: Boolean;
  Ref: TDWGHandle;
  Refs: TDictionary<TDWGHandle, Boolean>;

  { Добавляет хэндл в ветку обработчика AIndex; True — добавлен впервые.
    NOD и 0 в ветку не входят никогда. }
  function AddToBranch(AHandle: TDWGHandle; AIndex: Integer): Boolean;
  begin
    Result := (AHandle <> 0) and (AHandle <> FTemplateNODHandle) and
      not FTemplateBranch.ContainsKey(AHandle);
    if Result then
      FTemplateBranch.Add(AHandle, AIndex);
  end;

  { Ключ объекта AHandle в словаре его ветки ('' — не найден) }
  function BranchKeyOf(AHandle: TDWGHandle): string;
  var
    D: TZDXFDictionary;
    E: Integer;
  begin
    Result := '';
    for D in AModel.Dictionaries do
      if FTemplateBranch.ContainsKey(D.Handle) then
        for E := 0 to D.Count - 1 do
          if D[E].TargetHandle = AHandle then
            Exit(D[E].Key);
  end;

begin
  FTemplateBranch.Clear;
  SetLength(FTemplateRefs, 0);
  if AModel.NOD = nil then
    Exit;
  { Корни веток — словари по ключам обработчиков в NOD шаблона }
  for I := 0 to High(FHandlers) do
    if AModel.NOD.FindEntry(FHandlers[I].Key, Entry) then begin
      FTemplateDictHandles[I] := Entry.TargetHandle;
      AddToBranch(Entry.TargetHandle, I);
    end;
  { Замыкание: записи словарей ветки, расширенные словари объектов ветки и
    объекты, владелец которых (330) — объект ветки }
  repeat
    Changed := False;
    for Obj in AModel.Objects do begin
      if FTemplateBranch.TryGetValue(Obj.Handle, K) then begin
        if AddToBranch(Obj.XDictHandle, K) then
          Changed := True;
        Dict := AModel.FindDictionary(Obj.Handle);
        if Dict <> nil then
          for J := 0 to Dict.Count - 1 do
            if AddToBranch(Dict[J].TargetHandle, K) then
              Changed := True;
      end else if Obj.HasOwnerGroup and
         FTemplateBranch.TryGetValue(Obj.OwnerHandle, Owner) then
        if AddToBranch(Obj.Handle, Owner) then
          Changed := True;
    end;
  until not Changed;
  { Ссылки на объекты веток из остальных объектов шаблона (ссылка NOD на
    словарь-ветку — тоже: она перемапится на новый словарь) }
  Refs := TDictionary<TDWGHandle, Boolean>.Create;
  try
    for Obj in AModel.Objects do begin
      if FTemplateBranch.ContainsKey(Obj.Handle) then
        Continue;
      for J := 0 to Obj.PairCount - 1 do
        if IsHandleRefGroupCode(Obj.Pairs[J].Code) and
           TryDXFStrToHandle(Trim(Obj.Pairs[J].Value), Ref) and
           FTemplateBranch.TryGetValue(Ref, K) and
           not Refs.ContainsKey(Ref) then begin
          Refs.Add(Ref, True);
          I := Length(FTemplateRefs);
          SetLength(FTemplateRefs, I + 1);
          FTemplateRefs[I].Handle := Ref;
          FTemplateRefs[I].HandlerIndex := K;
          FTemplateRefs[I].Name := BranchKeyOf(Ref);
          NODLogTraceFormatStr(
            'uzeffdxfnodregistry: save: template %s %s refers to %s ("%s") of key "%s" branch',
            [Obj.ObjType, Obj.HandleStr, DXFHandleToStr(Ref),
             FTemplateRefs[I].Name, FHandlers[K].Key]);
        end;
    end;
  finally
    Refs.Free;
  end;
  for I := 0 to High(FHandlers) do
    NODLogTraceFormatStr(
      'uzeffdxfnodregistry: save: template key "%s": dictionary %s',
      [FHandlers[I].Key, DXFHandleToStr(FTemplateDictHandles[I])]);
  NODLogTraceFormatStr(
    'uzeffdxfnodregistry: save: template branches: %d objects, %d external references',
    [FTemplateBranch.Count, Length(FTemplateRefs)]);
end;

function TZNODSaveSession.LoadTemplate(const ATemplateFileName: string): Boolean;
begin
  FTemplateNODHandle := 0;
  if Count = 0 then
    Exit(False);
  try
    Result := LoadTemplateText(ReadDXFObjectsSectionText(ATemplateFileName));
  except
    on E: Exception do begin
      NODLogWarningFormatStr(
        'uzeffdxfnodregistry: save: template "%s": %s: %s',
        [ATemplateFileName, E.ClassName, E.Message]);
      Result := False;
    end;
  end;
  if not Result then
    NODLogWarningFormatStr(
      'uzeffdxfnodregistry: save: template "%s" has no NOD',
      [ATemplateFileName]);
end;

function TZNODSaveSession.LoadTemplateText(const AObjectsSection: string): Boolean;
var
  Model: TZNODModel;
  I: Integer;
begin
  FTemplateNODHandle := 0;
  FTemplateBranch.Clear;
  SetLength(FTemplateRefs, 0);
  for I := 0 to High(FTemplateDictHandles) do
    FTemplateDictHandles[I] := 0;
  if Count = 0 then
    Exit(False);
  Model := TZNODModel.Create;
  try
    if not Model.LoadFromText(AObjectsSection) then
      NODLogWarningFormatStr(
        'uzeffdxfnodregistry: save: template OBJECTS parse error: %s',
        [Model.ParseError]);
    if Model.NOD <> nil then
      FTemplateNODHandle := Model.NOD.Handle;
    BuildTemplateBranches(Model);
  finally
    Model.Free;
  end;
  Result := FTemplateNODHandle <> 0;
  if Result then
    NODLogTraceFormatStr(
      'uzeffdxfnodregistry: save: template NOD %s',
      [DXFHandleToStr(FTemplateNODHandle)]);
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

procedure TZNODSaveSession.MapTemplateHandles(AMap: TMapHandleToHandle;
  var AIODXFContext: TIODXFSaveContext);

  procedure MapHandle(AOld, ANew: TDWGHandle; const AKey: string);
  var
    Prev: TDWGHandle;
  begin
    if AMap.TryGetValue(AOld, Prev) and (Prev <> ANew) then
      NODLogWarningFormatStr(
        'uzeffdxfnodregistry: save: key "%s": template handle %s was already mapped to %s, remapped to %s',
        [AKey, DXFHandleToStr(AOld), DXFHandleToStr(Prev), DXFHandleToStr(ANew)]);
    AMap.AddOrSetValue(AOld, ANew);
    NODLogTraceFormatStr(
      'uzeffdxfnodregistry: save: key "%s": template %s -> %s',
      [AKey, DXFHandleToStr(AOld), DXFHandleToStr(ANew)]);
  end;

var
  I, K: Integer;
  NewHandle: TDWGHandle;
  H: TZNODHandler;
begin
  if FTemplateHandlesMapped or not FHandlesReserved then
    Exit;
  FTemplateHandlesMapped := True;
  for I := 0 to High(FHandlers) do
    if FDictHandles[I] <> 0 then begin
      if FTemplateDictHandles[I] <> 0 then
        MapHandle(FTemplateDictHandles[I], FDictHandles[I], FHandlers[I].Key)
      else if FTemplateNODHandle = 0 then
        NODLogWarningFormatStr(
          'uzeffdxfnodregistry: save: key "%s": template has no NOD, dictionary %s is not referenced from NOD',
          [FHandlers[I].Key, DXFHandleToStr(FDictHandles[I])]);
    end;
  for I := 0 to High(FTemplateRefs) do begin
    K := FTemplateRefs[I].HandlerIndex;
    H := FHandlers[K];
    { Словарь-ветка уже перемаплен выше; ветка, которую обработчик не
      заменяет, копируется из шаблона как есть }
    if (FDictHandles[K] = 0) or
       (FTemplateRefs[I].Handle = FTemplateDictHandles[K]) then
      Continue;
    NewHandle := 0;
    if Assigned(H.FindObjectHandleProc) then begin
      if FTemplateRefs[I].Name <> '' then
        NewHandle := H.FindObjectHandleProc(FTemplateRefs[I].Name, AIODXFContext);
      if (NewHandle = 0) and (H.DefaultName <> '') then
        NewHandle := H.FindObjectHandleProc(H.DefaultName, AIODXFContext);
    end;
    if NewHandle <> 0 then
      MapHandle(FTemplateRefs[I].Handle, NewHandle, H.Key)
    else
      NODLogWarningFormatStr(
        'uzeffdxfnodregistry: save: key "%s": template object %s ("%s") is referenced outside the branch and has no replacement',
        [H.Key, DXFHandleToStr(FTemplateRefs[I].Handle), FTemplateRefs[I].Name]);
  end;
end;

function TZNODSaveSession.IsTemplateHandleSkipped(AHandle: TDWGHandle): Boolean;
var
  K: Integer;
begin
  Result := FTemplateBranch.TryGetValue(AHandle, K) and (FDictHandles[K] <> 0);
end;

function TZNODSaveSession.HasTemplateSkips: Boolean;
var
  I: Integer;
begin
  for I := 0 to High(FHandlers) do
    if (FDictHandles[I] <> 0) and (FTemplateDictHandles[I] <> 0) then
      Exit(True);
  Result := False;
end;

function TZNODSaveSession.TakeNODInsertions(const ANextKey: string): TZDXFDictEntries;
var
  I, J, N: Integer;
  E: TZDXFDictEntry;
begin
  N := 0;
  SetLength(Result, Length(FHandlers));
  if FTemplateNODHandle = 0 then begin
    SetLength(Result, 0);
    Exit;
  end;
  for I := 0 to High(FHandlers) do
    if (FDictHandles[I] <> 0) and (FTemplateDictHandles[I] = 0) and
       not FNODEntryInserted[I] and
       ((ANextKey = '') or (CompareText(FHandlers[I].Key, ANextKey) < 0)) then begin
      FNODEntryInserted[I] := True;
      E.Key := FHandlers[I].Key;
      E.TargetHandle := FDictHandles[I];
      E.OwnershipCode := 350;
      { Вставка с сохранением порядка по ключу }
      J := N;
      while (J > 0) and (CompareText(Result[J - 1].Key, E.Key) > 0) do begin
        Result[J] := Result[J - 1];
        Dec(J);
      end;
      Result[J] := E;
      Inc(N);
      NODLogTraceFormatStr(
        'uzeffdxfnodregistry: save: NOD entry "%s" -> %s added',
        [E.Key, DXFHandleToStr(E.TargetHandle)]);
    end;
  SetLength(Result, N);
end;

end.
