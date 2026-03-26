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
  Модуль: uzeentproxymanager
  Назначение: Менеджер регистрации парсеров примитивов внутри Proxy объектов.

  Архитектура по аналогии с uzeentityfactory.pas:
  - Каждый примитив регистрирует обработчик при инициализации своего модуля
  - Регистрация выполняется через RegisterProxyOpCode()
  - Диспетчеризация по числовому OpCode — HandleOpCode()
  - Чтобы отключить примитив, достаточно исключить его .pas из проекта:
    его initialization не выполнится, OpCode не зарегистрируется, парсинг
    этого примитива не произойдёт без изменения главного модуля

  Интерфейс TProxyOpCodeHandler:
    ParseAndCollect — читает бинарные данные из потока, обновляет BBox
                      и возвращает вершины для отрисовки
}

unit uzeentproxymanager;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzeentproxystream,
  uzegeometrytypes,
  UGDBPoint3DArray;

const
  { Максимальный OpCode, поддерживаемый таблицей диспетчеризации }
  PROXY_MAX_OPCODE = 255;

type
  { Данные одного текстового примитива, переданные обработчиком.
    Используются в FormatEntity для вызова Representation.DrawTextContent. }
  TProxyTextItem = record
    { Точка вставки текста в OCS }
    Insert: TzePoint3d;
    { Строка текста }
    Text: string;
    { Высота символов }
    Height: Double;
    { Масштаб по ширине }
    WidthFactor: Double;
    { Угол поворота текста (радианы) }
    Angle: Double;
    { Имя шрифта (ANSI, может быть пустым — тогда используется Standard) }
    FontName: string;
  end;

  { Результат обработки одного OpCode-примитива.
    Хранит геометрию, BBox и текстовые данные, собранные парсером. }
  TProxyHandlerResult = record
    { Флаг: примитив успешно распаршен }
    Valid: Boolean;
    { Вершины контура для отрисовки (могут быть пустыми) }
    Vertices: GDBPoint3DArray;
    { Флаг: вершины заполнены }
    HasVertices: Boolean;
    { Минимальная точка BBox примитива }
    BBoxMin: TzePoint3d;
    { Максимальная точка BBox примитива }
    BBoxMax: TzePoint3d;
    { Флаг: BBox вычислен }
    HasBBox: Boolean;
    { Данные текстового примитива (заполняются только для OpCode текста) }
    TextItem: TProxyTextItem;
    { Флаг: TextItem заполнен }
    HasTextItem: Boolean;
  end;

  { Процедура-обработчик одного OpCode.
    Читает данные из потока, заполняет Result. }
  TProxyOpCodeHandlerProc = procedure(
    Stream: TProxyByteStream;
    out HandlerResult: TProxyHandlerResult);

  { Запись регистрации одного OpCode }
  TProxyOpCodeEntry = record
    { Флаг: запись заполнена }
    Registered: Boolean;
    { Читаемое название команды для логирования }
    Name: string;
    { Обработчик }
    Handler: TProxyOpCodeHandlerProc;
  end;

  { Диспетчер OpCode-обработчиков для Proxy Graphic команд.
    Аналог TEntityFactory из uzeentityfactory.pas, но для внутренних
    примитивов прокси-объекта. }
  TProxyOpCodeDispatcher = class
  private
    class var
      { Таблица зарегистрированных обработчиков, индекс = OpCode }
      FTable: array[0..PROXY_MAX_OPCODE] of TProxyOpCodeEntry;
      { Флаг первой инициализации }
      FInitialized: Boolean;

    { Инициализирует таблицу при первом обращении }
    class procedure EnsureInitialized;

  public
    { Регистрирует обработчик для заданного OpCode.
      Вызывается в секции initialization каждого модуля-парсера. }
    class procedure RegisterOpCode(
      const OpCode: Integer;
      const Name: string;
      const Handler: TProxyOpCodeHandlerProc);

    { Обрабатывает команду с заданным OpCode.
      Если обработчик зарегистрирован — вызывает его и возвращает результат.
      Если нет — Result.Valid = False. }
    class function HandleOpCode(
      const OpCode: Integer;
      Stream: TProxyByteStream;
      out HandlerResult: TProxyHandlerResult): Boolean;

    { Проверяет, зарегистрирован ли обработчик для OpCode }
    class function IsRegistered(const OpCode: Integer): Boolean;

    { Возвращает количество зарегистрированных обработчиков }
    class function GetRegisteredCount: Integer;
  end;

{ Вспомогательная функция: расширяет BBox точкой Pt.
  Если BBoxInitialized = False — инициализирует BBox этой точкой. }
procedure ExpandBBox(const Pt: TzePoint3d;
  var BBoxMin, BBoxMax: TzePoint3d; var BBoxInitialized: Boolean);

{ Расширяет BBox другим BBox (MinB, MaxB).
  Если BBoxInitialized = False — копирует MinB/MaxB как начальное значение. }
procedure MergeBBox(
  const MinB, MaxB: TzePoint3d;
  var BBoxMin, BBoxMax: TzePoint3d;
  var BBoxInitialized: Boolean);

implementation

uses
  uzcLog;

{ === Вспомогательные функции === }

procedure ExpandBBox(const Pt: TzePoint3d;
  var BBoxMin, BBoxMax: TzePoint3d; var BBoxInitialized: Boolean);
begin
  if not BBoxInitialized then
  begin
    BBoxMin := Pt;
    BBoxMax := Pt;
    BBoxInitialized := True;
  end
  else
  begin
    if Pt.x < BBoxMin.x then BBoxMin.x := Pt.x;
    if Pt.y < BBoxMin.y then BBoxMin.y := Pt.y;
    if Pt.z < BBoxMin.z then BBoxMin.z := Pt.z;
    if Pt.x > BBoxMax.x then BBoxMax.x := Pt.x;
    if Pt.y > BBoxMax.y then BBoxMax.y := Pt.y;
    if Pt.z > BBoxMax.z then BBoxMax.z := Pt.z;
  end;
end;

procedure MergeBBox(
  const MinB, MaxB: TzePoint3d;
  var BBoxMin, BBoxMax: TzePoint3d;
  var BBoxInitialized: Boolean);
begin
  ExpandBBox(MinB, BBoxMin, BBoxMax, BBoxInitialized);
  ExpandBBox(MaxB, BBoxMin, BBoxMax, BBoxInitialized);
end;

{ === TProxyOpCodeDispatcher === }

class procedure TProxyOpCodeDispatcher.EnsureInitialized;
var
  I: Integer;
begin
  if FInitialized then
    Exit;

  { Обнуляем таблицу }
  for I := 0 to PROXY_MAX_OPCODE do
  begin
    FTable[I].Registered := False;
    FTable[I].Name := '';
    FTable[I].Handler := nil;
  end;
  FInitialized := True;
end;

class procedure TProxyOpCodeDispatcher.RegisterOpCode(
  const OpCode: Integer;
  const Name: string;
  const Handler: TProxyOpCodeHandlerProc);
begin
  EnsureInitialized;

  { Проверяем диапазон OpCode }
  if (OpCode < 0) or (OpCode > PROXY_MAX_OPCODE) then
  begin
    programlog.LogOutFormatStr(
      'uzeentproxymanager: RegisterOpCode - OpCode %d out of range [0..%d]',
      [OpCode, PROXY_MAX_OPCODE], LM_Info);
    Exit;
  end;

  FTable[OpCode].Registered := True;
  FTable[OpCode].Name := Name;
  FTable[OpCode].Handler := Handler;

  programlog.LogOutFormatStr(
    'uzeentproxymanager: Registered OpCode %d (%s)',
    [OpCode, Name], LM_Info);
end;

class function TProxyOpCodeDispatcher.HandleOpCode(
  const OpCode: Integer;
  Stream: TProxyByteStream;
  out HandlerResult: TProxyHandlerResult): Boolean;
begin
  Result := False;
  HandlerResult.Valid := False;
  HandlerResult.HasVertices := False;
  HandlerResult.HasBBox := False;
  HandlerResult.HasTextItem := False;

  EnsureInitialized;

  { Проверяем диапазон }
  if (OpCode < 0) or (OpCode > PROXY_MAX_OPCODE) then
    Exit;

  { Вызываем зарегистрированный обработчик }
  if FTable[OpCode].Registered and Assigned(FTable[OpCode].Handler) then
  begin
    try
      FTable[OpCode].Handler(Stream, HandlerResult);
      Result := HandlerResult.Valid;
    except
      on E: Exception do
      begin
        programlog.LogOutFormatStr(
          'uzeentproxymanager: HandleOpCode %d (%s) exception: %s',
          [OpCode, FTable[OpCode].Name, E.Message], LM_Info);
        Result := False;
      end;
    end;
  end;
end;

class function TProxyOpCodeDispatcher.IsRegistered(const OpCode: Integer): Boolean;
begin
  EnsureInitialized;
  Result := (OpCode >= 0) and (OpCode <= PROXY_MAX_OPCODE)
    and FTable[OpCode].Registered;
end;

class function TProxyOpCodeDispatcher.GetRegisteredCount: Integer;
var
  I: Integer;
begin
  EnsureInitialized;
  Result := 0;
  for I := 0 to PROXY_MAX_OPCODE do
    if FTable[I].Registered then
      Inc(Result);
end;

initialization
  TProxyOpCodeDispatcher.EnsureInitialized;

end.
