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
  Модуль: uzvproxytoblock
  Назначение: Команда proxytoblock — конвертация выделенных
    ACAD_PROXY_ENTITY в блоки ZCAD с уникальными именами ZPE1..ZPEn.

  Алгоритм:
  1. Перебираем выделенные объекты чертежа.
  2. Для каждого GDBObjAcdProxy:
     a) Разбираем proxy-данные на контуры и тексты.
     b) Создаём определение блока (GDBObjBlockdef) с именем ZPEn.
     c) Заполняем блок линиями из контуров прокси-объекта.
     d) Вставляем блок (GDBObjBlockInsert) на позицию прокси.
     e) Удаляем исходный прокси-объект из чертежа.
  3. Перерисовываем чертёж.

  Зависимости:
  - uzeentacdproxy — класс GDBObjAcdProxy (прокси-объект)
  - uzeentitiesmanager — ENTF_CreateLine для создания линий
  - uzeblockdef — GDBObjBlockdef (определение блока)
  - uzeentblockinsert — GDBObjBlockInsert (вставка блока)
  - UGDBObjBlockdefArray — массив определений блоков
  - uzcdrawings — доступ к текущему чертежу
}

unit uzvproxytoblock;
{$mode delphi}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzcLog,
  uzccommandsabstract,
  uzccommandsimpl,
  uzccommandsmanager,
  uzeconsts,
  uzeentity,
  uzcdrawings,
  uzcdrawing,
  uzcinterface,
  uzcutils,
  gzctnrVectorTypes,
  uzegeometrytypes,
  uzegeometry,
  uzeentacdproxy,
  uzeentproxygraphicparser,
  uzeentproxymanager,
  uzeentitiesmanager,
  uzeblockdef,
  uzeentblockinsert,
  UGDBObjBlockdefArray,
  uzestyleslayers,
  uzestyleslinetypes,
  uzepalette,
  uzgldrawcontext,
  uzedrawingdef,
  uzeentgenericsubentry,
  uzeentsubordinated,
  UGDBSelectedObjArray,
  zcmultiobjectcreateundocommand,
  uzccommand_erase,
  gzctnrSTL,
  UGDBPoint3DArray;

implementation

const
  { Префикс имени блока для конвертированных прокси-объектов }
  ZPE_PREFIX = 'ZPE';

{ Находит следующий свободный номер для имени ZPEn.
  Перебирает все определения блоков чертежа и ищет максимальный
  суффикс среди имён, начинающихся с ZPE. }
function FindNextFreeZPENumber: Integer;
var
  BlockDef: PGDBObjBlockdef;
  ir: itrec;
  BlockName: string;
  NumStr: string;
  Num, MaxNum: Integer;
begin
  MaxNum := 0;
  BlockDef := drawings.GetCurrentDWG^.BlockDefArray.beginiterate(ir);
  if BlockDef <> nil then
    repeat
      BlockName := BlockDef^.Name;
      { Проверяем, начинается ли имя блока с ZPE }
      if (Length(BlockName) > Length(ZPE_PREFIX))
        and (CompareText(
          Copy(BlockName, 1, Length(ZPE_PREFIX)),
          ZPE_PREFIX) = 0)
      then begin
        NumStr := Copy(
          BlockName,
          Length(ZPE_PREFIX) + 1,
          Length(BlockName) - Length(ZPE_PREFIX));
        Num := StrToIntDef(NumStr, 0);
        if Num > MaxNum then
          MaxNum := Num;
      end;
      BlockDef := drawings.GetCurrentDWG^.BlockDefArray.iterate(ir);
    until BlockDef = nil;

  Result := MaxNum + 1;
end;

{ Создаёт определение блока и заполняет его линиями из контуров
  прокси-объекта. Базовая точка блока = центр BBox прокси. }
function CreateBlockDefFromProxy(
  Proxy: PGDBObjAcdProxy;
  const BlockName: string;
  const BasePoint: TzePoint3d
): PGDBObjBlockdef;
var
  BlockDefArray: PGDBObjBlockdefArray;
  SystemLayer: PGDBLayerProp;
  SystemLT: PGDBLtypeProp;
  Contour: TProxyContour;
  ContourIdx, VertIdx: Integer;
  P1, P2: TzePoint3d;
  ir: itrec;
  pV, pVFirst, pVPrev: PzePoint3d;
  LineCount: Integer;
begin
  { Получаем массив определений блоков чертежа }
  BlockDefArray := PGDBObjBlockdefArray(
    drawings.GetCurrentDWG^.GetBlockDefArraySimple);

  { Создаём новое определение блока }
  Result := BlockDefArray^.create(BlockName);
  Result^.Base := BasePoint;

  { Получаем системные стили слоя и типа линии }
  SystemLayer := drawings.GetCurrentDWG^
    .GetLayerTable^.GetSystemLayer;
  SystemLT := drawings.GetCurrentDWG^
    .GetLTypeTable^.GetSystemLT(TLTByBlock);

  LineCount := 0;

  { Перебираем контуры прокси-объекта и создаём линии }
  for ContourIdx := 0 to Proxy^.ContourCount - 1 do
  begin
    Contour := Proxy^.GetContour(ContourIdx);

    if Contour.Vertices.Count < 2 then
      Continue;

    { Получаем первую вершину }
    pVFirst := Contour.Vertices.beginiterate(ir);
    pVPrev := pVFirst;
    pV := Contour.Vertices.iterate(ir);

    { Создаём линии между последовательными вершинами }
    while pV <> nil do
    begin
      ENTF_CreateLine(
        Result, @Result^.ObjArray,
        SystemLayer, SystemLT,
        LnWtByBlock, ClByBlock,
        pVPrev^, pV^);
      Inc(LineCount);
      pVPrev := pV;
      pV := Contour.Vertices.iterate(ir);
    end;

    { Замыкаем контур, если он замкнут }
    if Contour.Closed and (pVPrev <> pVFirst) then
    begin
      ENTF_CreateLine(
        Result, @Result^.ObjArray,
        SystemLayer, SystemLT,
        LnWtByBlock, ClByBlock,
        pVPrev^, pVFirst^);
      Inc(LineCount);
    end;
  end;

  programlog.LogOutFormatStr(
    'uzvproxytoblock: CreateBlockDefFromProxy "%s"'
    + ' created with %d lines from %d contours',
    [BlockName, LineCount, Proxy^.ContourCount], LM_Info);
end;

{ Вставляет блок в чертёж на указанную позицию.
  Масштаб = 1.0, угол поворота = 0.0. }
function InsertBlockIntoDrawing(
  const BlockName: string;
  const InsertPoint: TzePoint3d
): PGDBObjBlockInsert;
var
  tb: PGDBObjBlockInsert;
  dc: TDrawContext;
begin
  { Создаём вставку блока через фабрику }
  Result := Pointer(
    drawings.GetCurrentDWG^.ConstructObjRoot
    .ObjArray.CreateObj(GDBBlockInsertID));
  PGDBObjBlockInsert(Result)^.initnul;
  PGDBObjBlockInsert(Result)^.init(
    drawings.GetCurrentROOT,
    drawings.GetCurrentDWG^.GetCurrentLayer, 0);

  { Задаём имя блока и позицию вставки }
  Result^.Name := BlockName;
  Result^.Local.p_insert := InsertPoint;

  { Пост-обработка: связывает вставку с определением блока }
  tb := Pointer(
    Result^.FromDXFPostProcessBeforeAdd(
      nil, drawings.GetCurrentDWG^));
  if tb <> nil then
  begin
    tb^.bp := Result^.bp;
    Result^.done;
    Freemem(Pointer(Result));
    Result := tb;
  end;

  { Добавляем в массив объектов чертежа }
  drawings.GetCurrentROOT^.AddObjectToObjArray(Addr(Result));
  PGDBObjEntity(Result)^.FromDXFPostProcessAfterAdd;

  { Вычисляем матрицу объекта и геометрию }
  Result^.CalcObjMatrix;
  Result^.BuildGeometry(drawings.GetCurrentDWG^);
  Result^.BuildVarGeometry(drawings.GetCurrentDWG^);
  dc := drawings.GetCurrentDWG^.CreateDrawingRC;
  Result^.FormatEntity(drawings.GetCurrentDWG^, dc);
  Result^.Visible := 0;

  programlog.LogOutFormatStr(
    'uzvproxytoblock: InsertBlockIntoDrawing "%s"'
    + ' at (%.3f, %.3f, %.3f)',
    [BlockName, InsertPoint.x, InsertPoint.y,
     InsertPoint.z], LM_Info);
end;

{ Главная функция команды proxytoblock.
  Конвертирует все выделенные GDBObjAcdProxy в блоки ZPEn. }
function proxytoblock_com(
  const Context: TZCADCommandContext;
  operands: TCommandOperands
): TCommandResult;
var
  pobj: pGDBObjEntity;
  pProxy: PGDBObjAcdProxy;
  ir: itrec;
  NextZPENum: Integer;
  BlockName: string;
  BasePoint: TzePoint3d;
  ConvertedCount: Integer;
  SkippedCount: Integer;
  psd: PSelectedObjDesc;
  ProxyList: array of PGDBObjAcdProxy;
  ProxyCount: Integer;
  I: Integer;
  domethod, undomethod: tmethod;
  Pair: TMyMapCounter<PGDBObjGenericSubEntry>.TDictionaryPair;
  Counter: TMyMapCounter<PGDBObjGenericSubEntry>;
begin
  Result := cmd_ok;

  { Проверяем, что есть выделенные объекты }
  if (drawings.GetCurrentROOT^.ObjArray.Count = 0)
    or (drawings.GetCurrentDWG^.wa.param.seldesc
      .Selectedobjcount = 0)
  then begin
    zcUI.TextMessage(
      'proxytoblock: нет выделенных объектов',
      TMWOHistoryOut);
    Exit;
  end;

  { Определяем следующий свободный номер ZPE }
  NextZPENum := FindNextFreeZPENumber;

  programlog.LogOutFormatStr(
    'uzvproxytoblock: starting, next ZPE number = %d',
    [NextZPENum], LM_Info);

  ConvertedCount := 0;
  SkippedCount := 0;

  { Собираем список прокси-объектов из выделения }
  ProxyCount := 0;
  SetLength(ProxyList, 0);

  psd := drawings.GetCurrentDWG.SelObjArray.beginiterate(ir);
  if psd <> nil then
    repeat
      pobj := psd^.objaddr;
      if pobj^.GetObjType = GDBAcdProxyID then
      begin
        Inc(ProxyCount);
        SetLength(ProxyList, ProxyCount);
        ProxyList[ProxyCount - 1] := PGDBObjAcdProxy(pobj);
      end
      else
        Inc(SkippedCount);
      psd := drawings.GetCurrentDWG.SelObjArray.iterate(ir);
    until psd = nil;

  if ProxyCount = 0 then
  begin
    zcUI.TextMessage(
      'proxytoblock: среди выделенных объектов нет'
      + ' ACAD_PROXY_ENTITY',
      TMWOHistoryOut);
    if SkippedCount > 0 then
      zcUI.TextMessage(
        'proxytoblock: пропущено объектов других типов: '
        + IntToStr(SkippedCount),
        TMWOHistoryOut);
    Exit;
  end;

  { Начинаем undo-блок }
  PTZCADDrawing(drawings.GetCurrentDWG)^
    .UndoStack.PushStartMarker('proxytoblock');

  { Обрабатываем каждый прокси-объект }
  for I := 0 to ProxyCount - 1 do
  begin
    pProxy := ProxyList[I];

    { Разбираем proxy-данные, если ещё не разобраны }
    pProxy^.EnsureParsed;

    { Проверяем, есть ли контуры }
    if pProxy^.ContourCount = 0 then
    begin
      programlog.LogOutFormatStr(
        'uzvproxytoblock: proxy has no contours, skipping',
        [], LM_Info);
      zcUI.TextMessage(
        'proxytoblock: прокси-объект без примитивов,'
        + ' пропускаем',
        TMWOHistoryOut);
      Continue;
    end;

    { Вычисляем базовую точку (центр BBox) }
    if pProxy^.HasCenterPoint then
      BasePoint := pProxy^.CenterPoint
    else if pProxy^.BBoxLoaded then
    begin
      BasePoint.x :=
        (pProxy^.BBoxMin.x + pProxy^.BBoxMax.x) / 2;
      BasePoint.y :=
        (pProxy^.BBoxMin.y + pProxy^.BBoxMax.y) / 2;
      BasePoint.z :=
        (pProxy^.BBoxMin.z + pProxy^.BBoxMax.z) / 2;
    end
    else
      BasePoint := NulVertex;

    { Генерируем уникальное имя блока }
    BlockName := ZPE_PREFIX + IntToStr(NextZPENum);
    Inc(NextZPENum);

    { Создаём определение блока с примитивами }
    CreateBlockDefFromProxy(pProxy, BlockName, BasePoint);

    { Вставляем блок на позицию прокси-объекта }
    InsertBlockIntoDrawing(BlockName, BasePoint);

    Inc(ConvertedCount);

    zcUI.TextMessage(
      'proxytoblock: создан блок ' + BlockName
      + ' (' + IntToStr(pProxy^.ContourCount)
      + ' контуров)',
      TMWOHistoryOut);
  end;

  { Удаляем прокси-объекты из чертежа (с поддержкой undo) }
  if ConvertedCount > 0 then
  begin
    Counter := TMyMapCounter<PGDBObjGenericSubEntry>.Create;
    try
      for I := 0 to ProxyCount - 1 do
      begin
        pProxy := ProxyList[I];
        if pProxy^.ContourCount > 0 then
          Counter.CountKey(
            PGDBObjGenericSubEntry(
              PGDBObjEntity(pProxy)^.bp.ListPos.Owner));
      end;

      for Pair in Counter do
      begin
        MySetObjCreateManipulator(
          Pair.key, undomethod, domethod);
        with PushMultiObjectCreateCommand(
          PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack,
          tmethod(domethod),
          tmethod(undomethod),
          Pair.Value) do
        begin
          for I := 0 to ProxyCount - 1 do
          begin
            pProxy := ProxyList[I];
            if (pProxy^.ContourCount > 0)
              and (PGDBObjGenericSubEntry(
                PGDBObjEntity(pProxy)^.bp.ListPos.Owner)
                = Pair.key)
            then begin
              AddObject(PGDBObjEntity(pProxy));
              PGDBObjEntity(pProxy)^.Selected := False;
            end;
          end;
          FreeArray := False;
          comit;
        end;
      end;
    finally
      Counter.Free;
    end;
  end;

  { Завершаем undo-блок }
  PTZCADDrawing(drawings.GetCurrentDWG)^
    .UndoStack.PushEndMarker;

  { Сбрасываем состояние выделения }
  drawings.GetCurrentDWG^.wa.param.seldesc
    .Selectedobjcount := 0;
  drawings.GetCurrentDWG^.wa.param.seldesc
    .OnMouseObject := nil;
  drawings.GetCurrentDWG^.wa.param.seldesc
    .LastSelectedObject := nil;
  drawings.GetCurrentDWG^.wa.param
    .lastonmouseobject := nil;
  zcUI.Do_GUIaction(nil, zcMsgUIReturnToDefaultObject);
  clearcp;

  { Перерисовываем чертёж }
  zcRedrawCurrentDrawing;

  { Выводим итоговое сообщение }
  zcUI.TextMessage(
    'proxytoblock: обработано ' + IntToStr(ConvertedCount)
    + ' прокси-объектов',
    TMWOHistoryOut);
  if SkippedCount > 0 then
    zcUI.TextMessage(
      'proxytoblock: пропущено объектов других типов: '
      + IntToStr(SkippedCount),
      TMWOHistoryOut);

  programlog.LogOutFormatStr(
    'uzvproxytoblock: done, converted=%d skipped=%d',
    [ConvertedCount, SkippedCount], LM_Info);
end;

initialization
  programlog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}], LM_Info, UnitsInitializeLMId);
  CreateZCADCommand(
    @proxytoblock_com, 'proxytoblock', CADWG, 0);

finalization
  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}], LM_Info, UnitsFinalizeLMId);
end.
