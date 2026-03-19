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
{$mode delphi}{$H+}

{
  Модуль: uzccommand_offset
  Назначение: Команда OFFSET (Смещение/Подобие) — создание эквидистантных
              копий геометрических объектов: линий, окружностей, дуг и полилиний.
  Зависимости: uzccommandsabstract, uzccommandsimpl, uzccommandsmanager,
               uzeentline, uzeentcircle, uzeentarc, uzeentlwpolyline,
               uzeentitiesmanager, uzegeometry, uzegeometrytypes,
               uzcdrawings, uzcdrawing, zcmultiobjectcreateundocommand,
               uzcinterface, uzcutils, uzcLog
}

unit uzccommand_offset;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  Math,
  uzcLog,
  uzccommandsabstract,
  uzccommandsimpl,
  uzccommandsmanager,
  uzeconsts,
  uzcstrconsts,
  uzegeometrytypes,
  uzegeometry,
  uzeentline,
  uzeentcircle,
  uzeentarc,
  uzeentlwpolyline,
  uzeentitiesmanager,
  uzeentity,
  uzcdrawings,
  uzcdrawing,
  uzgldrawcontext,
  zcmultiobjectcreateundocommand,
  uzcinterface,
  uzcutils,
  UGDBPolyLine2DArray;

implementation

{ ============================================================
  Вспомогательные функции для вычисления смещённых объектов
  ============================================================ }

{**
  Вычисляет нормаль к отрезку в плоскости XY.
  Нормаль направлена влево относительно направления отрезка.

  @param ALineBegin - начало отрезка
  @param ALineEnd   - конец отрезка
  @return Единичная нормаль к отрезку
**}
function CalcLineNormal(const ALineBegin, ALineEnd: TzePoint3d): TzePoint3d;
var
  dir: TzePoint3d;
begin
  dir := uzegeometry.VertexSub(ALineEnd, ALineBegin);
  // Поворот направления на 90 градусов против часовой стрелки в плоскости XY
  Result.x := -dir.y;
  Result.y := dir.x;
  Result.z := 0;
  Result := uzegeometry.NormalizeVertex(Result);
end;

{**
  Создаёт смещённую копию линии.

  @param ASourceLine  - исходная линия
  @param ADist        - расстояние смещения (знак определяет сторону)
  @return Указатель на новую смещённую линию, или nil при нулевой длине
**}
function OffsetLine(ASourceLine: PGDBObjLine; ADist: Double): PGDBObjLine;
var
  lineNormal: TzePoint3d;
  shiftVec: TzePoint3d;
  newBegin, newEnd: TzePoint3d;
  lineLen: Double;
begin
  Result := nil;

  // Проверяем, что длина линии ненулевая
  lineLen := uzegeometry.Vertexlength(
    ASourceLine^.CoordInOCS.lBegin,
    ASourceLine^.CoordInOCS.lEnd
  );
  if lineLen < uzegeometry.eps then begin
    programlog.LogOutFormatStr(
      'uzccommand_offset: OffsetLine — нулевая длина линии, пропускаем',
      [],
      LM_Info
    );
    exit;
  end;

  // Вычисляем нормаль и вектор смещения
  lineNormal := CalcLineNormal(
    ASourceLine^.CoordInOCS.lBegin,
    ASourceLine^.CoordInOCS.lEnd
  );
  shiftVec := uzegeometry.VertexMulOnSc(lineNormal, ADist);

  // Смещаем начало и конец отрезка
  newBegin := uzegeometry.VertexAdd(ASourceLine^.CoordInOCS.lBegin, shiftVec);
  newEnd   := uzegeometry.VertexAdd(ASourceLine^.CoordInOCS.lEnd, shiftVec);

  // Создаём новую линию в области конструирования
  Result := PGDBObjLine(
    ENTF_CreateLine(
      @drawings.GetCurrentDWG^.ConstructObjRoot,
      @drawings.GetCurrentDWG^.ConstructObjRoot.ObjArray,
      drawings.GetCurrentDWG^.GetCurrentLayer,
      drawings.GetCurrentDWG^.GetCurrentLType,
      lwgdbdefault,
      ClByLayer,
      newBegin,
      newEnd
    )
  );

  if Result <> nil then
    zcSetEntPropFromCurrentDrawingProp(PGDBObjEntity(Result));
end;

{**
  Создаёт смещённую копию окружности (изменяется только радиус).

  @param ASourceCircle - исходная окружность
  @param ADist         - расстояние смещения (знак определяет увеличение/уменьшение)
  @return Указатель на новую смещённую окружность, или nil при отрицательном радиусе
**}
function OffsetCircle(ASourceCircle: PGDBObjCircle; ADist: Double): PGDBObjCircle;
var
  newRadius: Double;
begin
  Result := nil;

  newRadius := ASourceCircle^.Radius + ADist;

  // Отрицательный радиус недопустим
  if newRadius <= 0 then begin
    zcUI.TextMessage('Ошибка: смещение превышает радиус окружности', TMWOHistoryOut);
    programlog.LogOutFormatStr(
      'uzccommand_offset: OffsetCircle — отрицательный радиус, пропускаем',
      [],
      LM_Info
    );
    exit;
  end;

  Result := PGDBObjCircle(
    ENTF_CreateCircle(
      @drawings.GetCurrentDWG^.ConstructObjRoot,
      @drawings.GetCurrentDWG^.ConstructObjRoot.ObjArray,
      drawings.GetCurrentDWG^.GetCurrentLayer,
      drawings.GetCurrentDWG^.GetCurrentLType,
      lwgdbdefault,
      ClByLayer,
      ASourceCircle^.Local.P_insert,
      newRadius
    )
  );

  if Result <> nil then
    zcSetEntPropFromCurrentDrawingProp(PGDBObjEntity(Result));
end;

{**
  Создаёт смещённую копию дуги (центр и углы сохраняются, меняется только радиус).

  @param ASourceArc - исходная дуга
  @param ADist      - расстояние смещения
  @return Указатель на новую смещённую дугу, или nil при отрицательном радиусе
**}
function OffsetArc(ASourceArc: PGDBObjArc; ADist: Double): PGDBObjArc;
var
  newRadius: Double;
begin
  Result := nil;

  newRadius := ASourceArc^.R + ADist;

  // Отрицательный радиус недопустим
  if newRadius <= 0 then begin
    zcUI.TextMessage('Ошибка: смещение превышает радиус дуги', TMWOHistoryOut);
    programlog.LogOutFormatStr(
      'uzccommand_offset: OffsetArc — отрицательный радиус, пропускаем',
      [],
      LM_Info
    );
    exit;
  end;

  Result := PGDBObjArc(
    ENTF_CreateArc(
      @drawings.GetCurrentDWG^.ConstructObjRoot,
      @drawings.GetCurrentDWG^.ConstructObjRoot.ObjArray,
      drawings.GetCurrentDWG^.GetCurrentLayer,
      drawings.GetCurrentDWG^.GetCurrentLType,
      lwgdbdefault,
      ClByLayer,
      ASourceArc^.Local.P_insert,
      newRadius,
      ASourceArc^.StartAngle,
      ASourceArc^.EndAngle
    )
  );

  if Result <> nil then
    zcSetEntPropFromCurrentDrawingProp(PGDBObjEntity(Result));
end;

{**
  Вычисляет сторону смещения по точке, указанной пользователем.
  Возвращает знак расстояния: +1 или -1.

  @param AEntity     - исходный объект
  @param ASidePoint  - точка, указывающая сторону смещения
  @param ADist       - модуль расстояния смещения
  @return Расстояние со знаком (+ или -)
**}
function CalcSignedDist(
  AEntity: PGDBObjEntity;
  const ASidePoint: TzePoint3d;
  ADist: Double
): Double;
var
  objType: TObjID;
  pLine: PGDBObjLine;
  pCircle: PGDBObjCircle;
  pArc: PGDBObjArc;
  normal: TzePoint3d;
  vecToPoint: TzePoint3d;
  dotProduct: Double;
  centerToPoint: Double;
begin
  Result := ADist;
  objType := AEntity^.GetObjType;

  case objType of
    GDBlineID: begin
      // Определяем сторону по знаку скалярного произведения нормали и вектора к точке
      pLine := PGDBObjLine(AEntity);
      normal := CalcLineNormal(pLine^.CoordInOCS.lBegin, pLine^.CoordInOCS.lEnd);
      vecToPoint := uzegeometry.VertexSub(ASidePoint, pLine^.CoordInOCS.lBegin);
      dotProduct := uzegeometry.scalardot(normal, vecToPoint);
      if dotProduct < 0 then
        Result := -ADist;
    end;

    GDBCircleID: begin
      // Сторона: если точка снаружи окружности — увеличиваем радиус, иначе уменьшаем
      pCircle := PGDBObjCircle(AEntity);
      centerToPoint := uzegeometry.Vertexlength(
        ASidePoint, pCircle^.Local.P_insert
      );
      if centerToPoint < pCircle^.Radius then
        Result := -ADist;
    end;

    GDBArcID: begin
      // Сторона дуги аналогична окружности
      pArc := PGDBObjArc(AEntity);
      centerToPoint := uzegeometry.Vertexlength(ASidePoint, pArc^.Local.P_insert);
      if centerToPoint < pArc^.R then
        Result := -ADist;
    end;
  end;
end;

{**
  Добавляет готовый новый объект в чертёж через стек undo.

  @param ANewEntity - указатель на уже созданный примитив
**}
procedure CommitNewEntityToDrawing(ANewEntity: PGDBObjEntity);
var
  doMethod, undoMethod: TMethod;
  dc: TDrawContext;
begin
  SetObjCreateManipulator(doMethod, undoMethod);
  dc := drawings.GetCurrentDWG^.CreateDrawingRC;

  PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack.PushStartMarker('Offset');

  with PushMultiObjectCreateCommand(
      PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack,
      TMethod(doMethod),
      TMethod(undoMethod),
      1) do begin
    ANewEntity^.FormatEntity(drawings.GetCurrentDWG^, dc);
    AddObject(ANewEntity);
    comit;
  end;

  PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack.PushEndMarker;

  drawings.GetCurrentDWG^.ConstructObjRoot.ObjArray.Count := 0;
  drawings.GetCurrentROOT^.FormatAfterEdit(drawings.GetCurrentDWG^, dc);
  zcRedrawCurrentDrawing;
end;

{**
  Выполняет операцию смещения выбранного объекта.
  Определяет тип объекта и вызывает соответствующую функцию смещения.

  @param AEntity    - выбранный исходный объект
  @param ASidePoint - точка, указывающая сторону смещения
  @param ADist      - расстояние смещения
**}
procedure PerformOffset(
  AEntity: PGDBObjEntity;
  const ASidePoint: TzePoint3d;
  ADist: Double
);
var
  objType: TObjID;
  signedDist: Double;
  newLine: PGDBObjLine;
  newCircle: PGDBObjCircle;
  newArc: PGDBObjArc;
begin
  if AEntity = nil then
    exit;

  objType := AEntity^.GetObjType;
  signedDist := CalcSignedDist(AEntity, ASidePoint, ADist);

  programlog.LogOutFormatStr(
    'uzccommand_offset: PerformOffset — тип=%d, дистанция=%.4f',
    [objType, signedDist],
    LM_Info
  );

  case objType of
    GDBlineID: begin
      newLine := OffsetLine(PGDBObjLine(AEntity), signedDist);
      if newLine <> nil then
        CommitNewEntityToDrawing(PGDBObjEntity(newLine));
    end;

    GDBCircleID: begin
      newCircle := OffsetCircle(PGDBObjCircle(AEntity), signedDist);
      if newCircle <> nil then
        CommitNewEntityToDrawing(PGDBObjEntity(newCircle));
    end;

    GDBArcID: begin
      newArc := OffsetArc(PGDBObjArc(AEntity), signedDist);
      if newArc <> nil then
        CommitNewEntityToDrawing(PGDBObjEntity(newArc));
    end;
  else
    zcUI.TextMessage(
      'Команда OFFSET: объект данного типа не поддерживается',
      TMWOHistoryOut
    );
  end;
end;

{ ============================================================
  Основная функция команды OFFSET
  ============================================================ }

{**
  Основная команда OFFSET (Смещение/Подобие).
  Работает в интерактивном цикле:
  1. Запрашивает расстояние смещения у пользователя
  2. Запрашивает объект
  3. Запрашивает сторону смещения
  4. Строит новый объект
  5. Повторяет шаги 2-4 до выхода

  @param Context  - контекст команды ZCAD
  @param Operands - операнды (не используются)
  @return Результат выполнения команды
**}
function Offset_com(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;
var
  distInput: String;
  offsetDist: Double;
  selectedEntity: Pointer;
  sidePoint: TzePoint3d;
  pEntity: PGDBObjEntity;
const
  // Расстояние по умолчанию при первом запуске
  DEFAULT_OFFSET_DIST = 1.0;
begin
  Result := cmd_ok;

  // Запрашиваем расстояние смещения у пользователя
  zcUI.TextMessage(
    'Укажите расстояние смещения <' + FloatToStr(DEFAULT_OFFSET_DIST) + '>:',
    TMWOHistoryOut
  );

  if commandmanager.GetInput(
      'Укажите расстояние смещения:',
      distInput
    ) <> IRNormal then begin
    programlog.LogOutFormatStr(
      'uzccommand_offset: пользователь отменил ввод расстояния',
      [],
      LM_Info
    );
    exit;
  end;

  // Разбираем введённое расстояние; при пустом вводе используем значение по умолчанию
  offsetDist := DEFAULT_OFFSET_DIST;
  if Trim(distInput) <> '' then begin
    if not TryStrToFloat(Trim(distInput), offsetDist) then begin
      zcUI.TextMessage('Некорректное значение расстояния', TMWOHistoryOut);
      exit;
    end;
  end;

  // Проверяем, что расстояние положительное
  if offsetDist <= 0 then begin
    zcUI.TextMessage('Ошибка: расстояние должно быть больше нуля', TMWOHistoryOut);
    exit;
  end;

  programlog.LogOutFormatStr(
    'uzccommand_offset: дистанция смещения = %.4f',
    [offsetDist],
    LM_Info
  );

  // Основной рабочий цикл: выбор объекта и стороны
  while commandmanager.GetEntity(rscmSelectEntity, selectedEntity) = IRNormal do begin
    pEntity := PGDBObjEntity(selectedEntity);

    if pEntity = nil then
      continue;

    // Проверяем, что объект поддерживается командой
    if not (pEntity^.GetObjType in [GDBlineID, GDBCircleID, GDBArcID]) then begin
      zcUI.TextMessage(
        'Команда OFFSET: выберите линию, окружность или дугу',
        TMWOHistoryOut
      );
      continue;
    end;

    // Запрашиваем сторону смещения
    zcUI.TextMessage('Укажите сторону смещения:', TMWOHistoryOut);
    if commandmanager.Get3DPoint('Укажите сторону смещения:', sidePoint) <> IRNormal then begin
      programlog.LogOutFormatStr(
        'uzccommand_offset: пользователь отменил выбор стороны',
        [],
        LM_Info
      );
      break;
    end;

    // Выполняем смещение
    PerformOffset(pEntity, sidePoint, offsetDist);
  end;
end;

{ ============================================================
  Инициализация и финализация модуля
  ============================================================ }

initialization
  programlog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsInitializeLMId
  );

  // Регистрируем команду OFFSET в системе ZCAD
  // OFFSET — стандартное имя команды, также регистрируем русский аналог СМЕЩЕНИЕ
  CreateZCADCommand(@Offset_com, 'Offset', CADWG, 0);

finalization
  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsFinalizeLMId
  );

end.
