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
{$MODE OBJFPC}{$H+}

{
  Модуль: uzccommand_align
  Назначение: Реализация команды ALIGN — выравнивание объектов по опорным точкам.
  Команда за одну операцию выполняет перемещение, поворот и масштабирование
  объектов, аналогично команде ALIGN в AutoCAD.
  Порядок работы:
    1. Пользователь выбирает объекты (до запуска команды).
    2. Указывает первую пару точек — определяет перемещение.
    3. Указывает вторую пару точек — определяет поворот.
    4. Опционально указывает третью пару точек (Enter — пропуск).
    5. Отвечает на вопрос о масштабировании [Yes/No].
  Зависимости:
    uzccommand_move    — базовый класс и вспомогательные типы объектов
    uzegeometry        — операции с матрицами и 3D-точками
    uzccommandsmanager — менеджер команд (Get3DPoint, GetInput)
    uzclog             — логирование
}
unit uzccommand_align;
{$INCLUDE zengineconfig.inc}

interface

uses
  Math,
  SysUtils,
  gzctnrVectorTypes,
  uzcdrawing,
  uzgldrawcontext,
  uzcdrawings,
  uzeutils,
  uzglviewareadata,
  uzccommand_move,
  uzccommandsabstract,
  uzccommandsimpl,
  uzccommandsmanager,
  uzcinterface,
  uzcstrconsts,
  uzegeometry,
  zcmultiobjectchangeundocommand,
  uzegeometrytypes,
  uzeentity,
  uzcLog;

const
  // Минимальное расстояние для корректного вычисления угла и масштаба
  ALIGN_MIN_DISTANCE = 1e-10;

{ Команда ALIGN — функция-обработчик, регистрируемая в менеджере команд }
function AlignCommand(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;

implementation

{ --- Вспомогательные процедуры вывода --- }

{
  PrintMessage
  Выводит строку сообщения в историю командной строки ZCAD.
}
procedure PrintMessage(const Msg: string);
begin
  zcUI.TextMessage(Msg, TMWOHistoryOut);
end;

{ --- Работа со списком объектов --- }

{
  CollectSelectedObjects
  Формирует список выбранных объектов для последующего применения трансформации.
  Параметры:
    pcoa — выходной указатель на вектор объектов.
  Возвращает количество выбранных объектов (0 — если ничего не выбрано).
}
function CollectSelectedObjects(out pcoa: ptpcoavector): integer;
var
  pobj: PGDBObjEntity;
  ir: itrec;
  counter: integer;
  tcd: TCopyObjectDesc;
begin
  counter := 0;
  pcoa := nil;

  // Первый проход: подсчёт выбранных объектов
  pobj := drawings.GetCurrentROOT^.ObjArray.beginiterate(ir);
  if pobj <> nil then
    repeat
      if pobj^.selected then
        Inc(counter);
      pobj := drawings.GetCurrentROOT^.ObjArray.iterate(ir);
    until pobj = nil;

  if counter = 0 then begin
    Result := 0;
    Exit;
  end;

  // Выделяем память и заполняем список ссылками на выбранные объекты
  Getmem(Pointer(pcoa), sizeof(tpcoavector));
  pcoa^.init(counter);

  pobj := drawings.GetCurrentROOT^.ObjArray.beginiterate(ir);
  if pobj <> nil then
    repeat
      if pobj^.selected then begin
        tcd.sourceEnt := pobj;
        tcd.tmpProxy := nil;
        tcd.copyEnt := nil;
        pcoa^.PushBackData(tcd);
      end;
      pobj := drawings.GetCurrentROOT^.ObjArray.iterate(ir);
    until pobj = nil;

  Result := counter;
end;

{
  FreeObjectsList
  Освобождает память вектора объектов.
}
procedure FreeObjectsList(var pcoa: ptpcoavector);
begin
  if pcoa <> nil then begin
    pcoa^.done;
    Freemem(Pointer(pcoa));
    pcoa := nil;
  end;
end;

{ --- Вычисление матрицы трансформации ALIGN --- }

{
  CalcAlignMatrix
  Вычисляет матрицу трансформации по двум парам точек.

  Алгоритм:
    1. Трансляция: перемещение так, чтобы srcPoint1 совпал с dstPoint1.
    2. Поворот вокруг dstPoint1: направление src1->src2 совмещается с dst1->dst2.
    3. Масштабирование (если applyScale = True):
       коэффициент = |dst1-dst2| / |src1-src2|.

  При нулевом расстоянии между точками поворот и масштабирование пропускаются.
}
function CalcAlignMatrix(
  const srcPoint1, dstPoint1: TzePoint3d;
  const srcPoint2, dstPoint2: TzePoint3d;
  const applyScale: boolean
): TzeTypedMatrix4d;
var
  srcLen, dstLen, scaleValue: double;
  srcAngle, dstAngle, rotAngle: double;
  rotationMatrix, scaleMatrix: TzeTypedMatrix4d;
  resultMatrix: TzeTypedMatrix4d;
begin
  // Шаг 1: Трансляция — смещаем srcPoint1 в dstPoint1
  resultMatrix := uzegeometry.CreateTranslationMatrix(
    uzegeometry.VertexSub(dstPoint1, srcPoint1)
  );

  // Шаг 2: Поворот — применяем только при ненулевых расстояниях
  srcLen := uzegeometry.Vertexlength(srcPoint1, srcPoint2);
  dstLen := uzegeometry.Vertexlength(dstPoint1, dstPoint2);

  if (srcLen > ALIGN_MIN_DISTANCE) and (dstLen > ALIGN_MIN_DISTANCE) then begin
    // Угол исходного направления (srcPoint1 -> srcPoint2)
    srcAngle := ArcTan2(
      srcPoint2.y - srcPoint1.y,
      srcPoint2.x - srcPoint1.x
    );

    // Угол целевого направления (dstPoint1 -> dstPoint2)
    dstAngle := ArcTan2(
      dstPoint2.y - dstPoint1.y,
      dstPoint2.x - dstPoint1.x
    );

    // Матрица поворота вокруг dstPoint1:
    //   T(-dstPoint1) * Rz(dstAngle - srcAngle) * T(dstPoint1)
    rotAngle := dstAngle - srcAngle;
    rotationMatrix := uzegeometry.CreateTranslationMatrix(-dstPoint1);
    rotationMatrix := uzegeometry.MatrixMultiply(
      rotationMatrix,
      uzegeometry.CreateRotationMatrixZ(rotAngle)
    );
    rotationMatrix := uzegeometry.MatrixMultiply(
      rotationMatrix,
      uzegeometry.CreateTranslationMatrix(dstPoint1)
    );

    resultMatrix := uzegeometry.MatrixMultiply(resultMatrix, rotationMatrix);

    // Шаг 3: Масштабирование — только по запросу пользователя
    if applyScale then begin
      scaleValue := dstLen / srcLen;

      // Матрица масштабирования относительно точки dstPoint1:
      //   T(-dstPoint1) * Scale(scaleValue) * T(dstPoint1)
      scaleMatrix := uzegeometry.CreateTranslationMatrix(-dstPoint1);
      scaleMatrix := uzegeometry.MatrixMultiply(
        scaleMatrix,
        CreateScaleMatrix(scaleValue)
      );
      scaleMatrix := uzegeometry.MatrixMultiply(
        scaleMatrix,
        uzegeometry.CreateTranslationMatrix(dstPoint1)
      );

      resultMatrix := uzegeometry.MatrixMultiply(resultMatrix, scaleMatrix);
    end;
  end;

  Result := resultMatrix;
end;

{ --- Применение трансформации к объектам --- }

{
  ApplyTransformToObjects
  Применяет матрицу трансформации dispmatr ко всем объектам из pcoa.
  Операция регистрируется в стеке undo/redo.
}
procedure ApplyTransformToObjects(
  const pcoa: ptpcoavector;
  const dispmatr: TzeTypedMatrix4d
);
var
  invertedMatrix: TzeTypedMatrix4d;
  ir: itrec;
  pcd: PTCopyObjectDesc;
  m: tmethod;
  dc: TDrawContext;
begin
  // Вычисляем обратную матрицу для поддержки undo
  invertedMatrix := dispmatr;
  uzegeometry.MatrixInvert(invertedMatrix);

  PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack.PushStartMarker('Align');
  with PushCreateTGMultiObjectChangeCommand(
      @PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack,
      dispmatr, invertedMatrix, pcoa^.Count) do begin
    pcd := pcoa^.beginiterate(ir);
    if pcd <> nil then
      repeat
        m := tmethod(@pcd^.sourceEnt^.Transform);
        AddMethod(m);
        Dec(pcd^.sourceEnt^.vp.LastCameraPos);
        pcd := pcoa^.iterate(ir);
      until pcd = nil;
    comit;
  end;
  PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack.PushEndMarker;

  // Обновляем отображение после применения трансформации
  dc := drawings.GetCurrentDWG^.CreateDrawingRC;
  drawings.GetCurrentROOT^.FormatAfterEdit(drawings.GetCurrentDWG^, dc);
end;

{ --- Запрос точки от пользователя --- }

{
  GetAlignPoint
  Запрашивает точку у пользователя через командную строку.
  Параметры:
    prompt     — строка подсказки.
    pt         — выходная точка.
    allowEmpty — True разрешает пропуск нажатием Enter.
  Возвращает True если точка выбрана, False при отмене или пропуске.
}
function GetAlignPoint(
  const prompt: string;
  out pt: TzePoint3d;
  const allowEmpty: boolean
): boolean;
var
  getResult: TzcInteractiveResult;
begin
  if allowEmpty then
    commandmanager.ChangeInputMode([IPEmpty], []);

  getResult := commandmanager.Get3DPoint(prompt, pt);
  Result := getResult = IRNormal;
end;

{ --- Основная функция команды --- }

{
  AlignCommand
  Реализует диалог команды ALIGN:
    1. Проверяет наличие выбранных объектов.
    2. Запрашивает первую пару точек (перемещение).
    3. Запрашивает вторую пару точек (поворот).
    4. Запрашивает третью пару точек (необязательно).
    5. Запрашивает применение масштабирования.
    6. Вычисляет и применяет итоговую трансформацию.
  При отмене (ESC) изменения не применяются.
}
function AlignCommand(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;
var
  pcoa: ptpcoavector;
  objectCount: integer;
  srcPoint1, dstPoint1: TzePoint3d;
  srcPoint2, dstPoint2: TzePoint3d;
  srcPoint3, dstPoint3: TzePoint3d;
  applyScale: boolean;
  scaleAnswer: string;
  dispmatr: TzeTypedMatrix4d;
  inputResult: TzcInteractiveResult;
begin
  Result := cmd_ok;
  pcoa := nil;
  applyScale := False;

  programlog.LogOutFormatStr(
    'uzccommand_align: запуск команды ALIGN',
    [],
    LM_Info
  );

  // Проверяем наличие выбранных объектов
  objectCount := CollectSelectedObjects(pcoa);
  if objectCount = 0 then begin
    PrintMessage(rscmSelEntBeforeComm);
    programlog.LogOutFormatStr(
      'uzccommand_align: нет выбранных объектов, команда завершена',
      [],
      LM_Info
    );
    Exit;
  end;

  programlog.LogOutFormatStr(
    'uzccommand_align: выбрано объектов: %d',
    [objectCount],
    LM_Info
  );

  try
    // --- Первая пара точек: перемещение ---
    if not GetAlignPoint('Specify first source point:', srcPoint1, False) then begin
      PrintMessage('Command cancelled.');
      Exit;
    end;

    if not GetAlignPoint('Specify first destination point:', dstPoint1, False) then begin
      PrintMessage('Command cancelled.');
      Exit;
    end;

    programlog.LogOutFormatStr(
      'uzccommand_align: src1=(%.3f,%.3f) dst1=(%.3f,%.3f)',
      [srcPoint1.x, srcPoint1.y, dstPoint1.x, dstPoint1.y],
      LM_Info
    );

    // --- Вторая пара точек: поворот ---
    if not GetAlignPoint('Specify second source point:', srcPoint2, False) then begin
      PrintMessage('Command cancelled.');
      Exit;
    end;

    if not GetAlignPoint('Specify second destination point:', dstPoint2, False) then begin
      PrintMessage('Command cancelled.');
      Exit;
    end;

    programlog.LogOutFormatStr(
      'uzccommand_align: src2=(%.3f,%.3f) dst2=(%.3f,%.3f)',
      [srcPoint2.x, srcPoint2.y, dstPoint2.x, dstPoint2.y],
      LM_Info
    );

    // --- Третья пара точек: необязательна, Enter — пропуск ---
    if GetAlignPoint(
        'Specify third source point or [Enter to skip]:',
        srcPoint3, True) then begin
      // Третья исходная точка указана — запрашиваем целевую
      if GetAlignPoint('Specify third destination point:', dstPoint3, False) then begin
        programlog.LogOutFormatStr(
          'uzccommand_align: третья пара: src3=(%.3f,%.3f) dst3=(%.3f,%.3f)',
          [srcPoint3.x, srcPoint3.y, dstPoint3.x, dstPoint3.y],
          LM_Info
        );
        // NOTE: Третья пара точек принята, но в текущей версии
        // используется только для подтверждения пользователем.
        // В будущем может быть использована для 3D-выравнивания.
      end else begin
        // Пользователь отменил ввод третьей целевой точки
        PrintMessage('Command cancelled.');
        Exit;
      end;
    end else begin
      programlog.LogOutFormatStr(
        'uzccommand_align: третья пара пропущена',
        [],
        LM_Info
      );
    end;

    // --- Запрос масштабирования [Yes/No] <No> ---
    commandmanager.ChangeInputMode([IPEmpty], []);
    inputResult := commandmanager.GetInput(
      'Scale objects based on alignment points? [Yes/No] <No>:',
      scaleAnswer
    );

    // Пустой ввод (Enter) или отмена — используем значение по умолчанию (No)
    if inputResult = IRNormal then begin
      scaleAnswer := LowerCase(Trim(scaleAnswer));
      applyScale := (scaleAnswer = 'yes') or (scaleAnswer = 'y');
    end;

    programlog.LogOutFormatStr(
      'uzccommand_align: масштабирование=%s',
      [BoolToStr(applyScale, 'Yes', 'No')],
      LM_Info
    );

    // --- Вычисляем и применяем итоговую трансформацию ---
    dispmatr := CalcAlignMatrix(
      srcPoint1, dstPoint1,
      srcPoint2, dstPoint2,
      applyScale
    );

    ApplyTransformToObjects(pcoa, dispmatr);

    programlog.LogOutFormatStr(
      'uzccommand_align: трансформация применена к %d объектам',
      [objectCount],
      LM_Info
    );

  finally
    // Освобождаем ресурсы независимо от пути завершения
    FreeObjectsList(pcoa);
  end;
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsInitializeLMId);
  // Регистрируем команду ALIGN в системе команд ZCAD
  CreateZCADCommand(@AlignCommand, 'Align', CADWG, 0);

finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsFinalizeLMId);
end.
