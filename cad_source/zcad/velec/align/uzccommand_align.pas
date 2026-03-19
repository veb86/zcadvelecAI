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
{$mode delphi}

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
  uzeparsercmdprompt,
  zcmultiobjectchangeundocommand,
  uzegeometrytypes,
  uzeentity,
  uzcLog;

resourcestring
  // Подсказки командной строки для каждого шага диалога
  RSCLPAlignSrc1       = 'Укажите первую исходную точку:';
  RSCLPAlignDst1       = 'Укажите первую целевую точку:';
  RSCLPAlignSrc2       = 'Укажите вторую исходную точку:';
  RSCLPAlignDst2       = 'Укажите вторую целевую точку:';
  RSCLPAlignSrc3       = 'Укажите третью исходную точку или нажмите Enter для пропуска:';
  RSCLPAlignDst3       = 'Укажите третью целевую точку:';
  // Интерактивное меню выбора масштабирования с клавишами [Y] и [N]
  RSCLPAlignScaleYesNo =
    'Масштабировать объекты по точкам выравнивания? [${"&[Y]es",Keys[y],StrId[CLPIdUser1]}, ${"&[N]o",Keys[n],StrId[CLPIdUser2]}] <No>:';

{ Команда ALIGN — функция-обработчик, регистрируемая в менеджере команд }
function AlignCommand(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;

implementation

const
  // Минимальное расстояние для корректного вычисления угла и масштаба
  ALIGN_MIN_DISTANCE = 1e-10;

var
  // Кэш разобранной строки меню масштабирования (инициализируется при первом вызове)
  clScaleYesNo: CMDLinePromptParser.TGeneralParsedText = nil;

{ --- Вычисление матрицы трансформации ALIGN --- }

{
  CalcAlignMatrix
  Вычисляет матрицу трансформации по двум парам точек.

  Алгоритм:
    1. Трансляция: смещение так, чтобы srcPoint1 совпал с dstPoint1.
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

  // Создаём контекст отрисовки до начала цикла трансформации
  dc := drawings.GetCurrentDWG^.CreateDrawingRC;

  PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack.PushStartMarker('Align');
  with PushCreateTGMultiObjectChangeCommand(
      @PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack,
      dispmatr, invertedMatrix, pcoa^.Count) do begin
    pcd := pcoa^.beginiterate(ir);
    if pcd <> nil then
      repeat
        // В режиме {$mode delphi} нельзя взять адрес метода через @obj^.Method,
        // поэтому используем явное присвоение полей tmethod (как в uzcutils.pas)
        m.Code := pointer(pcd^.sourceEnt^.Transform);
        m.Data := pcd^.sourceEnt;
        AddMethod(m);
        Dec(pcd^.sourceEnt^.vp.LastCameraPos);
        pcd^.sourceEnt^.Formatentity(drawings.GetCurrentDWG^, dc);
        pcd := pcoa^.iterate(ir);
      until pcd = nil;
    comit;
  end;
  PTZCADDrawing(drawings.GetCurrentDWG)^.UndoStack.PushEndMarker;

  // Обновляем отображение после применения трансформации
  drawings.GetCurrentROOT^.FormatAfterEdit(drawings.GetCurrentDWG^, dc);
end;

{ --- Работа со списком объектов --- }

{
  CollectSelectedObjects
  Формирует список выбранных объектов для последующего применения трансформации.
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

{ --- Основная функция команды --- }

{
  AlignCommand
  Реализует диалог команды ALIGN с интерактивным меню в стиле команды Rotate.

  Состояния команды (TAlignCmdMode):
    ACMWaitSrc1  — ожидание первой исходной точки
    ACMWaitDst1  — ожидание первой целевой точки
    ACMWaitSrc2  — ожидание второй исходной точки
    ACMWaitDst2  — ожидание второй целевой точки
    ACMWaitSrc3  — ожидание третьей исходной точки (Enter — пропуск)
    ACMWaitDst3  — ожидание третьей целевой точки
    ACMWaitScale — ожидание ответа о масштабировании [Yes/No]

  Для ввода точек используется Get3DPoint.
  Для ввода ответа на вопрос о масштабировании — GetInput с интерактивным меню.
  Пользователь может в любой момент нажать ESC для отмены.
}
function AlignCommand(
  const Context: TZCADCommandContext;
  Operands: TCommandOperands
): TCommandResult;
type
  TAlignCmdMode = (
    ACMWaitSrc1,
    ACMWaitDst1,
    ACMWaitSrc2,
    ACMWaitDst2,
    ACMWaitSrc3,
    ACMWaitDst3,
    ACMWaitScale
  );
var
  pcoa: ptpcoavector;
  objectCount: integer;
  srcPoint1, dstPoint1: TzePoint3d;
  srcPoint2, dstPoint2: TzePoint3d;
  applyScale: boolean;
  CmdMode: TAlignCmdMode;
  gr: TzcInteractiveResult;
  p1: TzePoint3d;
  inputStr: string;

  procedure LogMessage(const msg: string);
  begin
    zcUI.TextMessage(msg, TMWOHistoryOut);
  end;

  // Устанавливает текущее состояние диалога и обновляет подсказку командной строки
  procedure SetAlignCmdMode(ANewMode: TAlignCmdMode);
  begin
    CmdMode := ANewMode;
    case ANewMode of
      ACMWaitSrc1: begin
        LogMessage('ALIGN: Укажите первую исходную точку');
        commandmanager.ChangeInputMode([], [IPEmpty]);
        commandmanager.SetPrompt(RSCLPAlignSrc1);
      end;
      ACMWaitDst1: begin
        LogMessage(Format('ALIGN: Первая исходная точка = (%.3f, %.3f, %.3f)',
          [srcPoint1.x, srcPoint1.y, srcPoint1.z]));
        LogMessage('ALIGN: Укажите первую целевую точку');
        commandmanager.ChangeInputMode([], [IPEmpty]);
        commandmanager.SetPrompt(RSCLPAlignDst1);
      end;
      ACMWaitSrc2: begin
        LogMessage(Format('ALIGN: Первая целевая точка = (%.3f, %.3f, %.3f)',
          [dstPoint1.x, dstPoint1.y, dstPoint1.z]));
        LogMessage('ALIGN: Укажите вторую исходную точку');
        commandmanager.ChangeInputMode([], [IPEmpty]);
        commandmanager.SetPrompt(RSCLPAlignSrc2);
      end;
      ACMWaitDst2: begin
        LogMessage(Format('ALIGN: Вторая исходная точка = (%.3f, %.3f, %.3f)',
          [srcPoint2.x, srcPoint2.y, srcPoint2.z]));
        LogMessage('ALIGN: Укажите вторую целевую точку');
        commandmanager.ChangeInputMode([], [IPEmpty]);
        commandmanager.SetPrompt(RSCLPAlignDst2);
      end;
      ACMWaitSrc3: begin
        LogMessage(Format('ALIGN: Вторая целевая точка = (%.3f, %.3f, %.3f)',
          [dstPoint2.x, dstPoint2.y, dstPoint2.z]));
        LogMessage('ALIGN: Укажите третью исходную точку или нажмите Enter для пропуска');
        // Разрешаем пустой ввод (Enter) для пропуска третьей пары точек
        commandmanager.ChangeInputMode([IPEmpty], []);
        commandmanager.SetPrompt(RSCLPAlignSrc3);
      end;
      ACMWaitDst3: begin
        LogMessage(Format('ALIGN: Третья исходная точка = (%.3f, %.3f, %.3f)',
          [p1.x, p1.y, p1.z]));
        LogMessage('ALIGN: Укажите третью целевую точку');
        commandmanager.ChangeInputMode([], [IPEmpty]);
        commandmanager.SetPrompt(RSCLPAlignDst3);
      end;
      ACMWaitScale: begin
        if CmdMode <> ACMWaitDst3 then
          LogMessage('ALIGN: Третья пара точек пропущена');
        LogMessage('ALIGN: Масштабировать объекты? [Yes/No]');
        // Разрешаем пустой ввод (Enter = No по умолчанию)
        commandmanager.ChangeInputMode([IPEmpty], []);
        // Инициализируем кэш разобранной строки меню при первом вызове
        if clScaleYesNo = nil then
          clScaleYesNo := CMDLinePromptParser.GetTokens(RSCLPAlignScaleYesNo);
        commandmanager.SetPrompt(clScaleYesNo);
      end;
    end;
  end;

  // Применяет вычисленную трансформацию и логирует результат
  procedure ApplyAndFinish;
  var
    dispmatr: TzeTypedMatrix4d;
    srcLen, dstLen, scaleValue: double;
  begin
    // Вычисляем длины векторов для отладки
    srcLen := uzegeometry.Vertexlength(srcPoint1, srcPoint2);
    dstLen := uzegeometry.Vertexlength(dstPoint1, dstPoint2);

    LogMessage('');
    LogMessage('ALIGN: === ВЫЧИСЛЕНИЕ ТРАНСФОРМАЦИИ ===');
    LogMessage(Format('ALIGN: Src1=(%.3f,%.3f,%.3f), Dst1=(%.3f,%.3f,%.3f)',
      [srcPoint1.x, srcPoint1.y, srcPoint1.z, dstPoint1.x, dstPoint1.y, dstPoint1.z]));
    LogMessage(Format('ALIGN: Src2=(%.3f,%.3f,%.3f), Dst2=(%.3f,%.3f,%.3f)',
      [srcPoint2.x, srcPoint2.y, srcPoint2.z, dstPoint2.x, dstPoint2.y, dstPoint2.z]));
    LogMessage(Format('ALIGN: Длина исходного вектора: %.6f', [srcLen]));
    LogMessage(Format('ALIGN: Длина целевого вектора: %.6f', [dstLen]));

    if srcLen > ALIGN_MIN_DISTANCE then begin
      scaleValue := dstLen / srcLen;
      LogMessage(Format('ALIGN: Коэффициент масштабирования: %.6f', [scaleValue]));
    end
    else
      LogMessage('ALIGN: Предупреждение - нулевая длина исходного вектора!');

    LogMessage(Format('ALIGN: Масштабирование: %s', [BoolToStr(applyScale, True)]));
    LogMessage('ALIGN: ================================');
    LogMessage('');

    dispmatr := CalcAlignMatrix(
      srcPoint1, dstPoint1, srcPoint2, dstPoint2, applyScale
    );
    ApplyTransformToObjects(pcoa, dispmatr);

    LogMessage('ALIGN: Трансформация применена');
    programlog.LogOutFormatStr(
      'uzccommand_align: трансформация применена к %d объектам',
      [objectCount], LM_Info
    );
  end;

begin
  Result := cmd_ok;
  pcoa := nil;
  applyScale := False;

  LogMessage('========================================');
  LogMessage('ALIGN: Запуск команды ALIGN');
  LogMessage('========================================');

  programlog.LogOutFormatStr(
    'uzccommand_align: запуск команды ALIGN', [], LM_Info
  );

  // Проверяем наличие выбранных объектов
  objectCount := CollectSelectedObjects(pcoa);
  if objectCount = 0 then begin
    LogMessage('ALIGN: Ошибка - нет выбранных объектов');
    zcUI.TextMessage(rscmSelEntBeforeComm, TMWOHistoryOut);
    programlog.LogOutFormatStr(
      'uzccommand_align: нет выбранных объектов, команда завершена', [], LM_Info
    );
    Exit;
  end;

  LogMessage(Format('ALIGN: Выбрано объектов: %d', [objectCount]));
  programlog.LogOutFormatStr(
    'uzccommand_align: выбрано объектов: %d', [objectCount], LM_Info
  );

  try
    SetAlignCmdMode(ACMWaitSrc1);

    // Основной цикл: сбор точек (шаги src1..dst3)
    repeat
      gr := commandmanager.Get3DPoint('', p1);

      case gr of
        IRNormal:
          // Пользователь указал точку — записываем и переходим к следующему шагу
          case CmdMode of
            ACMWaitSrc1: begin
              srcPoint1 := p1;
              LogMessage(Format('ALIGN: Введена src1 = (%.3f, %.3f, %.3f)', [p1.x, p1.y, p1.z]));
              programlog.LogOutFormatStr(
                'uzccommand_align: src1=(%.3f,%.3f)', [p1.x, p1.y], LM_Info
              );
              SetAlignCmdMode(ACMWaitDst1);
            end;
            ACMWaitDst1: begin
              dstPoint1 := p1;
              LogMessage(Format('ALIGN: Введена dst1 = (%.3f, %.3f, %.3f)', [p1.x, p1.y, p1.z]));
              programlog.LogOutFormatStr(
                'uzccommand_align: dst1=(%.3f,%.3f)', [p1.x, p1.y], LM_Info
              );
              SetAlignCmdMode(ACMWaitSrc2);
            end;
            ACMWaitSrc2: begin
              srcPoint2 := p1;
              LogMessage(Format('ALIGN: Введена src2 = (%.3f, %.3f, %.3f)', [p1.x, p1.y, p1.z]));
              programlog.LogOutFormatStr(
                'uzccommand_align: src2=(%.3f,%.3f)', [p1.x, p1.y], LM_Info
              );
              SetAlignCmdMode(ACMWaitDst2);
            end;
            ACMWaitDst2: begin
              dstPoint2 := p1;
              LogMessage(Format('ALIGN: Введена dst2 = (%.3f, %.3f, %.3f)', [p1.x, p1.y, p1.z]));
              programlog.LogOutFormatStr(
                'uzccommand_align: dst2=(%.3f,%.3f)', [p1.x, p1.y], LM_Info
              );
              SetAlignCmdMode(ACMWaitSrc3);
            end;
            ACMWaitSrc3: begin
              // Третья исходная точка — переходим к ожиданию целевой
              programlog.LogOutFormatStr(
                'uzccommand_align: src3=(%.3f,%.3f)', [p1.x, p1.y], LM_Info
              );
              SetAlignCmdMode(ACMWaitDst3);
            end;
            ACMWaitDst3: begin
              // NOTE: Третья пара принята. В текущей версии не используется
              // для трансформации — зарезервирована для 3D-выравнивания.
              programlog.LogOutFormatStr(
                'uzccommand_align: dst3=(%.3f,%.3f)', [p1.x, p1.y], LM_Info
              );
              SetAlignCmdMode(ACMWaitScale);
            end;
          end;

        IRInput:
          // Пустой ввод Enter при IPEmpty: пропустить третью пару точек
          if CmdMode = ACMWaitSrc3 then begin
            LogMessage('ALIGN: Третья пара точек пропущена (Enter)');
            programlog.LogOutFormatStr(
              'uzccommand_align: третья пара пропущена', [], LM_Info
            );
            SetAlignCmdMode(ACMWaitScale);
          end;

      end; // case gr
    until (gr = IRCancel) or (CmdMode = ACMWaitScale);

    // Прерываем если пользователь отменил до достижения шага масштабирования
    if gr = IRCancel then begin
      LogMessage('ALIGN: Команда отменена пользователем');
      Exit;
    end;

    // Цикл ожидания ответа о масштабировании [Yes/No]
    repeat
      gr := commandmanager.GetInput('', inputStr);

      case gr of
        IRNormal, IRInput: begin
          // Пустой ввод (Enter) = No по умолчанию
          applyScale := False;
          LogMessage('ALIGN: Масштабирование: No (по умолчанию, Enter)');
          programlog.LogOutFormatStr(
            'uzccommand_align: масштабирование=No (по умолчанию)', [], LM_Info
          );
          ApplyAndFinish;
          Break;
        end;
        IRId:
          case commandmanager.GetLastId of
            CLPIdUser1: begin
              // Пользователь нажал [Y]es
              applyScale := True;
              LogMessage('ALIGN: Масштабирование: Yes (клавиша Y)');
              programlog.LogOutFormatStr(
                'uzccommand_align: масштабирование=Yes', [], LM_Info
              );
              ApplyAndFinish;
              Break;
            end;
            CLPIdUser2: begin
              // Пользователь нажал [N]o
              applyScale := False;
              LogMessage('ALIGN: Масштабирование: No (клавиша N)');
              programlog.LogOutFormatStr(
                'uzccommand_align: масштабирование=No', [], LM_Info
              );
              ApplyAndFinish;
              Break;
            end;
          end;
      end; // case gr
    until gr = IRCancel;

  finally
    // Освобождаем ресурсы независимо от пути завершения
    FreeObjectsList(pcoa);
    if Result = cmd_ok then begin
      LogMessage('ALIGN: Команда завершена успешно');
      LogMessage('========================================');
    end
    else begin
      LogMessage('ALIGN: Команда завершена с ошибкой');
    end;
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
  clScaleYesNo.Free;
end.
