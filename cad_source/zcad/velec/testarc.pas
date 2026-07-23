{
*****************************************************************************
*  Test Arc Command - Testing arc rotation and mirroring
*****************************************************************************
}
{$mode objfpc}{$H+}

unit testarc;
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Math,
  uzeentarc,
  uzeentity,
  uzeentityfactory,
  uzegeometrytypes,
  uzeconsts,
  uzeTypes,
  uzegeometry,
  uzgldrawcontext,
  uzccommandsmanager,
  uzccommandsabstract,
  uzccommandsimpl,
  uzcdrawings,
  uzcutils,
  uzclog,
  uzestyleslayers,
  uzcinterface;

function TestArcCommand_com(const Context:TZCADCommandContext;
  operands:TCommandOperands):TCommandResult;

implementation

procedure LogMessage(const msg:string);
begin
  zcUI.TextMessage(msg, TMWOHistoryOut);
end;

function Point3DToStr(const p:TzePoint3d):string;
begin
  Result := Format('X=%.6f, Y=%.6f, Z=%.6f', [p.x, p.y, p.z]);
end;

function Vector3DToStr(const p:TzeVector3d):string;
begin
  Result := Format('X=%.6f, Y=%.6f, Z=%.6f', [p.x, p.y, p.z]);
end;

function AngleToDeg(angle:double):double;
begin
  Result := angle * 180.0 / PI;
end;

procedure LogMatrix(const prefix:string; const m:TzeTypedMatrix4d);
begin
  LogMessage(prefix + ' ObjMatrix:');
  LogMessage(Format('     [%.6f, %.6f, %.6f, %.6f]', [m.mtr.v[0].v[0], m.mtr.v[0].v[1], m.mtr.v[0].v[2], m.mtr.v[0].v[3]]));
  LogMessage(Format('     [%.6f, %.6f, %.6f, %.6f]', [m.mtr.v[1].v[0], m.mtr.v[1].v[1], m.mtr.v[1].v[2], m.mtr.v[1].v[3]]));
  LogMessage(Format('     [%.6f, %.6f, %.6f, %.6f]', [m.mtr.v[2].v[0], m.mtr.v[2].v[1], m.mtr.v[2].v[2], m.mtr.v[2].v[3]]));
  LogMessage(Format('     [%.6f, %.6f, %.6f, %.6f]', [m.mtr.v[3].v[0], m.mtr.v[3].v[1], m.mtr.v[3].v[2], m.mtr.v[3].v[3]]));
end;

procedure LogArcInfo(const prefix:string; pa:PGDBObjArc);
var
  det: double;
begin
  // Вычисляем определитель матрицы 4x4 (для матрицы с последней строкой [0,0,0,1])
  // определитель равен определителю верхнего левого блока 3x3
  det := pa^.objmatrix.mtr.v[0].v[0] * (pa^.objmatrix.mtr.v[1].v[1] * pa^.objmatrix.mtr.v[2].v[2] - pa^.objmatrix.mtr.v[1].v[2] * pa^.objmatrix.mtr.v[2].v[1])
       - pa^.objmatrix.mtr.v[0].v[1] * (pa^.objmatrix.mtr.v[1].v[0] * pa^.objmatrix.mtr.v[2].v[2] - pa^.objmatrix.mtr.v[1].v[2] * pa^.objmatrix.mtr.v[2].v[0])
       + pa^.objmatrix.mtr.v[0].v[2] * (pa^.objmatrix.mtr.v[1].v[0] * pa^.objmatrix.mtr.v[2].v[1] - pa^.objmatrix.mtr.v[1].v[1] * pa^.objmatrix.mtr.v[2].v[0]);

  LogMessage(Format('   %s StartAngle: %.6f (%.2f°)', [prefix, pa^.StartAngle, AngleToDeg(pa^.StartAngle)]));
  LogMessage(Format('   %s EndAngle: %.6f (%.2f°)', [prefix, pa^.EndAngle, AngleToDeg(pa^.EndAngle)]));
  LogMessage(Format('   %s Angle (sweep): %.6f (%.2f°)', [prefix, pa^.angle, AngleToDeg(pa^.angle)]));
  LogMessage(Format('   %s P_insert (центр в WCS): %s', [prefix, Point3DToStr(pa^.P_insert_in_WCS)]));
  LogMessage(Format('   %s Local.p_insert: X=%.6f, Y=%.6f, Z=%.6f', [prefix, pa^.Local.p_insert.x, pa^.Local.p_insert.y, pa^.Local.p_insert.z]));
  LogMessage(Format('   %s q0 (начальная точка): %s', [prefix, Point3DToStr(pa^.q0)]));
  LogMessage(Format('   %s q1 (средняя точка): %s', [prefix, Point3DToStr(pa^.q1)]));
  LogMessage(Format('   %s q2 (конечная точка): %s', [prefix, Point3DToStr(pa^.q2)]));
  LogMessage(Format('   %s Radius: %.6f', [prefix, pa^.R]));
  LogMessage(Format('   %s ObjMatrix determinant: %.6f (зеркало=%s)', [prefix, det, BoolToStr(det < 0, True)]));

  // Вывод матрицы объекта
  LogMatrix(prefix + ' ', pa^.objmatrix);

  // Вывод локальной СК
  LogMessage(Format('   %s Local.basis.OX: %s', [prefix, Vector3DToStr(pa^.Local.basis.ox)]));
  LogMessage(Format('   %s Local.basis.OY: %s', [prefix, Vector3DToStr(pa^.Local.basis.oy)]));
  LogMessage(Format('   %s Local.basis.OZ: %s', [prefix, Vector3DToStr(pa^.Local.basis.oz)]));

  // Проверка: радиус должен соответствовать масштабу по осям
  LogMessage(Format('   %s Проверка масштаба: OX.x=%.6f, OY.y=%.6f, R=%.6f', [prefix, pa^.Local.basis.ox.x, pa^.Local.basis.oy.y, pa^.R]));
end;

function TestArcCommand_com(const Context:TZCADCommandContext;
  operands:TCommandOperands):TCommandResult;
var
  pa1, pa2, pa3, pa4, pa5: PGDBObjArc;
  dc: TDrawContext;
  centerPoint: TzePoint3d;
  radius: double;
  startAngle, endAngle: double;
  rotationAngle, rotationAngleY, rotationAngleX: double;
  rotMatrix, mirrorMatrix, dispmatr: TzeTypedMatrix4d;
  q0_before, q1_before, q2_before: TzePoint3d;
  q0_after, q1_after, q2_after: TzePoint3d;
begin
  LogMessage('========================================');
  LogMessage('TEST ARC COMMAND - Starting test');
  LogMessage('========================================');
  
  // Исходные данные
  centerPoint.x := -7;
  centerPoint.y := 29;
  centerPoint.z := 0;
  radius := 83;
  startAngle := 43 * PI / 180;
  endAngle := 184 * PI / 180;
  rotationAngle := 90 * PI / 180; // 90 градусов
  rotationAngleY := 33 * PI / 180; // 33 градуса для поворота вокруг Y
  rotationAngleX := 90 * PI / 180; // 90 градусов для поворота вокруг X
  
  LogMessage('');
  LogMessage('Исходные параметры дуги:');
  LogMessage('----------------------------------------');
  LogMessage(Format('   Центр: %s', [Point3DToStr(centerPoint)]));
  LogMessage(Format('   Радиус: %.6f', [radius]));
  LogMessage(Format('   Начальный угол: %.6f (%.2f°)', [startAngle, AngleToDeg(startAngle)]));
  LogMessage(Format('   Конечный угол: %.6f (%.2f°)', [endAngle, AngleToDeg(endAngle)]));
  
  // ============================================
  // ТЕСТ 1: ПОВОРОТ НА 90 ГРАДУСОВ
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ТЕСТ 1: ПОСЛЕДОВАТЕЛЬНЫЕ ПОВОРОТЫ ДУГИ');
  LogMessage('========================================');
  
  pa1 := AllocEnt(GDBArcID);
  pa1^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);
  
  zcSetEntPropFromCurrentDrawingProp(pa1);
  pa1^.vp.LineWeight := LnWt200;
  pa1^.vp.Color := 2; // желтый
  
  dc := drawings.GetCurrentDWG^.CreateDrawingRC;
  pa1^.FormatEntity(drawings.GetCurrentDWG^, dc);
  
  LogMessage('');
  LogMessage('ИСХОДНОЕ СОСТОЯНИЕ:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa1);
  
  // Поворот на 90 градусов
  LogMessage('');
  LogMessage('ПОВОРОТ НА 90°:');
  LogMessage('----------------------------------------');
  rotationAngle := 90 * PI / 180;
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngle, AngleToDeg(rotationAngle)]));
  
  // Создаем матрицу поворота вокруг центра дуги (как в команде RotateEnts)
  // В ZCAD матрицы транспонированы (перенос в 4-й строке), векторы умножаются справа: v' = v * M
  // Порядок умножения: M = T(-center) * R * T(center)
  // То есть: сначала T(center), потом R, потом T(-center)
  dispmatr := CreateTranslationMatrix(CreateVector(centerPoint.x, centerPoint.y, centerPoint.z));
  rotMatrix := CreateRotationMatrixZ(rotationAngle);
  rotMatrix := MatrixMultiply(rotMatrix, dispmatr);  // R * T(center)
  dispmatr := CreateTranslationMatrix(CreateVector(-centerPoint.x, -centerPoint.y, -centerPoint.z));
  rotMatrix := MatrixMultiply(dispmatr, rotMatrix);  // T(-center) * (R * T(center))
  
  LogMessage(Format('   Матрица поворота [0,0]=%.4f, [0,1]=%.4f, [1,0]=%.4f, [1,1]=%.4f', 
    [rotMatrix.mtr.v[0].v[0], rotMatrix.mtr.v[0].v[1], rotMatrix.mtr.v[1].v[0], rotMatrix.mtr.v[1].v[1]]));
  LogMessage(Format('   Матрица поворота [3,0]=%.4f, [3,1]=%.4f, [3,2]=%.4f', 
    [rotMatrix.mtr.v[3].v[0], rotMatrix.mtr.v[3].v[1], rotMatrix.mtr.v[3].v[2]]));
  
  LogMessage(Format('   ДО transform: P_insert_in_WCS: %s', [Point3DToStr(pa1^.P_insert_in_WCS)]));
  LogMessage(Format('   ДО transform: objmatrix.mtr.v[3]: X=%.6f, Y=%.6f, Z=%.6f', 
    [pa1^.objmatrix.mtr.v[3].v[0], pa1^.objmatrix.mtr.v[3].v[1], pa1^.objmatrix.mtr.v[3].v[2]]));
  
  pa1^.transform(rotMatrix);
  
  LogMessage(Format('   ПОСЛЕ transform: P_insert_in_WCS: %s', [Point3DToStr(pa1^.P_insert_in_WCS)]));
  LogMessage(Format('   ПОСЛЕ transform: objmatrix.mtr.v[3]: X=%.6f, Y=%.6f, Z=%.6f', 
    [pa1^.objmatrix.mtr.v[3].v[0], pa1^.objmatrix.mtr.v[3].v[1], pa1^.objmatrix.mtr.v[3].v[2]]));
  LogMessage(Format('   ПОСЛЕ transform: Local.p_insert: X=%.6f, Y=%.6f, Z=%.6f', 
    [pa1^.Local.p_insert.x, pa1^.Local.p_insert.y, pa1^.Local.p_insert.z]));
  
  LogArcInfo('   После 90°: ', pa1);
  
  // Поворот на 30 градусов
  LogMessage('');
  LogMessage('ПОВОРОТ НА 30°:');
  LogMessage('----------------------------------------');
  rotationAngle := 30 * PI / 180;
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngle, AngleToDeg(rotationAngle)]));
  
  rotMatrix := CreateRotationMatrixZ(rotationAngle);
  pa1^.transform(rotMatrix);
  
  LogArcInfo('   После 30°: ', pa1);
  
  // Поворот на 40 градусов
  LogMessage('');
  LogMessage('ПОВОРОТ НА 40°:');
  LogMessage('----------------------------------------');
  rotationAngle := 40 * PI / 180;
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngle, AngleToDeg(rotationAngle)]));
  
  rotMatrix := CreateRotationMatrixZ(rotationAngle);
  pa1^.transform(rotMatrix);
  
  LogMessage('');
  LogMessage('ИТОГ ПОСЛЕ ТРЕХ ПОВОРОТОВ (90° + 30° + 40° = 160°):');
  LogMessage('----------------------------------------');
  LogArcInfo('   Итог: ', pa1);
  
  // Проверка: углы в локальной системе координат не меняются при повороте вокруг центра
  LogMessage('');
  LogMessage('   ПРОВЕРКА:');
  LogMessage('   Примечание: StartAngle и EndAngle хранятся в локальной системе координат дуги');
  LogMessage('   и не меняются при повороте вокруг центра дуги.');
  LogMessage(Format('   StartAngle: %.6f (%.2f°) - не изменился', [pa1^.StartAngle, AngleToDeg(pa1^.StartAngle)]));
  LogMessage(Format('   EndAngle: %.6f (%.2f°) - не изменился', [pa1^.EndAngle, AngleToDeg(pa1^.EndAngle)]));
  LogMessage('   Углы относительно глобальной системы координат:');
  LogMessage(Format('   Ожидаемый StartAngle (глобальный): %.6f (%.2f°)', 
    [startAngle + 160*PI/180, AngleToDeg(startAngle + 160*PI/180)]));
  LogMessage(Format('   Ожидаемый EndAngle (глобальный): %.6f (%.2f°)', 
    [endAngle + 160*PI/180, AngleToDeg(endAngle + 160*PI/180)]));
  
  // Добавляем первую дугу в чертеж
  pa1^.Local.p_insert.x := pa1^.P_insert_in_WCS.x - 200;
  pa1^.P_insert_in_WCS.x := pa1^.P_insert_in_WCS.x - 200;
  PzePoint3d(@pa1^.objmatrix.mtr.v[3])^ := pa1^.P_insert_in_WCS;
  pa1^.precalc;
  zcAddEntToCurrentDrawingWithUndo(pa1);
  
  // ============================================
  // ТЕСТ 2: ЗЕРКАЛИРОВАНИЕ ОТНОСИТЕЛЬНО ОСИ X
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ТЕСТ 2: ЗЕРКАЛИРОВАНИЕ ДУГИ ОТНОСИТЕЛЬНО ОСИ X');
  LogMessage('========================================');
  
  pa2 := AllocEnt(GDBArcID);
  pa2^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);
  
  zcSetEntPropFromCurrentDrawingProp(pa2);
  pa2^.vp.LineWeight := LnWt200;
  pa2^.vp.Color := 1; // красный
  
  pa2^.FormatEntity(drawings.GetCurrentDWG^, dc);
  
  q0_before := pa2^.q0;
  q1_before := pa2^.q1;
  q2_before := pa2^.q2;
  
  LogMessage('');
  LogMessage('ДО ЗЕРКАЛИРОВАНИЯ:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa2);
  
  LogMessage('');
  LogMessage('ВЫПОЛНЕНИЕ ЗЕРКАЛИРОВАНИЯ:');
  LogMessage('----------------------------------------');
  LogMessage('   Ось зеркалирования: X (Y -> -Y)');
  
  // Создаем матрицу зеркалирования относительно оси X
  mirrorMatrix := CreateScaleMatrix(1, -1, 1);
  pa2^.transform(mirrorMatrix);
  
  q0_after := pa2^.q0;
  q1_after := pa2^.q1;
  q2_after := pa2^.q2;
  
  LogMessage('');
  LogMessage('ПОСЛЕ ЗЕРКАЛИРОВАНИЯ:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa2);
  
  LogMessage('');
  LogMessage('СРАВНЕНИЕ ДО И ПОСЛЕ:');
  LogMessage('----------------------------------------');
  LogMessage(Format('   q0 смещение: X=%.6f, Y=%.6f, Z=%.6f', 
    [q0_after.x - q0_before.x, q0_after.y - q0_before.y, q0_after.z - q0_before.z]));
  LogMessage(Format('   q1 смещение: X=%.6f, Y=%.6f, Z=%.6f', 
    [q1_after.x - q1_before.x, q1_after.y - q1_before.y, q1_after.z - q1_before.z]));
  LogMessage(Format('   q2 смещение: X=%.6f, Y=%.6f, Z=%.6f', 
    [q2_after.x - q2_before.x, q2_after.y - q2_before.y, q2_after.z - q2_before.z]));

  // Добавляем вторую дугу в чертеж со смещением
  pa2^.Local.p_insert.x := pa2^.P_insert_in_WCS.x + 200;
  pa2^.P_insert_in_WCS.x := pa2^.P_insert_in_WCS.x + 200;
  PzePoint3d(@pa2^.objmatrix.mtr.v[3])^ := pa2^.P_insert_in_WCS;
  pa2^.precalc;
  zcAddEntToCurrentDrawingWithUndo(pa2);

  // ============================================
  // ТЕСТ 3: ЗЕРКАЛИРОВАНИЕ ОТНОСИТЕЛЬНО ОСИ Y
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ТЕСТ 3: ЗЕРКАЛИРОВАНИЕ ДУГИ ОТНОСИТЕЛЬНО ОСИ Y');
  LogMessage('========================================');

  pa3 := AllocEnt(GDBArcID);
  pa3^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);

  zcSetEntPropFromCurrentDrawingProp(pa3);
  pa3^.vp.LineWeight := LnWt200;
  pa3^.vp.Color := 3; // зеленый

  pa3^.FormatEntity(drawings.GetCurrentDWG^, dc);

  q0_before := pa3^.q0;
  q1_before := pa3^.q1;
  q2_before := pa3^.q2;

  LogMessage('');
  LogMessage('ДО ЗЕРКАЛИРОВАНИЯ:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa3);

  LogMessage('');
  LogMessage('ВЫПОЛНЕНИЕ ЗЕРКАЛИРОВАНИЯ:');
  LogMessage('----------------------------------------');
  LogMessage('   Ось зеркалирования: Y (X -> -X)');

  // Создаем матрицу зеркалирования относительно оси Y
  mirrorMatrix := CreateScaleMatrix(-1, 1, 1);
  pa3^.transform(mirrorMatrix);

  q0_after := pa3^.q0;
  q1_after := pa3^.q1;
  q2_after := pa3^.q2;

  LogMessage('');
  LogMessage('ПОСЛЕ ЗЕРКАЛИРОВАНИЯ:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa3);

  LogMessage('');
  LogMessage('СРАВНЕНИЕ ДО И ПОСЛЕ:');
  LogMessage('----------------------------------------');
  LogMessage(Format('   q0 смещение: X=%.6f, Y=%.6f, Z=%.6f', 
    [q0_after.x - q0_before.x, q0_after.y - q0_before.y, q0_after.z - q0_before.z]));
  LogMessage(Format('   q1 смещение: X=%.6f, Y=%.6f, Z=%.6f', 
    [q1_after.x - q1_before.x, q1_after.y - q1_before.y, q1_after.z - q1_before.z]));
  LogMessage(Format('   q2 смещение: X=%.6f, Y=%.6f, Z=%.6f', 
    [q2_after.x - q2_before.x, q2_after.y - q2_before.y, q2_after.z - q2_before.z]));

  // Добавляем третью дугу в чертеж со смещением
  pa3^.Local.p_insert.x := pa3^.P_insert_in_WCS.x - 200;
  pa3^.P_insert_in_WCS.x := pa3^.P_insert_in_WCS.x - 200;
  PzePoint3d(@pa3^.objmatrix.mtr.v[3])^ := pa3^.P_insert_in_WCS;
  pa3^.precalc;
  zcAddEntToCurrentDrawingWithUndo(pa3);

  // ============================================
  // ТЕСТ 4: ПОВОРОТ ВОКРУГ ОСИ Y НА 33 ГРАДУСА
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ТЕСТ 4: ПОВОРОТ ДУГИ ВОКРУГ ОСИ Y НА 33°');
  LogMessage('========================================');
  
  pa4 := AllocEnt(GDBArcID);
  pa4^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);
  
  zcSetEntPropFromCurrentDrawingProp(pa4);
  pa4^.vp.LineWeight := LnWt200;
  pa4^.vp.Color := 4; // голубая

  pa4^.FormatEntity(drawings.GetCurrentDWG^, dc);

  LogMessage('');
  LogMessage('ДО ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa4);

  LogMessage('');
  LogMessage('ВЫПОЛНЕНИЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogMessage('   Ось поворота: Y');
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngleY, AngleToDeg(rotationAngleY)]));

  rotMatrix := CreateRotationMatrixY(rotationAngleY);
  pa4^.transform(rotMatrix);

  LogMessage('');
  LogMessage('ПОСЛЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa4);

  // Добавляем четвертую дугу в чертеж со смещением
  pa4^.Local.p_insert.x := pa4^.P_insert_in_WCS.x - 200;
  pa4^.Local.p_insert.y := pa4^.P_insert_in_WCS.y - 200;
  pa4^.P_insert_in_WCS.x := pa4^.P_insert_in_WCS.x - 200;
  pa4^.P_insert_in_WCS.y := pa4^.P_insert_in_WCS.y - 200;
  PzePoint3d(@pa4^.objmatrix.mtr.v[3])^ := pa4^.P_insert_in_WCS;
  pa4^.precalc;
  zcAddEntToCurrentDrawingWithUndo(pa4);

  // ============================================
  // ТЕСТ 5: ПОВОРОТ ВОКРУГ ОСИ X НА 90 ГРАДУСОВ
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ТЕСТ 5: ПОВОРОТ ДУГИ ВОКРУГ ОСИ X НА 90°');
  LogMessage('========================================');

  pa5 := AllocEnt(GDBArcID);
  pa5^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);

  zcSetEntPropFromCurrentDrawingProp(pa5);
  pa5^.vp.LineWeight := LnWt200;
  pa5^.vp.Color := 5; // синий
  
  pa5^.FormatEntity(drawings.GetCurrentDWG^, dc);

  LogMessage('');
  LogMessage('ДО ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa5);

  LogMessage('');
  LogMessage('ВЫПОЛНЕНИЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogMessage('   Ось поворота: X');
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngleX, AngleToDeg(rotationAngleX)]));

  rotMatrix := CreateRotationMatrixX(rotationAngleX);
  pa5^.transform(rotMatrix);

  LogMessage('');
  LogMessage('ПОСЛЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa5);

  // Добавляем пятую дугу в чертеж со смещением
  pa5^.Local.p_insert.x := pa5^.P_insert_in_WCS.x + 200;
  pa5^.Local.p_insert.y := pa5^.P_insert_in_WCS.y - 200;
  pa5^.P_insert_in_WCS.x := pa5^.P_insert_in_WCS.x + 200;
  pa5^.P_insert_in_WCS.y := pa5^.P_insert_in_WCS.y - 200;
  PzePoint3d(@pa5^.objmatrix.mtr.v[3])^ := pa5^.P_insert_in_WCS;
  pa5^.precalc;
  zcAddEntToCurrentDrawingWithUndo(pa5);
  
  LogMessage('');
  LogMessage('========================================');
  LogMessage('TEST ARC COMMAND - Test completed');
  LogMessage('========================================');
  LogMessage('');
  LogMessage('Результаты:');
  LogMessage('  - Желтая дуга: повернута на 90° вокруг Z (слева вверху)');
  LogMessage('  - Красная дуга: зеркалирована относительно оси X (справа вверху)');
  LogMessage('  - Зеленая дуга: зеркалирована относительно оси Y (слева в центре)');
  LogMessage('  - Голубая дуга: повернута на 33° вокруг Y (слева внизу)');
  LogMessage('  - Синяя дуга: повернута на 90° вокруг X (справа внизу)');
  LogMessage('========================================');
  
  Result := cmd_ok;
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsInitializeLMId);
  CreateZCADCommand(@TestArcCommand_com, 'testarc', CADWG, 0);

finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsFinalizeLMId);
end.
