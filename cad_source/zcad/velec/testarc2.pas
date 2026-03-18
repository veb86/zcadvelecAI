{
*****************************************************************************
*  Test Arc Command 2 - Testing arc rotation using existing commands
*****************************************************************************
}
{$mode objfpc}{$H+}

unit testarc2;
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
  uzcinterface,
  uzcCommand_RotateXYZ,
  uzccommand_rotateents;

function TestArc2Command_com(const Context:TZCADCommandContext;
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
  LogMessage(Format('   %s Local.basis.OX: %s', [prefix, Point3DToStr(pa^.Local.basis.ox)]));
  LogMessage(Format('   %s Local.basis.OY: %s', [prefix, Point3DToStr(pa^.Local.basis.oy)]));
  LogMessage(Format('   %s Local.basis.OZ: %s', [prefix, Point3DToStr(pa^.Local.basis.oz)]));

  // Проверка: радиус должен соответствовать масштабу по осям
  LogMessage(Format('   %s Проверка масштаба: OX.x=%.6f, OY.y=%.6f, R=%.6f', [prefix, pa^.Local.basis.ox.x, pa^.Local.basis.oy.y, pa^.R]));
end;

function TestArc2Command_com(const Context:TZCADCommandContext;
  operands:TCommandOperands):TCommandResult;
var
  pa1, pa2, pa3, pa4, pa5: PGDBObjArc;
  dc: TDrawContext;
  centerPoint: TzePoint3d;
  radius: double;
  startAngle, endAngle: double;
  rotationAngle: double;
  dispmatr, rotmatr, im: TzeTypedMatrix4d;
begin
  LogMessage('========================================');
  LogMessage('TEST ARC COMMAND 2 - Using existing rotate commands');
  LogMessage('========================================');

  // Исходные данные
  centerPoint.x := -7;
  centerPoint.y := 29;
  centerPoint.z := 0;
  radius := 83;
  startAngle := 43 * PI / 180;
  endAngle := 184 * PI / 180;

  LogMessage('');
  LogMessage('Исходные параметры дуги:');
  LogMessage('----------------------------------------');
  LogMessage(Format('   Центр: %s', [Point3DToStr(centerPoint)]));
  LogMessage(Format('   Радиус: %.6f', [radius]));
  LogMessage(Format('   Начальный угол: %.6f (%.2f°)', [startAngle, AngleToDeg(startAngle)]));
  LogMessage(Format('   Конечный угол: %.6f (%.2f°)', [endAngle, AngleToDeg(endAngle)]));

  dc := drawings.GetCurrentDWG^.CreateDrawingRC;

  // ============================================
  // ТЕСТ 1: ПОВОРОТ С ПОМОЩЬЮ Rotate_com (вокруг начала координат)
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ТЕСТ 1: ПОВОРОТ ЧЕРЕЗ Rotate_com (вокруг WCS origin)');
  LogMessage('========================================');

  pa1 := AllocEnt(GDBArcID);
  pa1^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);

  zcSetEntPropFromCurrentDrawingProp(pa1);
  pa1^.vp.LineWeight := LnWt200;
  pa1^.vp.Color := 2; // желтый

  pa1^.FormatEntity(drawings.GetCurrentDWG^, dc);

  LogMessage('');
  LogMessage('ИСХОДНОЕ СОСТОЯНИЕ:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa1);

  // Поворот на 90 градусов вокруг начала координат через Rotate_com
  LogMessage('');
  LogMessage('ПОВОРОТ НА 90° ВОКРУГ НАЧАЛА КООРДИНАТ (через Rotate_com):');
  LogMessage('----------------------------------------');
  rotationAngle := 90 * PI / 180;
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngle, AngleToDeg(rotationAngle)]));

  // Используем команду Rotate_com - она поворачивает вокруг текущего положения курсора (t3dp)
  // Для теста вызываем напрямую метод rotate с матрицей поворота вокруг начала координат
  pa1^.transform(RotateZ_com.CreateRotmatr(rotationAngle));

  LogMessage('');
  LogMessage('ПОСЛЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('   После 90°: ', pa1);

  // Добавляем первую дугу в чертеж
  zcAddEntToCurrentDrawingWithUndo(pa1);

  // ============================================
  // ТЕСТ 2: ПОВОРОТ С ПОМОЩЬЮ RotateEnts_com (вокруг центра объекта)
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ТЕСТ 2: ПОВОРОТ ЧЕРЕЗ RotateEnts_com (вокруг P_insert)');
  LogMessage('========================================');

  pa2 := AllocEnt(GDBArcID);
  pa2^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);

  zcSetEntPropFromCurrentDrawingProp(pa2);
  pa2^.vp.LineWeight := LnWt200;
  pa2^.vp.Color := 1; // красный

  pa2^.FormatEntity(drawings.GetCurrentDWG^, dc);

  LogMessage('');
  LogMessage('ИСХОДНОЕ СОСТОЯНИЕ:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa2);

  // Поворот на 90 градусов вокруг центра дуги через RotateEnts_com логику
  LogMessage('');
  LogMessage('ПОВОРОТ НА 90° ВОКРУГ ЦЕНТРА ДУГИ (через RotateEnts_com логику):');
  LogMessage('----------------------------------------');
  rotationAngle := 90 * PI / 180;
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngle, AngleToDeg(rotationAngle)]));

  // Создаем матрицу поворота вокруг центра дуги (как в RotateEnts_com)

  dispmatr := uzegeometry.CreateTranslationMatrix(CreateVertex(-pa2^.P_insert_in_WCS.x, -pa2^.P_insert_in_WCS.y, -pa2^.P_insert_in_WCS.z));
  rotmatr := uzegeometry.CreateRotationMatrixZ(rotationAngle);
  rotmatr := uzegeometry.MatrixMultiply(dispmatr, rotmatr);
  dispmatr := uzegeometry.CreateTranslationMatrix(CreateVertex(pa2^.P_insert_in_WCS.x, pa2^.P_insert_in_WCS.y, pa2^.P_insert_in_WCS.z));
  dispmatr := uzegeometry.MatrixMultiply(rotmatr, dispmatr);

  pa2^.transform(dispmatr);

  LogMessage('');
  LogMessage('ПОСЛЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('   После 90°: ', pa2);

  // Проверка: центр должен остаться на месте
  LogMessage('');
  LogMessage('ПРОВЕРКА: Центр дуги должен остаться на месте');
  LogMessage(Format('   До: X=%.6f, Y=%.6f, Z=%.6f', [centerPoint.x, centerPoint.y, centerPoint.z]));
  LogMessage(Format('   После: %s', [Point3DToStr(pa2^.P_insert_in_WCS)]));

  // Добавляем вторую дугу в чертеж
  zcAddEntToCurrentDrawingWithUndo(pa2);

  // ============================================
  // ТЕСТ 3: ПОВОРОТ НА 30° ЧЕРЕЗ RotateEnts_com
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ТЕСТ 3: ПОВОРОТ НА 30° ЧЕРЕЗ RotateEnts_com');
  LogMessage('========================================');

  pa3 := AllocEnt(GDBArcID);
  pa3^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);

  zcSetEntPropFromCurrentDrawingProp(pa3);
  pa3^.vp.LineWeight := LnWt200;
  pa3^.vp.Color := 3; // зеленый

  pa3^.FormatEntity(drawings.GetCurrentDWG^, dc);

  LogMessage('');
  LogMessage('ИСХОДНОЕ СОСТОЯНИЕ:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa3);

  rotationAngle := 30 * PI / 180;
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngle, AngleToDeg(rotationAngle)]));

  dispmatr := uzegeometry.CreateTranslationMatrix(CreateVertex(-pa3^.P_insert_in_WCS.x, -pa3^.P_insert_in_WCS.y, -pa3^.P_insert_in_WCS.z));
  rotmatr := uzegeometry.CreateRotationMatrixZ(rotationAngle);
  rotmatr := uzegeometry.MatrixMultiply(dispmatr, rotmatr);
  dispmatr := uzegeometry.CreateTranslationMatrix(CreateVertex(pa3^.P_insert_in_WCS.x, pa3^.P_insert_in_WCS.y, pa3^.P_insert_in_WCS.z));
  dispmatr := uzegeometry.MatrixMultiply(rotmatr, dispmatr);

  pa3^.transform(dispmatr);

  LogMessage('');
  LogMessage('ПОСЛЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('   После 30°: ', pa3);

  // Добавляем третью дугу в чертеж
  zcAddEntToCurrentDrawingWithUndo(pa3);

  // ============================================
  // ТЕСТ 4: ПОВОРОТ ВОКРУГ ОСИ Y ЧЕРЕЗ RotateY_com
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ТЕСТ 4: ПОВОРОТ ВОКРУГ ОСИ Y (через RotateY_com)');
  LogMessage('========================================');

  pa4 := AllocEnt(GDBArcID);
  pa4^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);

  zcSetEntPropFromCurrentDrawingProp(pa4);
  pa4^.vp.LineWeight := LnWt200;
  pa4^.vp.Color := 4; // голубая

  pa4^.FormatEntity(drawings.GetCurrentDWG^, dc);

  LogMessage('');
  LogMessage('ИСХОДНОЕ СОСТОЯНИЕ:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa4);

  rotationAngle := 33 * PI / 180;
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngle, AngleToDeg(rotationAngle)]));

  // Используем RotateY_com.CreateRotmatr
  pa4^.transform(RotateY_com.CreateRotmatr(rotationAngle));

  LogMessage('');
  LogMessage('ПОСЛЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('   После 33° вокруг Y: ', pa4);

  // Добавляем четвертую дугу в чертеж
  zcAddEntToCurrentDrawingWithUndo(pa4);

  // ============================================
  // ТЕСТ 5: ПОВОРОТ ВОКРУГ ОСИ X ЧЕРЕЗ RotateX_com
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ТЕСТ 5: ПОВОРОТ ВОКРУГ ОСИ X (через RotateX_com)');
  LogMessage('========================================');

  pa5 := AllocEnt(GDBArcID);
  pa5^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);

  zcSetEntPropFromCurrentDrawingProp(pa5);
  pa5^.vp.LineWeight := LnWt200;
  pa5^.vp.Color := 5; // синий

  pa5^.FormatEntity(drawings.GetCurrentDWG^, dc);

  LogMessage('');
  LogMessage('ИСХОДНОЕ СОСТОЯНИЕ:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa5);

  rotationAngle := 90 * PI / 180;
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngle, AngleToDeg(rotationAngle)]));

  // Используем RotateX_com.CreateRotmatr
  pa5^.transform(RotateX_com.CreateRotmatr(rotationAngle));

  LogMessage('');
  LogMessage('ПОСЛЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('   После 90° вокруг X: ', pa5);

  // Добавляем пятую дугу в чертеж
  zcAddEntToCurrentDrawingWithUndo(pa5);

  LogMessage('');
  LogMessage('========================================');
  LogMessage('TEST ARC COMMAND 2 - Test completed');
  LogMessage('========================================');
  LogMessage('');
  LogMessage('Результаты:');
  LogMessage('  - Желтая дуга: повернута на 90° вокруг начала координат (Rotate_com)');
  LogMessage('  - Красная дуга: повернута на 90° вокруг центра дуги (RotateEnts_com логика)');
  LogMessage('  - Зеленая дуга: повернута на 30° вокруг центра дуги (RotateEnts_com логика)');
  LogMessage('  - Голубая дуга: повернута на 33° вокруг оси Y (RotateY_com)');
  LogMessage('  - Синяя дуга: повернута на 90° вокруг оси X (RotateX_com)');
  LogMessage('========================================');

  Result := cmd_ok;
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsInitializeLMId);
  CreateZCADCommand(@TestArc2Command_com, 'testarc2', CADWG, 0);

finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsFinalizeLMId);
end.
