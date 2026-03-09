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

function AngleToDeg(angle:double):double;
begin
  Result := angle * 180.0 / PI;
end;

procedure LogArcInfo(const prefix:string; pa:PGDBObjArc);
begin
  LogMessage(Format('   %s StartAngle: %.6f (%.2f°)', [prefix, pa^.StartAngle, AngleToDeg(pa^.StartAngle)]));
  LogMessage(Format('   %s EndAngle: %.6f (%.2f°)', [prefix, pa^.EndAngle, AngleToDeg(pa^.EndAngle)]));
  LogMessage(Format('   %s Angle (sweep): %.6f (%.2f°)', [prefix, pa^.angle, AngleToDeg(pa^.angle)]));
  LogMessage(Format('   %s P_insert (центр в WCS): %s', [prefix, Point3DToStr(pa^.P_insert_in_WCS)]));
  LogMessage(Format('   %s q0 (начальная точка): %s', [prefix, Point3DToStr(pa^.q0)]));
  LogMessage(Format('   %s q1 (средняя точка): %s', [prefix, Point3DToStr(pa^.q1)]));
  LogMessage(Format('   %s q2 (конечная точка): %s', [prefix, Point3DToStr(pa^.q2)]));
end;

function TestArcCommand_com(const Context:TZCADCommandContext;
  operands:TCommandOperands):TCommandResult;
var
  pa1, pa2: PGDBObjArc;
  dc: TDrawContext;
  centerPoint: TzePoint3d;
  radius: double;
  startAngle, endAngle: double;
  rotationAngle: double;
  rotMatrix, mirrorMatrix: TzeTypedMatrix4d;
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
  LogMessage('ТЕСТ 1: ПОВОРОТ ДУГИ НА 90 ГРАДУСОВ');
  LogMessage('========================================');
  
  pa1 := AllocEnt(GDBArcID);
  pa1^.init(nil, nil, 0, centerPoint, radius, startAngle, endAngle);
  
  zcSetEntPropFromCurrentDrawingProp(pa1);
  pa1^.vp.LineWeight := LnWt200;
  pa1^.vp.Color := 4; // желтый
  
  dc := drawings.GetCurrentDWG^.CreateDrawingRC;
  pa1^.FormatEntity(drawings.GetCurrentDWG^, dc);
  
  q0_before := pa1^.q0;
  q1_before := pa1^.q1;
  q2_before := pa1^.q2;
  
  LogMessage('');
  LogMessage('ДО ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa1);
  
  LogMessage('');
  LogMessage('ВЫПОЛНЕНИЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogMessage(Format('   Угол поворота: %.6f (%.2f°)', [rotationAngle, AngleToDeg(rotationAngle)]));
  
  rotMatrix := CreateRotationMatrixZ(rotationAngle);
  pa1^.transform(rotMatrix);
  
  q0_after := pa1^.q0;
  q1_after := pa1^.q1;
  q2_after := pa1^.q2;
  
  LogMessage('');
  LogMessage('ПОСЛЕ ПОВОРОТА:');
  LogMessage('----------------------------------------');
  LogArcInfo('', pa1);
  
  LogMessage('');
  LogMessage('СРАВНЕНИЕ ДО И ПОСЛЕ:');
  LogMessage('----------------------------------------');
  LogMessage(Format('   q0 смещение: X=%.6f, Y=%.6f, Z=%.6f', 
    [q0_after.x - q0_before.x, q0_after.y - q0_before.y, q0_after.z - q0_before.z]));
  LogMessage(Format('   q1 смещение: X=%.6f, Y=%.6f, Z=%.6f', 
    [q1_after.x - q1_before.x, q1_after.y - q1_before.y, q1_after.z - q1_before.z]));
  LogMessage(Format('   q2 смещение: X=%.6f, Y=%.6f, Z=%.6f', 
    [q2_after.x - q2_before.x, q2_after.y - q2_before.y, q2_after.z - q2_before.z]));
  
  // Добавляем первую дугу в чертеж со смещением
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
  pa2^.vp.Color := 2; // красный
  
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
  
  LogMessage('');
  LogMessage('========================================');
  LogMessage('TEST ARC COMMAND - Test completed');
  LogMessage('========================================');
  LogMessage('');
  LogMessage('Результаты:');
  LogMessage('  - Желтая дуга: повернута на 90° (слева)');
  LogMessage('  - Красная дуга: зеркалирована относительно оси X (справа)');
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
