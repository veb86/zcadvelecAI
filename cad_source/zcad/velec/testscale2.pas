{
*****************************************************************************
*  Test Scale2 Command - Testing scale2 command with two rectangles
*****************************************************************************
}
{$mode objfpc}{$H+}

unit testscale2;
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Math,
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
  uzeentpolyline;

function TestScale2Command_com(const Context:TZCADCommandContext;
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

function CreateRectangle(const v1, v2, v3, v4: TzePoint3d; const color: Integer): PGDBObjPolyline;
begin
  // Создаем полилинию с владельцем - текущим чертежом
  Result := PGDBObjPolyline(CreateInitObjFree(GDBPolylineID, drawings.GetCurrentROOT));

  zcSetEntPropFromCurrentDrawingProp(Result);
  Result^.vp.LineWeight := LnWt200;
  Result^.vp.Color := color;

  // Добавляем вершины прямоугольника
  Result^.AddVertex(v1);
  Result^.AddVertex(v2);
  Result^.AddVertex(v3);
  Result^.AddVertex(v4);
  Result^.Closed := True;
  // FormatEntity будет вызван после добавления в чертеж через zcAddEntToCurrentDrawingWithUndo
end;

function TestScale2Command_com(const Context:TZCADCommandContext;
  operands:TCommandOperands):TCommandResult;
var
  pRect1, pRect2: PGDBObjPolyline;

  // Вершины первого прямоугольника
  r1v1, r1v2, r1v3, r1v4: TzePoint3d;

  // Вершины второго прямоугольника
  r2v1, r2v2, r2v3, r2v4: TzePoint3d;

begin
  LogMessage('========================================');
  LogMessage('TEST SCALE2 COMMAND - Масштабирование двух прямоугольников');
  LogMessage('========================================');

  // Инициализация вершин первого прямоугольника
  r1v1.x := 295; r1v1.y := 207; r1v1.z := 111;
  r1v2.x := 383; r1v2.y := 281; r1v2.z := 14;
  r1v3.x := 383; r1v3.y := 180; r1v3.z := -62;
  r1v4.x := 295; r1v4.y := 106; r1v4.z := 34;

  // Инициализация вершин второго прямоугольника
  r2v1.x := 425; r2v1.y := 77; r2v1.z := 0;
  r2v2.x := 502; r2v2.y := 77; r2v2.z := 0;
  r2v3.x := 502; r2v3.y := 29; r2v3.z := 0;
  r2v4.x := 425; r2v4.y := 29; r2v4.z := 0;

  LogMessage('');
  LogMessage('ПАРАМЕТРЫ ПРЯМОУГОЛЬНИКА №1 (базовый, для масштабирования):');
  LogMessage('----------------------------------------');
  LogMessage('   v1: ' + Point3DToStr(r1v1));
  LogMessage('   v2: ' + Point3DToStr(r1v2));
  LogMessage('   v3: ' + Point3DToStr(r1v3));
  LogMessage('   v4: ' + Point3DToStr(r1v4));

  LogMessage('');
  LogMessage('ПАРАМЕТРЫ ПРЯМОУГОЛЬНИКА №2 (будет масштабирован):');
  LogMessage('----------------------------------------');
  LogMessage('   v1: ' + Point3DToStr(r2v1));
  LogMessage('   v2: ' + Point3DToStr(r2v2));
  LogMessage('   v3: ' + Point3DToStr(r2v3));
  LogMessage('   v4: ' + Point3DToStr(r2v4));

  // ============================================
  // ШАГ 1: Создание первого прямоугольника (базовый)
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ШАГ 1: Создание прямоугольника №1 (базовый, желтый)');
  LogMessage('========================================');

  pRect1 := CreateRectangle(r1v1, r1v2, r1v3, r1v4, 2); // желтый

  LogMessage('Прямоугольник №1 создан:');
  LogMessage('   Цвет: желтый (2)');
  LogMessage('   Вершин: 4 (замкнутый)');

  // Добавляем первый прямоугольник в чертеж
  zcAddEntToCurrentDrawingWithUndo(pRect1);

  // ============================================
  // ШАГ 2: Создание второго прямоугольника
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ШАГ 2: Создание прямоугольника №2 (зеленый, будет масштабирован)');
  LogMessage('========================================');

  pRect2 := CreateRectangle(r2v1, r2v2, r2v3, r2v4, 3); // зеленый

  LogMessage('Прямоугольник №2 создан:');
  LogMessage('   Цвет: зеленый (3)');
  LogMessage('   Вершин: 4 (замкнутый)');

  // Добавляем второй прямоугольник в чертеж
  zcAddEntToCurrentDrawingWithUndo(pRect2);

  // ============================================
  // ШАГ 3: Выделение второго прямоугольника
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ШАГ 3: Выделение прямоугольника №2 для команды SCALE2');
  LogMessage('========================================');

  pRect2^.selected := True;

  LogMessage('Прямоугольник №2 выделен (selected = True)');

  // ============================================
  // ШАГ 4: Запуск команды SCALE2
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ШАГ 4: Запуск команды SCALE2');
  LogMessage('========================================');
  LogMessage('');
  LogMessage('Инструкция для ручного выполнения команды:');
  LogMessage('----------------------------------------');
  LogMessage('1. Введите команду: SCALE2');
  LogMessage('2. Укажите базовую точку масштабирования: вершина v1 прямоугольника №1');
  LogMessage('   Координаты: ' + Point3DToStr(r1v1));
  LogMessage('3. Укажите первую точку ссылки: вершина v2 прямоугольника №1');
  LogMessage('   Координаты: ' + Point3DToStr(r1v2));
  LogMessage('4. Укажите вторую точку ссылки: вершина v3 прямоугольника №1');
  LogMessage('   Координаты: ' + Point3DToStr(r1v3));
  LogMessage('');
  LogMessage('Ожидаемый результат:');
  LogMessage('  Прямоугольник №2 (зеленый) будет масштабирован относительно');
  LogMessage('  базовой точки (v1 прямоугольника №1) с коэффициентом,');
  LogMessage('  определенным по расстоянию между точками ссылки');
  LogMessage('========================================');
  LogMessage('');
  LogMessage('TEST SCALE2 COMMAND - Подготовка завершена');
  LogMessage('Теперь выполните команду SCALE2 вручную или через API');
  LogMessage('========================================');

  Result := cmd_ok;
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsInitializeLMId);
  CreateZCADCommand(@TestScale2Command_com, 'testscale2', CADWG, 0);

finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsFinalizeLMId);
end.
