{
*****************************************************************************
*  Test Align Command - Testing align command with two rectangles
*****************************************************************************
}
{$mode objfpc}{$H+}

unit testalign;
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

function TestAlignCommand_com(const Context:TZCADCommandContext;
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

function TestAlignCommand_com(const Context:TZCADCommandContext;
  operands:TCommandOperands):TCommandResult;
var
  pRect1, pRect2: PGDBObjPolyline;

  // Вершины первого прямоугольника
  r1v1, r1v2, r1v3, r1v4: TzePoint3d;

  // Вершины второго прямоугольника
  r2v1, r2v2, r2v3, r2v4: TzePoint3d;
  
begin
  LogMessage('========================================');
  LogMessage('TEST ALIGN COMMAND - Выравнивание двух прямоугольников');
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
  LogMessage('ПАРАМЕТРЫ ПРЯМОУГОЛЬНИКА №1 (целевой):');
  LogMessage('----------------------------------------');
  LogMessage('   v1: ' + Point3DToStr(r1v1));
  LogMessage('   v2: ' + Point3DToStr(r1v2));
  LogMessage('   v3: ' + Point3DToStr(r1v3));
  LogMessage('   v4: ' + Point3DToStr(r1v4));
  
  LogMessage('');
  LogMessage('ПАРАМЕТРЫ ПРЯМОУГОЛЬНИКА №2 (исходный, будет выровнен):');
  LogMessage('----------------------------------------');
  LogMessage('   v1: ' + Point3DToStr(r2v1));
  LogMessage('   v2: ' + Point3DToStr(r2v2));
  LogMessage('   v3: ' + Point3DToStr(r2v3));
  LogMessage('   v4: ' + Point3DToStr(r2v4));

  // ============================================
  // ШАГ 1: Создание первого прямоугольника (целевой)
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ШАГ 1: Создание прямоугольника №1 (целевой, желтый)');
  LogMessage('========================================');
  
  pRect1 := CreateRectangle(r1v1, r1v2, r1v3, r1v4, 2); // желтый

  LogMessage('Прямоугольник №1 создан:');
  LogMessage('   Цвет: желтый (2)');
  LogMessage('   Вершин: 4 (замкнутый)');

  // Добавляем первый прямоугольник в чертеж
  zcAddEntToCurrentDrawingWithUndo(pRect1);

  // ============================================
  // ШАГ 2: Создание второго прямоугольника (исходный)
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ШАГ 2: Создание прямоугольника №2 (исходный, красный)');
  LogMessage('========================================');

  pRect2 := CreateRectangle(r2v1, r2v2, r2v3, r2v4, 1); // красный

  LogMessage('Прямоугольник №2 создан:');
  LogMessage('   Цвет: красный (1)');
  LogMessage('   Вершин: 4 (замкнутый)');
  
  // Добавляем второй прямоугольник в чертеж
  zcAddEntToCurrentDrawingWithUndo(pRect2);
  
  // ============================================
  // ШАГ 3: Выделение второго прямоугольника
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ШАГ 3: Выделение прямоугольника №2 для команды ALIGN');
  LogMessage('========================================');
  
  pRect2^.selected := True;
  
  LogMessage('Прямоугольник №2 выделен (selected = True)');
  
  // ============================================
  // ШАГ 4: Запуск команды ALIGN
  // ============================================
  LogMessage('');
  LogMessage('========================================');
  LogMessage('ШАГ 4: Запуск команды ALIGN');
  LogMessage('========================================');
  LogMessage('');
  LogMessage('Инструкция для ручного выполнения команды:');
  LogMessage('----------------------------------------');
  LogMessage('1. Введите команду: ALIGN');
  LogMessage('2. Укажите первую исходную точку: вершина v1 прямоугольника №2');
  LogMessage('   Координаты: ' + Point3DToStr(r2v1));
  LogMessage('3. Укажите первую целевую точку: вершина v1 прямоугольника №1');
  LogMessage('   Координаты: ' + Point3DToStr(r1v1));
  LogMessage('4. Укажите вторую исходную точку: вершина v2 прямоугольника №2');
  LogMessage('   Координаты: ' + Point3DToStr(r2v2));
  LogMessage('5. Укажите вторую целевую точку: вершина v2 прямоугольника №1');
  LogMessage('   Координаты: ' + Point3DToStr(r1v2));
  LogMessage('6. Укажите третью исходную точку: вершина v3 прямоугольника №2');
  LogMessage('   Координаты: ' + Point3DToStr(r2v3));
  LogMessage('7. Укажите третью целевую точку: вершина v3 прямоугольника №1');
  LogMessage('   Координаты: ' + Point3DToStr(r1v3));
  LogMessage('8. На вопрос о масштабировании ответьте: No (N)');
  LogMessage('');
  LogMessage('Ожидаемый результат:');
  LogMessage('  Прямоугольник №2 (красный) будет выровнен по прямоугольнику №1 (желтый)');
  LogMessage('  Вершина v2 совпадет с v1 целевого прямоугольника');
  LogMessage('  Вершина v3 совпадет с v2 целевого прямоугольника');
  LogMessage('  Вершина v4 совпадет с v3 целевого прямоугольника');
  LogMessage('========================================');
  LogMessage('');
  LogMessage('TEST ALIGN COMMAND - Подготовка завершена');
  LogMessage('Теперь выполните команду ALIGN вручную или через API');
  LogMessage('========================================');
  
  Result := cmd_ok;
end;

initialization
  programlog.LogOutFormatStr('Unit "%s" initialization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsInitializeLMId);
  CreateZCADCommand(@TestAlignCommand_com, 'testalign', CADWG, 0);

finalization
  ProgramLog.LogOutFormatStr('Unit "%s" finalization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsFinalizeLMId);
end.
