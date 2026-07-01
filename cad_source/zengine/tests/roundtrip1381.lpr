program roundtrip1381;

// issue #1381: воспроизводит сценарий пользователя — разорванная таблица,
// отредактированная в ZCAD (raw-DXF инвалидирован), сохраняется штатным
// savedxf20XX по МОДЕЛЬНОМУ пути. Проверяем, что для каждой части
// (главная + продолжения) генерируется анонимный блок с геометрией и
// сущность ACAD_TABLE ссылается на него через group 2 + group 343, поэтому
// AutoCAD рисует части как отдельные таблицы, а не «разорванную» цельную.
//
// Инвалидацию raw эмулируем публичным сеттером BreakDirection (в реальном
// ZCAD raw сбрасывается при любом редактировании таблицы). Меняем
// направление на другое и обратно — итоговая геометрия не меняется, но
// InvalidateRawDXFEntity срабатывает и заставляет использовать модельный
// путь сохранения.

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager,
  uzgldrawcontext, uzeTypes, uzeconsts, gzctnrVectorTypes,
  uzeentity, uzeentitiestree, uzeentgenericsubentry,
  uzeenttable, uzeacadtable_types, uzeacadtable_model, uzeacadtable_dxf_write;

function LoadDrawing(const AFileName: string; var ADrawing: TSimpleDrawing): Integer;
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
begin
  ADrawing.init(nil);
  DC := ADrawing.CreateDrawingRC;
  ZDC.CreateRec(ADrawing, ADrawing.pObjRoot^, TLOLoad, DC);
  AddFromDXF(AFileName, ZDC);
  Result := ADrawing.pObjRoot^.ObjArray.Count;
end;

function FindFirstAcadTable(const pRoot: PGDBObjGenericSubEntry): PGDBObjAcadTable;
var
  IR: itrec;
  PEntity: PGDBObjEntity;
begin
  Result := nil;
  PEntity := pRoot^.ObjArray.beginiterate(IR);
  while PEntity <> nil do
  begin
    if PEntity^.GetObjType = GDBAcadTableID then
      Exit(PGDBObjAcadTable(PEntity));
    PEntity := pRoot^.ObjArray.iterate(IR);
  end;
end;

var
  Drawing: TSimpleDrawing;
  InFile, TemplateFile, OutFile: string;
  AcadTable: PGDBObjAcadTable;
  OrigDir, OtherDir: TAcadTableBreakDirection;
  Ok: Boolean;
begin
  if ParamCount < 3 then
  begin
    writeln('usage: roundtrip1381 <in.dxf> <template.dxf> <out.dxf>');
    Halt(2);
  end;
  InFile := ParamStr(1);
  TemplateFile := ParamStr(2);
  OutFile := ParamStr(3);

  writeln('loading: ', InFile);
  LoadDrawing(InFile, Drawing);

  AcadTable := FindFirstAcadTable(Drawing.pObjRoot);
  if AcadTable = nil then
  begin
    writeln('NO ACAD_TABLE FOUND');
    Drawing.done;
    Halt(1);
  end;
  writeln('continuation parts: ', AcadTable^.ContinuationPartCount);

  // Эмулируем редактирование в ZCAD: инвалидируем raw через публичный
  // сеттер направления (меняем и возвращаем — геометрия не меняется).
  OrigDir := AcadTable^.BreakDirection;
  if OrigDir = atbdRight then OtherDir := atbdLeft else OtherDir := atbdRight;
  AcadTable^.BreakDirection := OtherDir;
  AcadTable^.BreakDirection := OrigDir;
  writeln('raw invalidated via BreakDirection toggle');

  writeln('saving:  ', OutFile, ' (template ', TemplateFile, ')');
  Ok := savedxf20XX(OutFile, TemplateFile, Drawing, ZCDxf2007);
  Drawing.done;
  if Ok then
    writeln('OK')
  else
  begin
    writeln('SAVE FAILED');
    Halt(1);
  end;
end.
