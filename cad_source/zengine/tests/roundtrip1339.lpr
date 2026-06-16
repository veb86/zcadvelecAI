program roundtrip1339;

// issue #1339: загружает исходный DXF с ACAD_TABLE, сохраняет его через
// штатный savedxf20XX (тот же путь, что и "Сохранить как" в ZCAD) и пишет
// результат в файл. Затем внешний скрипт experiments/dxf_analyze.py
// проверяет отсутствие висячих ссылок у сущности ACAD_TABLE.

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager,
  uzgldrawcontext, uzeconsts, uzeTypes,
  // Регистрация класса сущности ACAD_TABLE (как в тестовом наборе),
  // иначе таблица грузится как generic/proxy и не сохраняется как ACAD_TABLE.
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

var
  Drawing: TSimpleDrawing;
  InFile, TemplateFile, OutFile: string;
  Ok: Boolean;
begin
  if ParamCount < 3 then
  begin
    writeln('usage: roundtrip1339 <in.dxf> <template.dxf> <out.dxf>');
    Halt(2);
  end;
  InFile := ParamStr(1);
  TemplateFile := ParamStr(2);
  OutFile := ParamStr(3);

  writeln('loading: ', InFile);
  LoadDrawing(InFile, Drawing);
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
