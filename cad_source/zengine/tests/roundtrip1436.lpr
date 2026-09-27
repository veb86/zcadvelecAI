program roundtrip1436;

// issue #1436: проверка записи/чтения таблицы LAYER, вынесенной в
// uzestyleslayerdxf.pas. Создаёт чертёж с набором слоёв с разными
// свойствами, сохраняет его штатным savedxf20XX (тот же путь, что и
// "Сохранить как" в ZCAD), затем загружает сохранённый файл обратно и
// сравнивает свойства слоёв. Также проверяется структура таблицы LAYER
// так же, как это делает AutoCAD: до ENDTAB допустимы только записи
// 0 LAYER (иначе "Ожидался 0 LAYER или 0 ENDTAB, получено 0 TABLE").
// ZCAD сам такой испорченный файл читает, поэтому одной перезагрузки мало.

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager,
  uzgldrawcontext, uzeconsts, uzeTypes, uzestyleslayers;

type
  TLayerSample = record
    Name: string;
    Color: Integer;
    LW: Integer;
    IsOn, IsLocked, IsPrinted: Boolean;
    Desk: string;
  end;

const
  Samples: array[0..4] of TLayerSample = (
    (Name: 'L1436_RED';     Color: 1; LW: 50;  IsOn: True;  IsLocked: False; IsPrinted: True;  Desk: ''),
    (Name: 'L1436_OFF';     Color: 3; LW: -3;  IsOn: False; IsLocked: False; IsPrinted: True;  Desk: ''),
    (Name: 'L1436_LOCKED';  Color: 5; LW: 25;  IsOn: True;  IsLocked: True;  IsPrinted: True;  Desk: ''),
    (Name: 'L1436_NOPRINT'; Color: 6; LW: -1;  IsOn: True;  IsLocked: False; IsPrinted: False; Desk: ''),
    (Name: 'L1436_DESK';    Color: 2; LW: 13;  IsOn: True;  IsLocked: False; IsPrinted: True;  Desk: 'Layer description 1436')
  );

procedure LoadDrawing(const AFileName: string; var ADrawing: TSimpleDrawing);
var
  DC: TDrawContext;
  ZDC: TZDrawingContext;
begin
  ADrawing.init(nil);
  DC := ADrawing.CreateDrawingRC;
  ZDC.CreateRec(ADrawing, ADrawing.pObjRoot^, TLOLoad, DC);
  AddFromDXF(AFileName, ZDC);
end;

function CheckLayers(var ADrawing: TSimpleDrawing): Integer;
var
  i: Integer;
  plp: PGDBLayerProp;
begin
  Result := 0;
  for i := Low(Samples) to High(Samples) do begin
    plp := ADrawing.LayerTable.getAddres(Samples[i].Name);
    if plp = nil then begin
      writeln('FAIL: layer ', Samples[i].Name, ' not found after reload');
      Inc(Result);
      Continue;
    end;
    if (plp^.color <> Samples[i].Color) or (plp^.lineweight <> Samples[i].LW)
       or (plp^._on <> Samples[i].IsOn) or (plp^._lock <> Samples[i].IsLocked)
       or (plp^._print <> Samples[i].IsPrinted) or (plp^.desk <> Samples[i].Desk) then begin
      writeln(Format('FAIL: layer %s: color=%d lw=%d on=%s lock=%s print=%s desk="%s"',
        [plp^.Name, plp^.color, plp^.lineweight, BoolToStr(plp^._on, True),
         BoolToStr(plp^._lock, True), BoolToStr(plp^._print, True), plp^.desk]));
      Inc(Result);
    end else
      writeln('ok:   layer ', plp^.Name);
  end;
end;

function CheckLayerTableStructure(const AFileName: string): Integer;
var
  Lines: TStringList;
  i, Tables: Integer;
  InTable: Boolean;
begin
  Result := 0;
  Tables := 0;
  InTable := False;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    i := 0;
    while i < Lines.Count - 1 do begin
      if Trim(Lines[i]) = '0' then begin
        if InTable then begin
          if Trim(Lines[i + 1]) = 'ENDTAB' then
            InTable := False
          else if Trim(Lines[i + 1]) <> 'LAYER' then begin
            writeln(Format('FAIL: line %d: expected 0 LAYER or 0 ENDTAB, got 0 %s',
              [i + 1, Trim(Lines[i + 1])]));
            Inc(Result);
          end;
        end else if (Trim(Lines[i + 1]) = 'TABLE') and (i + 3 < Lines.Count)
                    and (Trim(Lines[i + 2]) = '2') and (Trim(Lines[i + 3]) = 'LAYER') then begin
          InTable := True;
          Inc(Tables);
          Inc(i, 2);
        end;
      end;
      Inc(i, 2);
    end;
  finally
    Lines.Free;
  end;
  if Tables <> 1 then begin
    writeln('FAIL: expected exactly one LAYER table, found ', Tables);
    Inc(Result);
  end;
  if Result = 0 then
    writeln('ok:   LAYER table structure');
end;

var
  Drawing, Reloaded: TSimpleDrawing;
  TemplateFile, OutFile: string;
  i, Failed: Integer;
  DC: TDrawContext;
begin
  if ParamCount < 2 then begin
    writeln('usage: roundtrip1436 <template.dxf> <out.dxf>');
    Halt(2);
  end;
  TemplateFile := ParamStr(1);
  OutFile := ParamStr(2);

  Drawing.init(nil);
  DC := Drawing.CreateDrawingRC;
  for i := Low(Samples) to High(Samples) do
    with Samples[i] do
      Drawing.LayerTable.addlayer(Name, Color, LW, IsOn, IsLocked, IsPrinted, Desk, TLOLoad);

  writeln('saving:  ', OutFile, ' (template ', TemplateFile, ')');
  if not savedxf20XX(OutFile, TemplateFile, Drawing, ZCDxf2007) then begin
    writeln('SAVE FAILED');
    Halt(1);
  end;
  Drawing.done;

  Failed := CheckLayerTableStructure(OutFile);

  writeln('loading: ', OutFile);
  LoadDrawing(OutFile, Reloaded);
  Failed := Failed + CheckLayers(Reloaded);
  Reloaded.done;

  if Failed = 0 then
    writeln('OK')
  else begin
    writeln('FAILED: ', Failed);
    Halt(1);
  end;
end.
