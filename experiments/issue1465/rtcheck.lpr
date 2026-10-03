program rtcheck;

// issue #1465: проверка цикла чтение → запись → повторное чтение таблиц
// ACAD_TABLE. Печатает сводку всех таблиц (структура, типы строк Title/
// Header/Data, тексты, флаги разрыва, части и их положения/высоты) до и
// после сохранения и сравнивает их. С ключом edit перед сохранением
// инвалидирует raw-DXF (как правка в ZCAD), чтобы проверить модельный путь.
//
// Использование: rtcheck <in.dxf> <template.dxf> <out.dxf> [edit]

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Interfaces,
  uzeffdxf, uzeffdxfout, uzedrawingsimple, uzeffmanager,
  uzgldrawcontext, uzeTypes, uzeconsts, gzctnrVectorTypes,
  uzegeometrytypes, uzeentity, uzeentgenericsubentry,
  uzeacadtable_types, uzeacadtable_model;

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

function F(AValue: Double): string;
begin
  Result := FormatFloat('0.###', AValue);
end;

procedure DumpTable(T: PGDBObjAcadTable; AOut: TStrings);
var
  R, C, P: Integer;
  S: string;
  Pt: TzePoint3d;
  H: Double;
begin
  AOut.Add(Format('table rows=%d cols=%d ins=(%s,%s) style=%s',
    [T^.RowCount, T^.ColCount, F(T^.InsertPoint.x), F(T^.InsertPoint.y),
     T^.TableStyleName]));
  AOut.Add(Format('  break en=%d rtop=%d rbot=%d mpos=%d mh=%d sp=%s h=%s',
    [Ord(T^.BreakEnabled), Ord(T^.BreakRepeatTopLabels),
     Ord(T^.BreakRepeatBottomLabels), Ord(T^.BreakManualPosition),
     Ord(T^.BreakManualHeight), F(T^.BreakSpacing), F(T^.BreakHeight)]));
  for R := 0 to T^.RowCount - 1 do begin
    S := Format('  row %d type=%d h=%s:', [R, T^.RowStyleTypeAt(R),
      F(T^.RowHeightAt(R))]);
    for C := 0 to T^.ColCount - 1 do
      S := S + ' [' + T^.CellTextAt(R, C) + ']';
    AOut.Add(S);
  end;
  for P := 0 to T^.ContinuationPartCount - 1 do begin
    T^.ContinuationPartPlacement(P, Pt, H);
    S := Format('  part %d rows=%d ins=(%s,%s) h=%s:',
      [P, T^.ContinuationPartRowCount(P), F(Pt.x), F(Pt.y), F(H)]);
    for R := 0 to T^.ContinuationPartRowCount(P) - 1 do
      S := S + ' [' + T^.ContinuationPartCellText(P, R, 0) + ']';
    AOut.Add(S);
  end;
end;

procedure DumpDrawing(var ADrawing: TSimpleDrawing; AOut: TStrings;
  AEdit: Boolean);
var
  IR: itrec;
  E: PGDBObjEntity;
  T: PGDBObjAcadTable;
  Dir: TAcadTableBreakDirection;
begin
  E := ADrawing.pObjRoot^.ObjArray.beginiterate(IR);
  while E <> nil do begin
    if E^.GetObjType = GDBAcadTableID then begin
      T := PGDBObjAcadTable(E);
      DumpTable(T, AOut);
      if AEdit then begin
        // Инвалидация raw: смена направления туда и обратно
        Dir := T^.BreakDirection;
        if Dir = atbdRight then
          T^.BreakDirection := atbdLeft
        else
          T^.BreakDirection := atbdRight;
        T^.BreakDirection := Dir;
      end;
    end;
    E := ADrawing.pObjRoot^.ObjArray.iterate(IR);
  end;
end;

var
  Drawing: TSimpleDrawing;
  Before, After: TStringList;
  I: Integer;
  Edit, Same: Boolean;
begin
  if ParamCount < 3 then begin
    writeln('usage: rtcheck <in.dxf> <template.dxf> <out.dxf> [edit]');
    Halt(2);
  end;
  Edit := (ParamCount >= 4) and (ParamStr(4) = 'edit');
  Before := TStringList.Create;
  After := TStringList.Create;
  LoadDrawing(ParamStr(1), Drawing);
  DumpDrawing(Drawing, Before, Edit);
  if not savedxf20XX(ParamStr(3), ParamStr(2), Drawing, ZCDxf2007) then begin
    writeln('SAVE FAILED');
    Halt(1);
  end;
  Drawing.done;
  LoadDrawing(ParamStr(3), Drawing);
  DumpDrawing(Drawing, After, False);
  Drawing.done;
  writeln('--- before');
  writeln(Before.Text);
  Same := Before.Text = After.Text;
  if not Same then begin
    writeln('--- after');
    writeln(After.Text);
    for I := 0 to Before.Count - 1 do
      if (I >= After.Count) or (Before[I] <> After[I]) then
        writeln('DIFF line ', I, ': ', Before[I]);
  end;
  if Same then writeln('ROUNDTRIP SAME') else writeln('ROUNDTRIP DIFFERS');
  Before.Free;
  After.Free;
  if not Same then
    Halt(1);
end.
