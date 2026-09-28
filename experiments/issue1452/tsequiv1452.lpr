{ issue #1452: эквивалентность нового чтения стилей таблиц (NOD-обработчик,
  uzestylestablesdxfnod) и старого ReadTableStylesFromDXFObjects.
  Собирается, пока старая функция ещё есть (до её удаления на этапе 5):
    cp experiments/issue1452/tsequiv1452.lpr cad_source/zengine/tests/
    experiments/issue1452/build_and_run.sh tsequiv1452 <outdir> <файлы dxf…>
  Для каждого файла пишет <outdir>/<имя>.legacy.txt и .nod.txt (формат
  DumpTableStyles — тот же, что в nodstage5) и сообщает о расхождениях. }
program tsequiv1452;
{$Mode delphi}{$H+}
uses
  SysUtils, Classes, Interfaces, gzctnrVectorTypes, uzestylestablesdxf,
  uzeffdxfnod, uzestylestablesdxfnod;

var
  FS: TFormatSettings;

function F(V: Double): string;
begin
  Result := FloatToStr(V, FS);
end;

function DumpTableStyles(var T: GDBDXFTableStyleArray): string;
var
  S: PTGDBDXFTableStyle;
  C: PTGDBDXFTableCellStyle;
  It, It2: itrec;
  I: Integer;
begin
  Result := '';
  S := T.beginiterate(It);
  while S <> nil do begin
    Result := Result + Format('%s handle=%s xdict=%s 70=%d 71=%d 40=%s 41=%s 280=%d 281=%d cells=%d',
      [S^.Name, S^.DXFHandle, S^.XDictHandle, S^.Flags70, S^.Flags71,
       F(S^.HorzCellMargin), F(S^.VertCellMargin), Ord(S^.TitleSuppressed),
       Ord(S^.ColumnHeadingSuppressed), S^.CellFormats.Count]) + LineEnding;
    I := 0;
    C := S^.CellFormats.beginiterate(It2);
    while C <> nil do begin
      if I <= 2 then
        Result := Result + Format('  cell%d 7=%s', [I, S^.CellTextStyleName[I]])
      else
        Result := Result + Format('  cell%d', [I]);
      Result := Result + Format(' 140=%s 170=%d 62=%d 63=%d 283=%d',
        [F(C^.TextHeight), C^.Alignment, C^.TextColor, C^.BackgroundColor,
         Ord(C^.BackgroundColorEnabled)]) + LineEnding;
      Inc(I);
      C := S^.CellFormats.iterate(It2);
    end;
    S := T.iterate(It);
  end;
end;

function ObjectsSection(const AFile: string): string;
var
  L: TStringList;
  I, B: Integer;
begin
  Result := '';
  L := TStringList.Create;
  try
    L.LoadFromFile(AFile);
    B := -1;
    for I := 1 to L.Count - 1 do
      if (Trim(L[I]) = 'OBJECTS') and (Trim(L[I - 1]) = '2') then begin
        B := I - 3;
        Break;
      end;
    if B < 0 then
      Exit;
    for I := B to L.Count - 1 do begin
      Result := Result + L[I] + LineEnding;
      if (Trim(L[I]) = 'ENDSEC') and (I > B + 3) then
        Break;
    end;
  finally
    L.Free;
  end;
end;

procedure Save(const AName, AText: string);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    L.Text := AText;
    L.SaveToFile(AName);
  finally
    L.Free;
  end;
end;

var
  I, Diff: Integer;
  OutDir, Sec, A, B: string;
  Legacy, Nod: GDBDXFTableStyleArray;
  Model: TZNODModel;
begin
  FS := DefaultFormatSettings;
  FS.DecimalSeparator := '.';
  OutDir := IncludeTrailingPathDelimiter(ParamStr(2));
  ForceDirectories(OutDir);
  Diff := 0;
  for I := 3 to ParamCount do begin
    Sec := ObjectsSection(ParamStr(I));
    Legacy.init(10);
    Nod.init(10);
    Model := TZNODModel.Create;
    try
      {$WARN SYMBOL_DEPRECATED OFF}
      ReadTableStylesFromDXFObjects(Sec, Legacy);
      Model.LoadFromText(Sec);
      LoadTableStylesFromNOD(Model, Nod);
      A := DumpTableStyles(Legacy);
      B := DumpTableStyles(Nod);
      Save(OutDir + ExtractFileName(ParamStr(I)) + '.legacy.txt', A);
      Save(OutDir + ExtractFileName(ParamStr(I)) + '.nod.txt', B);
      if A = B then
        WriteLn('same  ', Nod.Count, ' ', ParamStr(I))
      else begin
        WriteLn('DIFF  ', Legacy.Count, '/', Nod.Count, ' ', ParamStr(I));
        Inc(Diff);
      end;
    finally
      Model.Free;
      Nod.Done;
      Legacy.Done;
    end;
  end;
  WriteLn('files with differences: ', Diff);
  if Diff > 0 then
    ExitCode := 1;
end.
