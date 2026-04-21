program test_issue980_real_dxf;
{$MODE delphi}{$H+}
uses SysUtils, Classes;

{ End-to-end: load tablerazdel.dxf, extract OBJECTS section, run
  ScanTableContinuationHandles, verify 209 and 259 are found. }

function NormalizeHandle(const S: string): string;
var
  I: Integer;
begin
  Result := UpperCase(Trim(S));
  I := 1;
  while (I < Length(Result)) and (Result[I] = '0') do
    Inc(I);
  if I > 1 then
    Result := Copy(Result, I, Length(Result) - I + 1);
end;

function ExtractDxfRawSection(const AFileName, SectionName: string): string;
var
  Lines: TStringList;
  I, SectionStart, SectionEnd: Integer;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName);
    SectionStart := -1;
    SectionEnd := -1;
    I := 0;
    while I < Lines.Count - 3 do
    begin
      if (Trim(Lines[I]) = '0') and (Trim(Lines[I + 1]) = 'SECTION') and
         (Trim(Lines[I + 2]) = '2') and (Trim(Lines[I + 3]) = SectionName) then
      begin
        SectionStart := I;
        Break;
      end;
      Inc(I);
    end;
    if SectionStart < 0 then Exit;
    I := SectionStart + 4;
    while I < Lines.Count - 1 do
    begin
      if (Trim(Lines[I]) = '0') and (Trim(Lines[I + 1]) = 'ENDSEC') then
      begin
        SectionEnd := I + 1;
        Break;
      end;
      Inc(I);
    end;
    if SectionEnd < 0 then Exit;
    for I := SectionStart to SectionEnd do
    begin
      if I > SectionStart then Result := Result + sLineBreak;
      Result := Result + Lines[I];
    end;
  finally
    Lines.Free;
  end;
end;

procedure ScanTableContinuationHandles(const RawObjectsSection: string;
  ContinuationHandles: TStringList);
var
  Lines: TStringList;
  I, Code: Integer;
  Value: string;
  InXRecord, InRoundtripBlock: Boolean;
begin
  if (RawObjectsSection = '') or (ContinuationHandles = nil) then Exit;
  Lines := TStringList.Create;
  try
    Lines.Text := RawObjectsSection;
    InXRecord := False;
    InRoundtripBlock := False;
    I := 0;
    while I < Lines.Count - 1 do
    begin
      if not TryStrToInt(Trim(Lines[I]), Code) then
      begin
        Inc(I, 2);
        Continue;
      end;
      Value := Trim(Lines[I + 1]);
      if Code = 0 then
      begin
        InXRecord := (UpperCase(Value) = 'XRECORD');
        InRoundtripBlock := False;
      end
      else if InXRecord then
      begin
        if (Code = 102) and
           (UpperCase(Value) = 'ACAD_ROUNDTRIP_2008_TABLE_ENTITY') then
          InRoundtripBlock := True
        else if InRoundtripBlock then
        begin
          if Code = 330 then
            ContinuationHandles.Add(NormalizeHandle(Value))
          else if Code = 361 then
            InRoundtripBlock := False;
        end;
      end;
      Inc(I, 2);
    end;
  finally
    Lines.Free;
  end;
end;

function RunTest(const DxfPath: string; const ExpectedHandles: array of string): Boolean;
var
  raw: string;
  list: TStringList;
  I, J: Integer;
  Found: Boolean;
begin
  Result := True;
  raw := ExtractDxfRawSection(DxfPath, 'OBJECTS');
  Writeln;
  Writeln('File: ', DxfPath);
  Writeln('  OBJECTS section: ', Length(raw), ' chars');
  if Length(raw) = 0 then
  begin
    Writeln('  FAIL: empty OBJECTS section');
    Exit(False);
  end;
  list := TStringList.Create;
  list.Sorted := True;
  list.Duplicates := dupIgnore;
  try
    ScanTableContinuationHandles(raw, list);
    Writeln('  Continuation handles found: ', list.Count);
    for I := 0 to list.Count - 1 do
      Writeln('    [', I, '] = ', list[I]);

    if list.Count <> Length(ExpectedHandles) then
    begin
      Writeln('  FAIL: expected ', Length(ExpectedHandles),
              ' handle(s), got ', list.Count);
      Exit(False);
    end;
    for I := 0 to High(ExpectedHandles) do
    begin
      Found := False;
      for J := 0 to list.Count - 1 do
        if list[J] = ExpectedHandles[I] then
        begin
          Found := True;
          Break;
        end;
      if not Found then
      begin
        Writeln('  FAIL: expected handle "', ExpectedHandles[I], '" not found');
        Exit(False);
      end;
    end;
    Writeln('  PASS');
  finally
    list.Free;
  end;
end;

var
  AllPass: Boolean;
begin
  AllPass := True;
  AllPass := RunTest('../cad_source/test/tablerazdel.dxf',  ['209', '259']) and AllPass;
  AllPass := RunTest('../cad_source/test/tablerazdel2.dxf', []) and AllPass;
  if AllPass then
    Halt(0)
  else
    Halt(1);
end.
