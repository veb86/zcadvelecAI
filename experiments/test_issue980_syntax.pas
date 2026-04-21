program test_issue980_syntax;
{$MODE delphi}{$H+}
uses SysUtils, Classes;

{ Standalone test of the helper functions added to uzeffdxf.pas
  to verify syntax and behaviour without ZCAD's dependencies. }

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

procedure ScanTableContinuationHandles(const RawObjectsSection: string;
  ContinuationHandles: TStringList);
var
  Lines: TStringList;
  I, Code: Integer;
  Value: string;
  InXRecord, InRoundtripBlock: Boolean;
begin
  if (RawObjectsSection = '') or (ContinuationHandles = nil) then
    Exit;

  Lines := TStringList.Create;
  try
    Lines.Text := RawObjectsSection;
    InXRecord := False;
    InRoundtripBlock := False;
    I := 0;
    while I < Lines.Count - 1 do
    begin
      if not TryStrToInt(Trim(Lines[I]), Code) then begin
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

procedure AssertEquals(const Expected, Actual: string; const Name: string);
begin
  if Expected = Actual then
    Writeln('PASS: ', Name)
  else
    Writeln('FAIL: ', Name, ' expected=', Expected, ' actual=', Actual);
end;

procedure TestNormalizeHandle;
begin
  AssertEquals('209',   NormalizeHandle('209'), 'basic');
  AssertEquals('209',   NormalizeHandle('0209'), 'leading_zero');
  AssertEquals('209',   NormalizeHandle('00000209'), 'many_leading_zeros');
  AssertEquals('FAB',   NormalizeHandle('fab'), 'lowercase');
  AssertEquals('FAB',   NormalizeHandle('FAB'), 'uppercase');
  AssertEquals('FAB',   NormalizeHandle('  fab  '), 'whitespace');
  AssertEquals('0',     NormalizeHandle('0'), 'single_zero');
  AssertEquals('0',     NormalizeHandle('00'), 'double_zero');
end;

procedure TestScan_SingleXRecordWithTwoContinuations;
var
  raw: string;
  list: TStringList;
begin
  raw := '  0'#10'XRECORD'#10'  5'#10'2AA'#10'100'#10'AcDbXrecord'#10 +
         '102'#10'ACAD_ROUNDTRIP_2008_TABLE_ENTITY'#10 +
         '360'#10'207'#10 +
         ' 70'#10'     1'#10 +
         '330'#10'209'#10 +
         '330'#10'259'#10 +
         '361'#10'208'#10 +
         '  0'#10'LAYOUT'#10;
  list := TStringList.Create;
  list.Sorted := True;
  list.Duplicates := dupIgnore;
  try
    ScanTableContinuationHandles(raw, list);
    AssertEquals('2', IntToStr(list.Count), 'TwoContinuations_Count');
    AssertEquals('209', list[0], 'TwoContinuations_First');
    AssertEquals('259', list[1], 'TwoContinuations_Second');
  finally
    list.Free;
  end;
end;

procedure TestScan_NoRoundtrip;
var
  raw: string;
  list: TStringList;
begin
  raw := '  0'#10'XRECORD'#10'  5'#10'AA'#10'100'#10'AcDbXrecord'#10 +
         '280'#10'     1'#10'  0'#10'DICTIONARY'#10;
  list := TStringList.Create;
  list.Sorted := True;
  list.Duplicates := dupIgnore;
  try
    ScanTableContinuationHandles(raw, list);
    AssertEquals('0', IntToStr(list.Count), 'NoRoundtrip_Count');
  finally
    list.Free;
  end;
end;

procedure TestScan_EmptyInput;
var
  list: TStringList;
begin
  list := TStringList.Create;
  try
    ScanTableContinuationHandles('', list);
    AssertEquals('0', IntToStr(list.Count), 'Empty_Count');
  finally
    list.Free;
  end;
end;

procedure TestScan_RoundtripTerminatedBy361;
var
  raw: string;
  list: TStringList;
begin
  { 361 must terminate the roundtrip block; the 330 AFTER it should be ignored. }
  raw := '  0'#10'XRECORD'#10 +
         '102'#10'ACAD_ROUNDTRIP_2008_TABLE_ENTITY'#10 +
         '330'#10'AAA'#10 +
         '361'#10'208'#10 +
         '330'#10'BBB'#10 +
         '  0'#10'LAYOUT'#10;
  list := TStringList.Create;
  list.Sorted := True;
  list.Duplicates := dupIgnore;
  try
    ScanTableContinuationHandles(raw, list);
    AssertEquals('1', IntToStr(list.Count), 'Terminated361_Count');
    AssertEquals('AAA', list[0], 'Terminated361_First');
  finally
    list.Free;
  end;
end;

begin
  Writeln('--- NormalizeHandle tests ---');
  TestNormalizeHandle;
  Writeln('--- ScanTableContinuationHandles tests ---');
  TestScan_EmptyInput;
  TestScan_NoRoundtrip;
  TestScan_SingleXRecordWithTwoContinuations;
  TestScan_RoundtripTerminatedBy361;
  Writeln('--- Done ---');
end.
