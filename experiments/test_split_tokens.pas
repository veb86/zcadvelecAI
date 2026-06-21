// Standalone check of the issue #1353 token-splitting logic.
// Mirrors IsBreakByte + SplitWordToTokens from uzvfspelluserdict.pas to verify
// the splitting behavior compiles and produces the expected tokens.
program test_split_tokens;

{$mode objfpc}{$H+}
{$Codepage UTF8}

uses
  Classes, SysUtils;

function IsBreakByte(AByte: char): boolean;
begin
  Result := (Ord(AByte) < $80) and not (AByte in ['a'..'z', 'A'..'Z']);
end;

procedure SplitWordToTokens(const AWord: string; ATokens: TStrings);
var
  i, startIdx, len: integer;
begin
  len := Length(AWord);
  i := 1;
  while i <= len do begin
    while (i <= len) and IsBreakByte(AWord[i]) do
      Inc(i);
    if i > len then
      Break;
    startIdx := i;
    while (i <= len) and not IsBreakByte(AWord[i]) do
      Inc(i);
    ATokens.Add(Copy(AWord, startIdx, i - startIdx));
  end;
end;

var
  failures: integer = 0;

procedure Check(const AWord, AExpected: string);
var
  t: TStringList;
  got: string;
begin
  t := TStringList.Create;
  try
    SplitWordToTokens(AWord, t);
    t.Delimiter := '|';
    t.StrictDelimiter := True;
    got := t.DelimitedText;
    if got <> AExpected then begin
      WriteLn('FAIL: ', AWord, ' -> [', got, '] expected [', AExpected, ']');
      Inc(failures);
    end else
      WriteLn('ok:   ', AWord, ' -> [', got, ']');
  finally
    t.Free;
  end;
end;

begin
  Check('охранно-пожарный', 'охранно|пожарный');
  Check('кто-то', 'кто|то');
  Check('слово/привет', 'слово|привет');
  Check('АВ-', 'АВ');
  Check('-слово', 'слово');
  Check('-12привет', 'привет');
  Check('привет', 'привет');
  Check('hello', 'hello');
  Check('---', '');
  Check('123', '');
  if failures = 0 then begin
    WriteLn('ALL OK');
    Halt(0);
  end else begin
    WriteLn(failures, ' FAILURES');
    Halt(1);
  end;
end.
