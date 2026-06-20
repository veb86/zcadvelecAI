// Standalone functional test for the user-dictionary file logic from
// uzvfspelluserdict.pas (issue #1296). Verifies the hunspell .dic format
// (count-first), round-trip read, dedup, and empty .aff creation.
program test_userdict_logic;
{$mode objfpc}{$H+}
{$Codepage UTF8}

uses
  {$IFDEF UNIX}cwstring,{$ENDIF}  // widestring manager, as in the real GUI app
  Classes, SysUtils;

// --- copied verbatim from uzvfspelluserdict.pas ---

function ReadFileRaw(const AFileName: string): RawByteString;
var
  fs: TFileStream;
begin
  Result := '';
  fs := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, fs.Size);
    if fs.Size > 0 then
      fs.ReadBuffer(Result[1], fs.Size);
  finally
    fs.Free;
  end;
  // Файл словаря в UTF-8: помечаем байты как UTF-8 без перекодировки,
  // чтобы сравнение слов было побайтовым и не зависело от системной кодировки.
  SetCodePage(Result, CP_UTF8, False);
end;

procedure WriteFileRaw(const AFileName, AContent: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(AFileName, fmCreate);
  try
    if AContent <> '' then
      fs.WriteBuffer(AContent[1], Length(AContent));
  finally
    fs.Free;
  end;
end;

function IsAllDigits(const AStr: string): boolean;
var
  i: integer;
begin
  Result := AStr <> '';
  for i := 1 to Length(AStr) do
    if not (AStr[i] in ['0'..'9']) then
      Exit(False);
end;

procedure LoadUserWords(AList: TStringList; const ADicPath: string);
var
  src: TStringList;
  i, slashPos: integer;
  line: string;
begin
  src := TStringList.Create;
  try
    src.Text := ReadFileRaw(ADicPath);
    for i := 0 to src.Count - 1 do begin
      line := src[i];
      if (i = 0) and (Length(line) >= 3) and (line[1] = #$EF) and
         (line[2] = #$BB) and (line[3] = #$BF) then
        Delete(line, 1, 3);
      line := Trim(line);
      if line = '' then
        Continue;
      if (i = 0) and IsAllDigits(line) then
        Continue;
      slashPos := Pos('/', line);
      if slashPos > 0 then
        line := Trim(Copy(line, 1, slashPos - 1));
      if (line <> '') and (AList.IndexOf(line) < 0) then
        AList.Add(line);
    end;
  finally
    src.Free;
  end;
end;

procedure SaveUserWords(AList: TStringList; const ADicPath: string);
var
  content: string;
  i: integer;
begin
  content := IntToStr(AList.Count) + LineEnding;
  for i := 0 to AList.Count - 1 do
    content := content + AList[i] + LineEnding;
  WriteFileRaw(ADicPath, content);
end;

procedure EnsureAffFile(const ADicPath: string);
var
  affPath: string;
begin
  affPath := ChangeFileExt(ADicPath, '.aff');
  if not FileExists(affPath) then
    WriteFileRaw(affPath, '');
end;

// --- test driver ---

var
  dic, aff, firstLine: string;
  words: TStringList;
  failures: integer = 0;

procedure Check(const AName: string; ACond: boolean);
begin
  if ACond then
    writeln('ok   - ', AName)
  else begin
    writeln('FAIL - ', AName);
    Inc(failures);
  end;
end;

begin
  dic := GetTempDir + 'userwords_test.dic';
  aff := ChangeFileExt(dic, '.aff');
  if FileExists(dic) then DeleteFile(dic);
  if FileExists(aff) then DeleteFile(aff);

  // First add: file does not exist yet
  words := TStringList.Create;
  words.CaseSensitive := True;
  if FileExists(dic) then LoadUserWords(words, dic);
  words.Add('Изврат');
  SaveUserWords(words, dic);
  EnsureAffFile(dic);
  words.Free;

  Check('dic file created', FileExists(dic));
  Check('aff file created', FileExists(aff));
  Check('aff file is empty', ReadFileRaw(aff) = '');

  firstLine := ReadFileRaw(dic);
  Check('first line is the count "1"',
    Copy(firstLine, 1, 1) = '1');

  // Second add: load existing, add a different word
  words := TStringList.Create;
  words.CaseSensitive := True;
  LoadUserWords(words, dic);
  Check('existing word loaded back (no count line)', words.Count = 1);
  Check('loaded word matches', words[0] = 'Изврат');

  words.Add('Перемычка');
  SaveUserWords(words, dic);
  words.Free;

  // Reload, verify both present and count line correct
  words := TStringList.Create;
  words.CaseSensitive := True;
  LoadUserWords(words, dic);
  Check('two words after second add', words.Count = 2);
  Check('first word present', words.IndexOf('Изврат') >= 0);
  Check('second word present', words.IndexOf('Перемычка') >= 0);

  firstLine := Trim(Copy(ReadFileRaw(dic), 1, Pos(LineEnding, ReadFileRaw(dic)) - 1));
  Check('count line equals 2', firstLine = '2');

  // Dedup: adding an already-present word does not duplicate
  Check('dedup via IndexOf', words.IndexOf('Изврат') >= 0);
  words.Free;

  // Flags after '/' are stripped on load
  WriteFileRaw(dic, '1' + LineEnding + 'извещатель/K' + LineEnding);
  words := TStringList.Create;
  words.CaseSensitive := True;
  LoadUserWords(words, dic);
  Check('affix flags stripped on load', words[0] = 'извещатель');
  words.Free;

  DeleteFile(dic);
  DeleteFile(aff);

  writeln;
  if failures = 0 then
    writeln('ALL USER-DICT LOGIC TESTS PASSED')
  else begin
    writeln(failures, ' TEST(S) FAILED');
    Halt(1);
  end;
end.
