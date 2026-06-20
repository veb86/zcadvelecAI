{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
}

// Работа с пользовательским словарём орфографии (issue #1296):
// добавление нового слова в словарь и его создание при необходимости.
// Словарь хранится в формате hunspell (.dic: первая строка - количество
// слов, далее сами слова) рядом с парным пустым файлом .aff, как у abbrv.

unit uzvfspelluserdict;

{$mode objfpc}{$H+}
{$Codepage UTF8}

interface

// Добавить слово в пользовательский словарь. Создаёт словарь, если его нет,
// и перезагружает SpellChecker, чтобы слово сразу учитывалось.
// Возвращает True, если слово добавлено (или уже присутствовало).
function AddWordToUserDictionary(const AWord: string): boolean;

implementation

uses
  Classes, SysUtils,
  uzclog,
  uzcSpeller;

// Прочитать файл как «сырые» байты без перекодировки (словарь в UTF-8).
// Байты помечаются как UTF-8, чтобы сравнение слов было побайтовым и не
// зависело от системной кодировки.
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
  SetCodePage(Result, CP_UTF8, False);
end;

// Записать текст в файл как «сырые» байты UTF-8 без BOM.
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

// Является ли строка только цифрами (строка-счётчик в начале .dic).
function IsAllDigits(const AStr: string): boolean;
var
  i: integer;
begin
  Result := AStr <> '';
  for i := 1 to Length(AStr) do
    if not (AStr[i] in ['0'..'9']) then
      Exit(False);
end;

// Загрузить слова из существующего .dic в список (без строки-счётчика,
// аффиксов после '/' и BOM).
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
      // Убрать UTF-8 BOM в начале первой строки, если он есть
      if (i = 0) and (Length(line) >= 3) and (line[1] = #$EF) and
         (line[2] = #$BB) and (line[3] = #$BF) then
        Delete(line, 1, 3);
      line := Trim(line);
      if line = '' then
        Continue;
      // Пропустить строку с количеством слов (первая строка hunspell .dic)
      if (i = 0) and IsAllDigits(line) then
        Continue;
      // Отбросить аффиксы вида слово/FLAGS
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

// Сохранить слова в .dic в формате hunspell (счётчик + слова).
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

// Создать парный пустой файл .aff, если его нет (требуется hunspell).
procedure EnsureAffFile(const ADicPath: string);
var
  affPath: string;
begin
  affPath := ChangeFileExt(ADicPath, '.aff');
  if not FileExists(affPath) then
    WriteFileRaw(affPath, '');
end;

function AddWordToUserDictionary(const AWord: string): boolean;
var
  word, dicPath: string;
  words: TStringList;
begin
  Result := False;

  word := Trim(AWord);
  if word = '' then begin
    programlog.LogOutStr(
      'AddWordToUserDictionary: empty word, nothing to add', LM_Warning);
    Exit;
  end;

  dicPath := GetUserDictionaryPath;
  words := TStringList.Create;
  try
    words.CaseSensitive := True;

    if FileExists(dicPath) then
      LoadUserWords(words, dicPath);

    if words.IndexOf(word) >= 0 then begin
      programlog.LogOutFormatStr(
        'AddWordToUserDictionary: word "%s" already in dictionary',
        [word], LM_Info);
      Result := True;
      Exit;
    end;

    words.Add(word);
    SaveUserWords(words, dicPath);
    EnsureAffFile(dicPath);

    // Перезагрузить словари, чтобы слово сразу считалось корректным
    ReloadSpellChecker;

    programlog.LogOutFormatStr(
      'AddWordToUserDictionary: added word "%s" to "%s"',
      [word, dicPath], LM_Info);
    Result := True;
  finally
    words.Free;
  end;
end;

end.
