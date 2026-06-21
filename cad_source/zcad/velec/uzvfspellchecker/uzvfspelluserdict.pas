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
// слов, далее сами слова) рядом с парным файлом .aff, который объявляет
// кодировку UTF-8 (SET UTF-8), чтобы добавленные слова распознавались.
//
// issue #1353: слово, у которого есть знак "-" (например "АВ-", "-12" или
// "охранно-пожарный"), всегда определялось как ошибка даже после добавления
// в словарь. Причина в том, что орфо-движок (uSpeller.SpellTextSimple)
// считает дефис (и любой другой однобайтовый символ, не являющийся латинской
// буквой: цифру, точку, "/" и т. п.) разделителем слова и ищет в словаре
// каждое буквенное «ядро» по отдельности, без таких символов. В словарь же
// попадало слово целиком вместе с разделителями, поэтому совпадения никогда
// не было — ни для краевых дефисов, ни для внутренних.
//
// Прошлая правка лишь отбрасывала ведущие/замыкающие разделители, поэтому
// слова с внутренним дефисом ("охранно-пожарный") и со спецсимволами hunspell
// ("слово/привет", где "/" вводит аффиксы) по-прежнему не распознавались.
// Теперь перед сохранением слово разбивается на те же буквенные токены, что
// ищет движок (максимальные цепочки байтов, не являющихся разделителями), и в
// словарь добавляется каждый токен по отдельности. Это разом покрывает дефис
// в начале, в конце и внутри слова и при этом никогда не пишет в .dic
// спецсимволы hunspell (так снимается вопрос об экранировании).

unit uzvfspelluserdict;

{$mode objfpc}{$H+}
{$Codepage UTF8}

interface

uses
  Classes;

// Добавить слово в пользовательский словарь. Создаёт словарь, если его нет,
// и перезагружает SpellChecker, чтобы слово сразу учитывалось.
// Возвращает True, если слово добавлено (или уже присутствовало).
function AddWordToUserDictionary(const AWord: string): boolean;

// Загрузить все слова пользовательского словаря в список (в порядке файла,
// без строки-счётчика, аффиксов после '/' и BOM). Возвращает True, если
// словарь существует и прочитан; список очищается в любом случае.
function LoadUserDictionaryWords(AList: TStrings): boolean;

// Удалить слово из пользовательского словаря и перезагрузить SpellChecker,
// чтобы изменение сразу учитывалось. Возвращает True, если слово было найдено
// и удалено.
function RemoveWordFromUserDictionary(const AWord: string): boolean;

implementation

uses
  SysUtils,
  uzclog,
  uzcSpeller;

const
  // Содержимое парного .aff: словарь хранится в UTF-8, поэтому явно указываем
  // кодировку директивой SET UTF-8 (как в штатных ru_RU.aff/en_US.aff).
  // Без неё hunspell считает словарь однобайтовым (ISO8859-1), и в некоторых
  // сборках добавленное слово не распознаётся (см. issue #1296).
  CUserDictAffContent = 'SET UTF-8' + LineEnding;

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

// Является ли байт «разделителем слова» с точки зрения орфо-движка
// (uSpeller.SpellTextSimple). Движок считает разделителем любой однобайтовый
// символ UTF-8 (Ord < $80), не являющийся латинской буквой a..z/A..Z, — в том
// числе дефис, цифры, точку. Многобайтовые символы (кириллица и пр.) — буквы,
// их байты (Ord >= $80) разделителями не считаются (issue #1353).
function IsBreakByte(AByte: char): boolean;
begin
  Result := (Ord(AByte) < $80) and not (AByte in ['a'..'z', 'A'..'Z']);
end;

// Разбить слово на буквенные токены — максимальные цепочки байтов, не
// являющихся разделителями. Это в точности те токены, которые орфо-движок
// (uSpeller.SpellTextSimple) ищет в словаре по отдельности. Любые разделители
// (дефис в начале/конце/внутри слова, цифры, точка, "/" и пр.) отбрасываются,
// поэтому в словарь попадают только буквенные ядра без спецсимволов hunspell
// (issue #1353).
procedure SplitWordToTokens(const AWord: string; ATokens: TStrings);
var
  i, startIdx, len: integer;
begin
  len := Length(AWord);
  i := 1;
  while i <= len do begin
    // Пропустить разделители
    while (i <= len) and IsBreakByte(AWord[i]) do
      Inc(i);
    if i > len then
      Break;
    // Считать максимальную цепочку «буквенных» байтов
    startIdx := i;
    while (i <= len) and not IsBreakByte(AWord[i]) do
      Inc(i);
    ATokens.Add(Copy(AWord, startIdx, i - startIdx));
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

// Прочитать весь файл в строку (или '' если файла нет).
function ReadFileIfExists(const AFileName: string): RawByteString;
begin
  if FileExists(AFileName) then
    Result := ReadFileRaw(AFileName)
  else
    Result := '';
end;

// Создать/обновить парный файл .aff с указанием кодировки UTF-8.
// Требуется hunspell; перезаписываем и старые пустые .aff, созданные ранее.
procedure EnsureAffFile(const ADicPath: string);
var
  affPath: string;
begin
  affPath := ChangeFileExt(ADicPath, '.aff');
  if ReadFileIfExists(affPath) <> CUserDictAffContent then
    WriteFileRaw(affPath, CUserDictAffContent);
end;

function AddWordToUserDictionary(const AWord: string): boolean;
var
  dicPath, token: string;
  words, tokens: TStringList;
  i: integer;
  changed: boolean;
begin
  Result := False;

  // Разбиваем слово на буквенные токены так же, как их ищет орфо-движок, и
  // добавляем каждый по отдельности. Иначе слово с дефисом (краевым или
  // внутренним) или со спецсимволом ("/") не совпадёт с тем, что ищет движок,
  // и останется «ошибкой» даже после добавления (issue #1353).
  tokens := TStringList.Create;
  words := TStringList.Create;
  try
    SplitWordToTokens(Trim(AWord), tokens);
    if tokens.Count = 0 then begin
      programlog.LogOutStr(
        'AddWordToUserDictionary: empty word, nothing to add', LM_Warning);
      Exit;
    end;

    dicPath := GetUserDictionaryPath;
    words.CaseSensitive := True;

    if FileExists(dicPath) then
      LoadUserWords(words, dicPath);

    changed := False;
    for i := 0 to tokens.Count - 1 do begin
      token := tokens[i];
      if words.IndexOf(token) >= 0 then begin
        programlog.LogOutFormatStr(
          'AddWordToUserDictionary: token "%s" already in dictionary',
          [token], LM_Info);
        Continue;
      end;
      words.Add(token);
      changed := True;
      programlog.LogOutFormatStr(
        'AddWordToUserDictionary: added token "%s" to "%s"',
        [token, dicPath], LM_Info);
    end;

    // Все токены уже были в словаре — слово считается добавленным
    if not changed then begin
      Result := True;
      Exit;
    end;

    SaveUserWords(words, dicPath);
    EnsureAffFile(dicPath);

    // Перезагрузить словари, чтобы слово сразу считалось корректным
    ReloadSpellChecker;

    Result := True;
  finally
    words.Free;
    tokens.Free;
  end;
end;

function LoadUserDictionaryWords(AList: TStrings): boolean;
var
  dicPath: string;
  words: TStringList;
begin
  Result := False;
  AList.Clear;

  dicPath := GetUserDictionaryPath;
  if not FileExists(dicPath) then begin
    programlog.LogOutFormatStr(
      'LoadUserDictionaryWords: dictionary "%s" not found', [dicPath],
      LM_Info);
    Exit;
  end;

  words := TStringList.Create;
  try
    words.CaseSensitive := True;
    LoadUserWords(words, dicPath);
    AList.Assign(words);
    Result := True;
    programlog.LogOutFormatStr(
      'LoadUserDictionaryWords: loaded %d words from "%s"',
      [words.Count, dicPath], LM_Info);
  finally
    words.Free;
  end;
end;

function RemoveWordFromUserDictionary(const AWord: string): boolean;
var
  dicPath, wordText: string;
  words: TStringList;
  idx: integer;
begin
  Result := False;

  wordText := Trim(AWord);
  if wordText = '' then begin
    programlog.LogOutStr(
      'RemoveWordFromUserDictionary: empty word, nothing to remove',
      LM_Warning);
    Exit;
  end;

  dicPath := GetUserDictionaryPath;
  if not FileExists(dicPath) then begin
    programlog.LogOutFormatStr(
      'RemoveWordFromUserDictionary: dictionary "%s" not found', [dicPath],
      LM_Info);
    Exit;
  end;

  words := TStringList.Create;
  try
    words.CaseSensitive := True;
    LoadUserWords(words, dicPath);

    idx := words.IndexOf(wordText);
    if idx < 0 then begin
      programlog.LogOutFormatStr(
        'RemoveWordFromUserDictionary: word "%s" not in dictionary',
        [wordText], LM_Info);
      Exit;
    end;

    words.Delete(idx);
    SaveUserWords(words, dicPath);
    EnsureAffFile(dicPath);

    // Перезагрузить словари, чтобы удалённое слово сразу снова считалось
    // ошибкой при последующих проверках
    ReloadSpellChecker;

    Result := True;
    programlog.LogOutFormatStr(
      'RemoveWordFromUserDictionary: removed "%s" from "%s"',
      [wordText, dicPath], LM_Info);
  finally
    words.Free;
  end;
end;

end.
