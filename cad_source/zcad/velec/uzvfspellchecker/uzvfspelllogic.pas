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

unit uzvfspelllogic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  uzcSpeller, uSpeller, uzclog,
  uzvfspelldata;

// Найти все орфографические ошибки в тексте
function FindAllErrors(const AText: string;
  ErrorManager: TSpellErrorManager): integer;

// Получить варианты исправления для слова
function GetSuggestions(const AWord: string): TStringList;

// Извлечь предложение из текста по позиции
function ExtractSentence(const AText: string; APosition: integer): string;

implementation

const
  // Символы-разделители предложений
  SENTENCE_DELIMITERS = '.!?';
  // Максимальная длина предложения для извлечения
  MAX_SENTENCE_LENGTH = 500;

// Проверить, является ли символ разделителем предложения
function IsSentenceDelimiter(AChar: char): boolean;
begin
  Result := Pos(AChar, SENTENCE_DELIMITERS) > 0;
end;

// Извлечь одно слово из текста начиная с позиции
function ExtractWordAt(const AText: string; APosition: integer;
  out WordStart, WordEnd: integer): string;
var
  textLen: integer;
begin
  Result := '';
  WordStart := APosition;
  WordEnd := APosition;
  textLen := Length(AText);

  if (APosition < 1) or (APosition > textLen) then
    Exit;

  // Найти начало слова
  while (WordStart > 1) and (AText[WordStart - 1] <> ' ') and
        not IsSentenceDelimiter(AText[WordStart - 1]) do
    Dec(WordStart);

  // Найти конец слова
  while (WordEnd <= textLen) and (AText[WordEnd] <> ' ') and
        not IsSentenceDelimiter(AText[WordEnd]) do
    Inc(WordEnd);

  Result := Copy(AText, WordStart, WordEnd - WordStart);
end;

// Извлечь предложение из текста
function ExtractSentence(const AText: string; APosition: integer): string;
var
  sentenceStart, sentenceEnd: integer;
  textLen: integer;
begin
  Result := '';
  textLen := Length(AText);

  if (APosition < 1) or (APosition > textLen) then
    Exit;

  sentenceStart := APosition;
  sentenceEnd := APosition;

  // Найти начало предложения
  while (sentenceStart > 1) and
        not IsSentenceDelimiter(AText[sentenceStart - 1]) do
    Dec(sentenceStart);

  // Найти конец предложения
  while (sentenceEnd <= textLen) and
        not IsSentenceDelimiter(AText[sentenceEnd]) do
    Inc(sentenceEnd);

  // Извлечь предложение с ограничением длины
  Result := Copy(AText, sentenceStart,
    Min(sentenceEnd - sentenceStart + 1, MAX_SENTENCE_LENGTH));
  Result := Trim(Result);

  programlog.LogOutFormatStr(
    'ExtractSentence: position=%d, sentence="%s"',
    [APosition, Result], LM_Info);
end;

// Проверить одно слово на ошибки
function CheckWord(const AWord: string; var ErrorDetails: string): boolean;
var
  spellOpts: TSpeller.TSpellOpts;
  spellResult: TSpeller.TSpellResult;
begin
  Result := False;

  if Length(AWord) = 0 then
    Exit;

  spellOpts := TSpeller.CSpellOptDetail;
  spellResult := SpellChecker.SpellTextSimple(AWord, ErrorDetails, spellOpts);

  // Считаем ошибкой только случай неправильного языка
  Result := (spellResult = TSpeller.WrongLang);
end;

// Разбить текст на слова и проверить каждое
function FindAllErrors(const AText: string;
  ErrorManager: TSpellErrorManager): integer;
var
  currentPos: integer;
  wordStart, wordEnd: integer;
  currentWord: string;
  errorDetails: string;
  sentence: string;
  errorPtr: PSpellError;
  textLen: integer;
begin
  Result := 0;
  ErrorManager.ClearErrors;

  textLen := Length(AText);
  if textLen = 0 then begin
    programlog.LogOutFormatStr('FindAllErrors: empty text', [], LM_Info);
    Exit;
  end;

  currentPos := 1;

  // Проход по всему тексту
  while currentPos <= textLen do begin
    // Пропустить пробелы и разделители
    while (currentPos <= textLen) and
          ((AText[currentPos] = ' ') or
           IsSentenceDelimiter(AText[currentPos])) do
      Inc(currentPos);

    if currentPos > textLen then
      Break;

    // Извлечь слово
    currentWord := ExtractWordAt(AText, currentPos, wordStart, wordEnd);

    if Length(currentWord) > 0 then begin
      // Проверить слово на ошибки
      if CheckWord(currentWord, errorDetails) then begin
        // Проверить, не встречалось ли это слово ранее
        errorPtr := ErrorManager.FindErrorByWord(currentWord);

        if Assigned(errorPtr) then begin
          // Увеличить счетчик вхождений
          ErrorManager.IncrementOccurrence(errorPtr);
        end else begin
          // Добавить новую ошибку
          sentence := ExtractSentence(AText, currentPos);
          ErrorManager.AddError(currentWord, sentence, currentPos);
          Inc(Result);
        end;
      end;
    end;

    currentPos := wordEnd;
  end;

  programlog.LogOutFormatStr('FindAllErrors: found %d unique errors',
    [Result], LM_Info);
end;

// Получить варианты исправления
function GetSuggestions(const AWord: string): TStringList;
var
  errorDetails: string;
  i: integer;
begin
  Result := TStringList.Create;

  if Length(AWord) = 0 then
    Exit;

  // Получить подробности ошибки от SpellChecker
  SpellChecker.SpellTextSimple(AWord, errorDetails,
    TSpeller.CSpellOptDetail);

  // Разобрать варианты из errorDetails
  // Примечание: формат зависит от реализации TSpeller
  // Здесь используется простой парсинг
  if Length(errorDetails) > 0 then begin
    // Попытка извлечь варианты, разделенные запятой
    Result.Delimiter := ',';
    Result.StrictDelimiter := True;
    Result.DelimitedText := errorDetails;

    // Очистить варианты от лишних пробелов
    for i := 0 to Result.Count - 1 do
      Result[i] := Trim(Result[i]);
  end;

  programlog.LogOutFormatStr('GetSuggestions: word="%s", count=%d',
    [AWord, Result.Count], LM_Info);
end;

end.
