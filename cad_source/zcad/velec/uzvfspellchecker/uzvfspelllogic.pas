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
  Classes, SysUtils, Math,
  uzcSpeller, uSpeller, uzclog,
  uzvfspelldata;

// Найти все орфографические ошибки в тексте
function FindAllErrors(const AText: string;
  ErrorManager: TSpellErrorManager): integer;

// Добавить ошибки без очистки текущего списка
function AppendErrorsFromText(const AText: string;
  ErrorManager: TSpellErrorManager; ABasePosition: integer = 0;
  AEntityPtr: Pointer = nil): integer;

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

// Проверить разделитель предложения
function IsSentenceDelimiter(AChar: char): boolean;
begin
  Result := (Pos(AChar, SENTENCE_DELIMITERS) > 0) or
    (AChar in [#10, #13]);
end;

// Проверить, является ли символ разделителем слова
function IsWordDelimiter(AChar: char): boolean;
begin
  Result := (AChar <= ' ') or IsSentenceDelimiter(AChar) or
    (AChar in [',', ';', ':', '(', ')', '[', ']', '{', '}', '<', '>',
      '"', '''', '/', '\', '|', '+', '=', '*', '`', '~', '@', '#',
      '$', '%', '^', '&']);
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
  while (WordStart > 1) and not IsWordDelimiter(AText[WordStart - 1]) do
    Dec(WordStart);

  // Найти конец слова
  while (WordEnd <= textLen) and not IsWordDelimiter(AText[WordEnd]) do
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
  spellResult: TSpeller.TLangHandle;
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
function AppendErrorsFromText(const AText: string;
  ErrorManager: TSpellErrorManager; ABasePosition: integer;
  AEntityPtr: Pointer): integer;
var
  currentPos: integer;
  wordStart, wordEnd: integer;
  currentWord: string;
  errorDetails: string;
  sentence: string;
  textLen: integer;
  absolutePosition: integer;
begin
  Result := 0;
  textLen := Length(AText);
  if textLen = 0 then begin
    programlog.LogOutFormatStr('AppendErrorsFromText: empty text', [], LM_Info);
    Exit;
  end;

  currentPos := 1;

  // Проход по всему тексту
  while currentPos <= textLen do begin
    // Пропустить разделители
    while (currentPos <= textLen) and IsWordDelimiter(AText[currentPos]) do
      Inc(currentPos);

    if currentPos > textLen then
      Break;

    // Извлечь слово
    currentWord := ExtractWordAt(AText, currentPos, wordStart, wordEnd);

    if Length(currentWord) > 0 then begin
      // Проверить слово на ошибки
      if CheckWord(currentWord, errorDetails) then begin
        sentence := ExtractSentence(AText, currentPos);
        absolutePosition := ABasePosition + currentPos;
        ErrorManager.AddError(currentWord, sentence, absolutePosition,
          AEntityPtr, currentPos);
        Inc(Result);
      end;
    end;

    currentPos := wordEnd;
  end;

  programlog.LogOutFormatStr('AppendErrorsFromText: found %d errors',
    [Result], LM_Info);
end;

// Найти все ошибки в тексте
function FindAllErrors(const AText: string;
  ErrorManager: TSpellErrorManager): integer;
begin
  ErrorManager.ClearErrors;
  Result := AppendErrorsFromText(AText, ErrorManager);

  programlog.LogOutFormatStr('FindAllErrors: found %d errors',
    [Result], LM_Info);
end;

// Извлечь список вариантов из строки вида "слово[вариант1, вариант2]"
function ExtractSuggestionListText(const ADetails: string): string;
var
  openBracketPos: integer;
  closeBracketPos: integer;
begin
  Result := Trim(ADetails);
  openBracketPos := Pos('[', ADetails);

  if openBracketPos = 0 then
    Exit;

  closeBracketPos := Length(ADetails);
  while (closeBracketPos > openBracketPos) and
        (ADetails[closeBracketPos] <> ']') do
    Dec(closeBracketPos);

  if closeBracketPos <= openBracketPos then begin
    Result := '';
    Exit;
  end;

  Result := Trim(Copy(ADetails, openBracketPos + 1,
    closeBracketPos - openBracketPos - 1));
end;

// Получить варианты исправления
function GetSuggestions(const AWord: string): TStringList;
var
  errorDetails: string;
  suggestionDetails: string;
  i: integer;
begin
  Result := TStringList.Create;

  if Length(AWord) = 0 then
    Exit;

  // Получить подробности ошибки от SpellChecker
  SpellChecker.SpellTextSimple(AWord, errorDetails,
    TSpeller.CSpellOptDetail);

  // Разобрать варианты из errorDetails
  suggestionDetails := ExtractSuggestionListText(errorDetails);
  if Length(suggestionDetails) > 0 then begin
    // Попытка извлечь варианты, разделенные запятой
    Result.Delimiter := ',';
    Result.StrictDelimiter := True;
    Result.DelimitedText := suggestionDetails;

    // Очистить варианты от лишних пробелов
    for i := Result.Count - 1 downto 0 do begin
      Result[i] := Trim(Result[i]);
      if Length(Result[i]) = 0 then
        Result.Delete(i);
    end;
  end;

  programlog.LogOutFormatStr('GetSuggestions: word="%s", count=%d',
    [AWord, Result.Count], LM_Info);
end;

end.
