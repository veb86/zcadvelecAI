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

unit uzvfspelldata;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
  // Одна орфографическая ошибка
  PSpellError = ^TSpellError;
  TSpellError = record
    ErrorWord: string;      // Слово с ошибкой
    Sentence: string;       // Предложение, содержащее ошибку
    Position: integer;      // Позиция ошибки в исходном тексте
    EntityPosition: integer; // Позиция ошибки в тексте примитива
    EntityPtr: Pointer;      // Текстовый примитив чертежа
  end;

  // Список ошибок
  TSpellErrorList = specialize TFPGList<PSpellError>;

  // Управление списком ошибок
  TSpellErrorManager = class
  private
    FErrorList: TSpellErrorList;

    // Освободить память для одной ошибки
    procedure FreeError(ErrorPtr: PSpellError);

  public
    constructor Create;
    destructor Destroy; override;

    // Добавить новую ошибку в список
    function AddError(const AWord, ASentence: string;
      APosition: integer; AEntityPtr: Pointer = nil;
      AEntityPosition: integer = 0): PSpellError;

    // Очистить весь список ошибок
    procedure ClearErrors;

    // Получить количество ошибок
    function GetErrorCount: integer;

    // Получить ошибку по индексу
    function GetError(Index: integer): PSpellError;

  end;

implementation

uses
  uzclog;

// Создать менеджер ошибок
constructor TSpellErrorManager.Create;
begin
  inherited Create;
  FErrorList := TSpellErrorList.Create;
  programlog.LogOutFormatStr('TSpellErrorManager.Create: initialized',
    [], LM_Info);
end;

// Уничтожить менеджер ошибок
destructor TSpellErrorManager.Destroy;
begin
  ClearErrors;
  FErrorList.Free;
  programlog.LogOutFormatStr('TSpellErrorManager.Destroy: finalized',
    [], LM_Info);
  inherited Destroy;
end;

// Освободить память для одной ошибки
procedure TSpellErrorManager.FreeError(ErrorPtr: PSpellError);
begin
  if Assigned(ErrorPtr) then begin
    ErrorPtr^.ErrorWord := '';
    ErrorPtr^.Sentence := '';
    Dispose(ErrorPtr);
  end;
end;

// Добавить новую ошибку
function TSpellErrorManager.AddError(const AWord, ASentence: string;
  APosition: integer; AEntityPtr: Pointer; AEntityPosition: integer): PSpellError;
begin
  New(Result);
  Result^.ErrorWord := AWord;
  Result^.Sentence := ASentence;
  Result^.Position := APosition;
  Result^.EntityPtr := AEntityPtr;
  Result^.EntityPosition := AEntityPosition;
  FErrorList.Add(Result);

  programlog.LogOutFormatStr(
    'TSpellErrorManager.AddError: word="%s", position=%d, entityPosition=%d',
    [AWord, APosition, AEntityPosition], LM_Info);
end;

// Очистить список ошибок
procedure TSpellErrorManager.ClearErrors;
var
  i: integer;
begin
  for i := 0 to FErrorList.Count - 1 do
    FreeError(FErrorList[i]);
  FErrorList.Clear;

  programlog.LogOutFormatStr('TSpellErrorManager.ClearErrors: cleared',
    [], LM_Info);
end;

// Получить количество ошибок
function TSpellErrorManager.GetErrorCount: integer;
begin
  Result := FErrorList.Count;
end;

// Получить ошибку по индексу
function TSpellErrorManager.GetError(Index: integer): PSpellError;
begin
  Result := nil;
  if (Index >= 0) and (Index < FErrorList.Count) then
    Result := FErrorList[Index];
end;

end.
