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

// Форма управления пользовательским словарём орфографии (issue #1361):
// показывает список добавленных пользователем слов, позволяет быстро найти
// нужное слово через поле поиска и удалить выбранное слово из словаря.
// Список слов отображается тем же элементом (TLazVirtualStringTree), что и
// список слов с ошибками в форме проверки орфографии (uzvfspellform).

unit uzvfuserdictform;

{$mode objfpc}{$H+}
{$Codepage UTF8}

interface

uses
  Classes, SysUtils, Forms, Controls, StdCtrls, ComCtrls, ExtCtrls, ActnList,
  laz.VirtualTrees,
  uzclog;

type
  // Тип узла дерева слов: индекс слова в отфильтрованном списке
  PUserWordNodeData = ^TUserWordNodeData;
  TUserWordNodeData = record
    WordIndex: integer;
  end;

  { TUserDictForm }
  TUserDictForm = class(TForm)
    ActionList: TActionList;
    DeleteWordAction: TAction;
    DeleteWordButton: TToolButton;
    MainToolBar: TToolBar;
    RefreshAction: TAction;
    RefreshButton: TToolButton;
    SearchEdit: TEdit;
    SearchLabel: TLabel;
    SearchPanel: TPanel;
    StatusLabel: TLabel;
    WordsTree: TLazVirtualStringTree;

    procedure DeleteWordActionExecute(Sender: TObject);
    procedure RefreshActionExecute(Sender: TObject);
    procedure SearchEditChange(Sender: TObject);
    procedure WordsTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);

  private
    // Все слова словаря (в порядке файла)
    FAllWords: TStringList;
    // Слова, отображаемые сейчас (после применения фильтра поиска)
    FFilteredWords: TStringList;

    // Перестроить дерево по текущему отфильтрованному списку
    procedure RebuildWordsTree;

    // Применить фильтр поиска к FAllWords и перестроить дерево
    procedure ApplyFilter;

    // Получить выбранное слово или '' если ничего не выбрано
    function GetFocusedWord: string;

    // Обновить метку с количеством слов
    procedure UpdateStatusLabel;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Загрузить слова из пользовательского словаря и обновить отображение
    procedure ReloadWords;
  end;

var
  UserDictForm: TUserDictForm;

implementation

uses
  LazUTF8,
  uzvfspelluserdict;

{$R *.lfm}

const
  // Индекс колонки со словом
  COL_USER_WORD = 0;
  // Сообщение при отсутствии выбранного слова для удаления
  STR_NO_WORD_SELECTED = 'Выберите слово для удаления из словаря';
  // Шаблон сообщения о количестве показанных слов
  STR_WORDS_COUNT = 'Слов в словаре: %d (показано: %d)';
  // Сообщение при пустом словаре
  STR_EMPTY_DICTIONARY = 'Пользовательский словарь пуст';

{ TUserDictForm }

// Создать форму
constructor TUserDictForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAllWords := TStringList.Create;
  FAllWords.CaseSensitive := True;
  FFilteredWords := TStringList.Create;
  FFilteredWords.CaseSensitive := True;

  // Настроить дерево слов так же, как дерево ошибок в uzvfspellform
  WordsTree.NodeDataSize := SizeOf(TUserWordNodeData);

  programlog.LogOutFormatStr('TUserDictForm.Create: initialized', [], LM_Info);
end;

// Уничтожить форму
destructor TUserDictForm.Destroy;
begin
  FFilteredWords.Free;
  FAllWords.Free;

  programlog.LogOutFormatStr('TUserDictForm.Destroy: finalized', [], LM_Info);

  inherited Destroy;
end;

// Загрузить слова из пользовательского словаря и обновить отображение
procedure TUserDictForm.ReloadWords;
begin
  LoadUserDictionaryWords(FAllWords);
  ApplyFilter;

  programlog.LogOutFormatStr('TUserDictForm.ReloadWords: %d words loaded',
    [FAllWords.Count], LM_Info);
end;

// Применить фильтр поиска: оставить слова, содержащие искомую подстроку
// (без учёта регистра), и перестроить дерево
procedure TUserDictForm.ApplyFilter;
var
  i: integer;
  filter, lowerWord: string;
begin
  filter := UTF8LowerCase(Trim(SearchEdit.Text));

  FFilteredWords.Clear;
  for i := 0 to FAllWords.Count - 1 do begin
    if filter = '' then
      FFilteredWords.Add(FAllWords[i])
    else begin
      // Оба слова приводим к нижнему регистру (UTF-8), а поиск подстроки
      // делаем побайтово (Pos): кодировка UTF-8 самосинхронизируема, поэтому
      // ложных совпадений на границах символов не возникает.
      lowerWord := UTF8LowerCase(FAllWords[i]);
      if Pos(filter, lowerWord) > 0 then
        FFilteredWords.Add(FAllWords[i]);
    end;
  end;

  RebuildWordsTree;
  UpdateStatusLabel;
end;

// Перестроить дерево по текущему отфильтрованному списку
procedure TUserDictForm.RebuildWordsTree;
var
  i: integer;
  node: PVirtualNode;
  nodeData: PUserWordNodeData;
begin
  WordsTree.BeginUpdate;
  try
    WordsTree.Clear;
    for i := 0 to FFilteredWords.Count - 1 do begin
      node := WordsTree.AddChild(nil);
      nodeData := WordsTree.GetNodeData(node);
      nodeData^.WordIndex := i;
    end;
  finally
    WordsTree.EndUpdate;
  end;
end;

// Обновить метку с количеством слов
procedure TUserDictForm.UpdateStatusLabel;
begin
  if FAllWords.Count = 0 then
    StatusLabel.Caption := STR_EMPTY_DICTIONARY
  else
    StatusLabel.Caption := Format(STR_WORDS_COUNT,
      [FAllWords.Count, FFilteredWords.Count]);
end;

// Получить выбранное слово или '' если ничего не выбрано
function TUserDictForm.GetFocusedWord: string;
var
  node: PVirtualNode;
  nodeData: PUserWordNodeData;
begin
  Result := '';
  node := WordsTree.FocusedNode;
  if not Assigned(node) then
    Exit;

  nodeData := WordsTree.GetNodeData(node);
  if not Assigned(nodeData) then
    Exit;

  if (nodeData^.WordIndex >= 0) and
     (nodeData^.WordIndex < FFilteredWords.Count) then
    Result := FFilteredWords[nodeData^.WordIndex];
end;

// Получить текст для ячейки дерева слов
procedure TUserDictForm.WordsTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  nodeData: PUserWordNodeData;
begin
  CellText := '';
  nodeData := Sender.GetNodeData(Node);
  if not Assigned(nodeData) then
    Exit;

  if (Column = COL_USER_WORD) and (nodeData^.WordIndex >= 0) and
     (nodeData^.WordIndex < FFilteredWords.Count) then
    CellText := FFilteredWords[nodeData^.WordIndex];
end;

// Обработчик изменения текста поиска: применить фильтр
procedure TUserDictForm.SearchEditChange(Sender: TObject);
begin
  ApplyFilter;
end;

// Обработчик действия "Обновить": перечитать словарь с диска
procedure TUserDictForm.RefreshActionExecute(Sender: TObject);
begin
  programlog.LogOutFormatStr('TUserDictForm.RefreshActionExecute: refresh',
    [], LM_Info);
  ReloadWords;
end;

// Обработчик действия "Удалить": удалить выбранное слово из словаря
procedure TUserDictForm.DeleteWordActionExecute(Sender: TObject);
var
  wordText: string;
begin
  wordText := GetFocusedWord;

  if wordText = '' then begin
    StatusLabel.Caption := STR_NO_WORD_SELECTED;
    programlog.LogOutFormatStr(
      'TUserDictForm.DeleteWordActionExecute: no word selected', [], LM_Info);
    Exit;
  end;

  if RemoveWordFromUserDictionary(wordText) then begin
    programlog.LogOutFormatStr(
      'TUserDictForm.DeleteWordActionExecute: removed "%s"', [wordText],
      LM_Info);
    // Перечитать словарь, чтобы список и счётчик обновились
    ReloadWords;
  end
  else
    programlog.LogOutFormatStr(
      'TUserDictForm.DeleteWordActionExecute: failed to remove "%s"',
      [wordText], LM_Info);
end;

end.
