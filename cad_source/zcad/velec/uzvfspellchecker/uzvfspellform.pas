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

unit uzvfspellform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  StdCtrls, ExtCtrls, VirtualTrees,
  uzclog,
  uzvfspelldata, uzvfspelllogic;

type
  // Тип узла дерева ошибок
  PErrorNodeData = ^TErrorNodeData;
  TErrorNodeData = record
    ErrorIndex: integer;  // Индекс ошибки в списке
  end;

  // Тип узла дерева вариантов
  PSuggestionNodeData = ^TSuggestionNodeData;
  TSuggestionNodeData = record
    SuggestionText: string;  // Текст варианта исправления
  end;

  { TSpellCheckerForm }
  TSpellCheckerForm = class(TForm)
    ActionList: TActionList;
    ErrorsTree: TVirtualStringTree;
    MainPanel: TPanel;
    MainToolBar: TToolBar;
    RefreshAction: TAction;
    RefreshButton: TToolButton;
    SentenceLabel: TLabel;
    SuggestionsTree: TVirtualStringTree;

    procedure ErrorsTreeFocusChanged(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex);
    procedure ErrorsTreeGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    procedure RefreshActionExecute(Sender: TObject);
    procedure SuggestionsTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);

  private
    FErrorManager: TSpellErrorManager;
    FCurrentText: string;
    FCurrentSuggestions: TStringList;

    // Обновить дерево ошибок
    procedure UpdateErrorsTree;

    // Обновить дерево вариантов для выбранной ошибки
    procedure UpdateSuggestionsTree(ErrorPtr: PSpellError);

    // Очистить дерево вариантов
    procedure ClearSuggestionsTree;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Проверить текст на ошибки
    procedure CheckText(const AText: string);
  end;

var
  SpellCheckerForm: TSpellCheckerForm;

implementation

{$R *.lfm}

const
  // Индекс колонки с текстом ошибки
  COL_ERROR_WORD = 0;
  // Индекс колонки с количеством вхождений
  COL_ERROR_COUNT = 1;
  // Индекс колонки с вариантом исправления
  COL_SUGGESTION = 0;

{ TSpellCheckerForm }

// Создать форму
constructor TSpellCheckerForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FErrorManager := TSpellErrorManager.Create;
  FCurrentText := '';
  FCurrentSuggestions := TStringList.Create;

  // Настроить дерево ошибок
  ErrorsTree.NodeDataSize := SizeOf(TErrorNodeData);

  // Настроить дерево вариантов
  SuggestionsTree.NodeDataSize := SizeOf(TSuggestionNodeData);

  programlog.LogOutFormatStr('TSpellCheckerForm.Create: initialized',
    [], LM_Info);
end;

// Уничтожить форму
destructor TSpellCheckerForm.Destroy;
begin
  FCurrentSuggestions.Free;
  FErrorManager.Free;

  programlog.LogOutFormatStr('TSpellCheckerForm.Destroy: finalized',
    [], LM_Info);

  inherited Destroy;
end;

// Проверить текст на ошибки
procedure TSpellCheckerForm.CheckText(const AText: string);
var
  errorCount: integer;
begin
  FCurrentText := AText;

  // Найти все ошибки
  errorCount := FindAllErrors(FCurrentText, FErrorManager);

  // Обновить отображение
  UpdateErrorsTree;
  ClearSuggestionsTree;
  SentenceLabel.Caption := 'Выберите слово из списка ошибок';

  programlog.LogOutFormatStr('TSpellCheckerForm.CheckText: found %d errors',
    [errorCount], LM_Info);
end;

// Обновить дерево ошибок
procedure TSpellCheckerForm.UpdateErrorsTree;
var
  i: integer;
  node: PVirtualNode;
  nodeData: PErrorNodeData;
begin
  ErrorsTree.BeginUpdate;
  try
    ErrorsTree.Clear;

    for i := 0 to FErrorManager.GetErrorCount - 1 do begin
      node := ErrorsTree.AddChild(nil);
      nodeData := ErrorsTree.GetNodeData(node);
      nodeData^.ErrorIndex := i;
    end;

  finally
    ErrorsTree.EndUpdate;
  end;

  programlog.LogOutFormatStr(
    'TSpellCheckerForm.UpdateErrorsTree: added %d nodes',
    [FErrorManager.GetErrorCount], LM_Info);
end;

// Обновить дерево вариантов
procedure TSpellCheckerForm.UpdateSuggestionsTree(ErrorPtr: PSpellError);
var
  i: integer;
  node: PVirtualNode;
  nodeData: PSuggestionNodeData;
begin
  if not Assigned(ErrorPtr) then begin
    ClearSuggestionsTree;
    Exit;
  end;

  // Получить варианты исправления
  FCurrentSuggestions.Clear;
  FCurrentSuggestions := GetSuggestions(ErrorPtr^.ErrorWord);

  SuggestionsTree.BeginUpdate;
  try
    SuggestionsTree.Clear;

    for i := 0 to FCurrentSuggestions.Count - 1 do begin
      node := SuggestionsTree.AddChild(nil);
      nodeData := SuggestionsTree.GetNodeData(node);
      nodeData^.SuggestionText := FCurrentSuggestions[i];
    end;

  finally
    SuggestionsTree.EndUpdate;
  end;

  programlog.LogOutFormatStr(
    'TSpellCheckerForm.UpdateSuggestionsTree: added %d suggestions',
    [FCurrentSuggestions.Count], LM_Info);
end;

// Очистить дерево вариантов
procedure TSpellCheckerForm.ClearSuggestionsTree;
begin
  SuggestionsTree.Clear;
  FCurrentSuggestions.Clear;
end;

// Получить текст для ячейки дерева ошибок
procedure TSpellCheckerForm.ErrorsTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  nodeData: PErrorNodeData;
  errorPtr: PSpellError;
begin
  CellText := '';
  nodeData := Sender.GetNodeData(Node);

  if not Assigned(nodeData) then
    Exit;

  errorPtr := FErrorManager.GetError(nodeData^.ErrorIndex);

  if not Assigned(errorPtr) then
    Exit;

  case Column of
    COL_ERROR_WORD:
      CellText := errorPtr^.ErrorWord;
    COL_ERROR_COUNT:
      CellText := IntToStr(errorPtr^.OccurrenceCount);
  end;
end;

// Получить текст для ячейки дерева вариантов
procedure TSpellCheckerForm.SuggestionsTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
  nodeData: PSuggestionNodeData;
begin
  CellText := '';
  nodeData := Sender.GetNodeData(Node);

  if not Assigned(nodeData) then
    Exit;

  if Column = COL_SUGGESTION then
    CellText := nodeData^.SuggestionText;
end;

// Обработчик изменения фокуса в дереве ошибок
procedure TSpellCheckerForm.ErrorsTreeFocusChanged(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex);
var
  nodeData: PErrorNodeData;
  errorPtr: PSpellError;
begin
  if not Assigned(Node) then begin
    ClearSuggestionsTree;
    SentenceLabel.Caption := 'Выберите слово из списка ошибок';
    Exit;
  end;

  nodeData := Sender.GetNodeData(Node);

  if not Assigned(nodeData) then
    Exit;

  errorPtr := FErrorManager.GetError(nodeData^.ErrorIndex);

  if not Assigned(errorPtr) then
    Exit;

  // Обновить варианты исправления
  UpdateSuggestionsTree(errorPtr);

  // Обновить метку с предложением
  SentenceLabel.Caption := errorPtr^.Sentence;

  programlog.LogOutFormatStr(
    'TSpellCheckerForm.ErrorsTreeFocusChanged: selected "%s"',
    [errorPtr^.ErrorWord], LM_Info);
end;

// Обработчик действия "Обновить"
procedure TSpellCheckerForm.RefreshActionExecute(Sender: TObject);
begin
  if Length(FCurrentText) > 0 then begin
    CheckText(FCurrentText);
    programlog.LogOutFormatStr('TSpellCheckerForm.RefreshActionExecute: OK',
      [], LM_Info);
  end;
end;

end.
