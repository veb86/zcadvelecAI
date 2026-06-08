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
  StdCtrls, ExtCtrls, laz.VirtualTrees,
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
    ErrorsTree: TLazVirtualStringTree;
    MainPanel: TPanel;
    MainToolBar: TToolBar;
    RefreshAction: TAction;
    RefreshButton: TToolButton;
    SentenceLabel: TLabel;
    SuggestionsTree: TLazVirtualStringTree;

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

    // Очистить результаты и показать сообщение
    procedure ResetResultsWithMessage(const MessageText: string);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Проверить текст на ошибки
    procedure CheckText(const AText: string);

    // Проверить текстовые примитивы текущего чертежа
    procedure CheckCurrentDrawing;
  end;

var
  SpellCheckerForm: TSpellCheckerForm;

implementation

uses
  gzctnrVectorTypes,
  uzcdrawings, uzeconsts, uzeentity, uzeenttext;

{$R *.lfm}

const
  // Разделитель текста между разными примитивами чертежа
  DRAWING_TEXT_SEPARATOR = #10;
  // Индекс колонки с текстом ошибки
  COL_ERROR_WORD = 0;
  // Индекс колонки с количеством вхождений
  COL_ERROR_COUNT = 1;
  // Индекс колонки с вариантом исправления
  COL_SUGGESTION = 0;
  // Сообщение по умолчанию для метки предложения
  STR_SELECT_ERROR = 'Выберите слово из списка ошибок';
  // Сообщение при отсутствии активного чертежа
  STR_NO_DRAWING = 'Нет активного чертежа';
  // Сообщение при отсутствии текстовых примитивов
  STR_NO_TEXT_ENTITIES = 'В чертеже нет примитивов TEXT/MTEXT';
  // Сообщение при отсутствии текста в найденных примитивах
  STR_NO_TEXT_CONTENT = 'В текстовых примитивах нет текста';

function IsSpellTextEntity(EntityPtr: PGDBObjEntity): boolean;
begin
  Result := False;

  if not Assigned(EntityPtr) then
    Exit;

  case EntityPtr^.GetObjType of
    GDBTextID, GDBMTextID:
      Result := True;
  end;
end;

function GetSpellTextContent(EntityPtr: PGDBObjEntity): string;
begin
  Result := '';

  if not IsSpellTextEntity(EntityPtr) then
    Exit;

  Result := string(PGDBObjText(EntityPtr)^.Content);
  if Result = '' then begin
    Result := string(PGDBObjText(EntityPtr)^.Template);
    programlog.LogOutFormatStr(
      'TSpellCheckerForm.GetSpellTextContent: content empty, template length=%d',
      [Length(Result)], LM_Info);
  end;
end;

procedure AppendDrawingText(var DrawingText: string; const EntityText: string);
begin
  if EntityText = '' then
    Exit;

  if DrawingText <> '' then
    DrawingText := DrawingText + DRAWING_TEXT_SEPARATOR;

  DrawingText := DrawingText + EntityText;
end;

function LoadCurrentDrawingText(out TextEntityCount, SkippedTextCount,
  EntityCount: integer): string;
var
  EntityPtr: PGDBObjEntity;
  Iterator: itrec;
  EntityText: string;
begin
  Result := '';
  TextEntityCount := 0;
  SkippedTextCount := 0;
  EntityCount := 0;

  EntityPtr := drawings.GetCurrentDWG^.GetCurrentROOT^.ObjArray.beginiterate(
    Iterator);
  if EntityPtr <> nil then
    repeat
      Inc(EntityCount);
      if IsSpellTextEntity(EntityPtr) then begin
        Inc(TextEntityCount);
        EntityText := Trim(GetSpellTextContent(EntityPtr));

        if EntityText = '' then
          Inc(SkippedTextCount)
        else
          AppendDrawingText(Result, EntityText);

        programlog.LogOutFormatStr(
          'TSpellCheckerForm.LoadCurrentDrawingText: text entity type=%d, ' +
          'text length=%d',
          [EntityPtr^.GetObjType, Length(EntityText)], LM_Info);
      end;

      EntityPtr := drawings.GetCurrentDWG^.GetCurrentROOT^.ObjArray.iterate(
        Iterator);
    until EntityPtr = nil;
end;

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

  programlog.LogOutFormatStr('TSpellCheckerForm.CheckText: text length=%d',
    [Length(AText)], LM_Info);

  // Найти все ошибки
  errorCount := FindAllErrors(FCurrentText, FErrorManager);

  // Обновить отображение
  UpdateErrorsTree;
  ClearSuggestionsTree;
  SentenceLabel.Caption := STR_SELECT_ERROR;

  programlog.LogOutFormatStr('TSpellCheckerForm.CheckText: found %d errors',
    [errorCount], LM_Info);
end;

// Проверить текстовые примитивы текущего чертежа
procedure TSpellCheckerForm.CheckCurrentDrawing;
var
  DrawingText: string;
  TextEntityCount: integer;
  SkippedTextCount: integer;
  EntityCount: integer;
begin
  programlog.LogOutFormatStr('TSpellCheckerForm.CheckCurrentDrawing: start',
    [], LM_Info);

  if drawings.GetCurrentDWG = nil then begin
    ResetResultsWithMessage(STR_NO_DRAWING);
    programlog.LogOutFormatStr(
      'TSpellCheckerForm.CheckCurrentDrawing: no current drawing', [],
      LM_Info);
    Exit;
  end;

  if drawings.GetCurrentDWG^.GetCurrentROOT = nil then begin
    ResetResultsWithMessage(STR_NO_DRAWING);
    programlog.LogOutFormatStr(
      'TSpellCheckerForm.CheckCurrentDrawing: no current root', [], LM_Info);
    Exit;
  end;

  DrawingText := LoadCurrentDrawingText(TextEntityCount, SkippedTextCount,
    EntityCount);

  programlog.LogOutFormatStr(
    'TSpellCheckerForm.CheckCurrentDrawing: scanned entities=%d, ' +
    'text entities=%d, empty text=%d, text length=%d',
    [EntityCount, TextEntityCount, SkippedTextCount, Length(DrawingText)],
    LM_Info);

  if TextEntityCount = 0 then begin
    ResetResultsWithMessage(STR_NO_TEXT_ENTITIES);
    Exit;
  end;

  if DrawingText = '' then begin
    ResetResultsWithMessage(STR_NO_TEXT_CONTENT);
    Exit;
  end;

  CheckText(DrawingText);
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
  suggestions: TStringList;
begin
  if not Assigned(ErrorPtr) then begin
    ClearSuggestionsTree;
    Exit;
  end;

  // Получить варианты исправления
  suggestions := GetSuggestions(ErrorPtr^.ErrorWord);
  FCurrentSuggestions.Free;
  FCurrentSuggestions := suggestions;

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

// Очистить результаты и показать сообщение
procedure TSpellCheckerForm.ResetResultsWithMessage(const MessageText: string);
begin
  FCurrentText := '';
  FErrorManager.ClearErrors;
  UpdateErrorsTree;
  ClearSuggestionsTree;
  SentenceLabel.Caption := MessageText;

  programlog.LogOutFormatStr(
    'TSpellCheckerForm.ResetResultsWithMessage: "%s"', [MessageText],
    LM_Info);
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
    SentenceLabel.Caption := STR_SELECT_ERROR;
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
  programlog.LogOutFormatStr(
    'TSpellCheckerForm.RefreshActionExecute: refresh requested', [],
    LM_Info);
  CheckCurrentDrawing;
end;

end.
