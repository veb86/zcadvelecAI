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
    ApplySuggestionAction: TAction;
    ApplySuggestionButton: TToolButton;
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
    procedure ApplySuggestionActionExecute(Sender: TObject);
    procedure MainPanelResize(Sender: TObject);
    procedure RefreshActionExecute(Sender: TObject);
    procedure SuggestionsTreeDblClick(Sender: TObject);
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

    // Получить выбранную ошибку
    function GetFocusedError: PSpellError;

    // Получить выбранный вариант исправления
    function GetFocusedSuggestion: string;

    // Сформировать сообщение с ошибкой и контекстом
    function FormatSentenceMessage(ErrorPtr: PSpellError): string;

    // Применить исправление к тексту или примитиву
    function ReplaceCurrentErrorWithSuggestion(ErrorPtr: PSpellError;
      const SuggestionText: string): boolean;

    // Применить выбранный вариант исправления
    procedure ApplySelectedSuggestion;

    // Обновить высоту списка ошибок в пропорции 70/30
    procedure UpdateMainPanelLayout;

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
  uzcdrawing, uzcdrawings, uzcutils, uzeconsts, uzeentity, uzeenttext,
  uzetypes, uzgldrawcontext, zUndoCmdSaveEntityState;

{$R *.lfm}

const
  // Разделитель между текстами примитивов чертежа
  DRAWING_TEXT_SEPARATOR = #10;
  // Высота списка ошибок в процентах от рабочей области
  ERROR_TREE_HEIGHT_PERCENT = 70;
  // Минимальная высота дерева при изменении размера
  MIN_TREE_HEIGHT = 80;
  // Индекс колонки с текстом ошибки
  COL_ERROR_WORD = 0;
  // Индекс колонки с вариантом исправления
  COL_SUGGESTION = 0;
  // Имя команды для undo
  SPELLCHECKER_UNDO_COMMAND = 'SpellCheckerCorrection';
  // Сообщение по умолчанию для метки предложения
  STR_SELECT_ERROR = 'Выберите слово из списка ошибок';
  // Сообщение при отсутствии активного чертежа
  STR_NO_DRAWING = 'Нет активного чертежа';
  // Сообщение при отсутствии текстовых примитивов
  STR_NO_TEXT_ENTITIES = 'В чертеже нет примитивов TEXT/MTEXT';
  // Сообщение при пустом тексте в найденных примитивах
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

procedure SetSpellTextContent(EntityPtr: PGDBObjEntity;
  const EntityText: string);
begin
  if not IsSpellTextEntity(EntityPtr) then
    Exit;

  PGDBObjText(EntityPtr)^.Template := TDXFEntsInternalStringType(EntityText);
end;

function ReplaceTextAtPosition(const AText: string; APosition: integer;
  const AOldWord, ANewWord: string; out ANewText: string): boolean;
begin
  Result := False;
  ANewText := AText;

  if (APosition < 1) or (APosition > Length(AText)) then
    Exit;

  if Copy(AText, APosition, Length(AOldWord)) <> AOldWord then
    Exit;

  ANewText := Copy(AText, 1, APosition - 1) + ANewWord +
    Copy(AText, APosition + Length(AOldWord), MaxInt);
  Result := True;
end;

procedure AppendDrawingText(var DrawingText: string; const EntityText: string);
begin
  if EntityText = '' then
    Exit;

  if DrawingText <> '' then
    DrawingText := DrawingText + DRAWING_TEXT_SEPARATOR;

  DrawingText := DrawingText + EntityText;
end;

function LoadCurrentDrawingText(ErrorManager: TSpellErrorManager;
  out TextEntityCount, SkippedTextCount, EntityCount, ErrorCount: integer): string;
var
  EntityPtr: PGDBObjEntity;
  Iterator: itrec;
  EntityText: string;
  BasePosition: integer;
begin
  Result := '';
  TextEntityCount := 0;
  SkippedTextCount := 0;
  EntityCount := 0;
  ErrorCount := 0;

  EntityPtr := drawings.GetCurrentDWG^.GetCurrentROOT^.ObjArray.beginiterate(
    Iterator);
  if EntityPtr <> nil then
    repeat
      Inc(EntityCount);
      if IsSpellTextEntity(EntityPtr) then begin
        Inc(TextEntityCount);
        EntityText := GetSpellTextContent(EntityPtr);

        if Trim(EntityText) = '' then
          Inc(SkippedTextCount)
        else begin
          BasePosition := Length(Result);
          if Result <> '' then
            BasePosition := BasePosition + Length(DRAWING_TEXT_SEPARATOR);
          AppendDrawingText(Result, EntityText);
          ErrorCount := ErrorCount + AppendErrorsFromText(EntityText,
            ErrorManager, BasePosition, EntityPtr);
        end;

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
  UpdateMainPanelLayout;

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
  ErrorCount: integer;
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

  FErrorManager.ClearErrors;
  DrawingText := LoadCurrentDrawingText(FErrorManager, TextEntityCount,
    SkippedTextCount, EntityCount, ErrorCount);

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

  FCurrentText := DrawingText;
  UpdateErrorsTree;
  ClearSuggestionsTree;
  SentenceLabel.Caption := STR_SELECT_ERROR;

  programlog.LogOutFormatStr(
    'TSpellCheckerForm.CheckCurrentDrawing: found %d errors',
    [ErrorCount], LM_Info);
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

// Получить выбранную ошибку
function TSpellCheckerForm.GetFocusedError: PSpellError;
var
  node: PVirtualNode;
  nodeData: PErrorNodeData;
begin
  Result := nil;
  node := ErrorsTree.FocusedNode;

  if not Assigned(node) then
    Exit;

  nodeData := ErrorsTree.GetNodeData(node);
  if not Assigned(nodeData) then
    Exit;

  Result := FErrorManager.GetError(nodeData^.ErrorIndex);
end;

// Получить выбранный вариант исправления
function TSpellCheckerForm.GetFocusedSuggestion: string;
var
  node: PVirtualNode;
  nodeData: PSuggestionNodeData;
begin
  Result := '';
  node := SuggestionsTree.FocusedNode;

  if not Assigned(node) then
    Exit;

  nodeData := SuggestionsTree.GetNodeData(node);
  if Assigned(nodeData) then
    Result := nodeData^.SuggestionText;
end;

// Сформировать сообщение с ошибкой и контекстом
function TSpellCheckerForm.FormatSentenceMessage(ErrorPtr: PSpellError): string;
begin
  Result := STR_SELECT_ERROR;
  if Assigned(ErrorPtr) then
    Result := Format('Ошибка: "%s". Контекст: %s',
      [ErrorPtr^.ErrorWord, ErrorPtr^.Sentence]);
end;

// Обновить высоту списка ошибок в пропорции 70/30
procedure TSpellCheckerForm.UpdateMainPanelLayout;
var
  newHeight: integer;
begin
  if MainPanel.ClientHeight <= 0 then
    Exit;

  newHeight := (MainPanel.ClientHeight * ERROR_TREE_HEIGHT_PERCENT) div 100;
  if newHeight < MIN_TREE_HEIGHT then
    newHeight := MIN_TREE_HEIGHT;
  if newHeight > MainPanel.ClientHeight - MIN_TREE_HEIGHT then
    newHeight := MainPanel.ClientHeight - MIN_TREE_HEIGHT;

  if newHeight > 0 then
    ErrorsTree.Height := newHeight;
end;

// Применить исправление к текстовому примитиву
function ApplySuggestionToEntity(EntityPtr: PGDBObjEntity;
  ErrorPtr: PSpellError; const SuggestionText: string): boolean;
var
  drawing: PTZCADDrawing;
  drawContext: TDrawContext;
  entityText: string;
  newEntityText: string;
begin
  Result := False;

  if not Assigned(EntityPtr) or not Assigned(ErrorPtr) then
    Exit;
  if drawings.GetCurrentDWG = nil then
    Exit;

  entityText := GetSpellTextContent(EntityPtr);
  if not ReplaceTextAtPosition(entityText, ErrorPtr^.EntityPosition,
    ErrorPtr^.ErrorWord, SuggestionText, newEntityText) then
    Exit;

  drawing := PTZCADDrawing(drawings.GetCurrentDWG);
  zcStartUndoCommand(drawing^, SPELLCHECKER_UNDO_COMMAND, False);
  try
    TUndoCmdSaveEntityState.CreateAndPush(EntityPtr, drawing^.UndoStack);
    SetSpellTextContent(EntityPtr, newEntityText);

    drawContext := drawing^.CreateDrawingRC;
    EntityPtr^.FormatEntity(drawing^, drawContext);
    EntityPtr^.YouChanged(drawing^);
    zcEndUndoCommand(drawing^);
  except
    zcEndUndoCommand(drawing^);
    raise;
  end;

  zcRedrawCurrentDrawing;
  Result := True;
end;

// Применить исправление к тексту или примитиву
function TSpellCheckerForm.ReplaceCurrentErrorWithSuggestion(
  ErrorPtr: PSpellError; const SuggestionText: string): boolean;
var
  newText: string;
begin
  Result := False;

  if not Assigned(ErrorPtr) or (SuggestionText = '') then
    Exit;

  if Assigned(ErrorPtr^.EntityPtr) and
     ApplySuggestionToEntity(PGDBObjEntity(ErrorPtr^.EntityPtr),
       ErrorPtr, SuggestionText) then begin
    CheckCurrentDrawing;
    Result := True;
    Exit;
  end;

  if ReplaceTextAtPosition(FCurrentText, ErrorPtr^.Position,
    ErrorPtr^.ErrorWord, SuggestionText, newText) then begin
    CheckText(newText);
    Result := True;
  end;
end;

// Применить выбранный вариант исправления
procedure TSpellCheckerForm.ApplySelectedSuggestion;
var
  errorPtr: PSpellError;
  suggestionText: string;
begin
  errorPtr := GetFocusedError;
  suggestionText := GetFocusedSuggestion;

  if ReplaceCurrentErrorWithSuggestion(errorPtr, suggestionText) then
    programlog.LogOutFormatStr(
      'TSpellCheckerForm.ApplySelectedSuggestion: applied "%s"',
      [suggestionText], LM_Info)
  else
    programlog.LogOutFormatStr(
      'TSpellCheckerForm.ApplySelectedSuggestion: nothing applied',
      [], LM_Info);
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
  SentenceLabel.Caption := FormatSentenceMessage(errorPtr);

  programlog.LogOutFormatStr(
    'TSpellCheckerForm.ErrorsTreeFocusChanged: selected "%s"',
    [errorPtr^.ErrorWord], LM_Info);
end;

// Обработчик действия "Применить"
procedure TSpellCheckerForm.ApplySuggestionActionExecute(Sender: TObject);
begin
  ApplySelectedSuggestion;
end;

// Обработчик изменения размера рабочей области
procedure TSpellCheckerForm.MainPanelResize(Sender: TObject);
begin
  UpdateMainPanelLayout;
end;

// Обработчик действия "Обновить"
procedure TSpellCheckerForm.RefreshActionExecute(Sender: TObject);
begin
  programlog.LogOutFormatStr(
    'TSpellCheckerForm.RefreshActionExecute: refresh requested', [],
    LM_Info);
  CheckCurrentDrawing;
end;

// Обработчик двойного щелчка по варианту исправления
procedure TSpellCheckerForm.SuggestionsTreeDblClick(Sender: TObject);
begin
  ApplySelectedSuggestion;
end;

end.
