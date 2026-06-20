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
{$mode objfpc}{$H+}

{** Модуль регистрационной системы команд редактора электронных таблиц.

    Позволяет добавлять новые кнопки и команды редактора отдельными файлами,
    не изменяя модуль формы uzvspreadsheet_gui. Каждая команда регистрируется
    вызовом RegisterSpreadsheetCommand (обычно в секции initialization своего
    модуля). Форма при создании строит панель инструментов по реестру команд
    с помощью TSpreadsheetCommandBar. }
unit uzvspreadsheet_cmdregistry;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  ActnList,
  ComCtrls,
  fpspreadsheetctrls,
  fpspreadsheetgrid;

type
  { Контекст выполнения команды редактора электронных таблиц.
    Передаётся обработчикам команд и содержит ссылки на основные компоненты
    редактора, необходимые для работы. }
  TSpreadsheetCommandContext = record
    WorkbookSource: TsWorkbookSource;  // Источник данных книги
    WorksheetGrid: TsWorksheetGrid;    // Компонент отображения таблицы
  end;

  { Стиль кнопки команды на панели инструментов }
  TSpreadsheetButtonStyle = (
    sbsButton,  // Обычная кнопка
    sbsCheck    // Кнопка-переключатель (нажата/отжата)
  );

  { Обработчик выполнения команды }
  TSpreadsheetCommandProc = procedure(const Context: TSpreadsheetCommandContext);

  { Обработчик обновления состояния команды (доступность, нажатость).
    Вызывается через OnUpdate действия, позволяет включать/выключать кнопку. }
  TSpreadsheetCommandUpdateProc = procedure(
    const Context: TSpreadsheetCommandContext; anAction: TAction);

  { Описание зарегистрированной команды редактора электронных таблиц }
  TSpreadsheetCommandInfo = class
  public
    Name: string;                          // Уникальный идентификатор команды
    Caption: string;                       // Подпись действия
    Hint: string;                          // Всплывающая подсказка
    ImageName: string;                     // Имя иконки в ImagesManager
    ShortCut: TShortCut;                   // Горячая клавиша (0 - нет)
    Style: TSpreadsheetButtonStyle;        // Стиль кнопки
    InitialDown: Boolean;                  // Начальное состояние переключателя
    SortOrder: Integer;                    // Порядок размещения на тулбаре
    GroupIndex: Integer;                   // Группа (между группами - разделитель)
    ExecuteProc: TSpreadsheetCommandProc;  // Обработчик выполнения
    UpdateProc: TSpreadsheetCommandUpdateProc; // Обработчик обновления состояния
  end;

{ Регистрирует команду редактора электронных таблиц в глобальном реестре.
  Вызывается модулями команд (обычно в секции initialization).
  @param(aName Уникальный идентификатор команды)
  @param(aCaption Подпись действия)
  @param(aHint Всплывающая подсказка)
  @param(aImageName Имя иконки в ImagesManager, '' - без иконки)
  @param(aExecuteProc Обработчик выполнения команды)
  @param(aSortOrder Порядок размещения кнопки на панели инструментов)
  @param(aGroupIndex Номер группы; смена группы добавляет разделитель)
  @param(aStyle Стиль кнопки)
  @param(aUpdateProc Обработчик обновления состояния, nil - не нужен)
  @param(aShortCut Горячая клавиша, 0 - нет)
  @param(aInitialDown Начальное состояние кнопки-переключателя) }
procedure RegisterSpreadsheetCommand(
  const aName: string;
  const aCaption: string;
  const aHint: string;
  const aImageName: string;
  aExecuteProc: TSpreadsheetCommandProc;
  aSortOrder: Integer;
  aGroupIndex: Integer;
  aStyle: TSpreadsheetButtonStyle = sbsButton;
  aUpdateProc: TSpreadsheetCommandUpdateProc = nil;
  aShortCut: TShortCut = 0;
  aInitialDown: Boolean = False);

{ Возвращает количество зарегистрированных команд }
function SpreadsheetCommandCount: Integer;

{ Возвращает зарегистрированную команду по индексу (без сортировки) }
function GetSpreadsheetCommand(aIndex: Integer): TSpreadsheetCommandInfo;

{ Возвращает список зарегистрированных команд, отсортированный по SortOrder.
  Список владеет только ссылками; объекты команд освобождать не нужно.
  Полученный список необходимо освободить вызывающей стороне. }
function GetSortedSpreadsheetCommands: TFPList;

type
  { Действие команды редактора электронных таблиц.
    Хранит ссылку на описание команды, чтобы единый обработчик мог
    вызвать нужный обработчик команды. }
  TSpreadsheetCommandAction = class(TAction)
  public
    CommandInfo: TSpreadsheetCommandInfo;
  end;

  { Построитель панели инструментов редактора по реестру команд.
    Создаёт действия и кнопки для всех зарегистрированных команд и
    направляет их выполнение в обработчики команд. }
  TSpreadsheetCommandBar = class
  private
    FActionList: TActionList;
    FToolBar: TToolBar;
    FContext: TSpreadsheetCommandContext;
    procedure OnActionExecute(Sender: TObject);
    procedure OnActionUpdate(Sender: TObject);
  public
    constructor Create(aActionList: TActionList; aToolBar: TToolBar);
    { Строит действия и кнопки на панели инструментов по реестру команд }
    procedure Build(const aContext: TSpreadsheetCommandContext);
  end;

implementation

uses
  Controls,
  uzcimagesmanager,
  uzclog;

var
  // Глобальный реестр зарегистрированных команд редактора таблиц
  SpreadsheetCommands: TFPList;

{ Сравнение команд по порядку размещения для сортировки списка }
function CompareCommandsBySortOrder(Item1, Item2: Pointer): Integer;
begin
  Result := TSpreadsheetCommandInfo(Item1).SortOrder -
            TSpreadsheetCommandInfo(Item2).SortOrder;
end;

procedure RegisterSpreadsheetCommand(
  const aName: string;
  const aCaption: string;
  const aHint: string;
  const aImageName: string;
  aExecuteProc: TSpreadsheetCommandProc;
  aSortOrder: Integer;
  aGroupIndex: Integer;
  aStyle: TSpreadsheetButtonStyle = sbsButton;
  aUpdateProc: TSpreadsheetCommandUpdateProc = nil;
  aShortCut: TShortCut = 0;
  aInitialDown: Boolean = False);
var
  Info: TSpreadsheetCommandInfo;
begin
  Info := TSpreadsheetCommandInfo.Create;
  Info.Name := aName;
  Info.Caption := aCaption;
  Info.Hint := aHint;
  Info.ImageName := aImageName;
  Info.ShortCut := aShortCut;
  Info.Style := aStyle;
  Info.InitialDown := aInitialDown;
  Info.SortOrder := aSortOrder;
  Info.GroupIndex := aGroupIndex;
  Info.ExecuteProc := aExecuteProc;
  Info.UpdateProc := aUpdateProc;
  SpreadsheetCommands.Add(Info);
end;

function SpreadsheetCommandCount: Integer;
begin
  Result := SpreadsheetCommands.Count;
end;

function GetSpreadsheetCommand(aIndex: Integer): TSpreadsheetCommandInfo;
begin
  Result := TSpreadsheetCommandInfo(SpreadsheetCommands.Items[aIndex]);
end;

function GetSortedSpreadsheetCommands: TFPList;
begin
  Result := TFPList.Create;
  Result.Assign(SpreadsheetCommands);
  Result.Sort(@CompareCommandsBySortOrder);
end;

{ TSpreadsheetCommandBar }

constructor TSpreadsheetCommandBar.Create(aActionList: TActionList;
  aToolBar: TToolBar);
begin
  inherited Create;
  FActionList := aActionList;
  FToolBar := aToolBar;
end;

{ Единый обработчик выполнения всех команд: вызывает обработчик команды,
  затем обновляет отображение таблицы. }
procedure TSpreadsheetCommandBar.OnActionExecute(Sender: TObject);
var
  Act: TSpreadsheetCommandAction;
begin
  if not (Sender is TSpreadsheetCommandAction) then
    Exit;

  Act := TSpreadsheetCommandAction(Sender);
  if Act.CommandInfo = nil then
    Exit;

  if Assigned(Act.CommandInfo.ExecuteProc) then
    Act.CommandInfo.ExecuteProc(FContext);

  // Принудительное обновление отображения таблицы после выполнения команды
  if FContext.WorksheetGrid <> nil then
    FContext.WorksheetGrid.Invalidate;
end;

{ Единый обработчик обновления состояния команд (доступность/нажатость) }
procedure TSpreadsheetCommandBar.OnActionUpdate(Sender: TObject);
var
  Act: TSpreadsheetCommandAction;
begin
  if not (Sender is TSpreadsheetCommandAction) then
    Exit;

  Act := TSpreadsheetCommandAction(Sender);
  if (Act.CommandInfo <> nil) and Assigned(Act.CommandInfo.UpdateProc) then
    Act.CommandInfo.UpdateProc(FContext, Act);
end;

procedure TSpreadsheetCommandBar.Build(
  const aContext: TSpreadsheetCommandContext);
var
  Commands: TFPList;
  Info: TSpreadsheetCommandInfo;
  Action: TSpreadsheetCommandAction;
  Button, Separator: TToolButton;
  i: Integer;
  HasPrev: Boolean;
  PrevGroup: Integer;
  BuiltCount: Integer;
begin
  FContext := aContext;

  Commands := GetSortedSpreadsheetCommands;
  try
    BuiltCount := Commands.Count;
    HasPrev := False;
    PrevGroup := 0;

    for i := 0 to Commands.Count - 1 do
    begin
      Info := TSpreadsheetCommandInfo(Commands.Items[i]);

      // Между разными группами команд добавляем разделитель
      if HasPrev and (Info.GroupIndex <> PrevGroup) then
      begin
        Separator := TToolButton.Create(FToolBar);
        Separator.Parent := FToolBar;
        Separator.Style := tbsSeparator;
        Separator.Width := 10;
      end;

      // Создаём действие команды
      Action := TSpreadsheetCommandAction.Create(FActionList);
      Action.ActionList := FActionList;
      Action.CommandInfo := Info;
      Action.Caption := Info.Caption;
      Action.Hint := Info.Hint;
      if Info.ImageName <> '' then
        Action.ImageIndex := ImagesManager.GetImageIndex(Info.ImageName);
      if Info.ShortCut <> 0 then
        Action.ShortCut := Info.ShortCut;
      Action.OnExecute := @OnActionExecute;
      if Assigned(Info.UpdateProc) then
        Action.OnUpdate := @OnActionUpdate;

      // Для переключателей задаём начальное состояние. Если обработчик
      // обновления не задан, включаем автопереключение, иначе состоянием
      // управляет обработчик обновления (UpdateProc).
      if Info.Style = sbsCheck then
      begin
        Action.Checked := Info.InitialDown;
        if not Assigned(Info.UpdateProc) then
          Action.AutoCheck := True;
      end;

      // Создаём кнопку и привязываем к ней действие
      Button := TToolButton.Create(FToolBar);
      Button.Parent := FToolBar;
      Button.ShowHint := True;
      if Info.Style = sbsCheck then
        Button.Style := tbsCheck;
      Button.Action := Action;

      HasPrev := True;
      PrevGroup := Info.GroupIndex;
    end;
  finally
    Commands.Free;
  end;

  programlog.LogOutFormatStr(
    'Панель инструментов редактора таблиц построена: %d команд',
    [BuiltCount], LM_Info);
end;

initialization
  SpreadsheetCommands := TFPList.Create;

finalization
  if SpreadsheetCommands <> nil then
  begin
    while SpreadsheetCommands.Count > 0 do
    begin
      TSpreadsheetCommandInfo(SpreadsheetCommands.Items[0]).Free;
      SpreadsheetCommands.Delete(0);
    end;
    FreeAndNil(SpreadsheetCommands);
  end;

end.
