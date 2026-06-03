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
{
  Модуль: uzcui_dimstyleedit
  Назначение: диалоговое окно редактирования одного размерного стиля
  DXF, аналог диалога "Изменение размерного стиля" в AutoCAD.
  Форма строится программно, без .lfm.
  Содержит 7 вкладок: Линии, Символы и стрелки, Текст, Размещение,
  Основные единицы, Альт. единицы, Допуски.
  Зависимости: uzestylesdim, uzcdrawings, uzedrawingsimple,
               uzcsysvars, uzcinterface, uzclog, LCL
}
unit uzcui_dimstyleedit;
{$INCLUDE zengineconfig.inc}

{$mode objfpc}{$H+}

interface

uses
  uzclog,
  uzestylesdim,
  uzcdrawings,
  uzedrawingsimple,
  uzcsysvars,
  uzcinterface,
  uzcgui2arrows,
  uzepalette,
  uzestylestexts,
  uzeTypes,
  uzbUnits,
  gzctnrVectorTypes,
  Classes, SysUtils, Forms, Controls, Graphics,
  ExtCtrls, StdCtrls, ComCtrls, Spin, Dialogs;

const
  { Размеры окна редактирования }
  CEditFormWidth  = 566;
  CEditFormHeight = 523;

  { Размеры области предпросмотра }
  CPreviewWidth  = 250;
  CPreviewHeight = 200;

  { Отступы и размеры элементов управления }
  CControlMargin    = 8;
  CLabelWidth       = 140;
  CEditWidth        = 80;
  CComboWidth       = 150;
  CControlHeight    = 24;
  CControlStep      = 28;
  CGroupBoxPadLeft  = 10;
  CGroupBoxPadTop   = 20;

  { Ширина и высота кнопок нижней панели }
  CBottomBtnWidth  = 80;
  CBottomBtnHeight = 28;
  CBottomPanelHeight = 45;

  { Подсказка для неразработанных параметров }
  CHintNotImplemented = 'Данный параметр не разработан';

  { Текст заглушки предпросмотра }
  CPreviewStubText = 'Предпросмотр недоступен';
  CPreviewFontSize = 10;

  { Отображаемые имена вертикального размещения текста }
  CTextVertNames: array[TDimTextVertPosition] of string = (
    'По центру',
    'Над линией',
    'Снаружи',
    'JIS',
    'Под линией'
  );

  { Отображаемые имена форматов единиц }
  CDimUnitNames: array[TDimUnit] of string = (
    'Научные',
    'Десятичные',
    'Инженерные',
    'Архитектурные',
    'Дробные',
    'Системные'
  );

  { Отображаемые имена десятичных разделителей }
  CDimDSepNames: array[TDimDSep] of string = (
    '. (точка)',
    ', (запятая)',
    '  (пробел)'
  );

type
  { TDimStyleEditDialog - диалог редактирования размерного стиля.
    Строится программно, без .lfm-файла.
    Вызывается из менеджера стилей при нажатии "Редактировать". }
  TDimStyleEditDialog = class(TForm)
  private
    { Указатель на редактируемый стиль }
    FDimStyle: PGDBDimStyle;
    { Резервная копия для отмены изменений }
    FBackupLines: TGDBDimLinesProp;
    FBackupArrows: TGDBDimArrowsProp;
    FBackupText: TGDBDimTextProp;
    FBackupPlacing: TGDBDimPlacingProp;
    FBackupUnits: TGDBDimUnitsProp;

    { Основной элемент — блокнот с вкладками }
    FPageControl: TPageControl;

    { Кнопки нижней панели }
    FButtonOK: TButton;
    FButtonCancel: TButton;
    FButtonHelp: TButton;

    { --- Элементы вкладки "Линии" --- }
    { Размерные линии }
    FComboDimLineColor: TComboBox;
    FComboDimLineType: TComboBox;
    FComboDimLineWeight: TComboBox;
    FEditDimDLE: TEdit;
    FEditDimBaseSpacing: TEdit;
    FCheckSuppressDL1: TCheckBox;
    FCheckSuppressDL2: TCheckBox;
    { Выносные линии }
    FComboExtLineColor: TComboBox;
    FComboExtLineType1: TComboBox;
    FComboExtLineType2: TComboBox;
    FComboExtLineWeight: TComboBox;
    FEditExtEXE: TEdit;
    FEditExtEXO: TEdit;
    FCheckSuppressEL1: TCheckBox;
    FCheckSuppressEL2: TCheckBox;
    FCheckFixedLenExt: TCheckBox;
    FEditFixedLen: TEdit;

    { --- Элементы вкладки "Символы и стрелки" --- }
    FComboArrow1: TComboBox;
    FComboArrow2: TComboBox;
    FComboArrowLeader: TComboBox;
    FEditArrowSize: TEdit;
    FRadioCenterNone: TRadioButton;
    FRadioCenterMark: TRadioButton;
    FRadioCenterLine: TRadioButton;
    FEditCenterSize: TEdit;

    { --- Элементы вкладки "Текст" --- }
    FComboTextStyle: TComboBox;
    FComboTextColor: TComboBox;
    FComboFillColor: TComboBox;
    FEditTextHeight: TEdit;
    FEditFractionScale: TEdit;
    FCheckTextFrame: TCheckBox;
    FComboTextVertPos: TComboBox;
    FComboTextHorizPos: TComboBox;
    FComboTextViewDir: TComboBox;
    FEditTextGap: TEdit;
    FRadioTextHoriz: TRadioButton;
    FRadioTextAligned: TRadioButton;
    FRadioTextISO: TRadioButton;

    { --- Элементы вкладки "Размещение" --- }
    FRadioFitOptimal: TRadioButton;
    FRadioFitArrows: TRadioButton;
    FRadioFitText: TRadioButton;
    FRadioFitBoth: TRadioButton;
    FRadioFitAlways: TRadioButton;
    FCheckSuppressArrows: TCheckBox;
    FRadioTMoveMoveLine: TRadioButton;
    FRadioTMoveLeader: TRadioButton;
    FRadioTMoveNoLeader: TRadioButton;
    FCheckAnnotative: TCheckBox;
    FRadioScaleLayout: TRadioButton;
    FRadioScaleGlobal: TRadioButton;
    FEditDimScale: TFloatSpinEdit;
    FCheckManualText: TCheckBox;
    FCheckDimLineBetween: TCheckBox;

    { --- Элементы вкладки "Основные единицы" --- }
    FComboUnitFormat: TComboBox;
    FComboPrecision: TComboBox;
    FComboFractionFmt: TComboBox;
    FComboDecSeparator: TComboBox;
    FEditRounding: TEdit;
    FEditPrefix: TEdit;
    FEditSuffix: TEdit;
    FEditMeasScale: TFloatSpinEdit;
    FCheckScaleLayout: TCheckBox;
    FCheckZeroLeading: TCheckBox;
    FCheckZeroTrailing: TCheckBox;
    FCheckZeroFeet: TCheckBox;
    FCheckZeroInches: TCheckBox;

    { Построение интерфейса }
    procedure BuildControls;
    procedure BuildBottomPanel;
    procedure BuildTabLines(ATab: TTabSheet);
    procedure BuildTabArrows(ATab: TTabSheet);
    procedure BuildTabText(ATab: TTabSheet);
    procedure BuildTabPlacing(ATab: TTabSheet);
    procedure BuildTabUnits(ATab: TTabSheet);
    procedure BuildTabAltUnits(ATab: TTabSheet);
    procedure BuildTabTolerances(ATab: TTabSheet);

    { Загрузка и сохранение данных }
    procedure LoadFromStyle;
    procedure SaveToStyle;

    { Обработчики кнопок }
    procedure OnOKClick(Sender: TObject);
    procedure OnCancelClick(Sender: TObject);

    { Рисует заглушку предпросмотра }
    procedure DrawPreviewStub(
      ACanvas: TCanvas; const ABounds: TRect);
    { Обработчик рисования предпросмотра }
    procedure OnPaintPreview(Sender: TObject);

    { Вспомогательные методы создания элементов управления }
    function CreateLabel(AParent: TWinControl;
      const ACaption: string;
      ALeft, ATop: Integer): TLabel;
    function CreateEdit(AParent: TWinControl;
      ALeft, ATop, AWidth: Integer): TEdit;
    function CreateCombo(AParent: TWinControl;
      ALeft, ATop, AWidth: Integer): TComboBox;
    function CreateCheck(AParent: TWinControl;
      const ACaption: string;
      ALeft, ATop: Integer): TCheckBox;
    function CreateRadio(AParent: TWinControl;
      const ACaption: string;
      ALeft, ATop: Integer): TRadioButton;

    { Помечает элемент как неразработанный }
    procedure MarkNotImplemented(AControl: TControl);

    { Парсинг DIMPOST — разделение на префикс и суффикс }
    procedure ParseDimPost(const AValue: string;
      out APrefix, ASuffix: string);
    function BuildDimPost(
      const APrefix, ASuffix: string): string;

  public
    constructor Create(AOwner: TComponent); override;
    { Инициализирует форму данными из переданного стиля }
    procedure SetDimStyle(ADimStyle: PGDBDimStyle);
  end;

implementation

{ ============================================================ }
{ Вспомогательные методы создания элементов управления          }
{ ============================================================ }

{ Создаёт метку (TLabel) с заданными параметрами }
function TDimStyleEditDialog.CreateLabel(
  AParent: TWinControl;
  const ACaption: string;
  ALeft, ATop: Integer): TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent  := AParent;
  Result.Caption := ACaption;
  Result.Left    := ALeft;
  Result.Top     := ATop + 4;
end;

{ Создаёт поле ввода (TEdit) с заданными параметрами }
function TDimStyleEditDialog.CreateEdit(
  AParent: TWinControl;
  ALeft, ATop, AWidth: Integer): TEdit;
begin
  Result := TEdit.Create(Self);
  Result.Parent := AParent;
  Result.Left   := ALeft;
  Result.Top    := ATop;
  Result.Width  := AWidth;
  Result.Height := CControlHeight;
end;

{ Создаёт выпадающий список (TComboBox) }
function TDimStyleEditDialog.CreateCombo(
  AParent: TWinControl;
  ALeft, ATop, AWidth: Integer): TComboBox;
begin
  Result := TComboBox.Create(Self);
  Result.Parent := AParent;
  Result.Style  := csDropDownList;
  Result.Left   := ALeft;
  Result.Top    := ATop;
  Result.Width  := AWidth;
  Result.Height := CControlHeight;
end;

{ Создаёт чекбокс (TCheckBox) }
function TDimStyleEditDialog.CreateCheck(
  AParent: TWinControl;
  const ACaption: string;
  ALeft, ATop: Integer): TCheckBox;
begin
  Result := TCheckBox.Create(Self);
  Result.Parent  := AParent;
  Result.Caption := ACaption;
  Result.Left    := ALeft;
  Result.Top     := ATop;
end;

{ Создаёт радиокнопку (TRadioButton) }
function TDimStyleEditDialog.CreateRadio(
  AParent: TWinControl;
  const ACaption: string;
  ALeft, ATop: Integer): TRadioButton;
begin
  Result := TRadioButton.Create(Self);
  Result.Parent  := AParent;
  Result.Caption := ACaption;
  Result.Left    := ALeft;
  Result.Top     := ATop;
end;

{ Помечает элемент управления как неразработанный }
procedure TDimStyleEditDialog.MarkNotImplemented(
  AControl: TControl);
begin
  AControl.Enabled  := False;
  AControl.ShowHint := True;
  AControl.Hint     := CHintNotImplemented;
end;

{ ============================================================ }
{ Парсинг DIMPOST                                               }
{ ============================================================ }

{ Разделяет строку DIMPOST на префикс и суффикс по маркеру <> }
procedure TDimStyleEditDialog.ParseDimPost(
  const AValue: string;
  out APrefix, ASuffix: string);
var
  MarkerPos: Integer;
begin
  MarkerPos := Pos('<>', AValue);
  if MarkerPos > 0 then
  begin
    APrefix := Copy(AValue, 1, MarkerPos - 1);
    ASuffix := Copy(AValue, MarkerPos + 2, Length(AValue));
  end
  else
  begin
    APrefix := '';
    ASuffix := AValue;
  end;
end;

{ Собирает строку DIMPOST из префикса и суффикса }
function TDimStyleEditDialog.BuildDimPost(
  const APrefix, ASuffix: string): string;
begin
  if (APrefix = '') and (ASuffix = '') then
    Result := ''
  else
    Result := APrefix + '<>' + ASuffix;
end;

{ ============================================================ }
{ Заглушка предпросмотра                                        }
{ ============================================================ }

{ Рисует серый прямоугольник с текстом "Предпросмотр недоступен" }
procedure TDimStyleEditDialog.DrawPreviewStub(
  ACanvas: TCanvas; const ABounds: TRect);
var
  TextW, TextH, TextX, TextY: Integer;
begin
  ACanvas.Brush.Color := clSilver;
  ACanvas.FillRect(ABounds);
  ACanvas.Pen.Color := clGray;
  ACanvas.Rectangle(ABounds);

  ACanvas.Font.Color := clBlack;
  ACanvas.Font.Size  := CPreviewFontSize;
  TextW := ACanvas.TextWidth(CPreviewStubText);
  TextH := ACanvas.TextHeight(CPreviewStubText);
  TextX := ABounds.Left
    + (ABounds.Right - ABounds.Left - TextW) div 2;
  TextY := ABounds.Top
    + (ABounds.Bottom - ABounds.Top - TextH) div 2;
  ACanvas.TextOut(TextX, TextY, CPreviewStubText);
end;

{ Обработчик события OnPaint для областей предпросмотра }
procedure TDimStyleEditDialog.OnPaintPreview(Sender: TObject);
var
  PaintBox: TPaintBox;
begin
  PaintBox := Sender as TPaintBox;
  DrawPreviewStub(PaintBox.Canvas,
    Rect(0, 0, PaintBox.Width, PaintBox.Height));
end;

{ ============================================================ }
{ Конструктор и построение формы                                }
{ ============================================================ }

constructor TDimStyleEditDialog.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Caption     := 'Изменение размерного стиля';
  Width       := CEditFormWidth;
  Height      := CEditFormHeight;
  Position    := poMainFormCenter;
  BorderStyle := bsDialog;
  FDimStyle   := nil;

  BuildControls;
end;

{ Инициализирует форму данными из переданного стиля }
procedure TDimStyleEditDialog.SetDimStyle(
  ADimStyle: PGDBDimStyle);
begin
  FDimStyle := ADimStyle;
  if FDimStyle = nil then
    Exit;

  { Сохраняем резервную копию для отмены }
  FBackupLines   := FDimStyle^.Lines;
  FBackupArrows  := FDimStyle^.Arrows;
  FBackupText    := FDimStyle^.Text;
  FBackupPlacing := FDimStyle^.Placing;
  FBackupUnits   := FDimStyle^.Units;

  Caption := 'Изменение размерного стиля: '
    + FDimStyle^.Name;

  LoadFromStyle;

  programlog.LogOutFormatStr(
    'uzcui_dimstyleedit: форма открыта для стиля "%s"',
    [FDimStyle^.Name], LM_Info);
end;

{ Создаёт все элементы интерфейса }
procedure TDimStyleEditDialog.BuildControls;
var
  Tab: TTabSheet;
begin
  BuildBottomPanel;

  { Создаём блокнот с вкладками }
  FPageControl := TPageControl.Create(Self);
  FPageControl.Parent := Self;
  FPageControl.Align  := alClient;

  { Вкладка 1: Линии }
  Tab := TTabSheet.Create(FPageControl);
  Tab.PageControl := FPageControl;
  Tab.Caption := 'Линии';
  BuildTabLines(Tab);

  { Вкладка 2: Символы и стрелки }
  Tab := TTabSheet.Create(FPageControl);
  Tab.PageControl := FPageControl;
  Tab.Caption := 'Символы и стрелки';
  BuildTabArrows(Tab);

  { Вкладка 3: Текст }
  Tab := TTabSheet.Create(FPageControl);
  Tab.PageControl := FPageControl;
  Tab.Caption := 'Текст';
  BuildTabText(Tab);

  { Вкладка 4: Размещение }
  Tab := TTabSheet.Create(FPageControl);
  Tab.PageControl := FPageControl;
  Tab.Caption := 'Размещение';
  BuildTabPlacing(Tab);

  { Вкладка 5: Основные единицы }
  Tab := TTabSheet.Create(FPageControl);
  Tab.PageControl := FPageControl;
  Tab.Caption := 'Основные единицы';
  BuildTabUnits(Tab);

  { Вкладка 6: Альт. единицы }
  Tab := TTabSheet.Create(FPageControl);
  Tab.PageControl := FPageControl;
  Tab.Caption := 'Альт. единицы';
  BuildTabAltUnits(Tab);

  { Вкладка 7: Допуски }
  Tab := TTabSheet.Create(FPageControl);
  Tab.PageControl := FPageControl;
  Tab.Caption := 'Допуски';
  BuildTabTolerances(Tab);
end;

{ Создаёт нижнюю панель с кнопками OK, Отмена, Справка }
procedure TDimStyleEditDialog.BuildBottomPanel;
var
  PanelBottom: TPanel;
begin
  PanelBottom := TPanel.Create(Self);
  PanelBottom.Parent     := Self;
  PanelBottom.Align      := alBottom;
  PanelBottom.Height     := CBottomPanelHeight;
  PanelBottom.BevelOuter := bvNone;

  FButtonOK := TButton.Create(Self);
  FButtonOK.Parent      := PanelBottom;
  FButtonOK.Caption     := 'OK';
  FButtonOK.Width       := CBottomBtnWidth;
  FButtonOK.Height      := CBottomBtnHeight;
  FButtonOK.Anchors     := [akRight, akBottom];
  FButtonOK.AnchorSide[akRight].Control := PanelBottom;
  FButtonOK.AnchorSide[akRight].Side    := asrRight;
  FButtonOK.AnchorSide[akBottom].Control := PanelBottom;
  FButtonOK.AnchorSide[akBottom].Side    := asrBottom;
  FButtonOK.BorderSpacing.Right  := 190;
  FButtonOK.BorderSpacing.Bottom := 8;
  FButtonOK.Default     := True;
  FButtonOK.OnClick     := @OnOKClick;

  FButtonCancel := TButton.Create(Self);
  FButtonCancel.Parent      := PanelBottom;
  FButtonCancel.Caption     := 'Отмена';
  FButtonCancel.Width       := CBottomBtnWidth;
  FButtonCancel.Height      := CBottomBtnHeight;
  FButtonCancel.Anchors     := [akRight, akBottom];
  FButtonCancel.AnchorSide[akRight].Control := FButtonOK;
  FButtonCancel.AnchorSide[akRight].Side    := asrRight;
  FButtonCancel.AnchorSide[akBottom].Control := PanelBottom;
  FButtonCancel.AnchorSide[akBottom].Side    := asrBottom;
  FButtonCancel.BorderSpacing.Right  := -90;
  FButtonCancel.BorderSpacing.Bottom := 8;
  FButtonCancel.Cancel      := True;
  FButtonCancel.OnClick     := @OnCancelClick;

  FButtonHelp := TButton.Create(Self);
  FButtonHelp.Parent      := PanelBottom;
  FButtonHelp.Caption     := 'Справка';
  FButtonHelp.Width       := CBottomBtnWidth;
  FButtonHelp.Height      := CBottomBtnHeight;
  FButtonHelp.Anchors     := [akRight, akBottom];
  FButtonHelp.AnchorSide[akRight].Control := PanelBottom;
  FButtonHelp.AnchorSide[akRight].Side    := asrRight;
  FButtonHelp.AnchorSide[akBottom].Control := PanelBottom;
  FButtonHelp.AnchorSide[akBottom].Side    := asrBottom;
  FButtonHelp.BorderSpacing.Right  := 10;
  FButtonHelp.BorderSpacing.Bottom := 8;
  FButtonHelp.Enabled := False;
  FButtonHelp.ShowHint := True;
  FButtonHelp.Hint := 'Справка не реализована на данном этапе';
end;

{ ============================================================ }
{ Включение файлов с построением вкладок                        }
{ ============================================================ }

{$I uzcui_dimstyleedit_tab_lines.inc}
{$I uzcui_dimstyleedit_tab_arrows.inc}
{$I uzcui_dimstyleedit_tab_text.inc}
{$I uzcui_dimstyleedit_tab_placing.inc}
{$I uzcui_dimstyleedit_tab_units.inc}
{$I uzcui_dimstyleedit_tab_altunits.inc}
{$I uzcui_dimstyleedit_tab_tolerances.inc}

{ ============================================================ }
{ Загрузка и сохранение данных стиля                            }
{ ============================================================ }

{ Загружает значения из структуры стиля в элементы управления }
procedure TDimStyleEditDialog.LoadFromStyle;
var
  VertIdx: TDimTextVertPosition;
  UnitIdx: TDimUnit;
  SepIdx: TDimDSep;
  DrawingPtr: PTSimpleDrawing;
  TextStylePtr: PGDBTextStyle;
  IterRec: itrec;
  FoundIdx, DecIdx: Integer;
  DimPrefix, DimSuffix: string;
begin
  if FDimStyle = nil then
    Exit;

  { --- Вкладка "Линии" --- }
  { Цвет размерной линии }
  FComboDimLineColor.ItemIndex := 0;
  { Удлинение за выносные }
  FEditDimDLE.Text := FormatFloat('0.0000',
    FDimStyle^.Lines.DIMDLE);
  { Удлинение за размерные }
  FEditExtEXE.Text := FormatFloat('0.0000',
    FDimStyle^.Lines.DIMEXE);
  { Отступ от объекта }
  FEditExtEXO.Text := FormatFloat('0.0000',
    FDimStyle^.Lines.DIMEXO);

  { --- Вкладка "Символы и стрелки" --- }
  { Стиль первой стрелки }
  FComboArrow1.ItemIndex := Ord(FDimStyle^.Arrows.DIMBLK1);
  { Стиль второй стрелки }
  FComboArrow2.ItemIndex := Ord(FDimStyle^.Arrows.DIMBLK2);
  { Стиль стрелки выноски }
  FComboArrowLeader.ItemIndex :=
    Ord(FDimStyle^.Arrows.DIMLDRBLK);
  { Размер стрелки }
  FEditArrowSize.Text := FormatFloat('0.0000',
    FDimStyle^.Arrows.DIMASZ);
  { Размер маркера центра }
  FEditCenterSize.Text := FormatFloat('0.0000',
    FDimStyle^.Lines.DIMCEN);

  { --- Вкладка "Текст" --- }
  { Заполняем список текстовых стилей из чертежа }
  FComboTextStyle.Items.Clear;
  DrawingPtr := drawings.GetCurrentDWG;
  if DrawingPtr <> nil then
  begin
    TextStylePtr :=
      DrawingPtr^.TextStyleTable.beginiterate(IterRec);
    while TextStylePtr <> nil do
    begin
      FComboTextStyle.Items.Add(TextStylePtr^.Name);
      TextStylePtr :=
        DrawingPtr^.TextStyleTable.iterate(IterRec);
    end;
  end;
  { Выбираем текущий текстовый стиль }
  if FDimStyle^.Text.DIMTXSTY <> nil then
  begin
    FoundIdx := FComboTextStyle.Items.IndexOf(
      FDimStyle^.Text.DIMTXSTY^.Name);
    if FoundIdx >= 0 then
      FComboTextStyle.ItemIndex := FoundIdx
    else if FComboTextStyle.Items.Count > 0 then
      FComboTextStyle.ItemIndex := 0;
  end
  else if FComboTextStyle.Items.Count > 0 then
    FComboTextStyle.ItemIndex := 0;

  { Цвет текста }
  FComboTextColor.ItemIndex := 0;
  { Высота текста }
  FEditTextHeight.Text := FormatFloat('0.0000',
    FDimStyle^.Text.DIMTXT);
  { Отступ от размерной линии }
  FEditTextGap.Text := FormatFloat('0.0000',
    FDimStyle^.Text.DIMGAP);

  { Вертикальное размещение текста }
  FComboTextVertPos.ItemIndex :=
    Ord(FDimStyle^.Text.DIMTAD);

  { Ориентация текста }
  if FDimStyle^.Text.DIMTIH and FDimStyle^.Text.DIMTOH then
    FRadioTextHoriz.Checked := True
  else if (not FDimStyle^.Text.DIMTIH)
    and (not FDimStyle^.Text.DIMTOH) then
    FRadioTextAligned.Checked := True
  else
    FRadioTextISO.Checked := True;

  { --- Вкладка "Размещение" --- }
  case FDimStyle^.Placing.DIMTMOVE of
    DTMMoveDimLine:  FRadioTMoveMoveLine.Checked := True;
    DTMCreateLeader: FRadioTMoveLeader.Checked := True;
    DTMnothung:      FRadioTMoveNoLeader.Checked := True;
  end;
  { Глобальный масштаб }
  FEditDimScale.Value := FDimStyle^.Units.DIMSCALE;

  { --- Вкладка "Основные единицы" --- }
  FComboUnitFormat.ItemIndex :=
    Ord(FDimStyle^.Units.DIMLUNIT);
  { Точность }
  DecIdx := FDimStyle^.Units.DIMDEC;
  if (DecIdx >= 0) and (DecIdx < FComboPrecision.Items.Count)
  then
    FComboPrecision.ItemIndex := DecIdx
  else
    FComboPrecision.ItemIndex := 0;
  { Десятичный разделитель }
  FComboDecSeparator.ItemIndex :=
    Ord(FDimStyle^.Units.DIMDSEP);
  { Округление }
  FEditRounding.Text := FormatFloat('0.0000',
    FDimStyle^.Units.DIMRND);
  { Префикс и суффикс }
  ParseDimPost(FDimStyle^.Units.DIMPOST,
    DimPrefix, DimSuffix);
  FEditPrefix.Text := DimPrefix;
  FEditSuffix.Text := DimSuffix;
  { Масштаб измерений }
  FEditMeasScale.Value := FDimStyle^.Units.DIMLFAC;
  { Подавление нулей }
  FCheckZeroLeading.Checked :=
    (FDimStyle^.Units.DIMZIN and 4) <> 0;
  FCheckZeroTrailing.Checked :=
    (FDimStyle^.Units.DIMZIN and 8) <> 0;
  FCheckZeroFeet.Checked :=
    (FDimStyle^.Units.DIMZIN and 1) <> 0;
  FCheckZeroInches.Checked :=
    (FDimStyle^.Units.DIMZIN and 2) <> 0;
end;

{ Сохраняет значения из элементов управления в структуру стиля }
procedure TDimStyleEditDialog.SaveToStyle;
var
  DrawingPtr: PTSimpleDrawing;
  TextStylePtr: PGDBTextStyle;
  DimZin: Integer;
begin
  if FDimStyle = nil then
    Exit;

  { --- Линии --- }
  FDimStyle^.Lines.DIMDLE :=
    StrToFloatDef(FEditDimDLE.Text, 0);
  FDimStyle^.Lines.DIMEXE :=
    StrToFloatDef(FEditExtEXE.Text, 0.18);
  FDimStyle^.Lines.DIMEXO :=
    StrToFloatDef(FEditExtEXO.Text, 0.0625);

  { --- Стрелки --- }
  if FComboArrow1.ItemIndex >= 0 then
    FDimStyle^.Arrows.DIMBLK1 :=
      TArrowStyle(FComboArrow1.ItemIndex);
  if FComboArrow2.ItemIndex >= 0 then
    FDimStyle^.Arrows.DIMBLK2 :=
      TArrowStyle(FComboArrow2.ItemIndex);
  if FComboArrowLeader.ItemIndex >= 0 then
    FDimStyle^.Arrows.DIMLDRBLK :=
      TArrowStyle(FComboArrowLeader.ItemIndex);
  FDimStyle^.Arrows.DIMASZ :=
    StrToFloatDef(FEditArrowSize.Text, 0.18);
  FDimStyle^.Lines.DIMCEN :=
    StrToFloatDef(FEditCenterSize.Text, 0.09);

  { --- Текст --- }
  { Устанавливаем текстовый стиль по имени }
  DrawingPtr := drawings.GetCurrentDWG;
  if (DrawingPtr <> nil)
    and (FComboTextStyle.ItemIndex >= 0) then
  begin
    TextStylePtr := PGDBTextStyle(
      DrawingPtr^.TextStyleTable.getAddres(
        FComboTextStyle.Items[FComboTextStyle.ItemIndex]));
    if TextStylePtr <> nil then
      FDimStyle^.Text.DIMTXSTY := TextStylePtr;
  end;
  FDimStyle^.Text.DIMTXT :=
    StrToFloatDef(FEditTextHeight.Text, 0.18);
  FDimStyle^.Text.DIMGAP :=
    StrToFloatDef(FEditTextGap.Text, 0.625);
  if FComboTextVertPos.ItemIndex >= 0 then
    FDimStyle^.Text.DIMTAD :=
      TDimTextVertPosition(FComboTextVertPos.ItemIndex);

  { Ориентация текста }
  if FRadioTextHoriz.Checked then
  begin
    FDimStyle^.Text.DIMTIH := True;
    FDimStyle^.Text.DIMTOH := True;
  end
  else if FRadioTextAligned.Checked then
  begin
    FDimStyle^.Text.DIMTIH := False;
    FDimStyle^.Text.DIMTOH := False;
  end
  else
  begin
    FDimStyle^.Text.DIMTIH := False;
    FDimStyle^.Text.DIMTOH := True;
  end;

  { --- Размещение --- }
  if FRadioTMoveMoveLine.Checked then
    FDimStyle^.Placing.DIMTMOVE := DTMMoveDimLine
  else if FRadioTMoveLeader.Checked then
    FDimStyle^.Placing.DIMTMOVE := DTMCreateLeader
  else
    FDimStyle^.Placing.DIMTMOVE := DTMnothung;
  FDimStyle^.Units.DIMSCALE := FEditDimScale.Value;

  { --- Основные единицы --- }
  if FComboUnitFormat.ItemIndex >= 0 then
    FDimStyle^.Units.DIMLUNIT :=
      TDimUnit(FComboUnitFormat.ItemIndex);
  if FComboPrecision.ItemIndex >= 0 then
    FDimStyle^.Units.DIMDEC := FComboPrecision.ItemIndex;
  if FComboDecSeparator.ItemIndex >= 0 then
    FDimStyle^.Units.DIMDSEP :=
      TDimDSep(FComboDecSeparator.ItemIndex);
  FDimStyle^.Units.DIMRND :=
    StrToFloatDef(FEditRounding.Text, 0);
  FDimStyle^.Units.DIMPOST :=
    BuildDimPost(FEditPrefix.Text, FEditSuffix.Text);
  FDimStyle^.Units.DIMLFAC := FEditMeasScale.Value;

  { Подавление нулей (DIMZIN) }
  DimZin := 0;
  if FCheckZeroLeading.Checked then
    DimZin := DimZin or 4;
  if FCheckZeroTrailing.Checked then
    DimZin := DimZin or 8;
  if FCheckZeroFeet.Checked then
    DimZin := DimZin or 1;
  if FCheckZeroInches.Checked then
    DimZin := DimZin or 2;
  FDimStyle^.Units.DIMZIN := DimZin;

  programlog.LogOutFormatStr(
    'uzcui_dimstyleedit: изменения сохранены в стиль "%s"',
    [FDimStyle^.Name], LM_Info);
end;

{ ============================================================ }
{ Обработчики кнопок                                            }
{ ============================================================ }

{ Кнопка OK — сохраняем изменения и закрываем }
procedure TDimStyleEditDialog.OnOKClick(Sender: TObject);
begin
  SaveToStyle;
  ModalResult := mrOk;
end;

{ Кнопка Отмена — восстанавливаем резервную копию }
procedure TDimStyleEditDialog.OnCancelClick(Sender: TObject);
begin
  if FDimStyle <> nil then
  begin
    FDimStyle^.Lines   := FBackupLines;
    FDimStyle^.Arrows  := FBackupArrows;
    FDimStyle^.Text    := FBackupText;
    FDimStyle^.Placing := FBackupPlacing;
    FDimStyle^.Units   := FBackupUnits;
  end;
  ModalResult := mrCancel;
end;

initialization
  programlog.LogOutFormatStr(
    'Unit "%s" initialization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsInitializeLMId);

finalization
  programlog.LogOutFormatStr(
    'Unit "%s" finalization', [{$INCLUDE %FILE%}],
    LM_Info, UnitsFinalizeLMId);
end.
