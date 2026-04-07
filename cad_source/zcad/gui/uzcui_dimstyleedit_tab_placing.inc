{ Включаемый файл: построение вкладки "Размещение" }
{ Содержит процедуру BuildTabPlacing для TDimStyleEditForm }

{ Создаёт элементы управления вкладки "Размещение":
  - группа "Параметры размещения" (оптимально/стрелки/текст)
  - группа "Размещение текста" (DIMTMOVE)
  - группа "Масштаб размерных элементов" (DIMSCALE)
  - группа "Подгонка элементов"
  - область предпросмотра (заглушка) }
procedure TDimStyleEditDialog.BuildTabPlacing(
  ATab: TTabSheet);
var
  GroupFitOptions: TGroupBox;
  GroupTextMove: TGroupBox;
  GroupScale: TGroupBox;
  GroupFine: TGroupBox;
  Preview: TPaintBox;
  RowTop: Integer;
  LeftCol: Integer;
begin
  LeftCol := CGroupBoxPadLeft;

  { Область предпросмотра }
  Preview := TPaintBox.Create(Self);
  Preview.Parent  := ATab;
  Preview.Left    := 290;
  Preview.Top     := 20;
  Preview.Width   := CPreviewWidth;
  Preview.Height  := CPreviewHeight;
  Preview.OnPaint := @OnPaintPreview;

  { --- Группа "Параметры размещения" --- }
  GroupFitOptions := TGroupBox.Create(Self);
  GroupFitOptions.Parent  := ATab;
  GroupFitOptions.Caption := 'Параметры размещения';
  GroupFitOptions.Left    := CControlMargin;
  GroupFitOptions.Top     := CControlMargin;
  GroupFitOptions.Width   := 275;
  GroupFitOptions.Height  := 215;

  RowTop := CGroupBoxPadTop;

  { Пояснительный текст }
  CreateLabel(GroupFitOptions,
    'Если текст и стрелки одновременно не могут',
    LeftCol, RowTop);
  CreateLabel(GroupFitOptions,
    'быть размещены между выносными линиями,',
    LeftCol, RowTop + 14);
  CreateLabel(GroupFitOptions,
    'то сначала вынести за выносные линии:',
    LeftCol, RowTop + 28);
  RowTop := RowTop + 48;

  { Варианты размещения — все не разработаны }
  FRadioFitOptimal := CreateRadio(GroupFitOptions,
    'Либо текст, либо стрелки (оптимально)',
    LeftCol, RowTop);
  MarkNotImplemented(FRadioFitOptimal);
  FRadioFitOptimal.Checked := True;
  RowTop := RowTop + 20;

  FRadioFitArrows := CreateRadio(GroupFitOptions,
    'Стрелки', LeftCol, RowTop);
  MarkNotImplemented(FRadioFitArrows);
  RowTop := RowTop + 20;

  FRadioFitText := CreateRadio(GroupFitOptions,
    'Текст', LeftCol, RowTop);
  MarkNotImplemented(FRadioFitText);
  RowTop := RowTop + 20;

  FRadioFitBoth := CreateRadio(GroupFitOptions,
    'Текст и стрелки', LeftCol, RowTop);
  MarkNotImplemented(FRadioFitBoth);
  RowTop := RowTop + 20;

  FRadioFitAlways := CreateRadio(GroupFitOptions,
    'Текст всегда между выносными', LeftCol, RowTop);
  MarkNotImplemented(FRadioFitAlways);
  RowTop := RowTop + 24;

  FCheckSuppressArrows := CreateCheck(GroupFitOptions,
    'Подавить стрелки, если они не' + #10
    + 'помещаются между выносными',
    LeftCol, RowTop);
  MarkNotImplemented(FCheckSuppressArrows);

  { --- Группа "Размещение текста" --- }
  GroupTextMove := TGroupBox.Create(Self);
  GroupTextMove.Parent  := ATab;
  GroupTextMove.Caption := 'Размещение текста';
  GroupTextMove.Left    := CControlMargin;
  GroupTextMove.Top     := 230;
  GroupTextMove.Width   := 275;
  GroupTextMove.Height  := 120;

  RowTop := CGroupBoxPadTop;

  CreateLabel(GroupTextMove,
    'При изменении позиции текста по умолчанию:',
    LeftCol, RowTop);
  RowTop := RowTop + 20;

  FRadioTMoveMoveLine := CreateRadio(GroupTextMove,
    'Перемещать размерную линию', LeftCol, RowTop);
  RowTop := RowTop + 22;

  FRadioTMoveLeader := CreateRadio(GroupTextMove,
    'Строить выноску', LeftCol, RowTop);
  RowTop := RowTop + 22;

  FRadioTMoveNoLeader := CreateRadio(GroupTextMove,
    'Не строить выноску', LeftCol, RowTop);

  { --- Группа "Масштаб размерных элементов" --- }
  GroupScale := TGroupBox.Create(Self);
  GroupScale.Parent  := ATab;
  GroupScale.Caption := 'Масштаб размерных элементов';
  GroupScale.Left    := 290;
  GroupScale.Top     := 230;
  GroupScale.Width   := 250;
  GroupScale.Height  := 90;

  RowTop := CGroupBoxPadTop;

  FCheckAnnotative := CreateCheck(GroupScale,
    'Аннотативный', LeftCol, RowTop);
  MarkNotImplemented(FCheckAnnotative);
  RowTop := RowTop + 22;

  FRadioScaleLayout := CreateRadio(GroupScale,
    'Масштаб размеров по листу', LeftCol, RowTop);
  MarkNotImplemented(FRadioScaleLayout);
  RowTop := RowTop + 22;

  FRadioScaleGlobal := CreateRadio(GroupScale,
    'Глобальный масштаб:', LeftCol, RowTop);
  FRadioScaleGlobal.Checked := True;

  FEditDimScale := TFloatSpinEdit.Create(Self);
  FEditDimScale.Parent      := GroupScale;
  FEditDimScale.Left         := 170;
  FEditDimScale.Top          := RowTop;
  FEditDimScale.Width        := 70;
  FEditDimScale.DecimalPlaces := 4;
  FEditDimScale.MinValue     := 0;
  FEditDimScale.MaxValue     := 99999;

  { --- Группа "Подгонка элементов" --- }
  GroupFine := TGroupBox.Create(Self);
  GroupFine.Parent  := ATab;
  GroupFine.Caption := 'Подгонка элементов';
  GroupFine.Left    := 290;
  GroupFine.Top     := 325;
  GroupFine.Width   := 250;
  GroupFine.Height  := 65;

  RowTop := CGroupBoxPadTop;

  FCheckManualText := CreateCheck(GroupFine,
    'Размещение текста вручную', LeftCol, RowTop);
  MarkNotImplemented(FCheckManualText);
  RowTop := RowTop + 22;

  FCheckDimLineBetween := CreateCheck(GroupFine,
    'Размерная линия между выносными',
    LeftCol, RowTop);
  MarkNotImplemented(FCheckDimLineBetween);
end;
