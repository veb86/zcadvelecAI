{ Включаемый файл: построение вкладки "Текст" }
{ Содержит процедуру BuildTabText для TDimStyleEditForm }

{ Создаёт элементы управления вкладки "Текст":
  - группа "Свойства текста" (стиль, цвет, высота)
  - группа "Размещение текста" (вертикальное, горизонтальное)
  - группа "Ориентация текста" (горизонтально/вдоль/ISO)
  - область предпросмотра (заглушка) }
procedure TDimStyleEditForm.BuildTabText(ATab: TTabSheet);
var
  GroupTextProp: TGroupBox;
  GroupTextPlace: TGroupBox;
  GroupTextOrient: TGroupBox;
  Preview: TPaintBox;
  VertIdx: TDimTextVertPosition;
  RowTop: Integer;
  LeftCol, RightCol: Integer;
begin
  LeftCol  := CGroupBoxPadLeft;
  RightCol := LeftCol + CLabelWidth;

  { Область предпросмотра }
  Preview := TPaintBox.Create(Self);
  Preview.Parent  := ATab;
  Preview.Left    := 290;
  Preview.Top     := 20;
  Preview.Width   := CPreviewWidth;
  Preview.Height  := CPreviewHeight;
  Preview.OnPaint := @OnPaintPreview;

  { --- Группа "Свойства текста" --- }
  GroupTextProp := TGroupBox.Create(Self);
  GroupTextProp.Parent  := ATab;
  GroupTextProp.Caption := 'Свойства текста';
  GroupTextProp.Left    := CControlMargin;
  GroupTextProp.Top     := CControlMargin;
  GroupTextProp.Width   := 275;
  GroupTextProp.Height  := 220;

  RowTop := CGroupBoxPadTop;

  { Текстовый стиль }
  CreateLabel(GroupTextProp, 'Текстовый стиль:',
    LeftCol, RowTop);
  FComboTextStyle := CreateCombo(
    GroupTextProp, RightCol, RowTop, 120);
  RowTop := RowTop + CControlStep;

  { Цвет текста }
  CreateLabel(GroupTextProp, 'Цвет текста:',
    LeftCol, RowTop);
  FComboTextColor := CreateCombo(
    GroupTextProp, RightCol, RowTop, 120);
  FComboTextColor.Items.Add('ПоБлоку');
  FComboTextColor.ItemIndex := 0;
  MarkNotImplemented(FComboTextColor);
  RowTop := RowTop + CControlStep;

  { Цвет заливки — не разработан }
  CreateLabel(GroupTextProp, 'Цвет заливки:',
    LeftCol, RowTop);
  FComboFillColor := CreateCombo(
    GroupTextProp, RightCol, RowTop, 120);
  FComboFillColor.Items.Add('Нет');
  FComboFillColor.ItemIndex := 0;
  MarkNotImplemented(FComboFillColor);
  RowTop := RowTop + CControlStep;

  { Высота текста }
  CreateLabel(GroupTextProp, 'Высота текста:',
    LeftCol, RowTop);
  FEditTextHeight := CreateEdit(
    GroupTextProp, RightCol, RowTop, CEditWidth);
  RowTop := RowTop + CControlStep;

  { Масштаб дробей — не разработан }
  CreateLabel(GroupTextProp, 'Масштаб дробей:',
    LeftCol, RowTop);
  FEditFractionScale := CreateEdit(
    GroupTextProp, RightCol, RowTop, CEditWidth);
  MarkNotImplemented(FEditFractionScale);
  RowTop := RowTop + CControlStep;

  { Текст в рамке — не разработан }
  FCheckTextFrame := CreateCheck(
    GroupTextProp, 'Текст в рамке', LeftCol, RowTop);
  MarkNotImplemented(FCheckTextFrame);

  { --- Группа "Размещение текста" --- }
  GroupTextPlace := TGroupBox.Create(Self);
  GroupTextPlace.Parent  := ATab;
  GroupTextPlace.Caption := 'Размещение текста';
  GroupTextPlace.Left    := CControlMargin;
  GroupTextPlace.Top     := 235;
  GroupTextPlace.Width   := 275;
  GroupTextPlace.Height  := 140;

  RowTop := CGroupBoxPadTop;

  { По вертикали }
  CreateLabel(GroupTextPlace, 'По вертикали:',
    LeftCol, RowTop);
  FComboTextVertPos := CreateCombo(
    GroupTextPlace, RightCol, RowTop, 120);
  for VertIdx := Low(TDimTextVertPosition)
    to High(TDimTextVertPosition) do
    FComboTextVertPos.Items.Add(CTextVertNames[VertIdx]);
  RowTop := RowTop + CControlStep;

  { По горизонтали — не разработан }
  CreateLabel(GroupTextPlace, 'По горизонтали:',
    LeftCol, RowTop);
  FComboTextHorizPos := CreateCombo(
    GroupTextPlace, RightCol, RowTop, 120);
  FComboTextHorizPos.Items.Add('По центру');
  FComboTextHorizPos.ItemIndex := 0;
  MarkNotImplemented(FComboTextHorizPos);
  RowTop := RowTop + CControlStep;

  { Направление взгляда — не разработан }
  CreateLabel(GroupTextPlace, 'Направление взгляда:',
    LeftCol, RowTop);
  FComboTextViewDir := CreateCombo(
    GroupTextPlace, RightCol, RowTop, 120);
  FComboTextViewDir.Items.Add('Слева направо');
  FComboTextViewDir.ItemIndex := 0;
  MarkNotImplemented(FComboTextViewDir);
  RowTop := RowTop + CControlStep;

  { Отступ от размерной линии }
  CreateLabel(GroupTextPlace,
    'Отступ от размерной линии:',
    LeftCol, RowTop);
  FEditTextGap := CreateEdit(
    GroupTextPlace, RightCol + 30, RowTop, CEditWidth);

  { --- Группа "Ориентация текста" --- }
  GroupTextOrient := TGroupBox.Create(Self);
  GroupTextOrient.Parent  := ATab;
  GroupTextOrient.Caption := 'Ориентация текста';
  GroupTextOrient.Left    := 290;
  GroupTextOrient.Top     := 235;
  GroupTextOrient.Width   := 250;
  GroupTextOrient.Height  := 140;

  RowTop := CGroupBoxPadTop + 5;

  FRadioTextHoriz := CreateRadio(GroupTextOrient,
    'Горизонтально', LeftCol, RowTop);
  RowTop := RowTop + 30;

  FRadioTextAligned := CreateRadio(GroupTextOrient,
    'Вдоль размерной линии', LeftCol, RowTop);
  RowTop := RowTop + 30;

  FRadioTextISO := CreateRadio(GroupTextOrient,
    'Согласно ISO', LeftCol, RowTop);
end;
