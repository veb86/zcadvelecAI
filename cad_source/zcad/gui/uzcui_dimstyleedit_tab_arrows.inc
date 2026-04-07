{ Включаемый файл: построение вкладки "Символы и стрелки" }
{ Содержит процедуру BuildTabArrows для TDimStyleEditForm }

{ Создаёт элементы управления вкладки "Символы и стрелки":
  - группа "Стрелки" (первая, вторая, выноска, размер)
  - группа "Маркеры центра" (нет/маркер/линия, размер)
  - область предпросмотра (заглушка) }
procedure TDimStyleEditDialog.BuildTabArrows(ATab: TTabSheet);
var
  GroupArrows: TGroupBox;
  GroupCenter: TGroupBox;
  Preview: TPaintBox;
  ArrowIdx: TArrowStyle;
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

  { --- Группа "Стрелки" --- }
  GroupArrows := TGroupBox.Create(Self);
  GroupArrows.Parent  := ATab;
  GroupArrows.Caption := 'Стрелки';
  GroupArrows.Left    := CControlMargin;
  GroupArrows.Top     := CControlMargin;
  GroupArrows.Width   := 275;
  GroupArrows.Height  := 200;

  RowTop := CGroupBoxPadTop;

  { Первая стрелка }
  CreateLabel(GroupArrows, 'Первая:', LeftCol, RowTop);
  FComboArrow1 := CreateCombo(
    GroupArrows, LeftCol, RowTop + 18, 250);
  for ArrowIdx := Low(TArrowStyle) to High(TArrowStyle) do
    FComboArrow1.Items.Add(CArrowNames[ArrowIdx]);
  RowTop := RowTop + CControlStep + 18;

  { Вторая стрелка }
  CreateLabel(GroupArrows, 'Вторая:', LeftCol, RowTop);
  FComboArrow2 := CreateCombo(
    GroupArrows, LeftCol, RowTop + 18, 250);
  for ArrowIdx := Low(TArrowStyle) to High(TArrowStyle) do
    FComboArrow2.Items.Add(CArrowNames[ArrowIdx]);
  RowTop := RowTop + CControlStep + 18;

  { Выноска }
  CreateLabel(GroupArrows, 'Выноска:', LeftCol, RowTop);
  FComboArrowLeader := CreateCombo(
    GroupArrows, LeftCol, RowTop + 18, 250);
  for ArrowIdx := Low(TArrowStyle) to High(TArrowStyle) do
    FComboArrowLeader.Items.Add(CArrowNames[ArrowIdx]);
  RowTop := RowTop + CControlStep + 18;

  { Размер стрелки }
  CreateLabel(GroupArrows, 'Размер стрелки:',
    LeftCol, RowTop);
  FEditArrowSize := CreateEdit(
    GroupArrows, LeftCol, RowTop + 18, CEditWidth);

  { --- Группа "Маркеры центра" --- }
  GroupCenter := TGroupBox.Create(Self);
  GroupCenter.Parent  := ATab;
  GroupCenter.Caption := 'Маркеры центра';
  GroupCenter.Left    := CControlMargin;
  GroupCenter.Top     := 215;
  GroupCenter.Width   := 275;
  GroupCenter.Height  := 100;

  RowTop := CGroupBoxPadTop;

  { Радиокнопки: Нет / Маркер / Линия }
  FRadioCenterNone := CreateRadio(
    GroupCenter, 'Нет', LeftCol, RowTop);
  MarkNotImplemented(FRadioCenterNone);

  FRadioCenterMark := CreateRadio(
    GroupCenter, 'Маркер', LeftCol, RowTop + 22);
  MarkNotImplemented(FRadioCenterMark);
  FRadioCenterMark.Checked := True;

  { Размер маркера центра }
  FEditCenterSize := CreateEdit(
    GroupCenter, RightCol, RowTop + 22, CEditWidth);

  FRadioCenterLine := CreateRadio(
    GroupCenter, 'Линия', LeftCol, RowTop + 44);
  MarkNotImplemented(FRadioCenterLine);
end;
