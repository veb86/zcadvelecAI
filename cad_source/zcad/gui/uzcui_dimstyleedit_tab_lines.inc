{ Включаемый файл: построение вкладки "Линии" }
{ Содержит процедуру BuildTabLines для TDimStyleEditForm }

{ Создаёт элементы управления вкладки "Линии":
  - группа "Размерные линии" (цвет, тип, вес, удлинение)
  - группа "Выносные линии" (цвет, тип 1/2, вес, удлинение)
  - область предпросмотра (заглушка) }
procedure TDimStyleEditDialog.BuildTabLines(ATab: TTabSheet);
var
  GroupDimLines: TGroupBox;
  GroupExtLines: TGroupBox;
  Preview: TPaintBox;
  RowTop: Integer;
  LeftCol, RightCol: Integer;
begin
  LeftCol  := CGroupBoxPadLeft;
  RightCol := LeftCol + CLabelWidth;

  { --- Область предпросмотра --- }
  Preview := TPaintBox.Create(Self);
  Preview.Parent  := ATab;
  Preview.Left    := 290;
  Preview.Top     := 20;
  Preview.Width   := CPreviewWidth;
  Preview.Height  := CPreviewHeight;
  Preview.OnPaint := @OnPaintPreview;

  { --- Группа "Размерные линии" --- }
  GroupDimLines := TGroupBox.Create(Self);
  GroupDimLines.Parent  := ATab;
  GroupDimLines.Caption := 'Размерные линии';
  GroupDimLines.Left    := CControlMargin;
  GroupDimLines.Top     := CControlMargin;
  GroupDimLines.Width   := 275;
  GroupDimLines.Height  := 210;

  RowTop := CGroupBoxPadTop;

  { Цвет }
  CreateLabel(GroupDimLines, 'Цвет:', LeftCol, RowTop);
  FComboDimLineColor := CreateCombo(
    GroupDimLines, RightCol, RowTop, CComboWidth);
  FComboDimLineColor.Items.Add('ПоБлоку');
  FComboDimLineColor.ItemIndex := 0;
  MarkNotImplemented(FComboDimLineColor);
  RowTop := RowTop + CControlStep;

  { Тип линий }
  CreateLabel(GroupDimLines, 'Тип линий:',
    LeftCol, RowTop);
  FComboDimLineType := CreateCombo(
    GroupDimLines, RightCol, RowTop, CComboWidth);
  FComboDimLineType.Items.Add('ПоБлоку');
  FComboDimLineType.ItemIndex := 0;
  MarkNotImplemented(FComboDimLineType);
  RowTop := RowTop + CControlStep;

  { Вес линий }
  CreateLabel(GroupDimLines, 'Вес линий:',
    LeftCol, RowTop);
  FComboDimLineWeight := CreateCombo(
    GroupDimLines, RightCol, RowTop, CComboWidth);
  FComboDimLineWeight.Items.Add('ПоБлоку');
  FComboDimLineWeight.ItemIndex := 0;
  MarkNotImplemented(FComboDimLineWeight);
  RowTop := RowTop + CControlStep;

  { Удлинение за выносные }
  CreateLabel(GroupDimLines, 'Удлинение за выносные:',
    LeftCol, RowTop);
  FEditDimDLE := CreateEdit(
    GroupDimLines, RightCol, RowTop, CEditWidth);
  RowTop := RowTop + CControlStep;

  { Шаг в базовых размерах — не разработан }
  CreateLabel(GroupDimLines, 'Шаг в базовых размерах:',
    LeftCol, RowTop);
  FEditDimBaseSpacing := CreateEdit(
    GroupDimLines, RightCol, RowTop, CEditWidth);
  MarkNotImplemented(FEditDimBaseSpacing);
  RowTop := RowTop + CControlStep;

  { Подавить размерные линии — не разработаны }
  CreateLabel(GroupDimLines, 'Подавить:',
    LeftCol, RowTop);
  FCheckSuppressDL1 := CreateCheck(
    GroupDimLines, '1-ю РЛ', RightCol, RowTop);
  MarkNotImplemented(FCheckSuppressDL1);
  FCheckSuppressDL2 := CreateCheck(
    GroupDimLines, '2-ю РЛ', RightCol + 80, RowTop);
  MarkNotImplemented(FCheckSuppressDL2);

  { --- Группа "Выносные линии" --- }
  GroupExtLines := TGroupBox.Create(Self);
  GroupExtLines.Parent  := ATab;
  GroupExtLines.Caption := 'Выносные линии';
  GroupExtLines.Left    := CControlMargin;
  GroupExtLines.Top     := 225;
  GroupExtLines.Width   := 540;
  GroupExtLines.Height  := 210;

  RowTop := CGroupBoxPadTop;

  { Цвет }
  CreateLabel(GroupExtLines, 'Цвет:', LeftCol, RowTop);
  FComboExtLineColor := CreateCombo(
    GroupExtLines, RightCol, RowTop, CComboWidth);
  FComboExtLineColor.Items.Add('ПоБлоку');
  FComboExtLineColor.ItemIndex := 0;
  MarkNotImplemented(FComboExtLineColor);
  RowTop := RowTop + CControlStep;

  { Тип выносной линии 1 }
  CreateLabel(GroupExtLines, 'Тип выносной линии 1:',
    LeftCol, RowTop);
  FComboExtLineType1 := CreateCombo(
    GroupExtLines, RightCol, RowTop, CComboWidth);
  FComboExtLineType1.Items.Add('ПоБлоку');
  FComboExtLineType1.ItemIndex := 0;
  MarkNotImplemented(FComboExtLineType1);
  RowTop := RowTop + CControlStep;

  { Тип выносной линии 2 }
  CreateLabel(GroupExtLines, 'Тип выносной линии 2:',
    LeftCol, RowTop);
  FComboExtLineType2 := CreateCombo(
    GroupExtLines, RightCol, RowTop, CComboWidth);
  FComboExtLineType2.Items.Add('ПоБлоку');
  FComboExtLineType2.ItemIndex := 0;
  MarkNotImplemented(FComboExtLineType2);
  RowTop := RowTop + CControlStep;

  { Вес линий }
  CreateLabel(GroupExtLines, 'Вес линий:',
    LeftCol, RowTop);
  FComboExtLineWeight := CreateCombo(
    GroupExtLines, RightCol, RowTop, CComboWidth);
  FComboExtLineWeight.Items.Add('ПоБлоку');
  FComboExtLineWeight.ItemIndex := 0;
  MarkNotImplemented(FComboExtLineWeight);
  RowTop := RowTop + CControlStep;

  { Удлинение за размерные и отступ — справа }
  CreateLabel(GroupExtLines, 'Удлинение за размерные:',
    300, CGroupBoxPadTop);
  FEditExtEXE := CreateEdit(
    GroupExtLines, 450, CGroupBoxPadTop, CEditWidth);

  CreateLabel(GroupExtLines, 'Отступ от объекта:',
    300, CGroupBoxPadTop + CControlStep);
  FEditExtEXO := CreateEdit(
    GroupExtLines, 450, CGroupBoxPadTop + CControlStep,
    CEditWidth);

  { Подавить выносные — не разработаны }
  CreateLabel(GroupExtLines, 'Подавить:',
    LeftCol, RowTop);
  FCheckSuppressEL1 := CreateCheck(
    GroupExtLines, '1-ю ВЛ', RightCol, RowTop);
  MarkNotImplemented(FCheckSuppressEL1);
  FCheckSuppressEL2 := CreateCheck(
    GroupExtLines, '2-ю ВЛ', RightCol + 80, RowTop);
  MarkNotImplemented(FCheckSuppressEL2);
  RowTop := RowTop + CControlStep;

  { Фиксированная длина — не разработана }
  FCheckFixedLenExt := CreateCheck(GroupExtLines,
    'Выносные линии фиксированной длины',
    300, CGroupBoxPadTop + CControlStep * 2);
  MarkNotImplemented(FCheckFixedLenExt);

  CreateLabel(GroupExtLines, 'Длина:',
    350, CGroupBoxPadTop + CControlStep * 3);
  FEditFixedLen := CreateEdit(GroupExtLines,
    450, CGroupBoxPadTop + CControlStep * 3, CEditWidth);
  MarkNotImplemented(FEditFixedLen);
end;
