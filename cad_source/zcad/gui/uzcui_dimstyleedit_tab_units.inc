{ Включаемый файл: построение вкладки "Основные единицы" }
{ Содержит процедуру BuildTabUnits для TDimStyleEditForm }

{ Создаёт элементы управления вкладки "Основные единицы":
  - группа "Линейные размеры" (формат, точность, разделитель)
  - группа "Масштаб измерений" (DIMLFAC)
  - группа "Подавление нулей" (DIMZIN)
  - область предпросмотра (заглушка) }
procedure TDimStyleEditForm.BuildTabUnits(
  ATab: TTabSheet);
var
  GroupLinear: TGroupBox;
  GroupMeasScale: TGroupBox;
  GroupZeroSupp: TGroupBox;
  GroupAngular: TGroupBox;
  Preview: TPaintBox;
  UnitIdx: TDimUnit;
  SepIdx: TDimDSep;
  DecIdx: Integer;
  PrecStr: string;
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

  { --- Группа "Линейные размеры" --- }
  GroupLinear := TGroupBox.Create(Self);
  GroupLinear.Parent  := ATab;
  GroupLinear.Caption := 'Линейные размеры';
  GroupLinear.Left    := CControlMargin;
  GroupLinear.Top     := CControlMargin;
  GroupLinear.Width   := 275;
  GroupLinear.Height  := 250;

  RowTop := CGroupBoxPadTop;

  { Формат единиц }
  CreateLabel(GroupLinear, 'Формат единиц:',
    LeftCol, RowTop);
  FComboUnitFormat := CreateCombo(
    GroupLinear, RightCol, RowTop, 130);
  for UnitIdx := Low(TDimUnit) to High(TDimUnit) do
    FComboUnitFormat.Items.Add(CDimUnitNames[UnitIdx]);
  RowTop := RowTop + CControlStep;

  { Точность }
  CreateLabel(GroupLinear, 'Точность:', LeftCol, RowTop);
  FComboPrecision := CreateCombo(
    GroupLinear, RightCol, RowTop, 130);
  for DecIdx := 0 to 8 do
  begin
    if DecIdx = 0 then
      PrecStr := '0'
    else
      PrecStr := '0.' + StringOfChar('0', DecIdx);
    FComboPrecision.Items.Add(PrecStr);
  end;
  RowTop := RowTop + CControlStep;

  { Формат дробей — не разработан }
  CreateLabel(GroupLinear, 'Формат дробей:',
    LeftCol, RowTop);
  FComboFractionFmt := CreateCombo(
    GroupLinear, RightCol, RowTop, 130);
  FComboFractionFmt.Items.Add('Горизонтально');
  FComboFractionFmt.ItemIndex := 0;
  MarkNotImplemented(FComboFractionFmt);
  RowTop := RowTop + CControlStep;

  { Десятичный разделитель }
  CreateLabel(GroupLinear, 'Десятичный разделитель:',
    LeftCol, RowTop);
  FComboDecSeparator := CreateCombo(
    GroupLinear, RightCol, RowTop, 130);
  for SepIdx := Low(TDimDSep) to High(TDimDSep) do
    FComboDecSeparator.Items.Add(CDimDSepNames[SepIdx]);
  RowTop := RowTop + CControlStep;

  { Округление }
  CreateLabel(GroupLinear, 'Округление:',
    LeftCol, RowTop);
  FEditRounding := CreateEdit(
    GroupLinear, RightCol, RowTop, CEditWidth);
  RowTop := RowTop + CControlStep;

  { Префикс }
  CreateLabel(GroupLinear, 'Префикс:', LeftCol, RowTop);
  FEditPrefix := CreateEdit(
    GroupLinear, RightCol, RowTop, 130);
  RowTop := RowTop + CControlStep;

  { Суффикс }
  CreateLabel(GroupLinear, 'Суффикс:', LeftCol, RowTop);
  FEditSuffix := CreateEdit(
    GroupLinear, RightCol, RowTop, 130);

  { --- Группа "Масштаб измерений" --- }
  GroupMeasScale := TGroupBox.Create(Self);
  GroupMeasScale.Parent  := ATab;
  GroupMeasScale.Caption := 'Масштаб измерений';
  GroupMeasScale.Left    := CControlMargin;
  GroupMeasScale.Top     := 265;
  GroupMeasScale.Width   := 275;
  GroupMeasScale.Height  := 65;

  RowTop := CGroupBoxPadTop;

  CreateLabel(GroupMeasScale, 'Масштаб:',
    LeftCol, RowTop);
  FEditMeasScale := TFloatSpinEdit.Create(Self);
  FEditMeasScale.Parent       := GroupMeasScale;
  FEditMeasScale.Left         := RightCol;
  FEditMeasScale.Top          := RowTop;
  FEditMeasScale.Width        := 80;
  FEditMeasScale.DecimalPlaces := 4;
  FEditMeasScale.MinValue     := 0;
  FEditMeasScale.MaxValue     := 99999;
  RowTop := RowTop + CControlStep;

  FCheckScaleLayout := CreateCheck(GroupMeasScale,
    'Только для размеров на листе', LeftCol, RowTop);
  MarkNotImplemented(FCheckScaleLayout);

  { --- Группа "Подавление нулей" --- }
  GroupZeroSupp := TGroupBox.Create(Self);
  GroupZeroSupp.Parent  := ATab;
  GroupZeroSupp.Caption := 'Подавление нулей';
  GroupZeroSupp.Left    := CControlMargin;
  GroupZeroSupp.Top     := 335;
  GroupZeroSupp.Width   := 275;
  GroupZeroSupp.Height  := 65;

  RowTop := CGroupBoxPadTop;

  FCheckZeroLeading := CreateCheck(GroupZeroSupp,
    'Ведущие', LeftCol, RowTop);
  FCheckZeroTrailing := CreateCheck(GroupZeroSupp,
    'Хвостовые', LeftCol + 120, RowTop);
  RowTop := RowTop + 22;

  FCheckZeroFeet := CreateCheck(GroupZeroSupp,
    '0 футов', LeftCol, RowTop);
  FCheckZeroInches := CreateCheck(GroupZeroSupp,
    '0 дюймов', LeftCol + 120, RowTop);

  { --- Группа "Угловые размеры" — не разработана --- }
  GroupAngular := TGroupBox.Create(Self);
  GroupAngular.Parent  := ATab;
  GroupAngular.Caption := 'Угловые размеры';
  GroupAngular.Left    := 290;
  GroupAngular.Top     := 240;
  GroupAngular.Width   := 250;
  GroupAngular.Height  := 160;

  RowTop := CGroupBoxPadTop;

  CreateLabel(GroupAngular, 'Формат единиц:',
    LeftCol, RowTop);
  MarkNotImplemented(CreateCombo(
    GroupAngular, RightCol, RowTop, 120));
  RowTop := RowTop + CControlStep;

  CreateLabel(GroupAngular, 'Точность:',
    LeftCol, RowTop);
  MarkNotImplemented(CreateCombo(
    GroupAngular, RightCol, RowTop, 120));
  RowTop := RowTop + CControlStep + 10;

  CreateLabel(GroupAngular, 'Подавление нулей',
    LeftCol, RowTop);
  RowTop := RowTop + 18;

  MarkNotImplemented(CreateCheck(
    GroupAngular, 'Ведущие', LeftCol, RowTop));
  MarkNotImplemented(CreateCheck(
    GroupAngular, 'Хвостовые', LeftCol + 120, RowTop));
end;
