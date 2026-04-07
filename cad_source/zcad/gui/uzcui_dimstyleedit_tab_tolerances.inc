{ Включаемый файл: построение вкладки "Допуски" }
{ Содержит процедуру BuildTabTolerances для TDimStyleEditForm }

{ Создаёт элементы управления вкладки "Допуски".
  Все параметры на данном этапе не разработаны —
  элементы отображаются заблокированными. }
procedure TDimStyleEditDialog.BuildTabTolerances(
  ATab: TTabSheet);
var
  GroupFormat: TGroupBox;
  GroupAlign: TGroupBox;
  GroupZero: TGroupBox;
  GroupAltTol: TGroupBox;
  GroupAltZero: TGroupBox;
  Preview: TPaintBox;
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

  { --- Группа "Формат допусков" --- }
  GroupFormat := TGroupBox.Create(Self);
  GroupFormat.Parent  := ATab;
  GroupFormat.Caption := 'Формат допусков';
  GroupFormat.Left    := CControlMargin;
  GroupFormat.Top     := CControlMargin;
  GroupFormat.Width   := 275;
  GroupFormat.Height  := 230;

  RowTop := CGroupBoxPadTop;

  { Способ }
  CreateLabel(GroupFormat, 'Способ:', LeftCol, RowTop);
  MarkNotImplemented(CreateCombo(
    GroupFormat, RightCol, RowTop, 130));
  RowTop := RowTop + CControlStep;

  { Точность }
  CreateLabel(GroupFormat, 'Точность:', LeftCol, RowTop);
  MarkNotImplemented(CreateCombo(
    GroupFormat, RightCol, RowTop, 130));
  RowTop := RowTop + CControlStep;

  { Максимальное значение }
  CreateLabel(GroupFormat, 'Максимальное значение:',
    LeftCol, RowTop);
  MarkNotImplemented(CreateEdit(
    GroupFormat, RightCol, RowTop, CEditWidth));
  RowTop := RowTop + CControlStep;

  { Минимальное значение }
  CreateLabel(GroupFormat, 'Минимальное значение:',
    LeftCol, RowTop);
  MarkNotImplemented(CreateEdit(
    GroupFormat, RightCol, RowTop, CEditWidth));
  RowTop := RowTop + CControlStep;

  { Масштаб высоты }
  CreateLabel(GroupFormat, 'Масштаб высоты:',
    LeftCol, RowTop);
  MarkNotImplemented(CreateEdit(
    GroupFormat, RightCol, RowTop, CEditWidth));
  RowTop := RowTop + CControlStep;

  { Выравнивание }
  CreateLabel(GroupFormat, 'Выравнивание:',
    LeftCol, RowTop);
  MarkNotImplemented(CreateCombo(
    GroupFormat, RightCol, RowTop, 130));

  { --- Группа "Выравнивание допусков" --- }
  GroupAlign := TGroupBox.Create(Self);
  GroupAlign.Parent  := ATab;
  GroupAlign.Caption := 'Выравнивание допусков';
  GroupAlign.Left    := CControlMargin;
  GroupAlign.Top     := 245;
  GroupAlign.Width   := 275;
  GroupAlign.Height  := 60;

  RowTop := CGroupBoxPadTop;

  MarkNotImplemented(CreateRadio(GroupAlign,
    'Выровнять по десятичным разделителям',
    LeftCol, RowTop));
  RowTop := RowTop + 22;

  MarkNotImplemented(CreateRadio(GroupAlign,
    'Выровнять рабочие обозначения',
    LeftCol, RowTop));

  { --- Группа "Подавление нулей" --- }
  GroupZero := TGroupBox.Create(Self);
  GroupZero.Parent  := ATab;
  GroupZero.Caption := 'Подавление нулей';
  GroupZero.Left    := CControlMargin;
  GroupZero.Top     := 310;
  GroupZero.Width   := 275;
  GroupZero.Height  := 65;

  RowTop := CGroupBoxPadTop;

  MarkNotImplemented(CreateCheck(
    GroupZero, 'Ведущие', LeftCol, RowTop));
  MarkNotImplemented(CreateCheck(
    GroupZero, '0 футов', LeftCol + 120, RowTop));
  RowTop := RowTop + 22;

  MarkNotImplemented(CreateCheck(
    GroupZero, 'Хвостовые', LeftCol, RowTop));
  MarkNotImplemented(CreateCheck(
    GroupZero, '0 дюймов', LeftCol + 120, RowTop));

  { --- Группа "Допуски для альтернативных единиц" --- }
  GroupAltTol := TGroupBox.Create(Self);
  GroupAltTol.Parent  := ATab;
  GroupAltTol.Caption := 'Допуски для альтернативных единиц';
  GroupAltTol.Left    := 290;
  GroupAltTol.Top     := 240;
  GroupAltTol.Width   := 250;
  GroupAltTol.Height  := 55;

  RowTop := CGroupBoxPadTop;

  CreateLabel(GroupAltTol, 'Точность:', LeftCol, RowTop);
  MarkNotImplemented(CreateCombo(
    GroupAltTol, RightCol, RowTop, 100));

  { --- Группа "Подавление нулей" для альт. единиц --- }
  GroupAltZero := TGroupBox.Create(Self);
  GroupAltZero.Parent  := ATab;
  GroupAltZero.Caption := 'Подавление нулей';
  GroupAltZero.Left    := 290;
  GroupAltZero.Top     := 300;
  GroupAltZero.Width   := 250;
  GroupAltZero.Height  := 65;

  RowTop := CGroupBoxPadTop;

  MarkNotImplemented(CreateCheck(
    GroupAltZero, 'Ведущие', LeftCol, RowTop));
  MarkNotImplemented(CreateCheck(
    GroupAltZero, '0 футов', LeftCol + 120, RowTop));
  RowTop := RowTop + 22;

  MarkNotImplemented(CreateCheck(
    GroupAltZero, 'Хвостовые', LeftCol, RowTop));
  MarkNotImplemented(CreateCheck(
    GroupAltZero, '0 дюймов', LeftCol + 120, RowTop));
end;
