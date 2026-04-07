{ Включаемый файл: построение вкладки "Альт. единицы" }
{ Содержит процедуру BuildTabAltUnits для TDimStyleEditForm }

{ Создаёт элементы управления вкладки "Альт. единицы".
  Все параметры на данном этапе не разработаны —
  элементы отображаются заблокированными. }
procedure TDimStyleEditDialog.BuildTabAltUnits(
  ATab: TTabSheet);
var
  GroupAlt: TGroupBox;
  GroupZero: TGroupBox;
  GroupPlacement: TGroupBox;
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

  { Чекбокс включения альтернативных единиц }
  MarkNotImplemented(CreateCheck(ATab,
    'Разрешить альтернативные единицы',
    CControlMargin, CControlMargin));

  { --- Группа "Альтернативные единицы" --- }
  GroupAlt := TGroupBox.Create(Self);
  GroupAlt.Parent  := ATab;
  GroupAlt.Caption := 'Альтернативные единицы';
  GroupAlt.Left    := CControlMargin;
  GroupAlt.Top     := 35;
  GroupAlt.Width   := 275;
  GroupAlt.Height  := 230;

  RowTop := CGroupBoxPadTop;

  { Формат единиц }
  CreateLabel(GroupAlt, 'Формат единиц:',
    LeftCol, RowTop);
  MarkNotImplemented(CreateCombo(
    GroupAlt, RightCol, RowTop, 130));
  RowTop := RowTop + CControlStep;

  { Точность }
  CreateLabel(GroupAlt, 'Точность:', LeftCol, RowTop);
  MarkNotImplemented(CreateCombo(
    GroupAlt, RightCol, RowTop, 130));
  RowTop := RowTop + CControlStep;

  { Коэффициент пересчёта }
  CreateLabel(GroupAlt, 'Коэффициент пересчёта:',
    LeftCol, RowTop);
  MarkNotImplemented(CreateEdit(
    GroupAlt, RightCol, RowTop, CEditWidth));
  RowTop := RowTop + CControlStep;

  { Округление длин }
  CreateLabel(GroupAlt, 'Округление длин:',
    LeftCol, RowTop);
  MarkNotImplemented(CreateEdit(
    GroupAlt, RightCol, RowTop, CEditWidth));
  RowTop := RowTop + CControlStep;

  { Префикс }
  CreateLabel(GroupAlt, 'Префикс:', LeftCol, RowTop);
  MarkNotImplemented(CreateEdit(
    GroupAlt, RightCol, RowTop, 130));
  RowTop := RowTop + CControlStep;

  { Суффикс }
  CreateLabel(GroupAlt, 'Суффикс:', LeftCol, RowTop);
  MarkNotImplemented(CreateEdit(
    GroupAlt, RightCol, RowTop, 130));

  { --- Группа "Подавление нулей" --- }
  GroupZero := TGroupBox.Create(Self);
  GroupZero.Parent  := ATab;
  GroupZero.Caption := 'Подавление нулей';
  GroupZero.Left    := CControlMargin;
  GroupZero.Top     := 270;
  GroupZero.Width   := 275;
  GroupZero.Height  := 65;

  RowTop := CGroupBoxPadTop;

  MarkNotImplemented(CreateCheck(
    GroupZero, 'Ведущие', LeftCol, RowTop));
  MarkNotImplemented(CreateCheck(
    GroupZero, 'Хвостовые', LeftCol + 120, RowTop));
  RowTop := RowTop + 22;

  MarkNotImplemented(CreateCheck(
    GroupZero, '0 футов', LeftCol, RowTop));
  MarkNotImplemented(CreateCheck(
    GroupZero, '0 дюймов', LeftCol + 120, RowTop));

  { --- Группа "Размещение" --- }
  GroupPlacement := TGroupBox.Create(Self);
  GroupPlacement.Parent  := ATab;
  GroupPlacement.Caption := 'Размещение';
  GroupPlacement.Left    := 290;
  GroupPlacement.Top     := 240;
  GroupPlacement.Width   := 250;
  GroupPlacement.Height  := 80;

  RowTop := CGroupBoxPadTop;

  MarkNotImplemented(CreateRadio(GroupPlacement,
    'За основным значением', LeftCol, RowTop));
  RowTop := RowTop + 22;

  MarkNotImplemented(CreateRadio(GroupPlacement,
    'Под основным значением', LeftCol, RowTop));
end;
