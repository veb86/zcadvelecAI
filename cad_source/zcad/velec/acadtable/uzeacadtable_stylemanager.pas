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
  Модуль: uzeacadtable_stylemanager
  Назначение: Регистрация и применение DXF-стилей таблицы.
  Ищет стиль по хэндлу из DXF, заполняет структуру TTableStyle
  данными из TABLESTYLE-объекта. Резолвит текстовые стили.
  Зависимости: uzeacadtable_types, uzeacadtable_styles,
               uzedrawingdef, uzestylestablesdxf, uzestylestexts, uzclog
}

unit uzeacadtable_stylemanager;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeacadtable_types, uzeacadtable_styles,
  uzedrawingdef, uzestylestablesdxf, uzestylestexts,
  uzclog, uzbLogIntf;

// Применяет DXF-стиль таблицы к внутренней структуре TTableStyle.
// Ищет стиль по хэндлу (группа DXF 342 в ACAD_TABLE).
// Если стиль с нужным хэндлом не найден — берёт первый из таблицы.
procedure ApplyDXFTableStyle(
  var ATableStyle: TTableStyle;
  const AStyleHandle: String;
  var ADrawing: TDrawingDef);

// Резолвит указатель на текстовый стиль по имени
// с fallback к Standard, затем к первому доступному.
function ResolveTextStyle(
  const AStyleName: String;
  var ADrawing: TDrawingDef): PGDBTextStyle;

// Заполняет TCellStyle из TGDBDXFTableCellStyle и имени текстового стиля
procedure FillCellStyleFromDXF(
  var CellStyle: TCellStyle;
  const ADXF: TGDBDXFTableCellStyle;
  const ATextStyleName: String);

implementation

procedure FillCellStyleFromDXF(
  var CellStyle: TCellStyle;
  const ADXF: TGDBDXFTableCellStyle;
  const ATextStyleName: String);
begin
  InitCellStyle(CellStyle);
  // Имя текстового стиля
  CellStyle.TextStyle := ATextStyleName;
  if ATextStyleName <> '' then
    Include(CellStyle.Overrides, soTextStyle);
  // Высота текста
  if ADXF.TextHeight > 0 then
  begin
    CellStyle.TextHeight := ADXF.TextHeight;
    Include(CellStyle.Overrides, soTextHeight);
  end;
  // Цвет текста
  CellStyle.TextColor := ADXF.TextColor;
  Include(CellStyle.Overrides, soTextColor);
  // Выравнивание
  if ADXF.Alignment > 0 then
  begin
    CellStyle.HorzAlign := DXFAlignToHorz(ADXF.Alignment);
    CellStyle.VertAlign := DXFAlignToVert(ADXF.Alignment);
    Include(CellStyle.Overrides, soHAlign);
    Include(CellStyle.Overrides, soVAlign);
  end;
  // Фон
  if ADXF.BackgroundColorEnabled then
  begin
    CellStyle.HasBackground := True;
    CellStyle.BackgroundColor := ADXF.BackgroundColor;
    Include(CellStyle.Overrides, soBackground);
  end;
end;

procedure ApplyDXFTableStyle(
  var ATableStyle: TTableStyle;
  const AStyleHandle: String;
  var ADrawing: TDrawingDef);
var
  DXFStyleTable: PGDBDXFTableStyleArray;
  StylePtr: PTGDBDXFTableStyle;
  IterRec: itrec;
  CellStylePtr: PTGDBDXFTableCellStyle;
  CellFormatsIter: itrec;
  CellIdx: Integer;
begin
  DXFStyleTable := ADrawing.GetDXFTableStyleTable;
  if DXFStyleTable = nil then
  begin
    programlog.LogOutStr(
      'AcadTable: stylemanager: ApplyDXFTableStyle — ' +
      'таблица DXF-стилей недоступна', LM_Info);
    Exit;
  end;

  if DXFStyleTable^.count = 0 then
  begin
    programlog.LogOutStr(
      'AcadTable: stylemanager: ApplyDXFTableStyle — ' +
      'таблица DXF-стилей пуста', LM_Info);
    Exit;
  end;

  // Ищем стиль по хэндлу из ACAD_TABLE (группа 342)
  StylePtr := DXFStyleTable^.GetStyleByHandle(AStyleHandle);
  if StylePtr = nil then
  begin
    programlog.LogOutFormatStr(
      'AcadTable: stylemanager: ApplyDXFTableStyle — ' +
      'стиль с хэндлом "%s" не найден, ' +
      'используем первый стиль',
      [AStyleHandle], LM_Info);
    StylePtr := DXFStyleTable^.beginiterate(IterRec);
    if StylePtr = nil then
      Exit;
  end;

  programlog.LogOutFormatStr(
    'AcadTable: stylemanager: ApplyDXFTableStyle — ' +
    'применяем стиль "%s" (хэндл=%s)',
    [StylePtr^.Name, AStyleHandle], LM_Info);

  ATableStyle.Name := StylePtr^.Name;

  // Порядок хранения в DXF (TABLESTYLE):
  // 0=Data, 1=Title, 2=Header
  CellIdx := 0;
  CellStylePtr :=
    StylePtr^.CellFormats.beginiterate(CellFormatsIter);
  while (CellStylePtr <> nil) and (CellIdx < 3) do
  begin
    case CellIdx of
      0:
        begin
          // Первый блок — стиль Data (данные)
          FillCellStyleFromDXF(
            ATableStyle.DataCell, CellStylePtr^,
            StylePtr^.CellTextStyleName[0]);
          // Data используется как базовый стиль
          FillCellStyleFromDXF(
            ATableStyle.DefaultCell, CellStylePtr^,
            StylePtr^.CellTextStyleName[0]);
        end;
      1:
        // Второй блок — стиль Title (название)
        FillCellStyleFromDXF(
          ATableStyle.TitleCell, CellStylePtr^,
          StylePtr^.CellTextStyleName[1]);
      2:
        // Третий блок — стиль Header (заголовок)
        FillCellStyleFromDXF(
          ATableStyle.HeaderCell, CellStylePtr^,
          StylePtr^.CellTextStyleName[2]);
    end;
    Inc(CellIdx);
    CellStylePtr :=
      StylePtr^.CellFormats.iterate(CellFormatsIter);
  end;

  programlog.LogOutFormatStr(
    'AcadTable: stylemanager: ApplyDXFTableStyle OK ' +
    'стиль="%s" title_height=%.2f ' +
    'header_height=%.2f data_height=%.2f',
    [ATableStyle.Name,
     ATableStyle.TitleCell.TextHeight,
     ATableStyle.HeaderCell.TextHeight,
     ATableStyle.DataCell.TextHeight], LM_Info);
end;

function ResolveTextStyle(
  const AStyleName: String;
  var ADrawing: TDrawingDef): PGDBTextStyle;
begin
  Result := nil;
  if AStyleName <> '' then
    Result :=
      ADrawing.GetTextStyleTable^.FindStyle(AStyleName, False);
  if Result = nil then
    Result :=
      ADrawing.GetTextStyleTable^.FindStyle('Standard', False);
  if (Result = nil) and
     (ADrawing.GetTextStyleTable^.Count > 0) then
    Result := PGDBTextStyle(
      pointer(ADrawing.GetTextStyleTable^.getDataMutable(0)));
end;

end.
