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
  Модуль: uzeacadtable_styles
  Назначение: Инициализация стилей ячеек и вспомогательные функции
  для работы со стилями AcadTable.
  Зависимости: uzeacadtable_types
}

unit uzeacadtable_styles;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeacadtable_types;

// Инициализирует стиль ячейки значениями по умолчанию
procedure InitCellStyle(var ACellStyle: TCellStyle);

// Инициализирует табличный стиль значениями по умолчанию
procedure InitTableStyle(var ATableStyle: TTableStyle);

// Преобразует числовое значение выравнивания AutoCAD
// в горизонтальный компонент.
// AutoCAD: 1=TopLeft, 2=TopCenter, 3=TopRight, 4-6=Middle*, 7-9=Bottom*
function DXFAlignToHorz(Alignment: Integer): THorzAlign;

// Преобразует числовое значение выравнивания AutoCAD
// в вертикальный компонент.
// AutoCAD: 1-3=Top, 4-6=Middle, 7-9=Bottom
function DXFAlignToVert(Alignment: Integer): TVertAlign;

implementation

procedure InitCellStyle(var ACellStyle: TCellStyle);
begin
  ACellStyle.TextStyle := '';
  ACellStyle.TextHeight := CAcadTableDefaultTextHeight;
  ACellStyle.TextColor := 0;
  ACellStyle.HorzAlign := haLeft;
  ACellStyle.VertAlign := vaMiddle;
  ACellStyle.HasBackground := False;
  ACellStyle.BackgroundColor := 0;
  ACellStyle.Borders := [];
  ACellStyle.BorderColor := 0;
  ACellStyle.Overrides := [];
end;

procedure InitTableStyle(var ATableStyle: TTableStyle);
begin
  InitCellStyle(ATableStyle.DefaultCell);
  InitCellStyle(ATableStyle.TitleCell);
  InitCellStyle(ATableStyle.HeaderCell);
  InitCellStyle(ATableStyle.DataCell);
  ATableStyle.DefaultRowHeight := CAcadTableDefaultRowHeight;
  ATableStyle.DefaultColWidth := CAcadTableDefaultColWidth;
end;

function DXFAlignToHorz(Alignment: Integer): THorzAlign;
var
  HorzPart: Integer;
begin
  // Горизонтальная часть — остаток от 3 (0=Left, 1=Center, 2=Right)
  HorzPart := (Alignment - 1) mod 3;
  case HorzPart of
    1: Result := haCenter;
    2: Result := haRight;
  else
    Result := haLeft;
  end;
end;

function DXFAlignToVert(Alignment: Integer): TVertAlign;
var
  VertPart: Integer;
begin
  // Вертикальная часть — округление вниз деления на 3
  VertPart := (Alignment - 1) div 3;
  case VertPart of
    1: Result := vaMiddle;
    2: Result := vaBottom;
  else
    Result := vaTop;
  end;
end;

end.
