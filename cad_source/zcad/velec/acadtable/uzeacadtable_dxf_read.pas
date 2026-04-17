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
  Модуль: uzeacadtable_dxf_read
  Назначение: Импорт данных таблицы ACAD_TABLE из DXF-потока.
  Читает группы DXF: точку вставки, размеры, тексты ячеек,
  параметры разрыва, хэндл стиля таблицы, объединения ячеек.
  Зависимости: uzeacadtable_types, uzeacadtable_styles,
               uzeffdxfsupport, uzMVReader, uzclog
}

unit uzeacadtable_dxf_read;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzeacadtable_types, uzeacadtable_styles,
  uzegeometrytypes, uzeffdxfsupport, uzMVReader,
  uzctnrvectordouble,
  uzclog, uzbLogIntf;

type
  // Результат загрузки из DXF — все данные таблицы
  TAcadTableDXFData = record
    InsertPoint: TzePoint3d;
    RowCount: Integer;
    ColCount: Integer;
    RowHeights: TZctnrVectorDouble;
    ColWidths: TZctnrVectorDouble;
    CellTexts: array of String;
    TableStyleHandle: String;
    TableFlags: Integer;
    // Параметры разрыва
    BreakEnabled: Boolean;
    BreakDirection: TAcadTableBreakDirection;
    BreakRepeatTopLabels: Boolean;
    BreakRepeatBottomLabels: Boolean;
    BreakManualPosition: Boolean;
    BreakManualHeight: Boolean;
    BreakSpacing: Double;
    // Данные ячеек
    CellAlignments: array of Integer;
    CellColSpans: array of Integer;
    CellRowSpans: array of Integer;
    CellVirtualFlags: array of Boolean;
  end;

// Загружает данные таблицы из DXF-потока AcDbTable.
// Вызывается после того, как маркер AcDbTable уже прочитан.
procedure ReadAcadTableFromDXF(
  var ARdr: TZMemReader;
  var AContext: TIODXFLoadContext;
  var AData: TAcadTableDXFData);

implementation

// Расширяет массив до нужного индекса и записывает текст
procedure SetCellTextByIndex(
  var AData: TAcadTableDXFData;
  ACellIdx: Integer; const AText: String);
begin
  if ACellIdx < 0 then Exit;
  if ACellIdx >= CAcadTableMaxCells then
  begin
    programlog.LogOutFormatStr(
      'AcadTable: dxf_read: SetCellTextByIndex — ' +
      'индекс %d превышает лимит %d',
      [ACellIdx, CAcadTableMaxCells], LM_Info);
    Exit;
  end;
  if ACellIdx > High(AData.CellTexts) then
    System.SetLength(AData.CellTexts, ACellIdx + 1);
  AData.CellTexts[ACellIdx] := AText;
end;

// Сохраняет данные текущей ячейки во временные массивы
procedure SaveCellData(
  var AData: TAcadTableDXFData;
  ACellIndex: Integer;
  ACellIsVirtual: Boolean;
  ACellAlignmentVal: Integer;
  ACellColSpan, ACellRowSpan: Integer);
begin
  if ACellIndex >= Length(AData.CellVirtualFlags) then
    System.SetLength(AData.CellVirtualFlags, ACellIndex + 1);
  AData.CellVirtualFlags[ACellIndex] := ACellIsVirtual;
  if ACellIndex >= Length(AData.CellAlignments) then
    System.SetLength(AData.CellAlignments, ACellIndex + 1);
  AData.CellAlignments[ACellIndex] := ACellAlignmentVal;
  if ACellIndex >= Length(AData.CellColSpans) then
    System.SetLength(AData.CellColSpans, ACellIndex + 1);
  AData.CellColSpans[ACellIndex] := ACellColSpan;
  if ACellIndex >= Length(AData.CellRowSpans) then
    System.SetLength(AData.CellRowSpans, ACellIndex + 1);
  AData.CellRowSpans[ACellIndex] := ACellRowSpan;
end;

procedure ReadAcadTableFromDXF(
  var ARdr: TZMemReader;
  var AContext: TIODXFLoadContext;
  var AData: TAcadTableDXFData);
var
  GroupCode: Integer;
  CellText: String;
  DoubleVal: Double;
  CellIndex: Integer;
  RowHeightsRead, ColWidthsRead: Boolean;
  InCellData, CellTextRead: Boolean;
  RowHeightCount, ColWidthCount: Integer;
  CellIsVirtual: Boolean;
  CellAlignmentVal, CellColSpan, CellRowSpan: Integer;
begin
  programlog.LogOutStr(
    'AcadTable: dxf_read: ReadAcadTableFromDXF START',
    LM_Info);

  CellIndex := 0;
  RowHeightsRead := False;
  ColWidthsRead := False;
  InCellData := False;
  CellTextRead := False;
  CellIsVirtual := False;
  CellAlignmentVal := 0;
  CellColSpan := 1;
  CellRowSpan := 1;
  RowHeightCount := 0;
  ColWidthCount := 0;
  System.SetLength(AData.CellTexts, 0);
  System.SetLength(AData.CellVirtualFlags, 0);
  System.SetLength(AData.CellAlignments, 0);
  System.SetLength(AData.CellColSpans, 0);
  System.SetLength(AData.CellRowSpans, 0);

  GroupCode := ARdr.ParseInteger;

  while GroupCode <> 0 do
  begin
    case GroupCode of
      // Точка вставки
      10: AData.InsertPoint.x := ARdr.ParseDouble;
      20: AData.InsertPoint.y := ARdr.ParseDouble;
      30: AData.InsertPoint.z := ARdr.ParseDouble;

      // Количество строк — только первый раз
      91:
      begin
        if not RowHeightsRead then
        begin
          AData.RowCount := ARdr.ParseInteger;
          if (AData.RowCount < 0) or
             (AData.RowCount > CAcadTableMaxDimension) then
            AData.RowCount := 0;
          programlog.LogOutFormatStr(
            'AcadTable: dxf_read: RowCount=%d',
            [AData.RowCount], LM_Info);
        end
        else
          ARdr.SkipString;
      end;

      // Количество столбцов — только первый раз
      92:
      begin
        if not RowHeightsRead then
        begin
          AData.ColCount := ARdr.ParseInteger;
          if (AData.ColCount < 0) or
             (AData.ColCount > CAcadTableMaxDimension) then
            AData.ColCount := 0;
          programlog.LogOutFormatStr(
            'AcadTable: dxf_read: ColCount=%d',
            [AData.ColCount], LM_Info);
        end
        else
          ARdr.SkipString;
      end;

      // Параметры разрыва (только вне блока ячейки)
      292:
      begin
        if not InCellData then
          AData.BreakEnabled := ARdr.ParseInteger <> 0
        else
          ARdr.SkipString;
      end;

      282:
      begin
        if not InCellData then
        begin
          case ARdr.ParseInteger of
            2: AData.BreakDirection := atbdDown;
            3: AData.BreakDirection := atbdLeft;
          else
            AData.BreakDirection := atbdRight;
          end;
        end
        else
          ARdr.SkipString;
      end;

      291:
      begin
        if not InCellData then
          AData.BreakRepeatTopLabels :=
            ARdr.ParseInteger <> 0
        else
          ARdr.SkipString;
      end;

      294:
      begin
        if not InCellData then
          AData.BreakRepeatBottomLabels :=
            ARdr.ParseInteger <> 0
        else
          ARdr.SkipString;
      end;

      293:
      begin
        if not InCellData then
          AData.BreakManualPosition :=
            ARdr.ParseInteger <> 0
        else
          ARdr.SkipString;
      end;

      295:
      begin
        if not InCellData then
          AData.BreakManualHeight :=
            ARdr.ParseInteger <> 0
        else
          ARdr.SkipString;
      end;

      146:
      begin
        if not InCellData then
          AData.BreakSpacing := ARdr.ParseDouble
        else
          ARdr.SkipString;
      end;

      // Высота строки (повторяется RowCount раз)
      141:
      begin
        DoubleVal := ARdr.ParseDouble;
        if DoubleVal <= 0 then
          DoubleVal := CAcadTableDefaultRowHeight;
        AData.RowHeights.PushBackData(DoubleVal);
        RowHeightsRead := True;
        Inc(RowHeightCount);
        programlog.LogOutFormatStr(
          'AcadTable: dxf_read: RowHeight[%d]=%.2f',
          [RowHeightCount - 1, DoubleVal], LM_Info);
      end;

      // Ширина столбца (повторяется ColCount раз)
      142:
      begin
        DoubleVal := ARdr.ParseDouble;
        if DoubleVal <= 0 then
          DoubleVal := CAcadTableDefaultColWidth;
        AData.ColWidths.PushBackData(DoubleVal);
        ColWidthsRead := True;
        Inc(ColWidthCount);
        programlog.LogOutFormatStr(
          'AcadTable: dxf_read: ColWidth[%d]=%.2f',
          [ColWidthCount - 1, DoubleVal], LM_Info);
      end;

      // Начало данных ячейки — код 171 (тип ячейки)
      171:
      begin
        InCellData := True;
        CellTextRead := False;
        CellIsVirtual := False;
        CellAlignmentVal := 0;
        CellColSpan := 1;
        CellRowSpan := 1;
        ARdr.SkipString;
      end;

      // Флаг виртуальной ячейки — код 173
      173:
      begin
        if InCellData then
          CellIsVirtual := ARdr.ParseInteger <> 0
        else
          ARdr.SkipString;
      end;

      // Выравнивание ячейки — код 170
      170:
      begin
        if InCellData then
          CellAlignmentVal := ARdr.ParseInteger
        else
          ARdr.SkipString;
      end;

      // ColSpan — код 175
      175:
      begin
        if InCellData then
          CellColSpan := ARdr.ParseInteger
        else
          ARdr.SkipString;
      end;

      // RowSpan — код 176
      176:
      begin
        if InCellData then
          CellRowSpan := ARdr.ParseInteger
        else
          ARdr.SkipString;
      end;

      // Текст ячейки — код 302 (DXF 2007+, приоритет)
      302:
      begin
        if InCellData and not CellTextRead then
        begin
          dxfLoadString(ARdr, CellText, AContext.Header);
          SetCellTextByIndex(AData, CellIndex, CellText);
          SaveCellData(AData, CellIndex,
            CellIsVirtual, CellAlignmentVal,
            CellColSpan, CellRowSpan);
          programlog.LogOutFormatStr(
            'AcadTable: dxf_read: Cell[%d] text="%s" ' +
            'virtual=%d align=%d ' +
            'colSpan=%d rowSpan=%d',
            [CellIndex, CellText, Ord(CellIsVirtual),
             CellAlignmentVal,
             CellColSpan, CellRowSpan], LM_Info);
          Inc(CellIndex);
          CellTextRead := True;
        end
        else
          ARdr.SkipString;
      end;

      // Текст ячейки (старый формат, код 1)
      1:
      begin
        if InCellData and not CellTextRead then
        begin
          dxfLoadString(ARdr, CellText, AContext.Header);
          SetCellTextByIndex(AData, CellIndex, CellText);
          SaveCellData(AData, CellIndex,
            CellIsVirtual, CellAlignmentVal,
            CellColSpan, CellRowSpan);
          programlog.LogOutFormatStr(
            'AcadTable: dxf_read: Cell[%d] text="%s" ' +
            'virtual=%d align=%d ' +
            'colSpan=%d rowSpan=%d',
            [CellIndex, CellText, Ord(CellIsVirtual),
             CellAlignmentVal,
             CellColSpan, CellRowSpan], LM_Info);
          Inc(CellIndex);
          CellTextRead := True;
        end
        else
          ARdr.SkipString;
      end;

      // Флаги свойств таблицы (group code 90)
      90:
      begin
        if not RowHeightsRead then
        begin
          AData.TableFlags := ARdr.ParseInteger;
          programlog.LogOutFormatStr(
            'AcadTable: dxf_read: TableFlags=0x%x ' +
            '(titleSup=%d headerSup=%d)',
            [AData.TableFlags,
             Ord((AData.TableFlags and 2) <> 0),
             Ord((AData.TableFlags and 4) <> 0)],
            LM_Info);
        end
        else
          ARdr.SkipString;
      end;

      // Хэндл стиля таблицы
      342:
      begin
        AData.TableStyleHandle := ARdr.ParseString;
        programlog.LogOutFormatStr(
          'AcadTable: dxf_read: TableStyleHandle="%s"',
          [AData.TableStyleHandle], LM_Info);
      end;

      // Пропускаем служебные коды ячеек
      11, 21, 31, 93, 94, 95, 96, 172, 174, 177, 178,
      145, 300, 301, 304, 274, 275, 276, 278, 279, 280,
      281, 283, 284, 285, 286, 288, 289, 63, 64, 65,
      66, 68, 69, 70, 40, 41, 343, 344, 340:
        ARdr.SkipString;
    else
      ARdr.SkipString;
    end;

    GroupCode := ARdr.ParseInteger;
  end;

  programlog.LogOutFormatStr(
    'AcadTable: dxf_read: ReadAcadTableFromDXF END ' +
    'rows=%d cols=%d cells=%d merges(virtual)=%d',
    [AData.RowCount, AData.ColCount,
     Length(AData.CellTexts),
     Length(AData.CellVirtualFlags)], LM_Info);
  programlog.LogOutFormatStr(
    'AcadTable: dxf_read: BreakData enabled=%d dir=%d ' +
    'repeatTop=%d repeatBottom=%d manualPos=%d ' +
    'manualHeight=%d spacing=%.3f',
    [Ord(AData.BreakEnabled),
     Ord(AData.BreakDirection),
     Ord(AData.BreakRepeatTopLabels),
     Ord(AData.BreakRepeatBottomLabels),
     Ord(AData.BreakManualPosition),
     Ord(AData.BreakManualHeight),
     AData.BreakSpacing], LM_Info);
end;

end.
