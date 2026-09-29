import React, { useState } from 'react';
import { TableFragment, TableCell } from '../types/dxf';
import { AutoCADPropertyInspector } from './AutoCADPropertyInspector';
import {
  Table,
  Download,
  Layers,
  Hash,
  Move,
  Grid,
  Split,
  CheckCircle2,
  XCircle,
  Info,
  AlignLeft,
  AlignCenter,
  AlignRight,
  Type,
  FileSpreadsheet,
  Sliders,
  Filter,
  ArrowDown,
  ArrowRight,
} from 'lucide-react';

interface TableDetailsProps {
  table: TableFragment | null;
  allTables: TableFragment[];
  onSelectTable: (table: TableFragment) => void;
}

export const TableDetails: React.FC<TableDetailsProps> = ({
  table,
  allTables,
  onSelectTable,
}) => {
  const [selectedCell, setSelectedCell] = useState<TableCell | null>(null);
  const [viewMode, setViewMode] = useState<'properties' | 'grid' | 'all-cells'>('properties');
  const [rowStyleFilter, setRowStyleFilter] = useState<'ALL' | 'Title' | 'Header' | 'Data'>('ALL');

  if (!table) {
    return (
      <div className="flex flex-col items-center justify-center p-8 text-center text-slate-400 border border-dashed border-slate-800 rounded-xl bg-slate-900/30">
        <Table className="w-10 h-10 mb-3 text-slate-600" />
        <p className="text-sm font-medium">No ACAD_TABLE selected</p>
        <p className="text-xs text-slate-500 mt-1">
          Select a table fragment from the list or click on one in the CAD canvas
        </p>
      </div>
    );
  }

  // Active cell: either the user-selected cell or default to the first non-empty cell
  const currentCell =
    selectedCell && selectedCell.row < table.rows && selectedCell.col < table.cols
      ? selectedCell
      : table.cells.find((c) => c.text.length > 0) || table.cells[0] || null;

  // Export current table to CSV with rich metadata
  const exportToCSV = () => {
    const headers = [
      'Row',
      'Col',
      'Text',
      'CellStyle',
      'RowStyle',
      'ColumnStyle',
      'Alignment',
      'AlignmentCode_DXF170',
      'CellType_DXF171',
      'ColWidth_DXF142',
      'RowHeight_DXF141',
      'ColSpan_DXF175',
      'RowSpan_DXF176',
      'IsMerged_DXF173',
      'TextStyle_DXF7',
      'TextHeight_DXF140',
      'Rotation_DXF145',
      'OverrideFlags_DXF91',
    ];

    const rows: string[][] = [headers];

    table.cells.forEach((cl) => {
      rows.push([
        String(cl.row),
        String(cl.col),
        `"${(cl.text || '').replace(/"/g, '""')}"`,
        cl.cellStyle || 'по строке/столбцу',
        cl.rowStyle || 'Data',
        cl.colStyle || 'нет',
        cl.alignmentName || 'Middle Center',
        String(cl.alignmentCode ?? 5),
        cl.cellType || 'Text',
        String(cl.width ?? table.columnWidths[cl.col] ?? 2.5),
        String(cl.height ?? table.rowHeights[cl.row] ?? 0.36),
        String(cl.colSpan ?? 1),
        String(cl.rowSpan ?? 1),
        cl.isMerged ? 'TRUE' : 'FALSE',
        cl.textStyle || 'Standard',
        String(cl.textHeight ?? 0.18),
        String(cl.rotation ?? 0),
        String(cl.overrideFlags ?? ''),
      ]);
    });

    const csvContent = rows.map((r) => r.join(',')).join('\n');
    const blob = new Blob([csvContent], { type: 'text/csv;charset=utf-8;' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    link.download = `table_${table.handle}_full_cells.csv`;
    link.click();
    URL.revokeObjectURL(url);
  };

  const filteredCells = table.cells.filter((c) => {
    if (rowStyleFilter === 'ALL') return true;
    return (c.rowStyle || 'Data') === rowStyleFilter;
  });

  const getRowStyleBadge = (style?: 'Title' | 'Header' | 'Data') => {
    switch (style) {
      case 'Title':
        return (
          <span className="px-1.5 py-0.5 rounded text-[10px] font-semibold bg-amber-500/20 text-amber-300 border border-amber-500/30">
            TITLE
          </span>
        );
      case 'Header':
        return (
          <span className="px-1.5 py-0.5 rounded text-[10px] font-semibold bg-blue-500/20 text-blue-300 border border-blue-500/30">
            HEADER
          </span>
        );
      default:
        return (
          <span className="px-1.5 py-0.5 rounded text-[10px] font-semibold bg-slate-800 text-slate-300 border border-slate-700">
            DATA
          </span>
        );
    }
  };

  return (
    <div className="flex flex-col gap-4">
      {/* Fragment Selector Pills */}
      {allTables.length > 1 && (
        <div className="flex flex-wrap items-center gap-2 pb-2 border-b border-slate-800">
          <span className="text-xs font-semibold text-slate-400 uppercase tracking-wider">
            Фрагменты ({allTables.length}):
          </span>
          {allTables.map((t, idx) => (
            <button
              key={t.id}
              onClick={() => {
                onSelectTable(t);
                setSelectedCell(null);
              }}
              className={`px-2.5 py-1 text-xs rounded-md font-mono transition-all ${
                t.id === table.id
                  ? 'bg-blue-600 text-white shadow-sm font-semibold'
                  : 'bg-slate-800 hover:bg-slate-700 text-slate-300'
              }`}
            >
              Фрагмент #{idx + 1} ({t.handle})
            </button>
          ))}
        </div>
      )}

      {/* Metadata Overview Cards */}
      <div className="grid grid-cols-2 sm:grid-cols-3 lg:grid-cols-6 gap-2.5">
        <div className="bg-slate-900/70 border border-slate-800 p-3 rounded-lg">
          <div className="flex items-center gap-1.5 text-xs text-slate-400 mb-1">
            <Hash className="w-3.5 h-3.5 text-blue-400" />
            <span>Handle</span>
          </div>
          <div className="font-mono text-sm font-semibold text-slate-100">
            {table.handle}
          </div>
        </div>

        <div className="bg-slate-900/70 border border-slate-800 p-3 rounded-lg">
          <div className="flex items-center gap-1.5 text-xs text-slate-400 mb-1">
            <Move className="w-3.5 h-3.5 text-cyan-400" />
            <span>Insert (X, Y)</span>
          </div>
          <div className="font-mono text-xs font-semibold text-slate-100 truncate">
            {table.x.toFixed(1)}, {table.y.toFixed(1)}
          </div>
        </div>

        <div className="bg-slate-900/70 border border-slate-800 p-3 rounded-lg">
          <div className="flex items-center gap-1.5 text-xs text-slate-400 mb-1">
            <Grid className="w-3.5 h-3.5 text-amber-400" />
            <span>Размер таблицы</span>
          </div>
          <div className="font-mono text-sm font-semibold text-slate-100">
            {table.totalTableRows || table.rows} × {table.cols}
          </div>
          <div className="text-[10px] text-slate-500 font-mono">
            фрагм: {table.rows} стр.
          </div>
        </div>

        <div className="bg-slate-900/70 border border-slate-800 p-3 rounded-lg">
          <div className="flex items-center gap-1.5 text-xs text-slate-400 mb-1">
            <ArrowDown className="w-3.5 h-3.5 text-indigo-400" />
            <span>Направление</span>
          </div>
          <div className="font-mono text-sm font-semibold text-indigo-300">
            {table.flowDirection || 'Вниз'}
          </div>
          <div className="text-[10px] text-slate-500">
            Flow Direction
          </div>
        </div>

        <div className="bg-slate-900/70 border border-slate-800 p-3 rounded-lg">
          <div className="flex items-center gap-1.5 text-xs text-slate-400 mb-1">
            <ArrowRight className="w-3.5 h-3.5 text-purple-400" />
            <span>Разрыв: напр.</span>
          </div>
          <div className="font-mono text-sm font-semibold text-purple-300">
            {table.breakDirection || 'Вправо'}
          </div>
          <div className="text-[10px] text-slate-500 font-mono">
            {table.breakSpacing ? `Интервал: ${table.breakSpacing.toFixed(2)}` : 'Интервал: 0.99'}
          </div>
        </div>

        <div className="bg-slate-900/70 border border-slate-800 p-3 rounded-lg">
          <div className="flex items-center gap-1.5 text-xs text-slate-400 mb-1">
            <Layers className="w-3.5 h-3.5 text-emerald-400" />
            <span>Слой (Layer)</span>
          </div>
          <div className="font-mono text-xs font-semibold text-slate-100 truncate">
            {table.layer || '0'}
          </div>
        </div>
      </div>

      {/* Fragment Break Height, Manual Height, and Position Engine */}
      <div className="bg-slate-900/90 border border-indigo-900/50 rounded-xl p-4 text-xs">
        <div className="flex flex-wrap items-center justify-between gap-3 pb-3 mb-3 border-b border-slate-800">
          <div className="flex items-center gap-2">
            <Sliders className="w-4 h-4 text-indigo-400" />
            <span className="font-bold text-sm text-indigo-200">
              Высота разбиения и геометрия фрагмента
            </span>
            <span className="text-[11px] font-mono px-2 py-0.5 rounded bg-indigo-950/80 text-indigo-300 border border-indigo-800/60">
              {table.fragmentIndex ? `Фрагмент ${table.fragmentIndex} из ${table.totalFragments}` : 'Одиночная таблица'}
            </span>
          </div>
          <div className="flex items-center gap-2">
            <span className="text-slate-400 text-[11px]">Режим позиции:</span>
            <span className={`px-2 py-0.5 rounded text-[11px] font-semibold border ${
              table.positionMode === 'manual'
                ? 'bg-amber-500/20 text-amber-300 border-amber-500/40'
                : 'bg-blue-500/20 text-blue-300 border-blue-500/40'
            }`}>
              {table.positionMode === 'manual' ? 'Произвольная позиция (Manual)' : 'Автоматическое смещение'}
            </span>
          </div>
        </div>

        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-3">
          {/* Current Break Height */}
          <div className="bg-slate-950/70 border border-slate-800 p-3 rounded-lg flex flex-col justify-between">
            <div className="flex items-center justify-between text-slate-400 text-[11px] mb-1">
              <span>Высота разбиения (Break Height)</span>
              <Split className="w-3.5 h-3.5 text-purple-400" />
            </div>
            <div className="text-lg font-bold font-mono text-purple-300">
              {table.breakHeight.toFixed(3)} <span className="text-xs font-normal text-slate-400">мм</span>
            </div>
            <div className="text-[10px] text-slate-500 mt-1">
              Высота, после которой строчки переносятся в следующий фрагмент
            </div>
          </div>

          {/* Manual Break Height Parameter (Задание высоты вручную) */}
          <div className="bg-slate-950/70 border border-slate-800 p-3 rounded-lg flex flex-col justify-between">
            <div className="flex items-center justify-between text-slate-400 text-[11px] mb-1">
              <span className="font-semibold text-emerald-300">Задание высоты вручную</span>
              <Sliders className="w-3.5 h-3.5 text-emerald-400" />
            </div>
            <div className="flex items-center gap-2 mt-0.5">
              <input
                type="number"
                step="0.1"
                min="1"
                value={table.manualBreakHeight ?? parseFloat(table.breakHeight.toFixed(3))}
                onChange={(e) => {
                  const val = parseFloat(e.target.value) || 0;
                  table.manualBreakHeight = val;
                  table.isManualBreakHeight = true;
                  // Trigger local re-render by toggling selectedCell if needed
                  setSelectedCell((prev) => (prev ? { ...prev } : null));
                }}
                className="w-24 px-2 py-1 bg-slate-900 border border-emerald-700/60 rounded text-sm font-mono text-emerald-200 font-bold focus:outline-none focus:border-emerald-400"
              />
              <span className="text-xs text-slate-400 font-mono">мм</span>
              {table.isManualBreakHeight && (
                <span className="text-[10px] px-1.5 py-0.5 rounded bg-emerald-900/40 text-emerald-300 border border-emerald-700/50">
                  Задано
                </span>
              )}
            </div>
            <div className="text-[10px] text-slate-500 mt-1">
              Параметр ручного лимита высоты фрагмента в структуре таблицы
            </div>
          </div>

          {/* Fragment Position Coordinates */}
          <div className="bg-slate-950/70 border border-slate-800 p-3 rounded-lg flex flex-col justify-between">
            <div className="flex items-center justify-between text-slate-400 text-[11px] mb-1">
              <span>Координаты (X, Y, Z)</span>
              <Move className="w-3.5 h-3.5 text-cyan-400" />
            </div>
            <div className="text-xs font-mono font-semibold text-cyan-300 space-y-0.5">
              <div>X: <span className="text-slate-100">{table.x.toFixed(3)}</span></div>
              <div>Y: <span className="text-slate-100">{table.y.toFixed(3)}</span></div>
            </div>
            {table.deltaFromPrevious && (
              <div className="text-[10px] font-mono text-cyan-400/80 mt-1 border-t border-slate-800/80 pt-1">
                Δ от пред.: dX={table.deltaFromPrevious.dx.toFixed(2)}, dY={table.deltaFromPrevious.dy.toFixed(2)}
              </div>
            )}
          </div>

          {/* Header vs Data Heights breakdown */}
          <div className="bg-slate-950/70 border border-slate-800 p-3 rounded-lg flex flex-col justify-between">
            <div className="text-slate-400 text-[11px] mb-1">Составляющие высоты</div>
            <div className="text-xs font-mono space-y-1">
              <div className="flex justify-between">
                <span className="text-amber-400">Шапка / Метки:</span>
                <span className="text-slate-200">{table.headerHeight ? table.headerHeight.toFixed(3) : '—'} мм</span>
              </div>
              <div className="flex justify-between">
                <span className="text-blue-400">Данные:</span>
                <span className="text-slate-200">{table.dataHeight ? table.dataHeight.toFixed(3) : '—'} мм</span>
              </div>
              <div className="flex justify-between border-t border-slate-800 pt-0.5 text-[11px]">
                <span className="text-slate-400">Строк:</span>
                <span className="text-slate-300">{table.rows} шт</span>
              </div>
            </div>
          </div>
        </div>

        {/* Repeat Top Labels (Повторение верхних меток) Parameter Card */}
        <div className="mt-3 p-3 rounded-lg bg-slate-950/70 border border-indigo-900/40">
          <div className="flex items-center justify-between flex-wrap gap-2 mb-2">
            <div className="flex items-center gap-2">
              <Layers className="w-4 h-4 text-indigo-400" />
              <span className="text-xs font-bold text-indigo-200">
                Параметр повторения верхних меток (Repeat Top Labels)
              </span>
              <button
                type="button"
                onClick={() => {
                  table.repeatTopLabels = !table.repeatTopLabels;
                  setSelectedCell((prev) => (prev ? { ...prev } : null));
                }}
                className={`text-[11px] px-2.5 py-0.5 rounded font-mono font-semibold transition-colors cursor-pointer border ${
                  table.repeatTopLabels
                    ? 'bg-indigo-600/30 text-indigo-200 border-indigo-500/50 hover:bg-indigo-600/50'
                    : 'bg-slate-800 text-slate-400 border-slate-700 hover:bg-slate-700'
                }`}
              >
                {table.repeatTopLabels ? '✓ Повторение включено' : '✕ Повторение отключено'}
              </button>
            </div>
            <div className="text-[11px] font-mono text-indigo-300">
              Метки: <span className="font-bold text-slate-100">{table.topLabelsRowCount || 0}</span> строк
              {table.topLabelsHeight ? ` (${table.topLabelsHeight.toFixed(2)} мм)` : ''}
            </div>
          </div>
          <div className="text-[11px] text-slate-400 leading-relaxed">
            {table.repeatTopLabels ? (
              <span>
                <span className="text-emerald-400 font-medium">Активно:</span> На каждую разбитую часть таблицы в первых строчках выводятся первые строчки таблицы с типом{' '}
                <span className="text-amber-300 font-semibold">Title</span> и{' '}
                <span className="text-sky-300 font-semibold">Header</span>. Они повторяются до тех пор, пока не встретят{' '}
                <span className="text-emerald-300 font-semibold">Data</span>.
              </span>
            ) : (
              <span>
                <span className="text-slate-500">Отключено:</span> Верхние метки выводятся только в первом фрагменте. Во все последующие разбитые фрагменты сразу выводятся строки Data без дублирования шапки.
              </span>
            )}
          </div>
          {table.topLabelRowStyles && table.topLabelRowStyles.length > 0 && (
            <div className="mt-2 flex items-center gap-1.5 flex-wrap text-[10px] font-mono">
              <span className="text-slate-500">Повторяемые строки верхних меток:</span>
              {table.topLabelRowStyles.map((st, idx) => (
                <span
                  key={idx}
                  className={`px-1.5 py-0.5 rounded border ${
                    st === 'Title'
                      ? 'bg-amber-950/40 text-amber-300 border-amber-800/60 font-semibold'
                      : 'bg-sky-950/40 text-sky-300 border-sky-800/60'
                  }`}
                >
                  R{idx}: {st}
                </span>
              ))}
              <span className="text-slate-500">→ затем Data</span>
            </div>
          )}
        </div>

        {/* Multi-Fragment Breakdown List */}
        {allTables.length > 1 && (
          <div className="mt-3 pt-3 border-t border-slate-800/80">
            <div className="text-[11px] font-semibold text-slate-300 mb-2">
              Сводная сетка высот и позиций всех фрагментов таблицы:
            </div>
            <div className="grid grid-cols-1 sm:grid-cols-3 gap-2 font-mono text-[11px]">
              {allTables.map((t, i) => (
                <div
                  key={t.id}
                  onClick={() => onSelectTable(t)}
                  className={`p-2.5 rounded-lg border cursor-pointer transition-all ${
                    t.id === table.id
                      ? 'bg-indigo-950/40 border-indigo-500 text-indigo-100 shadow-sm'
                      : 'bg-slate-950/50 border-slate-800/80 hover:border-slate-700 text-slate-400'
                  }`}
                >
                  <div className="flex items-center justify-between font-bold mb-1">
                    <span>Фрагмент #{i + 1} ({t.handle})</span>
                    <span className="text-purple-300">H: {t.breakHeight.toFixed(2)}мм</span>
                  </div>
                  <div className="text-[10px] text-slate-400">
                    Поз: ({t.x.toFixed(1)}, {t.y.toFixed(1)}) | Строк: {t.rows}
                  </div>
                  {t.manualBreakHeight !== undefined && (
                    <div className="text-[10px] text-emerald-400/90 mt-0.5">
                      Ручная высота: {t.manualBreakHeight.toFixed(2)}мм
                    </div>
                  )}
                </div>
              ))}
            </div>
          </div>
        )}
      </div>

      {/* AutoCAD Table Break Flags (DXF Group Code 90) Analysis */}
      {table.breakOptionInfo && (
        <div className="bg-slate-900/90 border border-purple-900/50 rounded-lg p-3 text-xs">
          <div className="flex items-center justify-between flex-wrap gap-2 pb-2 mb-2 border-b border-slate-800">
            <div className="flex items-center gap-2">
              <Split className="w-4 h-4 text-purple-400" />
              <span className="font-semibold text-purple-200">
                Table Break Options (DXF Group Code 90)
              </span>
              <span className="font-mono bg-purple-950/80 text-purple-300 border border-purple-800/50 px-2 py-0.5 rounded text-[11px]">
                Значение: {table.breakOptionInfo.rawFlags} ({table.breakOptionInfo.hexString}, {table.breakOptionInfo.binaryString})
              </span>
            </div>
            <div>
              {table.breakOptionInfo.breakEnabled ? (
                <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded bg-emerald-500/20 text-emerald-300 border border-emerald-500/40 font-semibold text-[11px]">
                  <CheckCircle2 className="w-3 h-3" />
                  Table Break: ВКЛЮЧЁН
                </span>
              ) : (
                <span className="inline-flex items-center gap-1 px-2 py-0.5 rounded bg-slate-800 text-slate-400 text-[11px]">
                  <XCircle className="w-3 h-3" />
                  Table Break: Отключен
                </span>
              )}
            </div>
          </div>

          {/* Detailed Bit Breakdown */}
          <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-5 gap-2 font-mono text-[11px] mb-2.5">
            <div
              className={`p-2 rounded border ${
                table.breakOptionInfo.breakNone
                  ? 'bg-slate-800/80 border-slate-700 text-slate-200'
                  : 'bg-slate-950/40 border-slate-900 text-slate-500'
              }`}
            >
              <div className="text-[10px] text-slate-400">Bit 1 (0x01 = 1)</div>
              <div className="font-semibold">kTableBreakNone</div>
              <div className="mt-0.5">{table.breakOptionInfo.breakNone ? 'SET' : '0 (No)'}</div>
            </div>

            <div
              className={`p-2 rounded border ${
                table.breakOptionInfo.breakEnabled
                  ? 'bg-emerald-950/40 border-emerald-800/60 text-emerald-200 font-bold'
                  : 'bg-slate-950/40 border-slate-900 text-slate-500'
              }`}
            >
              <div className="text-[10px] text-emerald-400/80">Bit 2 (0x02 = 2)</div>
              <div>kTableBreakEnable</div>
              <div className="mt-0.5 text-emerald-400">
                {table.breakOptionInfo.breakEnabled ? 'SET (ВКЛЮЧЕН!)' : '0 (No)'}
              </div>
            </div>

            <div
              className={`p-2 rounded border ${
                table.breakOptionInfo.autoHeight
                  ? 'bg-blue-950/40 border-blue-800/60 text-blue-200'
                  : 'bg-slate-950/40 border-slate-900 text-slate-500'
              }`}
            >
              <div className="text-[10px] text-blue-400/80">Bit 3 (0x04 = 4)</div>
              <div>kTableBreakAuto</div>
              <div className="mt-0.5 text-blue-300">
                {table.breakOptionInfo.autoHeight ? 'SET (Auto Height)' : '0 (No)'}
              </div>
            </div>

            <div
              className={`p-2 rounded border ${
                table.breakOptionInfo.manualPositioning
                  ? 'bg-amber-950/40 border-amber-800/60 text-amber-200'
                  : 'bg-slate-950/40 border-slate-900 text-slate-500'
              }`}
            >
              <div className="text-[10px] text-slate-400">Bit 4 (0x08 = 8)</div>
              <div>kAllowManualPos</div>
              <div className="mt-0.5">
                {table.breakOptionInfo.manualPositioning ? 'SET' : '0 (No)'}
              </div>
            </div>

            <div
              className={`p-2 rounded border ${
                table.breakOptionInfo.repeatHeader
                  ? 'bg-purple-950/40 border-purple-800/60 text-purple-200'
                  : 'bg-slate-950/40 border-slate-900 text-slate-500'
              }`}
            >
              <div className="text-[10px] text-purple-400/80">Bit 5 (0x10 = 16)</div>
              <div>kRepeatHeader</div>
              <div className="mt-0.5 text-purple-300">
                {table.breakOptionInfo.repeatHeader ? 'SET (Repeat Header)' : '0 (No)'}
              </div>
            </div>
          </div>

          <div className="flex items-start gap-1.5 text-[11px] text-slate-400 bg-slate-950/60 p-2 rounded border border-slate-800/80">
            <Info className="w-3.5 h-3.5 text-purple-400 shrink-0 mt-0.5" />
            <span>
              <strong>ObjectARX AcDbTable specification:</strong> 22 (0x16) = 16 (0x10 RepeatHeader) + 4 (0x04 AutoHeight) + 2 (0x02 BreakEnable).
            </span>
          </div>
        </div>
      )}

      {/* Dimensions & Sizing Indicators (DXF 142 Column Widths, DXF 141 Row Heights) */}
      <div className="space-y-1.5 text-xs text-slate-400 py-1 bg-slate-900/40 p-2.5 rounded-lg border border-slate-800/80">
        <div className="flex items-center gap-2 overflow-x-auto">
          <span className="font-semibold text-slate-300 shrink-0">
            Ширина столбцов (DXF 142, {table.columnWidths.length} столбцов):
          </span>
          <div className="flex items-center gap-1 font-mono">
            {table.columnWidths.map((w, idx) => (
              <span
                key={idx}
                className="bg-slate-800/80 px-1.5 py-0.5 rounded text-blue-300 border border-slate-700/60"
              >
                C{idx + 1}: {w.toFixed(2)}
              </span>
            ))}
          </div>
        </div>

        {table.rowHeights && table.rowHeights.length > 0 && (
          <div className="flex items-center gap-2 overflow-x-auto pt-1">
            <span className="font-semibold text-slate-300 shrink-0">
              Высота строк (DXF 141, {table.rowHeights.length} строк):
            </span>
            <div className="flex items-center gap-1 font-mono flex-wrap">
              {table.rowHeights.slice(0, 10).map((h, idx) => (
                <span
                  key={idx}
                  className="bg-slate-800/80 px-1.5 py-0.5 rounded text-amber-300 border border-slate-700/60"
                >
                  R{idx + 1}: {h.toFixed(2)}
                </span>
              ))}
              {table.rowHeights.length > 10 && (
                <span className="text-slate-500 text-[11px] self-center">
                  +{table.rowHeights.length - 10} more
                </span>
              )}
            </div>
          </div>
        )}
      </div>

      {/* DETAILED INSPECTOR CARD FOR CURRENTLY SELECTED CELL */}
      {currentCell && (
        <div className="bg-slate-900/90 border border-cyan-500/40 rounded-xl p-4 shadow-lg">
          <div className="flex items-center justify-between pb-3 border-b border-slate-800 flex-wrap gap-2">
            <div className="flex items-center gap-2">
              <Sliders className="w-4 h-4 text-cyan-400" />
              <span className="text-sm font-bold text-slate-100">
                Инспектор ячейки [Строка {currentCell.row}, Столбец {currentCell.col}]
              </span>
              {getRowStyleBadge(currentCell.rowStyle)}
              {currentCell.isMerged && (
                <span className="px-1.5 py-0.5 rounded text-[10px] font-semibold bg-rose-500/20 text-rose-300 border border-rose-500/30">
                  MERGED
                </span>
              )}
            </div>
            <div className="text-xs text-slate-400 font-mono">
              Ширина: {currentCell.width?.toFixed(2)} × Высота: {currentCell.height?.toFixed(2)}
            </div>
          </div>

          <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-5 gap-3 pt-3 text-xs">
            {/* 1. Cell Text */}
            <div className="bg-slate-950/60 border border-slate-800/80 rounded-lg p-2.5 flex flex-col justify-between">
              <div className="text-[11px] text-slate-400 flex items-center gap-1 mb-1">
                <Type className="w-3.5 h-3.5 text-amber-400" />
                <span>Текст ячейки (DXF 302/1):</span>
              </div>
              <div className="font-semibold text-slate-100 font-mono text-sm break-words">
                {currentCell.text ? `"${currentCell.text}"` : <span className="text-slate-500 font-normal">(Пустая ячейка)</span>}
              </div>
            </div>

            {/* 2. Cell Style (Стиль ячейки) */}
            <div className="bg-slate-950/60 border border-slate-800/80 rounded-lg p-2.5 flex flex-col justify-between">
              <div className="text-[11px] text-slate-400 mb-1">Стиль ячейки (Cell Style):</div>
              <div>
                <div className="font-semibold text-cyan-200">
                  {currentCell.cellStyle || 'по строке/столбцу'}
                </div>
                <div className="text-slate-500 text-[10px] mt-0.5">
                  (наследуется от строки/столбца)
                </div>
              </div>
            </div>

            {/* 3. Row Style (Стиль строки) */}
            <div className="bg-slate-950/60 border border-slate-800/80 rounded-lg p-2.5 flex flex-col justify-between">
              <div className="text-[11px] text-slate-400 mb-1">Стиль строки (Row Style):</div>
              <div className="flex items-center gap-1.5">
                <span className="font-semibold text-slate-200">
                  {currentCell.rowStyle || 'Data'}
                </span>
                <span className="text-slate-400 text-[11px]">
                  (H = {currentCell.height?.toFixed(3) || '0.360'})
                </span>
              </div>
            </div>

            {/* 4. Column Style (Стиль столбца) */}
            <div className="bg-slate-950/60 border border-slate-800/80 rounded-lg p-2.5 flex flex-col justify-between">
              <div className="text-[11px] text-slate-400 mb-1">Стиль столбца (Col Style):</div>
              <div>
                <span className="font-semibold text-slate-400 italic">
                  {currentCell.colStyle || 'нет'}
                </span>
                <span className="text-slate-500 text-[10px] ml-1">
                  (не задан)
                </span>
              </div>
            </div>

            {/* 5. Cell Alignment */}
            <div className="bg-slate-950/60 border border-slate-800/80 rounded-lg p-2.5 flex flex-col justify-between">
              <div className="text-[11px] text-slate-400 flex items-center gap-1 mb-1">
                <AlignCenter className="w-3.5 h-3.5 text-cyan-400" />
                <span>Выравнивание (DXF 170):</span>
              </div>
              <div className="font-semibold text-cyan-300">
                {currentCell.alignmentName || 'Middle Center'}
              </div>
            </div>

            {/* 6. Cell Type & Font */}
            <div className="bg-slate-950/60 border border-slate-800/80 rounded-lg p-2.5">
              <div className="text-[11px] text-slate-400 mb-1">Тип и Шрифт:</div>
              <div className="text-slate-200">
                <span className="font-semibold">{currentCell.cellType || 'Text'}</span>
                <span className="text-slate-400 ml-1">({currentCell.textStyle || 'Standard'})</span>
              </div>
            </div>

            {/* 7. Text Height */}
            <div className="bg-slate-950/60 border border-slate-800/80 rounded-lg p-2.5">
              <div className="text-[11px] text-slate-400 mb-1">Высота текста (DXF 140):</div>
              <div className="font-mono font-semibold text-amber-300">
                {currentCell.textHeight !== undefined ? currentCell.textHeight.toFixed(3) : '0.180'}
              </div>
            </div>

            {/* 8. Dimensions (W x H) */}
            <div className="bg-slate-950/60 border border-slate-800/80 rounded-lg p-2.5">
              <div className="text-[11px] text-slate-400 mb-1">Размеры (Ширина × Высота):</div>
              <div className="font-mono text-slate-200">
                {currentCell.width?.toFixed(2)} × {currentCell.height?.toFixed(3)}
              </div>
            </div>

            {/* 9. Span & Merging */}
            <div className="bg-slate-950/60 border border-slate-800/80 rounded-lg p-2.5">
              <div className="text-[11px] text-slate-400 mb-1">Объединение (Span/Merged):</div>
              <div className="font-mono text-slate-200">
                {currentCell.rowSpan || 1}R × {currentCell.colSpan || 1}C
                {currentCell.isMerged ? ' (Merged Slave)' : ' (Master)'}
              </div>
            </div>

            {/* 10. Overrides & Rotation */}
            <div className="bg-slate-950/60 border border-slate-800/80 rounded-lg p-2.5">
              <div className="text-[11px] text-slate-400 mb-1">Флаги (DXF 91) / Поворот:</div>
              <div className="font-mono text-slate-300">
                {currentCell.overrideFlags !== undefined ? `Flags: ${currentCell.overrideFlags}` : 'Default'}{' '}
                / {currentCell.rotation || 0}°
              </div>
            </div>
          </div>
        </div>
      )}

      {/* Main View Area with Tabs */}
      <div className="border border-slate-800 rounded-lg overflow-hidden bg-slate-950/60 shadow-inner">
        {/* Header Toolbar */}
        <div className="flex items-center justify-between px-3 py-2.5 bg-slate-900/90 border-b border-slate-800 flex-wrap gap-2">
          <div className="flex items-center gap-2">
            <div className="flex rounded-md bg-slate-950 p-0.5 border border-slate-800 text-xs">
              <button
                onClick={() => setViewMode('properties')}
                className={`px-3 py-1 rounded transition-colors flex items-center gap-1.5 ${
                  viewMode === 'properties'
                    ? 'bg-cyan-600 text-white font-semibold shadow-sm'
                    : 'text-slate-400 hover:text-slate-200'
                }`}
              >
                <Sliders className="w-3.5 h-3.5 text-cyan-300" />
                Свойства AutoCAD (Палитра)
              </button>
              <button
                onClick={() => setViewMode('grid')}
                className={`px-3 py-1 rounded transition-colors flex items-center gap-1.5 ${
                  viewMode === 'grid'
                    ? 'bg-blue-600 text-white font-semibold shadow-sm'
                    : 'text-slate-400 hover:text-slate-200'
                }`}
              >
                <Grid className="w-3.5 h-3.5" />
                Сетка ячеек (Интерактивная)
              </button>
              <button
                onClick={() => setViewMode('all-cells')}
                className={`px-3 py-1 rounded transition-colors flex items-center gap-1.5 ${
                  viewMode === 'all-cells'
                    ? 'bg-blue-600 text-white font-semibold shadow-sm'
                    : 'text-slate-400 hover:text-slate-200'
                }`}
              >
                <FileSpreadsheet className="w-3.5 h-3.5" />
                Все ячейки (Табличный реестр)
              </button>
            </div>
          </div>

          <div className="flex items-center gap-2">
            {viewMode === 'all-cells' && (
              <div className="flex items-center gap-1 text-xs">
                <Filter className="w-3 h-3 text-slate-400" />
                <span className="text-slate-400">Стиль строки:</span>
                <select
                  value={rowStyleFilter}
                  onChange={(e) => setRowStyleFilter(e.target.value as any)}
                  className="bg-slate-800 border border-slate-700 text-slate-200 rounded px-2 py-0.5 text-xs"
                >
                  <option value="ALL">Все стили ({table.cells.length})</option>
                  <option value="Title">Title (Заголовки)</option>
                  <option value="Header">Header (Шапки)</option>
                  <option value="Data">Data (Данные)</option>
                </select>
              </div>
            )}

            <button
              onClick={exportToCSV}
              className="flex items-center gap-1.5 px-3 py-1 text-xs bg-slate-800 hover:bg-slate-700 text-slate-200 rounded transition-colors font-medium border border-slate-700"
            >
              <Download className="w-3.5 h-3.5 text-blue-400" />
              Экспорт всех ячеек в CSV
            </button>
          </div>
        </div>

        {/* View Mode: AutoCAD Property Palette */}
        {viewMode === 'properties' && (
          <div className="p-4 bg-slate-950/40">
            <div className="mb-3 text-xs text-slate-400">
              Полный перечень свойств таблицы в соответствии со спецификацией AutoCAD / nanoCAD (Стиль, Строк, Столбцов, Направление, Размеры, Геометрия, Разрыв таблиц):
            </div>
            <AutoCADPropertyInspector table={table} />
          </div>
        )}

        {/* View Mode 1: Interactive Table Grid */}
        {viewMode === 'grid' && (
          <div className="overflow-x-auto max-h-[420px]">
            <table className="w-full text-left border-collapse text-xs">
              <thead>
                <tr className="bg-slate-900 border-b border-slate-800 text-slate-400 font-mono">
                  <th className="p-2 border-r border-slate-800 w-16 text-center">Строка / Стиль</th>
                  {Array.from({ length: table.cols }).map((_, c) => (
                    <th
                      key={c}
                      className="p-2 border-r border-slate-800 font-semibold text-amber-300 min-w-[120px]"
                    >
                      Столбец {c + 1}
                      <span className="block text-[10px] text-slate-500 font-normal">
                        W = {table.columnWidths[c]?.toFixed(2) || '2.50'}
                      </span>
                    </th>
                  ))}
                </tr>
              </thead>
              <tbody className="divide-y divide-slate-800/60 font-mono">
                {Array.from({ length: table.rows }).map((_, r) => {
                  const firstCell = table.cells.find((cl) => cl.row === r);
                  const rowStyle = firstCell?.rowStyle || 'Data';
                  const rowHeight = firstCell?.height || table.rowHeights[r] || 0.36;

                  return (
                    <tr
                      key={r}
                      className={`hover:bg-blue-950/20 transition-colors ${
                        rowStyle === 'Title'
                          ? 'bg-amber-950/15'
                          : rowStyle === 'Header'
                          ? 'bg-blue-950/15'
                          : ''
                      }`}
                    >
                      <td className="p-2 border-r border-slate-800 text-center bg-slate-900/40">
                        <div className="font-semibold text-slate-300">R{r}</div>
                        <div className="mt-0.5">{getRowStyleBadge(rowStyle)}</div>
                        <div className="text-[10px] text-slate-500 font-mono mt-0.5">
                          H: {rowHeight.toFixed(2)}
                        </div>
                      </td>

                      {Array.from({ length: table.cols }).map((_, c) => {
                        const cell = table.cells.find((cl) => cl.row === r && cl.col === c);
                        const isSelected =
                          currentCell && currentCell.row === r && currentCell.col === c;

                        return (
                          <td
                            key={c}
                            onClick={() => cell && setSelectedCell(cell)}
                            className={`p-2.5 border-r border-slate-800/60 cursor-pointer transition-all ${
                              isSelected
                                ? 'bg-cyan-500/20 ring-2 ring-cyan-400 ring-inset font-semibold text-cyan-200'
                                : cell?.isMerged
                                ? 'bg-slate-900/20 text-slate-500'
                                : 'hover:bg-slate-800/50 text-slate-200'
                            }`}
                            title={`Кликните для детального просмотра: Строка ${r}, Столбец ${c}`}
                          >
                            <div className="flex items-center justify-between gap-1 mb-1">
                              <span className="text-[10px] text-slate-500 font-mono">
                                [{r},{c}]
                              </span>
                              {cell?.alignmentCode && (
                                <span className="text-[9px] text-slate-400 bg-slate-800 px-1 rounded">
                                  {cell.alignmentName?.split(' ')[0] || 'Center'}
                                </span>
                              )}
                            </div>
                            <div className="font-medium text-xs truncate max-w-[200px]">
                              {cell?.text ? (
                                cell.text
                              ) : cell?.isMerged ? (
                                <span className="italic text-slate-500">(merged)</span>
                              ) : (
                                <span className="text-slate-600">—</span>
                              )}
                            </div>
                          </td>
                        );
                      })}
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        )}

        {/* View Mode 2: Full Cells Table Registry */}
        {viewMode === 'all-cells' && (
          <div className="overflow-x-auto max-h-[420px]">
            <table className="w-full text-left border-collapse text-xs font-mono">
              <thead>
                <tr className="bg-slate-900 border-b border-slate-800 text-slate-400">
                  <th className="p-2 border-r border-slate-800 w-14 text-center">Поз.</th>
                  <th className="p-2 border-r border-slate-800 min-w-[160px]">Текст ячейки</th>
                  <th className="p-2 border-r border-slate-800">Стиль ячейки</th>
                  <th className="p-2 border-r border-slate-800">Стиль строки</th>
                  <th className="p-2 border-r border-slate-800">Стиль столбца</th>
                  <th className="p-2 border-r border-slate-800">Выравнивание</th>
                  <th className="p-2 border-r border-slate-800">Тип ячейки</th>
                  <th className="p-2 border-r border-slate-800">Размеры (W × H)</th>
                  <th className="p-2 border-r border-slate-800">Span / Merged</th>
                  <th className="p-2">Шрифт / H</th>
                </tr>
              </thead>
              <tbody className="divide-y divide-slate-800/60">
                {filteredCells.map((cl, idx) => {
                  const isSelected =
                    currentCell && currentCell.row === cl.row && currentCell.col === cl.col;

                  return (
                    <tr
                      key={idx}
                      onClick={() => setSelectedCell(cl)}
                      className={`cursor-pointer transition-colors ${
                        isSelected
                          ? 'bg-cyan-500/20 text-cyan-200'
                          : 'hover:bg-slate-800/40 text-slate-200'
                      }`}
                    >
                      <td className="p-2 border-r border-slate-800 text-center font-bold text-slate-400">
                        R{cl.row}C{cl.col}
                      </td>
                      <td className="p-2 border-r border-slate-800 font-semibold text-slate-100">
                        {cl.text ? `"${cl.text}"` : <span className="text-slate-500 font-normal">—</span>}
                      </td>
                      <td className="p-2 border-r border-slate-800 text-cyan-200 font-medium">
                        {cl.cellStyle || 'по строке/столбцу'}
                      </td>
                      <td className="p-2 border-r border-slate-800">
                        {getRowStyleBadge(cl.rowStyle)}
                      </td>
                      <td className="p-2 border-r border-slate-800 text-slate-400 italic">
                        {cl.colStyle || 'нет'}
                      </td>
                      <td className="p-2 border-r border-slate-800 text-cyan-300">
                        {cl.alignmentName || 'Middle Center'}
                      </td>
                      <td className="p-2 border-r border-slate-800 text-slate-300">
                        {cl.cellType || 'Text'}
                      </td>
                      <td className="p-2 border-r border-slate-800 text-slate-300">
                        {cl.width?.toFixed(2)} × {cl.height?.toFixed(2)}
                      </td>
                      <td className="p-2 border-r border-slate-800 text-slate-300">
                        {cl.rowSpan || 1}×{cl.colSpan || 1}{' '}
                        {cl.isMerged && <span className="text-rose-400 text-[10px]">(Merged)</span>}
                      </td>
                      <td className="p-2 text-slate-300">
                        {cl.textStyle || 'Standard'} / {cl.textHeight?.toFixed(2) || '0.18'}
                      </td>
                    </tr>
                  );
                })}
              </tbody>
            </table>
          </div>
        )}
      </div>
    </div>
  );
};
