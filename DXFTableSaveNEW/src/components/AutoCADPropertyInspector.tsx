import React, { useState } from 'react';
import { TableFragment, AutoCADTableProperties } from '../types/dxf';
import {
  ChevronDown,
  ChevronRight,
  Sliders,
  Copy,
  Check,
  Info,
  Maximize2,
} from 'lucide-react';

interface AutoCADPropertyInspectorProps {
  table: TableFragment;
  className?: string;
}

export const AutoCADPropertyInspector: React.FC<AutoCADPropertyInspectorProps> = ({
  table,
  className = '',
}) => {
  const [copiedKey, setCopiedKey] = useState<string | null>(null);
  const [openSections, setOpenSections] = useState<Record<string, boolean>>({
    table: true,
    geometry: true,
    breaks: true,
    geomProps: true,
    geomSettings: true,
  });

  const toggleSection = (sec: string) => {
    setOpenSections((prev) => ({ ...prev, [sec]: !prev[sec] }));
  };

  const copyValue = (key: string, val: string) => {
    navigator.clipboard.writeText(val);
    setCopiedKey(key);
    setTimeout(() => setCopiedKey(null), 1500);
  };

  const p: AutoCADTableProperties = table.cadProperties || {
    tableStyle: table.styleName || 'Standard',
    rows: table.totalTableRows || table.rows,
    cols: table.cols,
    flowDirection: table.flowDirection || 'Вниз',
    tableWidth: parseFloat(table.columnWidths.reduce((a, b) => a + b, 0).toFixed(4)),
    tableHeight: parseFloat((table.totalTableHeight || table.breakHeight).toFixed(4)),
    positionX: parseFloat(table.x.toFixed(4)),
    positionY: parseFloat(table.y.toFixed(4)),
    positionZ: parseFloat(table.z.toFixed(4)),
    breakEnabled: Boolean(table.breakFlags && table.breakFlags > 0 || table.isFragmentOfMultiTable),
    breakDirection: table.breakDirection || 'Вправо',
    repeatTopLabels: table.repeatTopLabels ?? true,
    repeatBottomLabels: false,
    manualPositioning: table.positionMode === 'manual',
    manualBreakHeight: Boolean(table.isManualBreakHeight),
    breakHeight: parseFloat(table.breakHeight.toFixed(4)),
    breakSpacing: parseFloat((table.breakSpacing ?? 0.99).toFixed(4)),
    areaSum: 0.0,
    lengthSum: 0.0,
    volumeSum: 0.0,
    linearScaleFactor: 1.0,
  };

  const renderRow = (
    label: string,
    value: string | number,
    tooltip?: string,
    badgeColor?: string
  ) => {
    const valStr = String(value);
    const isCopied = copiedKey === label;

    return (
      <div
        key={label}
        className="group flex items-center justify-between py-1.5 px-3 border-b border-slate-800/80 hover:bg-slate-850/50 transition-colors text-xs"
        title={tooltip}
      >
        <div className="flex items-center gap-1.5 text-slate-400 select-none min-w-0 pr-2">
          <span className="truncate">{label}</span>
          {tooltip && <Info className="w-3 h-3 text-slate-500 opacity-0 group-hover:opacity-100 transition-opacity flex-shrink-0" />}
        </div>
        <div className="flex items-center gap-2 flex-shrink-0">
          {badgeColor ? (
            <span
              className={`px-2 py-0.5 rounded font-mono font-medium text-[11px] ${badgeColor}`}
            >
              {valStr}
            </span>
          ) : (
            <span className="font-mono text-slate-100 font-medium select-all">
              {valStr}
            </span>
          )}
          <button
            onClick={() => copyValue(label, valStr)}
            className="opacity-0 group-hover:opacity-100 text-slate-500 hover:text-slate-300 p-0.5 rounded transition-opacity"
            title="Скопировать значение"
          >
            {isCopied ? (
              <Check className="w-3 h-3 text-emerald-400" />
            ) : (
              <Copy className="w-3 h-3" />
            )}
          </button>
        </div>
      </div>
    );
  };

  return (
    <div
      className={`bg-slate-900 border border-slate-700/80 rounded-lg shadow-xl overflow-hidden font-sans ${className}`}
      id={`cad-properties-panel-${table.handle}`}
    >
      {/* Header bar styled like AutoCAD / nanoCAD Properties Window */}
      <div className="flex items-center justify-between px-3 py-2 bg-slate-800/90 border-b border-slate-700 select-none">
        <div className="flex items-center gap-2">
          <Sliders className="w-4 h-4 text-cyan-400" />
          <span className="font-semibold text-xs tracking-wide text-slate-200">
            Свойства таблицы (AutoCAD / nanoCAD)
          </span>
          <span className="text-[10px] font-mono px-1.5 py-0.2 bg-cyan-950/80 text-cyan-300 border border-cyan-800/50 rounded">
            0x{table.handle}
          </span>
        </div>
        <div className="flex items-center gap-2">
          <span className="text-[11px] text-slate-400">
            Фрагмент {table.fragmentIndex || 1} из {table.totalFragments || 1}
          </span>
        </div>
      </div>

      <div className="divide-y divide-slate-800">
        {/* Секция: Таблица */}
        <div>
          <button
            onClick={() => toggleSection('table')}
            className="w-full flex items-center justify-between px-3 py-1.5 bg-slate-850/80 hover:bg-slate-800 text-slate-300 font-semibold text-xs text-left transition-colors select-none"
          >
            <div className="flex items-center gap-1.5">
              {openSections.table ? (
                <ChevronDown className="w-3.5 h-3.5 text-slate-400" />
              ) : (
                <ChevronRight className="w-3.5 h-3.5 text-slate-400" />
              )}
              <span>Таблица</span>
            </div>
            <span className="text-[10px] font-normal text-slate-500">
              Стиль и размеры
            </span>
          </button>
          {openSections.table && (
            <div className="bg-slate-900/60">
              {renderRow('Стиль таблицы', p.tableStyle, 'AutoCAD TableStyle name (DXF 2)')}
              {renderRow('Строк', p.rows, 'Общее количество логических строк всей таблицы')}
              {renderRow('Столбцов', p.cols, 'Количество колонок (DXF 92)')}
              {renderRow(
                'Направление',
                p.flowDirection,
                'Направление строк таблицы (Вниз / Вверх)',
                p.flowDirection === 'Вниз' ? 'bg-blue-950/60 text-blue-300' : 'bg-amber-950/60 text-amber-300'
              )}
              {renderRow(
                'Ширина таблицы',
                `${p.tableWidth.toFixed(4)}`,
                'Суммарная ширина всех столбцов'
              )}
              {renderRow(
                'Высота таблицы',
                `${p.tableHeight.toFixed(4)}`,
                'Полная логическая высота таблицы'
              )}
            </div>
          )}
        </div>

        {/* Секция: Геометрия */}
        <div>
          <button
            onClick={() => toggleSection('geometry')}
            className="w-full flex items-center justify-between px-3 py-1.5 bg-slate-850/80 hover:bg-slate-800 text-slate-300 font-semibold text-xs text-left transition-colors select-none"
          >
            <div className="flex items-center gap-1.5">
              {openSections.geometry ? (
                <ChevronDown className="w-3.5 h-3.5 text-slate-400" />
              ) : (
                <ChevronRight className="w-3.5 h-3.5 text-slate-400" />
              )}
              <span>Геометрия</span>
            </div>
            <span className="text-[10px] font-normal text-slate-500">
              Точка вставки
            </span>
          </button>
          {openSections.geometry && (
            <div className="bg-slate-900/60">
              {renderRow('Положение X', p.positionX.toFixed(4), 'Координата X точки вставки (DXF 10)')}
              {renderRow('Положение Y', p.positionY.toFixed(4), 'Координата Y точки вставки (DXF 20)')}
              {renderRow('Положение Z', p.positionZ.toFixed(4), 'Координата Z точки вставки (DXF 30)')}
            </div>
          )}
        </div>

        {/* Секция: Разрыв таблиц */}
        <div>
          <button
            onClick={() => toggleSection('breaks')}
            className="w-full flex items-center justify-between px-3 py-1.5 bg-slate-850/80 hover:bg-slate-800 text-slate-300 font-semibold text-xs text-left transition-colors select-none"
          >
            <div className="flex items-center gap-1.5">
              {openSections.breaks ? (
                <ChevronDown className="w-3.5 h-3.5 text-slate-400" />
              ) : (
                <ChevronRight className="w-3.5 h-3.5 text-slate-400" />
              )}
              <span>Разрыв таблиц</span>
            </div>
            <span className="text-[10px] font-normal text-slate-500">
              Многоколоночное разбиение
            </span>
          </button>
          {openSections.breaks && (
            <div className="bg-slate-900/60">
              {renderRow(
                'Включено',
                p.breakEnabled ? 'Да' : 'Нет',
                'Флаг включения разбиения таблицы (DXF 90 Bit 2: 0x2)',
                p.breakEnabled ? 'bg-emerald-950/80 text-emerald-300' : 'bg-slate-800 text-slate-400'
              )}
              {renderRow(
                'Направление',
                p.breakDirection,
                'Направление переноса фрагментов (Вправо, Вниз, Влево, Вверх)',
                'bg-indigo-950/80 text-indigo-300'
              )}
              {renderRow(
                'Повторение верхних меток',
                p.repeatTopLabels ? 'Да' : 'Нет',
                'Повтор строк Title и Header на каждом фрагменте (DXF 90 Bit 5: 0x10)',
                p.repeatTopLabels ? 'bg-teal-950/80 text-teal-300' : 'bg-slate-800 text-slate-400'
              )}
              {renderRow('Повторение нижних меток', p.repeatBottomLabels ? 'Да' : 'Нет')}
              {renderRow(
                'Задание положения вручную',
                p.manualPositioning ? 'Да' : 'Нет',
                'Ручное позиционирование фрагментов пользователем (DXF 90 Bit 4: 0x8)'
              )}
              {renderRow(
                'Задание высоты вручную',
                p.manualBreakHeight ? 'Да' : 'Нет',
                'Ручное задание высоты разбиения для фрагментов'
              )}
              {renderRow(
                'Высота разбиения',
                p.breakHeight.toFixed(4),
                'Высота после которой строки переносятся в следующий фрагмент'
              )}
              {renderRow(
                'Интервал',
                p.breakSpacing.toFixed(4),
                'Расстояние (отступ) между фрагментами таблицы'
              )}
            </div>
          )}
        </div>

        {/* Секция: Геометрические свойства */}
        <div>
          <button
            onClick={() => toggleSection('geomProps')}
            className="w-full flex items-center justify-between px-3 py-1.5 bg-slate-850/80 hover:bg-slate-800 text-slate-300 font-semibold text-xs text-left transition-colors select-none"
          >
            <div className="flex items-center gap-1.5">
              {openSections.geomProps ? (
                <ChevronDown className="w-3.5 h-3.5 text-slate-400" />
              ) : (
                <ChevronRight className="w-3.5 h-3.5 text-slate-400" />
              )}
              <span>Геометрические свойства</span>
            </div>
            <span className="text-[10px] font-normal text-slate-500">
              Суммы
            </span>
          </button>
          {openSections.geomProps && (
            <div className="bg-slate-900/60">
              {renderRow('Площадь (сумма)', p.areaSum.toFixed(4))}
              {renderRow('Длина (сумма)', p.lengthSum.toFixed(4))}
              {renderRow('Объем (сумма)', p.volumeSum.toFixed(4))}
            </div>
          )}
        </div>

        {/* Секция: Геометрические свойства (настройка) */}
        <div>
          <button
            onClick={() => toggleSection('geomSettings')}
            className="w-full flex items-center justify-between px-3 py-1.5 bg-slate-850/80 hover:bg-slate-800 text-slate-300 font-semibold text-xs text-left transition-colors select-none"
          >
            <div className="flex items-center gap-1.5">
              {openSections.geomSettings ? (
                <ChevronDown className="w-3.5 h-3.5 text-slate-400" />
              ) : (
                <ChevronRight className="w-3.5 h-3.5 text-slate-400" />
              )}
              <span>Геометрические свойства (настройка)</span>
            </div>
            <span className="text-[10px] font-normal text-slate-500">
              Масштабирование
            </span>
          </button>
          {openSections.geomSettings && (
            <div className="bg-slate-900/60">
              {renderRow('Линейный масштабный коэф...', p.linearScaleFactor.toFixed(4))}
            </div>
          )}
        </div>
      </div>
    </div>
  );
};
