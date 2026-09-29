import React from 'react';
import { DXFAnalysisResult } from '../types/dxf';
import {
  Layers,
  FileCode2,
  Box,
  Hash,
  Database,
  Grid,
  Info,
  Split,
} from 'lucide-react';

interface AnalysisOverviewProps {
  analysis: DXFAnalysisResult;
}

export const AnalysisOverview: React.FC<AnalysisOverviewProps> = ({ analysis }) => {
  const dx = analysis.extents.maxX - analysis.extents.minX;
  const dy = analysis.extents.maxY - analysis.extents.minY;

  return (
    <div className="flex flex-col gap-6">
      {/* High-level Summary Metrics */}
      <div className="grid grid-cols-2 sm:grid-cols-4 gap-3">
        <div className="bg-slate-900/70 border border-slate-800 p-3.5 rounded-xl">
          <div className="flex items-center gap-1.5 text-xs text-slate-400 mb-1">
            <FileCode2 className="w-3.5 h-3.5 text-blue-400" />
            <span>AutoCAD Version</span>
          </div>
          <div className="text-sm font-bold text-slate-100 font-mono">
            {analysis.version}
          </div>
          <div className="text-[11px] text-slate-500 mt-0.5">
            {(analysis.fileSizeBytes / 1024).toFixed(1)} KB file size
          </div>
        </div>

        <div className="bg-slate-900/70 border border-slate-800 p-3.5 rounded-xl">
          <div className="flex items-center gap-1.5 text-xs text-slate-400 mb-1">
            <Grid className="w-3.5 h-3.5 text-amber-400" />
            <span>ACAD_TABLE Entities</span>
          </div>
          <div className="text-sm font-bold text-amber-300 font-mono">
            {analysis.tables.length} {analysis.tables.length === 1 ? 'Table' : 'Fragments'}
          </div>
          <div className="text-[11px] text-slate-500 mt-0.5">
            {analysis.tables.length > 1 ? 'Multi-break schedule' : 'Single table entity'}
          </div>
        </div>

        <div className="bg-slate-900/70 border border-slate-800 p-3.5 rounded-xl">
          <div className="flex items-center gap-1.5 text-xs text-slate-400 mb-1">
            <Box className="w-3.5 h-3.5 text-emerald-400" />
            <span>Entities & Geometry</span>
          </div>
          <div className="text-sm font-bold text-slate-100 font-mono">
            {Object.values(analysis.entityCounts).reduce((a, b) => a + b, 0)} Entities
          </div>
          <div className="text-[11px] text-slate-500 mt-0.5">
            {analysis.layers.length} CAD Layer(s)
          </div>
        </div>

        <div className="bg-slate-900/70 border border-slate-800 p-3.5 rounded-xl">
          <div className="flex items-center gap-1.5 text-xs text-slate-400 mb-1">
            <Hash className="w-3.5 h-3.5 text-purple-400" />
            <span>Max Handle Seed</span>
          </div>
          <div className="text-sm font-bold text-purple-300 font-mono">
            0x{analysis.maxHandleHex}
          </div>
          <div className="text-[11px] text-slate-500 mt-0.5">
            {analysis.tableStyles.length} TABLESTYLE record(s)
          </div>
        </div>
      </div>

      {/* Multi-Fragment Break Analysis (Deep Inspection) */}
      {analysis.tables.length > 1 && (
        <div className="bg-purple-950/20 border border-purple-900/40 p-4 rounded-xl">
          <div className="flex items-center gap-2 text-sm font-bold text-purple-300 mb-2">
            <Split className="w-4 h-4" />
            <span>Multi-Fragment Table Break Analysis</span>
          </div>
          <p className="text-xs text-slate-300 mb-3">
            This drawing contains an AutoCAD table split across {analysis.tables.length} table
            fragments (just like documented in <code className="text-purple-200">acadtable2007_analysis.txt</code>).
            Each fragment maintains independent insertion coordinates, row ranges, and owner pointers:
          </p>

          <div className="grid grid-cols-1 sm:grid-cols-2 md:grid-cols-4 gap-2.5">
            {analysis.tables.map((tbl, i) => (
              <div
                key={tbl.id}
                className="bg-slate-900/80 border border-purple-800/30 p-3 rounded-lg text-xs font-mono"
              >
                <div className="text-purple-300 font-bold mb-1 flex items-center justify-between">
                  <span>Fragment #{i + 1}</span>
                  <span className="text-emerald-400 font-semibold">{tbl.breakHeight ? tbl.breakHeight.toFixed(2) : '—'} мм</span>
                </div>
                <div className="text-slate-400 text-[11px]">Handle: <span className="text-slate-200">{tbl.handle}</span></div>
                <div className="text-slate-400 text-[11px]">Insert: <span className="text-cyan-300">({tbl.x.toFixed(1)}, {tbl.y.toFixed(1)})</span></div>
                <div className="text-slate-400 text-[11px]">Size: <span className="text-slate-200">{tbl.rows}R x {tbl.cols}C</span></div>
                <div className="text-slate-400 text-[11px]">
                  Направление: <span className="text-indigo-300 font-semibold">{tbl.flowDirection || 'Вниз'}</span>
                </div>
                <div className="text-slate-400 text-[11px]">
                  Разрыв: <span className="text-purple-300 font-semibold">{tbl.breakDirection || 'Вправо'}</span> ({tbl.breakSpacing ? `${tbl.breakSpacing.toFixed(2)} мм` : '0.99 мм'})
                </div>
                <div className="text-slate-400 text-[11px]">
                  Высота разбиения: <span className="text-purple-300">{tbl.breakHeight ? tbl.breakHeight.toFixed(2) : '—'} мм</span>
                </div>
                {tbl.manualBreakHeight !== undefined && (
                  <div className="text-slate-400 text-[11px]">
                    Ручная высота: <span className="text-emerald-300">{tbl.manualBreakHeight.toFixed(2)} мм</span>
                  </div>
                )}
                <div className="text-slate-400 text-[11px]">
                  Повтор верхних меток:{' '}
                  <span className={tbl.repeatTopLabels ? 'text-indigo-300 font-semibold' : 'text-slate-500'}>
                    {tbl.repeatTopLabels ? `Да (${tbl.topLabelsRowCount || 0} стр.)` : 'Нет'}
                  </span>
                </div>
                <div className="text-slate-400 text-[11px]">Layer: <span className="text-slate-200">{tbl.layer}</span></div>
                {tbl.breakOptionInfo && (
                  <div className="mt-1.5 pt-1.5 border-t border-purple-900/40 text-[10px]">
                    <span className="text-purple-300">Break 90: </span>
                    <span className="text-emerald-300 font-bold">{tbl.breakOptionInfo.rawFlags}</span>
                    <span className="text-slate-400"> ({tbl.breakOptionInfo.breakEnabled ? 'Break ENABLED' : 'Disabled'})</span>
                  </div>
                )}
              </div>
            ))}
          </div>
        </div>
      )}

      {/* Sections and Entity Breakdown */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
        {/* Entity Counts Table */}
        <div className="bg-slate-900/60 border border-slate-800 p-4 rounded-xl">
          <div className="flex items-center justify-between mb-3">
            <h3 className="text-xs font-bold uppercase tracking-wider text-slate-300 flex items-center gap-2">
              <Box className="w-4 h-4 text-blue-400" />
              Entities Breakdown
            </h3>
            <span className="text-xs text-slate-500 font-mono">
              ENTITIES Section
            </span>
          </div>

          <div className="divide-y divide-slate-800/60 font-mono text-xs">
            {Object.entries(analysis.entityCounts).map(([type, count]) => (
              <div
                key={type}
                className="flex items-center justify-between py-2 hover:bg-slate-800/30 px-1 rounded"
              >
                <span className="text-slate-300 font-semibold">{type}</span>
                <span className="bg-slate-800 px-2 py-0.5 rounded text-cyan-300">
                  {count}
                </span>
              </div>
            ))}
            {Object.keys(analysis.entityCounts).length === 0 && (
              <div className="py-4 text-center text-slate-500 italic">
                No entities found in ENTITIES section
              </div>
            )}
          </div>
        </div>

        {/* Section Breakdown and Extents */}
        <div className="flex flex-col gap-4">
          <div className="bg-slate-900/60 border border-slate-800 p-4 rounded-xl">
            <div className="flex items-center justify-between mb-3">
              <h3 className="text-xs font-bold uppercase tracking-wider text-slate-300 flex items-center gap-2">
                <Database className="w-4 h-4 text-emerald-400" />
                DXF Sections Found
              </h3>
            </div>

            <div className="grid grid-cols-3 gap-2 text-xs font-mono">
              {['HEADER', 'CLASSES', 'TABLES', 'BLOCKS', 'ENTITIES', 'OBJECTS'].map(
                (sec) => {
                  const present = (analysis.sectionCounts[sec] || 0) > 0;
                  return (
                    <div
                      key={sec}
                      className={`p-2 rounded border text-center ${
                        present
                          ? 'bg-slate-800/80 border-slate-700 text-slate-200'
                          : 'bg-slate-900/30 border-slate-800 text-slate-600 line-through'
                      }`}
                    >
                      {sec}
                    </div>
                  );
                }
              )}
            </div>
          </div>

          {/* Drawing Bounds / Extents */}
          <div className="bg-slate-900/60 border border-slate-800 p-4 rounded-xl">
            <div className="flex items-center justify-between mb-2">
              <h3 className="text-xs font-bold uppercase tracking-wider text-slate-300 flex items-center gap-2">
                <Info className="w-4 h-4 text-cyan-400" />
                Drawing Bounds & Extents
              </h3>
            </div>

            <div className="grid grid-cols-2 gap-2 text-xs font-mono">
              <div className="bg-slate-950/70 p-2 rounded border border-slate-800">
                <span className="text-slate-500 block text-[10px]">Min (X, Y)</span>
                <span className="text-cyan-400">
                  {analysis.extents.minX.toFixed(2)}, {analysis.extents.minY.toFixed(2)}
                </span>
              </div>
              <div className="bg-slate-950/70 p-2 rounded border border-slate-800">
                <span className="text-slate-500 block text-[10px]">Max (X, Y)</span>
                <span className="text-emerald-400">
                  {analysis.extents.maxX.toFixed(2)}, {analysis.extents.maxY.toFixed(2)}
                </span>
              </div>
            </div>

            <div className="text-[11px] text-slate-400 mt-2 font-mono">
              Span: <span className="text-slate-200">{dx.toFixed(2)} mm</span> (width) ×{' '}
              <span className="text-slate-200">{dy.toFixed(2)} mm</span> (height)
            </div>
          </div>
        </div>
      </div>
    </div>
  );
};
