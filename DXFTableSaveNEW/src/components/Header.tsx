import React, { useRef } from 'react';
import { DXFAnalysisResult } from '../types/dxf';
import {
  Table2,
  Upload,
  Download,
  FileCode,
  CheckCircle2,
  Layers,
  Sparkles,
} from 'lucide-react';

interface HeaderProps {
  currentFileName: string;
  analysis: DXFAnalysisResult | null;
  onFileUpload: (file: File) => void;
  onLoadPreset: (sampleName: string) => void;
  onDownloadCurrentDxf: () => void;
  hasModifications: boolean;
  isLoading: boolean;
}

export const Header: React.FC<HeaderProps> = ({
  currentFileName,
  analysis,
  onFileUpload,
  onLoadPreset,
  onDownloadCurrentDxf,
  hasModifications,
  isLoading,
}) => {
  const fileInputRef = useRef<HTMLInputElement | null>(null);

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files[0]) {
      onFileUpload(e.target.files[0]);
    }
  };

  return (
    <header className="bg-slate-900/90 backdrop-blur-md border-b border-slate-800 sticky top-0 z-50">
      <div className="max-w-7xl mx-auto px-4 sm:px-6 py-3 flex flex-col md:flex-row items-center justify-between gap-3">
        {/* Logo and App Title */}
        <div className="flex items-center gap-3 w-full md:w-auto justify-between md:justify-start">
          <div className="flex items-center gap-2.5">
            <div className="w-9 h-9 rounded-lg bg-blue-600/20 border border-blue-500/40 flex items-center justify-center text-blue-400 shadow-sm">
              <Table2 className="w-5 h-5" />
            </div>
            <div>
              <div className="flex items-center gap-2">
                <h1 className="text-base font-bold tracking-tight text-white">
                  DXF Table Save & Analyzer
                </h1>
                <span className="text-[10px] font-mono uppercase bg-blue-950 text-blue-300 border border-blue-800 px-1.5 py-0.5 rounded">
                  CAD 2007+
                </span>
              </div>
              <p className="text-xs text-slate-400">
                Inspect, generate, and inject ACAD_TABLE & vector tables into DXF drawings
              </p>
            </div>
          </div>

          {/* Preset Buttons for Mobile */}
          <div className="flex md:hidden items-center gap-1">
            <button
              onClick={() => onLoadPreset('acadtable2007.dxf')}
              className="text-[11px] px-2 py-1 bg-slate-800 hover:bg-slate-700 text-slate-200 rounded"
            >
              Demo DXF
            </button>
          </div>
        </div>

        {/* Quick Sample Presets (Desktop) */}
        <div className="hidden lg:flex items-center gap-1.5 bg-slate-950/70 border border-slate-800 p-1 rounded-lg">
          <span className="text-[11px] font-medium text-slate-400 px-2 flex items-center gap-1">
            <Sparkles className="w-3 h-3 text-amber-400" />
            Presets:
          </span>
          <button
            onClick={() => onLoadPreset('acadtableandhrefImage2007.dxf')}
            className={`text-xs px-2.5 py-1 rounded transition-colors flex items-center gap-1.5 ${
              currentFileName === 'acadtableandhrefImage2007.dxf'
                ? 'bg-cyan-600 text-white font-medium shadow-sm'
                : 'text-cyan-300 hover:bg-slate-800'
            }`}
            title="AutoCAD sample with Split Table + External Reference Image (testimage.png)"
          >
            <span>Таблица + Картинка (Xref)</span>
          </button>
          <button
            onClick={() => onLoadPreset('acadtable2007.dxf')}
            className={`text-xs px-2.5 py-1 rounded transition-colors ${
              currentFileName === 'acadtable2007.dxf'
                ? 'bg-blue-600 text-white font-medium shadow-sm'
                : 'text-slate-300 hover:bg-slate-800'
            }`}
            title="AutoCAD 2007 sample with table break fragments"
          >
            acadtable2007.dxf
          </button>
          <button
            onClick={() => onLoadPreset('acadtableandOLE2007.dxf')}
            className={`text-xs px-2.5 py-1 rounded transition-colors ${
              currentFileName === 'acadtableandOLE2007.dxf'
                ? 'bg-blue-600 text-white font-medium shadow-sm'
                : 'text-slate-300 hover:bg-slate-800'
            }`}
            title="AutoCAD sample with Table and OLE object"
          >
            acadtableandOLE2007.dxf
          </button>
          <button
            onClick={() => onLoadPreset('ZCADonlyline.dxf')}
            className={`text-xs px-2.5 py-1 rounded transition-colors ${
              currentFileName === 'ZCADonlyline.dxf'
                ? 'bg-blue-600 text-white font-medium shadow-sm'
                : 'text-slate-300 hover:bg-slate-800'
            }`}
            title="ZCAD sample drawing with line geometry"
          >
            ZCADonlyline.dxf
          </button>
        </div>

        {/* Actions: Upload DXF, Download Drawing */}
        <div className="flex items-center gap-2.5 w-full md:w-auto justify-end">
          <input
            type="file"
            ref={fileInputRef}
            onChange={handleFileChange}
            accept=".dxf"
            className="hidden"
          />

          <button
            onClick={() => fileInputRef.current?.click()}
            disabled={isLoading}
            className="flex items-center gap-1.5 px-3 py-1.5 text-xs font-medium bg-slate-800 hover:bg-slate-700 text-slate-200 rounded-lg border border-slate-700 transition-colors shadow-sm cursor-pointer"
            title="Upload any .dxf file"
          >
            <Upload className="w-3.5 h-3.5 text-blue-400" />
            <span>Upload DXF</span>
          </button>

          <button
            onClick={onDownloadCurrentDxf}
            disabled={!analysis}
            className={`flex items-center gap-1.5 px-3.5 py-1.5 text-xs font-semibold rounded-lg transition-all shadow-sm ${
              hasModifications
                ? 'bg-emerald-600 hover:bg-emerald-500 text-white'
                : 'bg-slate-800 hover:bg-slate-700 text-slate-200 border border-slate-700'
            }`}
            title="Download the current DXF drawing"
          >
            <Download className="w-3.5 h-3.5" />
            <span>Download DXF</span>
            {hasModifications && (
              <span className="w-2 h-2 rounded-full bg-emerald-300 animate-pulse" />
            )}
          </button>
        </div>
      </div>
    </header>
  );
};
