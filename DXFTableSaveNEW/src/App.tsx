import React, { useState, useEffect } from 'react';
import { DXFAnalysisResult, TableFragment, DXFImageEntity } from './types/dxf';
import { analyzeDXF } from './services/dxfParser';
import { Header } from './components/Header';
import { CADViewer } from './components/CADViewer';
import { TableDetails } from './components/TableDetails';
import { TableGenerator } from './components/TableGenerator';
import { AnalysisOverview } from './components/AnalysisOverview';
import { RawTagInspector } from './components/RawTagInspector';
import { ExternalReferenceManager } from './components/ExternalReferenceManager';
import {
  Eye,
  Table,
  PlusCircle,
  BarChart3,
  Code2,
  FileCode,
  AlertCircle,
  Loader2,
  Image as ImageIcon,
  Layers,
  Move,
  Upload,
} from 'lucide-react';

export const App: React.FC = () => {
  const [currentFileName, setCurrentFileName] = useState<string>('acadtableandhrefImage2007.dxf');
  const [currentDxfText, setCurrentDxfText] = useState<string>('');
  const [analysis, setAnalysis] = useState<DXFAnalysisResult | null>(null);
  const [selectedTable, setSelectedTable] = useState<TableFragment | null>(null);
  const [selectedImage, setSelectedImage] = useState<DXFImageEntity | null>(null);
  const [activeTab, setActiveTab] = useState<
    'viewer' | 'tables' | 'xrefs' | 'generator' | 'overview' | 'tags'
  >('viewer');
  const [isLoading, setIsLoading] = useState<boolean>(true);
  const [hasModifications, setHasModifications] = useState<boolean>(false);
  const [errorMsg, setErrorMsg] = useState<string | null>(null);

  // Load a preset file from public/samples/
  const loadPreset = async (sampleName: string) => {
    setIsLoading(true);
    setErrorMsg(null);
    try {
      const resp = await fetch(`/samples/${sampleName}`);
      if (!resp.ok) {
        throw new Error(`Failed to load preset ${sampleName}: ${resp.statusText}`);
      }
      const text = await resp.text();
      setCurrentDxfText(text);
      setCurrentFileName(sampleName);

      const parsed = analyzeDXF(sampleName, text, text.length);
      setAnalysis(parsed);
      setSelectedTable(parsed.tables.length > 0 ? parsed.tables[0] : null);
      setSelectedImage(parsed.images && parsed.images.length > 0 ? parsed.images[0] : null);
      setHasModifications(false);
    } catch (err: any) {
      setErrorMsg(err.message || 'Error loading sample file');
    } finally {
      setIsLoading(false);
    }
  };

  // Initial load: AutoCAD 2007 with Table + External Reference Image
  useEffect(() => {
    loadPreset('acadtableandhrefImage2007.dxf');
  }, []);

  // Upload user's custom DXF file
  const handleFileUpload = (file: File) => {
    setIsLoading(true);
    setErrorMsg(null);
    const reader = new FileReader();

    reader.onload = (e) => {
      try {
        const text = e.target?.result as string;
        setCurrentDxfText(text);
        setCurrentFileName(file.name);

        const parsed = analyzeDXF(file.name, text, file.size);
        setAnalysis(parsed);
        setSelectedTable(parsed.tables.length > 0 ? parsed.tables[0] : null);
        setSelectedImage(parsed.images && parsed.images.length > 0 ? parsed.images[0] : null);
        setHasModifications(false);
      } catch (err: any) {
        setErrorMsg('Failed to parse DXF file: ' + err.message);
      } finally {
        setIsLoading(false);
      }
    };

    reader.onerror = () => {
      setErrorMsg('Failed to read uploaded file');
      setIsLoading(false);
    };

    reader.readAsText(file);
  };

  // Called when user uploads or replaces an external image file in UI
  const handleCustomImageUploaded = (imageHandle: string, dataUrl: string) => {
    if (!analysis) return;
    const updatedImages = analysis.images.map((img) =>
      img.handle === imageHandle ? { ...img, resolvedSrcUrl: dataUrl } : img
    );
    const updatedXrefs = analysis.externalReferences.map((xref) =>
      xref.entityHandle === imageHandle
        ? { ...xref, customDataUrl: dataUrl, status: 'Loaded' as const }
        : xref
    );
    const updatedRenderables = analysis.renderableEntities.map((ent) =>
      ent.type === 'IMAGE' && ent.handle === imageHandle
        ? { ...ent, resolvedSrcUrl: dataUrl }
        : ent
    );
    setAnalysis({
      ...analysis,
      images: updatedImages,
      externalReferences: updatedXrefs,
      renderableEntities: updatedRenderables,
    });
    if (selectedImage && selectedImage.handle === imageHandle) {
      setSelectedImage({ ...selectedImage, resolvedSrcUrl: dataUrl });
    }
  };

  // Called when a new table is injected into current drawing
  const handleTableInjected = (modifiedDxf: string) => {
    setCurrentDxfText(modifiedDxf);
    setHasModifications(true);
    const updated = analyzeDXF(
      currentFileName,
      modifiedDxf,
      modifiedDxf.length
    );
    setAnalysis(updated);
    if (updated.tables.length > 0) {
      setSelectedTable(updated.tables[updated.tables.length - 1]);
    }
    setActiveTab('viewer');
  };

  // Download the currently active DXF
  const handleDownloadCurrentDxf = () => {
    if (!currentDxfText) return;
    const blob = new Blob([currentDxfText], { type: 'application/dxf;charset=utf-8' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.href = url;
    const cleanName = currentFileName.replace(/\.dxf$/i, '');
    link.download = hasModifications ? `${cleanName}_with_table.dxf` : currentFileName;
    link.click();
    URL.revokeObjectURL(url);
  };

  return (
    <div className="min-h-screen bg-slate-950 text-slate-100 flex flex-col">
      {/* Top Header Navigation */}
      <Header
        currentFileName={currentFileName}
        analysis={analysis}
        onFileUpload={handleFileUpload}
        onLoadPreset={loadPreset}
        onDownloadCurrentDxf={handleDownloadCurrentDxf}
        hasModifications={hasModifications}
        isLoading={isLoading}
      />

      {/* Main Workspace */}
      <main className="flex-1 max-w-7xl w-full mx-auto p-4 sm:p-6 flex flex-col gap-4">
        {/* Error Alert */}
        {errorMsg && (
          <div className="p-3 bg-red-950/60 border border-red-800 text-red-200 text-xs rounded-xl flex items-center gap-2">
            <AlertCircle className="w-4 h-4 shrink-0 text-red-400" />
            <span>{errorMsg}</span>
          </div>
        )}

        {/* Tab Navigation */}
        <div className="flex flex-wrap items-center justify-between gap-2 border-b border-slate-800 pb-3">
          <div className="flex flex-wrap items-center gap-1.5">
            <button
              onClick={() => setActiveTab('viewer')}
              className={`flex items-center gap-2 px-3.5 py-2 text-xs font-semibold rounded-lg transition-all ${
                activeTab === 'viewer'
                  ? 'bg-blue-600 text-white shadow-md'
                  : 'bg-slate-900 hover:bg-slate-800 text-slate-300'
              }`}
            >
              <Eye className="w-3.5 h-3.5" />
              CAD Drawing & Table Preview
            </button>

            <button
              onClick={() => setActiveTab('tables')}
              className={`flex items-center gap-2 px-3.5 py-2 text-xs font-semibold rounded-lg transition-all ${
                activeTab === 'tables'
                  ? 'bg-blue-600 text-white shadow-md'
                  : 'bg-slate-900 hover:bg-slate-800 text-slate-300'
              }`}
            >
              <Table className="w-3.5 h-3.5" />
              Decoded Tables ({analysis?.tables.length || 0})
            </button>

            <button
              onClick={() => setActiveTab('xrefs')}
              className={`flex items-center gap-2 px-3.5 py-2 text-xs font-semibold rounded-lg transition-all ${
                activeTab === 'xrefs'
                  ? 'bg-cyan-600 text-white shadow-md'
                  : 'bg-slate-900 hover:bg-slate-800 text-slate-300'
              }`}
            >
              <ImageIcon className="w-3.5 h-3.5" />
              External References & Images ({analysis?.externalReferences?.length || 0})
            </button>

            <button
              onClick={() => setActiveTab('generator')}
              className={`flex items-center gap-2 px-3.5 py-2 text-xs font-semibold rounded-lg transition-all ${
                activeTab === 'generator'
                  ? 'bg-blue-600 text-white shadow-md'
                  : 'bg-slate-900 hover:bg-slate-800 text-slate-300'
              }`}
            >
              <PlusCircle className="w-3.5 h-3.5" />
              Table Builder & Injector
            </button>

            <button
              onClick={() => setActiveTab('overview')}
              className={`flex items-center gap-2 px-3.5 py-2 text-xs font-semibold rounded-lg transition-all ${
                activeTab === 'overview'
                  ? 'bg-blue-600 text-white shadow-md'
                  : 'bg-slate-900 hover:bg-slate-800 text-slate-300'
              }`}
            >
              <BarChart3 className="w-3.5 h-3.5" />
              Drawing Metrics & Analysis
            </button>

            <button
              onClick={() => setActiveTab('tags')}
              className={`flex items-center gap-2 px-3.5 py-2 text-xs font-semibold rounded-lg transition-all ${
                activeTab === 'tags'
                  ? 'bg-blue-600 text-white shadow-md'
                  : 'bg-slate-900 hover:bg-slate-800 text-slate-300'
              }`}
            >
              <Code2 className="w-3.5 h-3.5" />
              Raw Tag Inspector
            </button>
          </div>

          {/* Quick Active File Badge */}
          {analysis && (
            <div className="flex items-center gap-2 text-xs text-slate-400 font-mono">
              <FileCode className="w-3.5 h-3.5 text-blue-400" />
              <span className="truncate max-w-[200px]">{currentFileName}</span>
              <span className="text-slate-600">|</span>
              <span className="text-cyan-400">{analysis.version}</span>
            </div>
          )}
        </div>

        {/* Tab Contents */}
        {isLoading ? (
          <div className="flex flex-col items-center justify-center min-h-[400px] text-slate-400 gap-3">
            <Loader2 className="w-8 h-8 animate-spin text-blue-500" />
            <span className="text-sm font-medium">Parsing DXF structure & tags...</span>
          </div>
        ) : analysis ? (
          <div className="flex-1">
            {activeTab === 'viewer' && (
              <div className="flex flex-col lg:flex-row gap-4 h-[640px]">
                {/* CAD Canvas */}
                <div className="flex-1 h-full rounded-xl overflow-hidden border border-slate-800 shadow-2xl">
                  <CADViewer
                    analysis={analysis}
                    selectedTable={selectedTable}
                    onSelectTable={(tbl) => {
                      setSelectedTable(tbl);
                      if (tbl) setSelectedImage(null);
                    }}
                    selectedImage={selectedImage}
                    onSelectImage={(img) => {
                      setSelectedImage(img);
                      if (img) setSelectedTable(null);
                    }}
                  />
                </div>

                {/* Right Quick Inspector Panel */}
                <div className="w-full lg:w-96 flex flex-col gap-4 overflow-y-auto max-h-[640px]">
                  {selectedImage ? (
                    <div className="bg-slate-900/60 border border-slate-800 p-4 rounded-xl flex flex-col gap-3">
                      <div className="flex items-center justify-between border-b border-slate-800 pb-2">
                        <span className="text-xs font-bold uppercase tracking-wider text-slate-300 flex items-center gap-1.5">
                          <ImageIcon className="w-3.5 h-3.5 text-cyan-400" />
                          <span>Image Inspector</span>
                        </span>
                        <span className="font-mono text-cyan-400 text-xs">
                          0x{selectedImage.handle}
                        </span>
                      </div>

                      {/* Image Thumbnail */}
                      <div className="w-full h-36 bg-slate-950 rounded-lg border border-slate-800 flex items-center justify-center p-2 relative overflow-hidden">
                        <img
                          src={selectedImage.resolvedSrcUrl || `/samples/${selectedImage.imageFileName}`}
                          alt={selectedImage.imageFileName}
                          className="max-w-full max-h-full object-contain"
                        />
                      </div>

                      <div className="space-y-1.5 text-xs">
                        <div className="flex justify-between py-1 border-b border-slate-800/60">
                          <span className="text-slate-400">Файл:</span>
                          <span className="font-mono text-white truncate max-w-[180px]">
                            {selectedImage.imageFileName}
                          </span>
                        </div>
                        <div className="flex justify-between py-1 border-b border-slate-800/60">
                          <span className="text-slate-400">Разрешение:</span>
                          <span className="font-mono text-cyan-400">
                            {selectedImage.imageSize.width} × {selectedImage.imageSize.height} px
                          </span>
                        </div>
                        <div className="flex justify-between py-1 border-b border-slate-800/60">
                          <span className="text-slate-400">В чертеже:</span>
                          <span className="font-mono text-emerald-400">
                            {selectedImage.cadWidth} × {selectedImage.cadHeight} мм
                          </span>
                        </div>
                        <div className="flex justify-between py-1 border-b border-slate-800/60">
                          <span className="text-slate-400">Положение:</span>
                          <span className="font-mono text-slate-300">
                            X: {selectedImage.x.toFixed(2)}, Y: {selectedImage.y.toFixed(2)}
                          </span>
                        </div>
                        <div className="flex justify-between py-1 border-b border-slate-800/60">
                          <span className="text-slate-400">Поворот:</span>
                          <span className="font-mono text-slate-300">
                            {selectedImage.rotationDeg}°
                          </span>
                        </div>
                        <div className="flex justify-between py-1 border-b border-slate-800/60">
                          <span className="text-slate-400">IMAGEDEF:</span>
                          <span className="font-mono text-cyan-400">
                            0x{selectedImage.imageDefHandle}
                          </span>
                        </div>
                        <div className="flex justify-between py-1 border-b border-slate-800/60">
                          <span className="text-slate-400">Яркость/Контраст:</span>
                          <span className="font-mono text-slate-300">
                            {selectedImage.brightness}% / {selectedImage.contrast}%
                          </span>
                        </div>
                      </div>

                      <div className="pt-2 flex flex-col gap-2">
                        <button
                          onClick={() => setActiveTab('xrefs')}
                          className="w-full py-1.5 px-3 bg-cyan-600/20 hover:bg-cyan-600/30 text-cyan-300 border border-cyan-700/50 rounded-lg text-xs font-semibold transition-colors flex items-center justify-center gap-1.5"
                        >
                          <ImageIcon className="w-3.5 h-3.5" />
                          <span>Диспетчер внешних ссылок</span>
                        </button>
                      </div>
                    </div>
                  ) : (
                    <div className="bg-slate-900/60 border border-slate-800 p-4 rounded-xl">
                      <h2 className="text-xs font-bold uppercase tracking-wider text-slate-400 mb-3 flex items-center justify-between">
                        <span>Selected Table Inspector</span>
                        {selectedTable && (
                          <span className="font-mono text-cyan-400">
                            {selectedTable.handle}
                          </span>
                        )}
                      </h2>
                      <TableDetails
                        table={selectedTable}
                        allTables={analysis.tables}
                        onSelectTable={(tbl) => {
                          setSelectedTable(tbl);
                          setSelectedImage(null);
                        }}
                      />
                    </div>
                  )}
                </div>
              </div>
            )}

            {activeTab === 'xrefs' && (
              <div className="bg-slate-900/40 border border-slate-800 p-6 rounded-2xl">
                <ExternalReferenceManager
                  analysis={analysis}
                  selectedImage={selectedImage}
                  onSelectImage={(img) => {
                    setSelectedImage(img);
                    setActiveTab('viewer');
                  }}
                  onCustomImageUploaded={handleCustomImageUploaded}
                />
              </div>
            )}

            {activeTab === 'tables' && (
              <div className="bg-slate-900/40 border border-slate-800 p-6 rounded-2xl">
                <div className="mb-4">
                  <h2 className="text-base font-bold text-slate-100">
                    Decoded ACAD_TABLE Entities & Cell Grids
                  </h2>
                  <p className="text-xs text-slate-400 mt-1">
                    Extracted table cells, handles, coordinates, and multi-fragment break structure
                  </p>
                </div>
                <TableDetails
                  table={selectedTable}
                  allTables={analysis.tables}
                  onSelectTable={(tbl) => setSelectedTable(tbl)}
                />
              </div>
            )}

            {activeTab === 'generator' && (
              <div className="bg-slate-900/40 border border-slate-800 p-6 rounded-2xl">
                <div className="mb-4">
                  <h2 className="text-base font-bold text-slate-100">
                    AutoCAD & ZCAD Table Generator and Injector
                  </h2>
                  <p className="text-xs text-slate-400 mt-1">
                    Design custom engineering schedules, configure multi-fragment splits, and inject directly into your DXF drawings
                  </p>
                </div>
                <TableGenerator
                  currentDxfText={currentDxfText}
                  analysis={analysis}
                  onTableInjected={handleTableInjected}
                />
              </div>
            )}

            {activeTab === 'overview' && (
              <div className="bg-slate-900/40 border border-slate-800 p-6 rounded-2xl">
                <div className="mb-4">
                  <h2 className="text-base font-bold text-slate-100">
                    Drawing Structure, Headers & Sections
                  </h2>
                  <p className="text-xs text-slate-400 mt-1">
                    Comprehensive DXF metadata, section markers, entity breakdowns, and extents
                  </p>
                </div>
                <AnalysisOverview analysis={analysis} />
              </div>
            )}

            {activeTab === 'tags' && (
              <div className="bg-slate-900/40 border border-slate-800 p-6 rounded-2xl">
                <div className="mb-4">
                  <h2 className="text-base font-bold text-slate-100">
                    Raw DXF Tag Stream Inspector
                  </h2>
                  <p className="text-xs text-slate-400 mt-1">
                    Examine low-level group codes, handles, pointers, and tag pairs
                  </p>
                </div>
                <RawTagInspector dxfContent={currentDxfText} />
              </div>
            )}
          </div>
        ) : null}
      </main>

      {/* Footer */}
      <footer className="border-t border-slate-800/80 bg-slate-950 py-3 text-center text-xs text-slate-500">
        DXF Table Save & Analyzer — Full compatibility with AutoCAD 2007+, ZCAD, LibreCAD, and standard DXF formats.
      </footer>
    </div>
  );
};

export default App;
