import React, { useRef } from 'react';
import { DXFAnalysisResult, DXFImageEntity, DXFExternalReference } from '../types/dxf';
import {
  Image as ImageIcon,
  FileCode,
  CheckCircle2,
  AlertTriangle,
  Upload,
  Download,
  Info,
  Layers,
  Move,
  Maximize,
  Sliders,
  ExternalLink,
  Eye,
} from 'lucide-react';

interface ExternalReferenceManagerProps {
  analysis: DXFAnalysisResult;
  selectedImage: DXFImageEntity | null;
  onSelectImage: (image: DXFImageEntity | null) => void;
  onCustomImageUploaded?: (imageHandle: string, dataUrl: string) => void;
}

export const ExternalReferenceManager: React.FC<ExternalReferenceManagerProps> = ({
  analysis,
  selectedImage,
  onSelectImage,
  onCustomImageUploaded,
}) => {
  const fileInputRef = useRef<HTMLInputElement | null>(null);
  const targetImageRef = useRef<DXFImageEntity | null>(null);

  const images = analysis.images || [];
  const xrefs = analysis.externalReferences || [];

  const handleFileChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    if (e.target.files && e.target.files[0] && targetImageRef.current) {
      const file = e.target.files[0];
      const reader = new FileReader();
      reader.onload = (event) => {
        const dataUrl = event.target?.result as string;
        if (targetImageRef.current && onCustomImageUploaded) {
          onCustomImageUploaded(targetImageRef.current.handle, dataUrl);
        }
      };
      reader.readAsDataURL(file);
    }
  };

  const triggerUploadForImage = (img: DXFImageEntity) => {
    targetImageRef.current = img;
    fileInputRef.current?.click();
  };

  const activeImage = selectedImage || (images.length > 0 ? images[0] : null);

  return (
    <div className="flex flex-col gap-6">
      {/* Hidden File Input for uploading replacement image */}
      <input
        type="file"
        ref={fileInputRef}
        onChange={handleFileChange}
        accept="image/png,image/jpeg,image/svg+xml,image/bmp,image/webp"
        className="hidden"
      />

      {/* Header Summary Banner */}
      <div className="bg-slate-900/70 border border-slate-800 rounded-xl p-5 shadow-lg">
        <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4">
          <div className="flex items-center gap-3">
            <div className="w-10 h-10 rounded-lg bg-cyan-950/80 border border-cyan-800/60 flex items-center justify-center text-cyan-400">
              <ImageIcon className="w-5 h-5" />
            </div>
            <div>
              <h2 className="text-sm font-bold text-white flex items-center gap-2">
                <span>Диспетчер внешних ссылок и растровых изображений</span>
                <span className="text-xs font-mono font-normal bg-cyan-950 text-cyan-300 border border-cyan-800 px-2 py-0.5 rounded-full">
                  {xrefs.length} ссылок / {images.length} растров
                </span>
              </h2>
              <p className="text-xs text-slate-400 mt-0.5">
                Обнаруженные в файле DXF объекты <code className="text-cyan-300 font-mono">IMAGEDEF</code>,{' '}
                <code className="text-cyan-300 font-mono">IMAGE</code>, словари{' '}
                <code className="text-cyan-300 font-mono">ACAD_IMAGE_DICT</code> и внешние ссылки AutoCAD.
              </p>
            </div>
          </div>

          <div className="flex items-center gap-2">
            <button
              onClick={() => {
                if (images.length > 0) {
                  onSelectImage(images[0]);
                }
              }}
              className="flex items-center gap-1.5 px-3 py-1.5 text-xs font-medium bg-slate-800 hover:bg-slate-700 text-slate-200 rounded-lg border border-slate-700 transition-colors"
            >
              <Eye className="w-3.5 h-3.5 text-cyan-400" />
              <span>Выделить на чертеже</span>
            </button>
          </div>
        </div>
      </div>

      {xrefs.length === 0 && images.length === 0 ? (
        <div className="flex flex-col items-center justify-center p-12 text-center text-slate-400 border border-dashed border-slate-800 rounded-xl bg-slate-900/30">
          <ImageIcon className="w-12 h-12 mb-3 text-slate-600" />
          <p className="text-sm font-medium text-slate-300">
            В текущем чертеже не найдено внешних ссылок или растровых изображений
          </p>
          <p className="text-xs text-slate-500 mt-1 max-w-md">
            Откройте файл <code className="text-cyan-400">acadtableandhrefImage2007.dxf</code> в панели пресетов вверху для просмотра разделенной таблицы и растрового изображения.
          </p>
        </div>
      ) : (
        <div className="grid grid-cols-1 lg:grid-cols-3 gap-6">
          {/* List of References */}
          <div className="lg:col-span-2 flex flex-col gap-4">
            <div className="bg-slate-900/80 border border-slate-800 rounded-xl overflow-hidden shadow-lg">
              <div className="px-4 py-3 bg-slate-950/60 border-b border-slate-800 flex items-center justify-between">
                <span className="text-xs font-bold uppercase tracking-wider text-slate-300">
                  Список внешних ссылок (External References)
                </span>
                <span className="text-[11px] text-slate-500">
                  AutoCAD XREF Palette Specification
                </span>
              </div>

              <div className="divide-y divide-slate-800/80">
                {images.map((img) => {
                  const isSelected = selectedImage?.handle === img.handle;
                  return (
                    <div
                      key={img.handle}
                      onClick={() => onSelectImage(img)}
                      className={`p-4 flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4 cursor-pointer transition-colors ${
                        isSelected
                          ? 'bg-cyan-950/30 border-l-4 border-cyan-500'
                          : 'hover:bg-slate-800/40'
                      }`}
                    >
                      <div className="flex items-center gap-3">
                        <div className="w-12 h-12 rounded-lg bg-slate-950 border border-slate-800 flex items-center justify-center overflow-hidden shrink-0">
                          <img
                            src={img.resolvedSrcUrl || `/samples/${img.imageFileName}`}
                            alt={img.imageFileName}
                            className="w-full h-full object-contain"
                            onError={(e) => {
                              // Fallback on error
                              (e.target as HTMLElement).style.display = 'none';
                            }}
                          />
                          <ImageIcon className="w-5 h-5 text-slate-600" />
                        </div>

                        <div>
                          <div className="flex items-center gap-2">
                            <span className="text-sm font-semibold text-white">
                              {img.imageFileName}
                            </span>
                            <span className="text-[10px] font-mono bg-blue-950 text-blue-300 border border-blue-800 px-1.5 py-0.5 rounded">
                              IMAGE
                            </span>
                            <span className="text-[10px] font-mono text-slate-500">
                              0x{img.handle}
                            </span>
                          </div>

                          <div className="text-xs text-slate-400 mt-0.5 font-mono flex flex-wrap items-center gap-x-3 gap-y-1">
                            <span>
                              Путь: <span className="text-slate-300">{img.imagePath || `.\\${img.imageFileName}`}</span>
                            </span>
                            <span>
                              Разрешение:{' '}
                              <span className="text-cyan-400">
                                {img.imageSize.width} × {img.imageSize.height} px
                              </span>
                            </span>
                          </div>

                          <div className="text-xs text-slate-400 mt-0.5 flex items-center gap-3">
                            <span>
                              В чертеже:{' '}
                              <span className="text-emerald-400 font-semibold font-mono">
                                {img.cadWidth} × {img.cadHeight} мм
                              </span>
                            </span>
                            <span className="text-slate-600">•</span>
                            <span>Слой: <span className="text-slate-300">{img.layer}</span></span>
                          </div>
                        </div>
                      </div>

                      <div className="flex items-center gap-2 shrink-0 self-end sm:self-center">
                        <button
                          onClick={(e) => {
                            e.stopPropagation();
                            triggerUploadForImage(img);
                          }}
                          className="flex items-center gap-1 px-2.5 py-1 text-xs font-medium bg-slate-800 hover:bg-slate-700 text-slate-200 border border-slate-700 rounded-md transition-colors"
                          title="Заменить или подгрузить файл изображения"
                        >
                          <Upload className="w-3 h-3 text-cyan-400" />
                          <span>Заменить файл</span>
                        </button>

                        <a
                          href={`/samples/${img.imageFileName}`}
                          download={img.imageFileName}
                          onClick={(e) => e.stopPropagation()}
                          className="p-1.5 hover:bg-slate-800 text-slate-300 rounded-md transition-colors border border-transparent hover:border-slate-700"
                          title="Скачать изображение"
                        >
                          <Download className="w-3.5 h-3.5" />
                        </a>
                      </div>
                    </div>
                  );
                })}
              </div>
            </div>

            {/* Drawing Extents & Placement info */}
            {activeImage && (
              <div className="bg-slate-900/60 border border-slate-800 rounded-xl p-4">
                <h3 className="text-xs font-bold uppercase tracking-wider text-slate-300 mb-3 flex items-center gap-2">
                  <Move className="w-3.5 h-3.5 text-cyan-400" />
                  <span>Геометрия размещения растра в мировой системе координат (WCS)</span>
                </h3>

                <div className="grid grid-cols-2 sm:grid-cols-4 gap-3 text-xs font-mono">
                  <div className="bg-slate-950 p-2.5 rounded-lg border border-slate-800">
                    <span className="text-slate-500 block text-[10px]">Точка вставки X (DXF 10)</span>
                    <span className="text-cyan-400 font-semibold">{activeImage.x.toFixed(4)}</span>
                  </div>
                  <div className="bg-slate-950 p-2.5 rounded-lg border border-slate-800">
                    <span className="text-slate-500 block text-[10px]">Точка вставки Y (DXF 20)</span>
                    <span className="text-emerald-400 font-semibold">{activeImage.y.toFixed(4)}</span>
                  </div>
                  <div className="bg-slate-950 p-2.5 rounded-lg border border-slate-800">
                    <span className="text-slate-500 block text-[10px]">Ширина растра (мм)</span>
                    <span className="text-amber-400 font-semibold">{activeImage.cadWidth}</span>
                  </div>
                  <div className="bg-slate-950 p-2.5 rounded-lg border border-slate-800">
                    <span className="text-slate-500 block text-[10px]">Высота растра (мм)</span>
                    <span className="text-amber-400 font-semibold">{activeImage.cadHeight}</span>
                  </div>
                </div>

                <div className="mt-3 grid grid-cols-2 sm:grid-cols-3 gap-3 text-xs font-mono">
                  <div className="bg-slate-950 p-2.5 rounded-lg border border-slate-800">
                    <span className="text-slate-500 block text-[10px]">Вектор U (DXF 11, 21, 31)</span>
                    <span className="text-slate-300">
                      [{activeImage.uVector.x.toFixed(6)}, {activeImage.uVector.y.toFixed(6)}]
                    </span>
                  </div>
                  <div className="bg-slate-950 p-2.5 rounded-lg border border-slate-800">
                    <span className="text-slate-500 block text-[10px]">Вектор V (DXF 12, 22, 32)</span>
                    <span className="text-slate-300">
                      [{activeImage.vVector.x.toFixed(6)}, {activeImage.vVector.y.toFixed(6)}]
                    </span>
                  </div>
                  <div className="bg-slate-950 p-2.5 rounded-lg border border-slate-800">
                    <span className="text-slate-500 block text-[10px]">Угол поворота</span>
                    <span className="text-purple-400 font-semibold">{activeImage.rotationDeg}°</span>
                  </div>
                </div>
              </div>
            )}
          </div>

          {/* Right Panel: Selected Image Deep Inspector */}
          <div className="flex flex-col gap-4">
            {activeImage ? (
              <div className="bg-slate-900/80 border border-slate-800 rounded-xl p-5 shadow-lg flex flex-col gap-4">
                <div className="flex items-center justify-between border-b border-slate-800 pb-3">
                  <span className="text-xs font-bold uppercase tracking-wider text-slate-300">
                    Свойства изображения
                  </span>
                  <span className="text-xs font-mono text-cyan-400">
                    0x{activeImage.handle}
                  </span>
                </div>

                {/* Preview Thumbnail */}
                <div className="w-full h-40 bg-slate-950 rounded-lg border border-slate-800 flex items-center justify-center p-2 overflow-hidden relative">
                  <img
                    src={activeImage.resolvedSrcUrl || `/samples/${activeImage.imageFileName}`}
                    alt={activeImage.imageFileName}
                    className="max-w-full max-h-full object-contain"
                  />
                  <div className="absolute bottom-2 right-2 px-2 py-0.5 bg-slate-900/80 backdrop-blur rounded text-[10px] font-mono text-slate-400 border border-slate-800">
                    {activeImage.imageSize.width} × {activeImage.imageSize.height} px
                  </div>
                </div>

                {/* Detailed property table */}
                <div className="space-y-2 text-xs">
                  <div className="flex justify-between py-1 border-b border-slate-800/60">
                    <span className="text-slate-400">Имя файла:</span>
                    <span className="font-mono text-white">{activeImage.imageFileName}</span>
                  </div>
                  <div className="flex justify-between py-1 border-b border-slate-800/60">
                    <span className="text-slate-400">Объект IMAGEDEF:</span>
                    <span className="font-mono text-cyan-400">0x{activeImage.imageDefHandle}</span>
                  </div>
                  <div className="flex justify-between py-1 border-b border-slate-800/60">
                    <span className="text-slate-400">Отображение растра:</span>
                    <span className="text-emerald-400 font-medium">
                      {activeImage.showImage ? 'Включено (Да)' : 'Отключено (Нет)'}
                    </span>
                  </div>
                  <div className="flex justify-between py-1 border-b border-slate-800/60">
                    <span className="text-slate-400">Прозрачность:</span>
                    <span className="text-slate-300">
                      {activeImage.transparency ? 'Включена' : 'Отключена'}
                    </span>
                  </div>
                  <div className="flex justify-between py-1 border-b border-slate-800/60">
                    <span className="text-slate-400">Контур подрезки (Clip):</span>
                    <span className="text-slate-300">
                      {activeImage.clipping ? 'Активен' : 'Отключен'}
                    </span>
                  </div>
                  <div className="flex justify-between py-1 border-b border-slate-800/60">
                    <span className="text-slate-400">Яркость (DXF 281):</span>
                    <span className="font-mono text-slate-200">{activeImage.brightness}%</span>
                  </div>
                  <div className="flex justify-between py-1 border-b border-slate-800/60">
                    <span className="text-slate-400">Контрастность (DXF 282):</span>
                    <span className="font-mono text-slate-200">{activeImage.contrast}%</span>
                  </div>
                  <div className="flex justify-between py-1 border-b border-slate-800/60">
                    <span className="text-slate-400">Слияние / Fade (DXF 283):</span>
                    <span className="font-mono text-slate-200">{activeImage.fade}%</span>
                  </div>
                  {activeImage.imageDef && (
                    <div className="flex justify-between py-1 border-b border-slate-800/60">
                      <span className="text-slate-400">Единицы разрешения:</span>
                      <span className="text-slate-300">{activeImage.imageDef.resolutionUnitName}</span>
                    </div>
                  )}
                </div>

                <div className="pt-2 flex flex-col gap-2">
                  <button
                    onClick={() => triggerUploadForImage(activeImage)}
                    className="w-full flex items-center justify-center gap-2 px-3 py-2 text-xs font-semibold bg-cyan-600 hover:bg-cyan-500 text-white rounded-lg transition-colors shadow-sm"
                  >
                    <Upload className="w-3.5 h-3.5" />
                    <span>Загрузить новый растр для ссылки</span>
                  </button>
                </div>
              </div>
            ) : (
              <div className="bg-slate-900/40 border border-slate-800 rounded-xl p-6 text-center text-slate-500 text-xs">
                Выберите изображение из списка для просмотра свойств
              </div>
            )}
          </div>
        </div>
      )}
    </div>
  );
};
