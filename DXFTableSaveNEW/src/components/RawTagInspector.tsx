import React, { useState, useMemo } from 'react';
import { DXFTag } from '../types/dxf';
import { parseDXFTags } from '../services/dxfParser';
import { Search, Filter, Code2, ChevronLeft, ChevronRight } from 'lucide-react';

interface RawTagInspectorProps {
  dxfContent: string;
}

const COMMON_CODES: { code: number; label: string }[] = [
  { code: 0, label: 'Entity/Section Type' },
  { code: 2, label: 'Name / Section Name' },
  { code: 5, label: 'Handle (Hex ID)' },
  { code: 8, label: 'Layer' },
  { code: 10, label: 'Primary X / Insert X' },
  { code: 20, label: 'Primary Y / Insert Y' },
  { code: 90, label: 'Row Count / Int32' },
  { code: 91, label: 'Col Count / Int32' },
  { code: 100, label: 'Subclass Marker' },
  { code: 300, label: 'Cell String Data' },
  { code: 330, label: 'Soft Pointer / Owner' },
  { code: 342, label: 'Hard Pointer / Style' },
];

export const RawTagInspector: React.FC<RawTagInspectorProps> = ({ dxfContent }) => {
  const [searchTerm, setSearchTerm] = useState<string>('');
  const [selectedGroupCode, setSelectedGroupCode] = useState<number | null>(null);
  const [currentPage, setCurrentPage] = useState<number>(1);
  const pageSize = 100;

  // Parse raw tags once per file
  const tags: DXFTag[] = useMemo(() => {
    return parseDXFTags(dxfContent);
  }, [dxfContent]);

  // Filtered tags
  const filteredTags = useMemo(() => {
    return tags.filter((t) => {
      if (selectedGroupCode !== null && t.code !== selectedGroupCode) {
        return false;
      }
      if (searchTerm) {
        const term = searchTerm.toLowerCase();
        const codeMatches = t.code.toString().includes(term);
        const valMatches = String(t.value).toLowerCase().includes(term);
        return codeMatches || valMatches;
      }
      return true;
    });
  }, [tags, selectedGroupCode, searchTerm]);

  const totalPages = Math.ceil(filteredTags.length / pageSize) || 1;
  const pagedTags = useMemo(() => {
    const start = (currentPage - 1) * pageSize;
    return filteredTags.slice(start, start + pageSize);
  }, [filteredTags, currentPage, pageSize]);

  return (
    <div className="flex flex-col gap-4">
      {/* Search & Code Filter Bar */}
      <div className="flex flex-col sm:flex-row items-stretch sm:items-center justify-between gap-3 bg-slate-900/80 border border-slate-800 p-3 rounded-xl">
        <div className="relative flex-1">
          <Search className="absolute left-3 top-2.5 w-4 h-4 text-slate-500" />
          <input
            type="text"
            placeholder="Search tags, handles, text strings (e.g., 'AcDbTable', '5', 'SECTION')..."
            value={searchTerm}
            onChange={(e) => {
              setSearchTerm(e.target.value);
              setCurrentPage(1);
            }}
            className="w-full pl-9 pr-3 py-1.5 bg-slate-950 border border-slate-700/70 rounded-lg text-xs text-slate-200 placeholder-slate-500 focus:outline-none focus:border-blue-500 font-mono"
          />
        </div>

        <div className="flex items-center gap-2">
          <Filter className="w-3.5 h-3.5 text-slate-400" />
          <select
            value={selectedGroupCode !== null ? selectedGroupCode : ''}
            onChange={(e) => {
              const val = e.target.value;
              setSelectedGroupCode(val !== '' ? parseInt(val, 10) : null);
              setCurrentPage(1);
            }}
            className="px-2.5 py-1.5 bg-slate-950 border border-slate-700/70 rounded-lg text-xs text-slate-200 focus:outline-none font-mono"
          >
            <option value="">All Group Codes</option>
            {COMMON_CODES.map((c) => (
              <option key={c.code} value={c.code}>
                Code {c.code} ({c.label})
              </option>
            ))}
          </select>
        </div>
      </div>

      {/* Stats and Navigation */}
      <div className="flex items-center justify-between text-xs text-slate-400 px-1 font-mono">
        <div>
          Showing {pagedTags.length} of {filteredTags.length.toLocaleString()} matched tags
          (Total in file: {tags.length.toLocaleString()})
        </div>

        <div className="flex items-center gap-2">
          <button
            onClick={() => setCurrentPage((p) => Math.max(p - 1, 1))}
            disabled={currentPage <= 1}
            className="p-1 rounded bg-slate-800 hover:bg-slate-700 disabled:opacity-30 disabled:cursor-not-allowed"
          >
            <ChevronLeft className="w-4 h-4" />
          </button>
          <span>
            Page {currentPage} / {totalPages}
          </span>
          <button
            onClick={() => setCurrentPage((p) => Math.min(p + 1, totalPages))}
            disabled={currentPage >= totalPages}
            className="p-1 rounded bg-slate-800 hover:bg-slate-700 disabled:opacity-30 disabled:cursor-not-allowed"
          >
            <ChevronRight className="w-4 h-4" />
          </button>
        </div>
      </div>

      {/* Tag Stream Table */}
      <div className="border border-slate-800 rounded-xl overflow-hidden bg-slate-950/70 font-mono text-xs">
        <div className="grid grid-cols-12 bg-slate-900 px-3 py-2 border-b border-slate-800 text-slate-400 font-semibold">
          <div className="col-span-1 text-center">#</div>
          <div className="col-span-2">Group Code</div>
          <div className="col-span-3">Meaning / Description</div>
          <div className="col-span-6">Raw Tag Value</div>
        </div>

        <div className="divide-y divide-slate-800/60 max-h-[480px] overflow-y-auto">
          {pagedTags.map((tag, idx) => {
            const absoluteIdx = (currentPage - 1) * pageSize + idx + 1;
            const desc =
              COMMON_CODES.find((c) => c.code === tag.code)?.label ||
              (tag.code >= 10 && tag.code < 40 ? 'Coordinate Point' : 'Data Field');

            const isHighlighted =
              tag.code === 0 ||
              tag.code === 5 ||
              tag.code === 100 ||
              tag.value === 'ACAD_TABLE' ||
              tag.value === 'TABLESTYLE';

            return (
              <div
                key={idx}
                className={`grid grid-cols-12 px-3 py-1.5 items-center hover:bg-slate-800/40 transition-colors ${
                  isHighlighted ? 'bg-blue-950/20' : ''
                }`}
              >
                <div className="col-span-1 text-center text-slate-500 text-[11px]">
                  {absoluteIdx}
                </div>
                <div className="col-span-2 font-bold text-cyan-400">
                  {tag.code}
                </div>
                <div className="col-span-3 text-[11px] text-slate-400 truncate">
                  {desc}
                </div>
                <div
                  className={`col-span-6 truncate font-medium ${
                    tag.code === 0
                      ? 'text-amber-400 font-bold'
                      : tag.code === 5
                      ? 'text-purple-300'
                      : tag.code === 100
                      ? 'text-emerald-400'
                      : 'text-slate-200'
                  }`}
                  title={String(tag.value)}
                >
                  {String(tag.value)}
                </div>
              </div>
            );
          })}
        </div>
      </div>
    </div>
  );
};
