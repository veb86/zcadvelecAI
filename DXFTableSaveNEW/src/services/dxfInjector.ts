import { NewTableDefinition } from '../types/dxf';

/**
 * Port of dxf_injector.py and dxf_table_template.py into TypeScript
 * Generates TABLESTYLE and ACAD_TABLE DXF records, plus universal vector table fallbacks
 */

export function nextHexHandle(currentHex: string): { nextHex: string; num: number } {
  let num = parseInt(currentHex, 16);
  if (isNaN(num) || num <= 0) num = 0x2000;
  num += 1;
  return {
    nextHex: num.toString(16).toUpperCase(),
    num,
  };
}

/**
 * Finds the line range for a specific DXF section
 */
export function findSectionLines(
  lines: string[],
  sectionName: string
): { start: number; end: number } | null {
  let inSection = false;
  let startIdx = -1;
  let endIdx = -1;

  for (let i = 0; i < lines.length; i++) {
    const trimmed = lines[i].trim();
    if (trimmed === 'SECTION') {
      // Check the next tag (code 2 has section name)
      if (i + 2 < lines.length && lines[i + 2].trim() === sectionName) {
        inSection = true;
        startIdx = i;
        continue;
      }
    }

    if (inSection && trimmed === 'ENDSEC') {
      endIdx = i;
      break;
    }
  }

  if (inSection && endIdx !== -1) {
    return { start: startIdx, end: endIdx };
  }
  return null;
}

/**
 * Generates AutoCAD 2007+ TABLESTYLE record for the OBJECTS section
 */
export function generateTableStyleDxf(
  styleName: string,
  handle: string,
  ownerHandle: string = '0'
): string {
  return [
    '  0',
    'TABLESTYLE',
    '  5',
    handle,
    '330',
    ownerHandle,
    '100',
    'AcDbXObject',
    '100',
    'AcDbTableStyle',
    '  2',
    styleName,
    '100',
    'AcDbTableStyle',
    '170',
    '1',
    '171',
    '3',
    '172',
    '3',
    '140',
    '2.5',
    '141',
    '2.5',
    '142',
    '2.5',
    '173',
    '0',
    '174',
    '0',
    '175',
    '0',
    '176',
    '0',
    '177',
    '0',
    '178',
    '0',
    '179',
    '0',
    '280',
    '1',
    '281',
    '0',
    '300',
    'FlowingText',
    '301',
    'Standard',
    '302',
    'Standard',
    '303',
    'Standard',
  ].join('\n');
}

/**
 * Generates native ACAD_TABLE entity for the ENTITIES section
 */
export function generateAcadTableEntityDxf(
  tableDef: {
    handle: string;
    styleHandle: string;
    x: number;
    y: number;
    z: number;
    layer: string;
    columnWidths: number[];
    rowHeights: number[];
    rowsData: string[][];
  }
): string {
  const numRows = tableDef.rowsData.length;
  const numCols = tableDef.columnWidths.length;

  const lines: string[] = [
    '  0',
    'ACAD_TABLE',
    '  5',
    tableDef.handle,
    '330',
    '1F',
    '100',
    'AcDbEntity',
    '  8',
    tableDef.layer || '0',
    '100',
    'AcDbTable',
    ' 10',
    tableDef.x.toFixed(4),
    ' 20',
    tableDef.y.toFixed(4),
    ' 30',
    tableDef.z.toFixed(4),
    '210',
    '0.0',
    '220',
    '0.0',
    '230',
    '1.0',
    '100',
    'AcDbTable',
    ' 90',
    numRows.toString(),
    ' 91',
    numCols.toString(),
    '330',
    tableDef.styleHandle,
    '280',
    '1',
    '281',
    '0',
    '300',
    'FlowingText',
    '301',
    'Standard',
  ];

  // Output row heights (DXF Group Code 141, repeated per row)
  for (const h of tableDef.rowHeights) {
    lines.push('141', h.toFixed(4));
  }

  // Output column widths (DXF Group Code 142, repeated per column)
  for (const w of tableDef.columnWidths) {
    lines.push('142', w.toFixed(4));
  }

  // Output cell strings
  for (let r = 0; r < numRows; r++) {
    for (let c = 0; c < numCols; c++) {
      const cellVal = tableDef.rowsData[r]?.[c] || '';
      lines.push('300', cellVal.replace(/\r?\n/g, ' '));
      lines.push('170', '1'); // text type
    }
  }

  return lines.join('\n');
}

/**
 * Generates universal Vector CAD Table (LINE + TEXT entities)
 * Guaranteed to open and render in ANY CAD tool: AutoCAD, ZCAD, LibreCAD, QCad, etc.
 */
export function generateVectorTableDxf(
  tableDef: {
    x: number;
    y: number;
    z: number;
    layer: string;
    columnWidths: number[];
    rowHeights: number[];
    rowsData: string[][];
    textHeight: number;
    startHandleHex: string;
  }
): { dxfContent: string; endHandleHex: string } {
  let currentHex = tableDef.startHandleHex;
  const lines: string[] = [];

  const totalWidth = tableDef.columnWidths.reduce((a, b) => a + b, 0);
  const totalHeight = tableDef.rowHeights.reduce((a, b) => a + b, 0);

  // Outer border lines
  const addLine = (x1: number, y1: number, x2: number, y2: number) => {
    const handleInfo = nextHexHandle(currentHex);
    currentHex = handleInfo.nextHex;
    lines.push(
      '  0',
      'LINE',
      '  5',
      currentHex,
      '100',
      'AcDbEntity',
      '  8',
      tableDef.layer || '0',
      '100',
      'AcDbLine',
      ' 10',
      x1.toFixed(4),
      ' 20',
      y1.toFixed(4),
      ' 30',
      tableDef.z.toFixed(4),
      ' 11',
      x2.toFixed(4),
      ' 21',
      y2.toFixed(4),
      ' 31',
      tableDef.z.toFixed(4)
    );
  };

  const addText = (text: string, x: number, y: number, height: number) => {
    if (!text) return;
    const handleInfo = nextHexHandle(currentHex);
    currentHex = handleInfo.nextHex;
    lines.push(
      '  0',
      'TEXT',
      '  5',
      currentHex,
      '100',
      'AcDbEntity',
      '  8',
      tableDef.layer || '0',
      '100',
      'AcDbText',
      ' 10',
      x.toFixed(4),
      ' 20',
      y.toFixed(4),
      ' 30',
      tableDef.z.toFixed(4),
      ' 40',
      height.toFixed(4),
      '  1',
      text.replace(/\r?\n/g, ' ')
    );
  };

  const x0 = tableDef.x;
  const y0 = tableDef.y;

  // Horizontal lines for each row
  let currentY = y0;
  addLine(x0, currentY, x0 + totalWidth, currentY);

  for (let r = 0; r < tableDef.rowHeights.length; r++) {
    currentY -= tableDef.rowHeights[r];
    addLine(x0, currentY, x0 + totalWidth, currentY);
  }

  // Vertical lines for each column
  let currentX = x0;
  addLine(currentX, y0, currentX, y0 - totalHeight);

  for (let c = 0; c < tableDef.columnWidths.length; c++) {
    currentX += tableDef.columnWidths[c];
    addLine(currentX, y0, currentX, y0 - totalHeight);
  }

  // Cell text labels
  let textY = y0;
  for (let r = 0; r < tableDef.rowsData.length; r++) {
    const rowH = tableDef.rowHeights[r] || 8.0;
    const midY = textY - rowH / 2 - tableDef.textHeight / 3;
    let textX = x0;

    for (let c = 0; c < tableDef.columnWidths.length; c++) {
      const colW = tableDef.columnWidths[c];
      const cellVal = tableDef.rowsData[r]?.[c] || '';
      // Left pad 2mm
      addText(cellVal, textX + 2.0, midY, tableDef.textHeight);
      textX += colW;
    }
    textY -= rowH;
  }

  return {
    dxfContent: lines.join('\n'),
    endHandleHex: currentHex,
  };
}

/**
 * Creates a complete standalone DXF file containing the table
 */
export function createStandaloneTableDxf(tableDef: NewTableDefinition): string {
  const fragments = splitTableIntoFragments(tableDef);
  let handleHex = '100';

  const styleHandleInfo = nextHexHandle(handleHex);
  handleHex = styleHandleInfo.nextHex;
  const styleDxf = generateTableStyleDxf(tableDef.styleName, handleHex);

  let entitiesDxf = '';

  fragments.forEach((frag) => {
    if (tableDef.injectionMode === 'native_acad_table' || tableDef.injectionMode === 'both') {
      const tableHandleInfo = nextHexHandle(handleHex);
      handleHex = tableHandleInfo.nextHex;
      const acadDxf = generateAcadTableEntityDxf({
        handle: handleHex,
        styleHandle: styleHandleInfo.nextHex,
        x: frag.x,
        y: frag.y,
        z: frag.z,
        layer: tableDef.layer,
        columnWidths: tableDef.columnWidths,
        rowHeights: Array(frag.rows.length).fill(tableDef.rowHeight),
        rowsData: frag.rows,
      });
      entitiesDxf += acadDxf + '\n';
    }

    if (tableDef.injectionMode === 'vector_table' || tableDef.injectionMode === 'both') {
      const vectorRes = generateVectorTableDxf({
        x: frag.x,
        y: frag.y,
        z: frag.z,
        layer: tableDef.layer,
        columnWidths: tableDef.columnWidths,
        rowHeights: Array(frag.rows.length).fill(tableDef.rowHeight),
        rowsData: frag.rows,
        textHeight: tableDef.textHeight,
        startHandleHex: handleHex,
      });
      entitiesDxf += vectorRes.dxfContent + '\n';
      handleHex = vectorRes.endHandleHex;
    }
  });

  return [
    '  0',
    'SECTION',
    '  2',
    'HEADER',
    '  9',
    '$ACADVER',
    '  1',
    'AC1021',
    '  9',
    '$HANDSEED',
    '  5',
    handleHex,
    '  0',
    'ENDSEC',
    '  0',
    'SECTION',
    '  2',
    'TABLES',
    '  0',
    'ENDSEC',
    '  0',
    'SECTION',
    '  2',
    'BLOCKS',
    '  0',
    'ENDSEC',
    '  0',
    'SECTION',
    '  2',
    'ENTITIES',
    entitiesDxf.trim(),
    '  0',
    'ENDSEC',
    '  0',
    'SECTION',
    '  2',
    'OBJECTS',
    styleDxf,
    '  0',
    'ENDSEC',
    '  0',
    'EOF',
  ].join('\n');
}

export interface GeneratedFragment {
  name: string;
  x: number;
  y: number;
  z: number;
  rows: string[][];
  breakHeight: number;
  manualBreakHeight?: number;
  isManualHeight?: boolean;
  repeatTopLabels: boolean;
  topLabelsRowCount: number;
  topLabelsHeight: number;
  headerHeight: number;
  dataHeight: number;
  startRow: number;
  rowCount: number;
}

/**
 * Splits a table into multiple fragments with manual height or row-count thresholds
 * Supports arbitrary positioning, individual break heights per fragment,
 * and repeating top labels (Title and Header rows until first Data row)
 */
export function splitTableIntoFragments(tableDef: NewTableDefinition): GeneratedFragment[] {
  const rowH = tableDef.rowHeight || 8.0;
  
  // Construct top label rows (Title + Header)
  const topLabelRows: string[][] = [];
  if (tableDef.title && tableDef.title.trim().length > 0) {
    topLabelRows.push([tableDef.title, ...Array(Math.max(0, tableDef.headers.length - 1)).fill('')]);
  }
  topLabelRows.push(tableDef.headers);

  const topLabelsRowCount = topLabelRows.length;
  const topLabelsHeight = topLabelsRowCount * rowH;
  const dataRows = tableDef.rows;
  const allRows = [...topLabelRows, ...dataRows];
  const totalSingleHeight = allRows.length * rowH;

  const repeatTopLabels = tableDef.repeatTopLabels !== false && tableDef.repeatHeaderOnSplit !== false;

  if (!tableDef.enableSplitting) {
    return [
      {
        name: 'Fragment 1 (Complete)',
        x: tableDef.insertX,
        y: tableDef.insertY,
        z: tableDef.insertZ,
        rows: allRows,
        breakHeight: totalSingleHeight,
        manualBreakHeight: tableDef.manualBreakHeight || totalSingleHeight,
        isManualHeight: false,
        repeatTopLabels,
        topLabelsRowCount,
        topLabelsHeight,
        headerHeight: topLabelsHeight,
        dataHeight: dataRows.length * rowH,
        startRow: 1,
        rowCount: allRows.length,
      },
    ];
  }

  const fragments: GeneratedFragment[] = [];
  const splitMethod = tableDef.splitMethod || 'rows';

  if (splitMethod === 'manual_height') {
    // Dynamic height-based splitting: rows wrap once fragment break height threshold is exceeded
    let rowIdx = 0;
    let fragIdx = 0;

    while (rowIdx < dataRows.length) {
      const targetHeight =
        tableDef.fragmentManualHeights?.[fragIdx] ??
        tableDef.manualBreakHeight ??
        80.0;

      // When repeatTopLabels is true, every fragment includes topLabelRows (Title & Header)
      const currentFragTopLabels = (fragIdx === 0 || repeatTopLabels) ? topLabelRows : [];
      const currentTopLabelsH = currentFragTopLabels.length * rowH;

      // Available height for data rows inside this fragment
      const availableDataH = Math.max(targetHeight - currentTopLabelsH, rowH);
      const maxRowsForThisFrag = Math.max(1, Math.floor(availableDataH / rowH));

      const chunkRows = dataRows.slice(rowIdx, rowIdx + maxRowsForThisFrag);
      const fragmentRows = [...currentFragTopLabels, ...chunkRows];
      const actualFragHeight = fragmentRows.length * rowH;

      const pos = tableDef.fragmentPositions?.[fragIdx];
      const fragX = pos ? pos.x : tableDef.insertX + fragIdx * tableDef.fragmentOffsetX;
      const fragY = pos ? pos.y : tableDef.insertY + fragIdx * tableDef.fragmentOffsetY;

      fragments.push({
        name: `Fragment ${fragIdx + 1} (Rows ${rowIdx + 1}-${rowIdx + chunkRows.length}, H: ${actualFragHeight.toFixed(1)}mm)`,
        x: fragX,
        y: fragY,
        z: tableDef.insertZ,
        rows: fragmentRows,
        breakHeight: actualFragHeight,
        manualBreakHeight: targetHeight,
        isManualHeight: true,
        repeatTopLabels,
        topLabelsRowCount: currentFragTopLabels.length,
        topLabelsHeight: currentTopLabelsH,
        headerHeight: currentTopLabelsH,
        dataHeight: chunkRows.length * rowH,
        startRow: rowIdx + 1,
        rowCount: fragmentRows.length,
      });

      rowIdx += chunkRows.length;
      fragIdx++;
    }
  } else {
    // Row count-based splitting
    const rowsPerFrag = Math.max(1, tableDef.rowsPerFragment || 5);
    const chunkCount = Math.ceil(dataRows.length / rowsPerFrag);

    for (let i = 0; i < chunkCount; i++) {
      const chunkStart = i * rowsPerFrag;
      const chunkRows = dataRows.slice(chunkStart, chunkStart + rowsPerFrag);
      
      const currentFragTopLabels = (i === 0 || repeatTopLabels) ? topLabelRows : [];
      const currentTopLabelsH = currentFragTopLabels.length * rowH;
      const fragmentRows = [...currentFragTopLabels, ...chunkRows];

      const actualFragHeight = fragmentRows.length * rowH;

      const pos = tableDef.fragmentPositions?.[i];
      const fragX = pos ? pos.x : tableDef.insertX + i * tableDef.fragmentOffsetX;
      const fragY = pos ? pos.y : tableDef.insertY + i * tableDef.fragmentOffsetY;

      const targetHeight =
        tableDef.fragmentManualHeights?.[i] ??
        tableDef.manualBreakHeight ??
        actualFragHeight;

      fragments.push({
        name: `Fragment ${i + 1} (Rows ${chunkStart + 1}-${chunkStart + chunkRows.length})`,
        x: fragX,
        y: fragY,
        z: tableDef.insertZ,
        rows: fragmentRows,
        breakHeight: actualFragHeight,
        manualBreakHeight: targetHeight,
        isManualHeight: false,
        repeatTopLabels,
        topLabelsRowCount: currentFragTopLabels.length,
        topLabelsHeight: currentTopLabelsH,
        headerHeight: currentTopLabelsH,
        dataHeight: chunkRows.length * rowH,
        startRow: chunkStart + 1,
        rowCount: fragmentRows.length,
      });
    }
  }

  return fragments;
}

/**
 * Injects TABLESTYLE and table entities into an existing DXF drawing
 */
export function injectTableIntoDxf(
  originalDxfText: string,
  tableDef: NewTableDefinition,
  currentMaxHandleHex: string = '1000'
): { modifiedDxf: string; injectedCount: number; errors?: string[] } {
  const lines = originalDxfText.split(/\r?\n/);
  const errors: string[] = [];

  const fragments = splitTableIntoFragments(tableDef);
  let currentHandle = currentMaxHandleHex;

  // Generate TABLESTYLE for OBJECTS
  const styleHandleInfo = nextHexHandle(currentHandle);
  currentHandle = styleHandleInfo.nextHex;
  const styleDxf = generateTableStyleDxf(tableDef.styleName, currentHandle);

  // Generate Entities for ENTITIES
  let entitiesDxf = '';
  fragments.forEach((frag) => {
    if (tableDef.injectionMode === 'native_acad_table' || tableDef.injectionMode === 'both') {
      const tableHandleInfo = nextHexHandle(currentHandle);
      currentHandle = tableHandleInfo.nextHex;
      const acadDxf = generateAcadTableEntityDxf({
        handle: currentHandle,
        styleHandle: styleHandleInfo.nextHex,
        x: frag.x,
        y: frag.y,
        z: frag.z,
        layer: tableDef.layer,
        columnWidths: tableDef.columnWidths,
        rowHeights: Array(frag.rows.length).fill(tableDef.rowHeight),
        rowsData: frag.rows,
      });
      entitiesDxf += acadDxf + '\n';
    }

    if (tableDef.injectionMode === 'vector_table' || tableDef.injectionMode === 'both') {
      const vectorRes = generateVectorTableDxf({
        x: frag.x,
        y: frag.y,
        z: frag.z,
        layer: tableDef.layer,
        columnWidths: tableDef.columnWidths,
        rowHeights: Array(frag.rows.length).fill(tableDef.rowHeight),
        rowsData: frag.rows,
        textHeight: tableDef.textHeight,
        startHandleHex: currentHandle,
      });
      entitiesDxf += vectorRes.dxfContent + '\n';
      currentHandle = vectorRes.endHandleHex;
    }
  });

  // 1. Inject into OBJECTS section
  let objRange = findSectionLines(lines, 'OBJECTS');
  if (objRange) {
    const styleLines = styleDxf.split('\n');
    lines.splice(objRange.end, 0, ...styleLines);
  } else {
    // If no OBJECTS section, append one before EOF
    const eofIdx = lines.findIndex((l) => l.trim() === 'EOF');
    const insertIdx = eofIdx !== -1 ? eofIdx - 1 : lines.length;
    const objectsSection = [
      '  0',
      'SECTION',
      '  2',
      'OBJECTS',
      styleDxf,
      '  0',
      'ENDSEC',
    ];
    lines.splice(insertIdx, 0, ...objectsSection);
  }

  // 2. Inject into ENTITIES section
  const entRange = findSectionLines(lines, 'ENTITIES');
  if (entRange) {
    const entLines = entitiesDxf.trim().split('\n');
    lines.splice(entRange.end, 0, ...entLines);
  } else {
    errors.push('ENTITIES section not found in original DXF file');
    return { modifiedDxf: originalDxfText, injectedCount: 0, errors };
  }

  return {
    modifiedDxf: lines.join('\n'),
    injectedCount: fragments.length,
  };
}
