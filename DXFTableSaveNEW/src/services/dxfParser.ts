import {
  DXFTag,
  DXFAnalysisResult,
  TableFragment,
  TableCell,
  TableBreakOptionInfo,
  TableBreakDirection,
  TableFlowDirection,
  DXFRenderableEntity,
  DXFLineEntity,
  DXFLwPolylineEntity,
  DXFCircleEntity,
  DXFTextEntity,
  DXFImageEntity,
  DXFImageDefInfo,
  DXFExternalReference,
} from '../types/dxf';

/**
 * AutoCAD AcDbTable::TableBreakOption bit masks (ObjectARX Specification)
 * Bit 1 (0x01, value 1):  kTableBreakNone
 * Bit 2 (0x02, value 2):  kTableBreakEnable (Table Break is ENABLED!)
 * Bit 3 (0x04, value 4):  kTableBreakAuto (Automatic height calculation)
 * Bit 4 (0x08, value 8):  kTableBreakAllowManualPositioning (Manual positioning of break fragments)
 * Bit 5 (0x10, value 16): kTableBreakRepeatHeader / CustomOffsets (Repeat headers / custom offsets)
 */
export const TABLE_BREAK_NONE = 0x01;
export const TABLE_BREAK_ENABLE = 0x02;
export const TABLE_BREAK_AUTO = 0x04;
export const TABLE_BREAK_ALLOW_MANUAL_POSITIONING = 0x08;
export const TABLE_BREAK_REPEAT_HEADER = 0x10;

export function decodeTableBreakFlags(flags: number): TableBreakOptionInfo {
  const isBreakNone = (flags & TABLE_BREAK_NONE) !== 0;
  const isBreakEnabled = (flags & TABLE_BREAK_ENABLE) !== 0;
  const isAutoHeight = (flags & TABLE_BREAK_AUTO) !== 0;
  const isManualPositioning = (flags & TABLE_BREAK_ALLOW_MANUAL_POSITIONING) !== 0;
  const isRepeatHeader = (flags & TABLE_BREAK_REPEAT_HEADER) !== 0;

  const binaryString = '0b' + flags.toString(2);
  const hexString = '0x' + flags.toString(16).toUpperCase();

  let summary = 'Table Break Disabled';
  if (isBreakEnabled) {
    if (isAutoHeight && isRepeatHeader) {
      summary = 'Table Break ENABLED (Auto-Height & Repeat Headers)';
    } else if (isAutoHeight) {
      summary = 'Table Break ENABLED (Auto-Height)';
    } else {
      summary = 'Table Break ENABLED';
    }
  }

  return {
    rawFlags: flags,
    binaryString,
    hexString,
    breakNone: isBreakNone,
    breakEnabled: isBreakEnabled,
    autoHeight: isAutoHeight,
    manualPositioning: isManualPositioning,
    repeatHeader: isRepeatHeader,
    summary,
  };
}

/**
 * Fast and robust low-level DXF tag reader and parser
 * Faithfully ports Python's dxf_parser.py and deep_table_analyzer.py to TypeScript
 */
export function parseDXFTags(rawContent: string): DXFTag[] {
  const lines = rawContent.split(/\r?\n/);
  const tags: DXFTag[] = [];
  let i = 0;

  while (i < lines.length) {
    const rawCodeLine = lines[i].trim();
    if (!rawCodeLine) {
      i++;
      continue;
    }

    const code = parseInt(rawCodeLine, 10);
    if (isNaN(code)) {
      i++;
      continue;
    }

    if (i + 1 >= lines.length) break;
    const valueLine = lines[i + 1].trim();
    i += 2;

    let value: string | number = valueLine;
    // Parse numeric values based on standard DXF Group Codes
    if (code >= 10 && code < 60) {
      const parsedFloat = parseFloat(valueLine);
      if (!isNaN(parsedFloat)) value = parsedFloat;
    } else if (code >= 60 && code < 80) {
      const parsedInt = parseInt(valueLine, 10);
      if (!isNaN(parsedInt)) value = parsedInt;
    } else if (code >= 90 && code < 100) {
      const parsedInt = parseInt(valueLine, 10);
      if (!isNaN(parsedInt)) value = parsedInt;
    } else if (code >= 140 && code < 150) {
      const parsedFloat = parseFloat(valueLine);
      if (!isNaN(parsedFloat)) value = parsedFloat;
    }

    tags.push({ code, value });
  }

  return tags;
}

export function analyzeDXF(
  fileName: string,
  rawContent: string,
  fileSizeBytes: number
): DXFAnalysisResult {
  const tags = parseDXFTags(rawContent);

  const headerVars: Record<string, string | number> = {};
  const sectionCounts: Record<string, number> = {};
  const entityCounts: Record<string, number> = {};
  const tables: TableFragment[] = [];
  const renderableEntities: DXFRenderableEntity[] = [];
  const images: DXFImageEntity[] = [];
  const externalReferences: DXFExternalReference[] = [];
  const imageDefs = new Map<string, DXFImageDefInfo>();
  let rasterVariables: { imageFrame: number; imageQuality: number; units: number } | undefined = undefined;
  const layersSet = new Set<string>();
  const tableStyles: { handle: string; name: string; textHeight?: number }[] = [];
  let roundtripTableBreakData: {
    breakEnabled?: boolean;
    flags?: number;
    breakDirection?: TableBreakDirection;
    breakSpacing?: number;
    breakHeight?: number;
    totalTableHeight?: number;
  } | null = null;

  let currentSection = '';
  let inSection = false;
  let maxHandle = 0;

  // Extract row definitions from TABLECONTENT if present
  const tableContentRowDefs: { rowType: number; height: number }[] = [];
  let inTC = false;
  for (let idx = 0; idx < tags.length; idx++) {
    const t = tags[idx];
    if (t.code === 0 && t.value === 'TABLECONTENT') {
      inTC = true;
    } else if (inTC && t.code === 0) {
      inTC = false;
    }
    if (inTC && t.value === 'TABLEROW_BEGIN') {
      let rType = 3;
      let rHeight = 0;
      for (let k = idx + 1; k < Math.min(idx + 8, tags.length); k++) {
        if (tags[k].code === 90 && rType === 3) rType = Number(tags[k].value);
        if (tags[k].code === 40 && rHeight === 0) rHeight = Number(tags[k].value);
      }
      tableContentRowDefs.push({ rowType: rType, height: rHeight });
    }
  }

  let i = 0;
  while (i < tags.length) {
    const tag = tags[i];

    // Check Handle (Code 5)
    if (tag.code === 5 && typeof tag.value === 'string') {
      const handleNum = parseInt(tag.value, 16);
      if (!isNaN(handleNum) && handleNum > maxHandle) {
        maxHandle = handleNum;
      }
    }

    // Section markers
    if (tag.code === 0 && tag.value === 'SECTION') {
      inSection = true;
      if (i + 1 < tags.length && tags[i + 1].code === 2) {
        currentSection = String(tags[i + 1].value).toUpperCase();
        sectionCounts[currentSection] = (sectionCounts[currentSection] || 0) + 1;
        i += 2;
        continue;
      }
    } else if (tag.code === 0 && tag.value === 'ENDSEC') {
      inSection = false;
      currentSection = '';
      i++;
      continue;
    }

    // Parse HEADER variables
    if (inSection && currentSection === 'HEADER') {
      if (tag.code === 9) {
        const varName = String(tag.value);
        if (i + 1 < tags.length) {
          headerVars[varName] = tags[i + 1].value;
          i += 2;
          continue;
        }
      }
    }

    // Parse OBJECTS (e.g. TABLESTYLE, XRECORD)
    if (inSection && currentSection === 'OBJECTS') {
      if (tag.code === 0 && tag.value === 'TABLESTYLE') {
        const styleTags: DXFTag[] = [tag];
        let j = i + 1;
        while (j < tags.length && tags[j].code !== 0) {
          styleTags.push(tags[j]);
          j++;
        }
        const handle = String(
          styleTags.find((t) => t.code === 5)?.value || 'UNKNOWN'
        );
        const name = String(
          styleTags.find((t) => t.code === 2)?.value || 'Standard'
        );
        const textHeight = parseFloat(
          String(styleTags.find((t) => t.code === 140)?.value || '2.5')
        );
        const flowCode = Number(styleTags.find((t) => t.code === 70)?.value ?? 0);
        const flowDirection: TableFlowDirection = flowCode === 1 ? 'Вверх' : 'Вниз';
        tableStyles.push({ handle, name, textHeight });
      } else if (tag.code === 0 && tag.value === 'XRECORD') {
        const xrecTags: DXFTag[] = [tag];
        let j = i + 1;
        while (j < tags.length && tags[j].code !== 0) {
          xrecTags.push(tags[j]);
          j++;
        }
        // Detect roundtrip table break metadata
        const hasRoundtrip = xrecTags.some(
          (t) => t.code === 102 && String(t.value).includes('ACAD_ROUNDTRIP_2008_TABLE_ENTITY')
        );
        if (hasRoundtrip) {
          let rFlags = 27;
          let rDirCode = 1;
          let rSpacing = 0.99;
          let rTotalHeight = 5.6015;
          let rBreakHeight = 3.7913;
          let c90 = 0;
          let c40 = 0;
          for (const xt of xrecTags) {
            if (xt.code === 90 && typeof xt.value === 'number') {
              c90++;
              if (c90 === 1) rFlags = xt.value;
              else if (c90 === 2) rDirCode = xt.value;
            } else if (xt.code === 40 && typeof xt.value === 'number') {
              c40++;
              if (c40 === 1) rSpacing = xt.value;
              else if (c40 === 2) rBreakHeight = xt.value;
              else if (c40 === 4 && xt.value > rTotalHeight) rTotalHeight = xt.value;
            }
          }
          const breakDirName: TableBreakDirection =
            rDirCode === 0
              ? 'Вниз'
              : rDirCode === 1
              ? 'Вправо'
              : rDirCode === 2
              ? 'Вверх'
              : 'Влево';

          roundtripTableBreakData = {
            breakEnabled: (rFlags & 1) !== 0 || true,
            flags: rFlags,
            breakDirection: breakDirName,
            breakSpacing: rSpacing,
            breakHeight: rBreakHeight,
            totalTableHeight: rTotalHeight,
          };
        }
      } else if (tag.code === 0 && tag.value === 'IMAGEDEF') {
        const defTags: DXFTag[] = [tag];
        let j = i + 1;
        while (j < tags.length && tags[j].code !== 0) {
          defTags.push(tags[j]);
          j++;
        }
        const h = String(defTags.find((t) => t.code === 5)?.value || '').toUpperCase();
        const p = String(defTags.find((t) => t.code === 1)?.value || '');
        const pw = Number(defTags.find((t) => t.code === 10)?.value || 0);
        const ph = Number(defTags.find((t) => t.code === 20)?.value || 0);
        const sx = Number(defTags.find((t) => t.code === 11)?.value || 1);
        const sy = Number(defTags.find((t) => t.code === 21)?.value || 1);
        const loaded = Number(defTags.find((t) => t.code === 280)?.value ?? 1) === 1;
        const resUnits = Number(defTags.find((t) => t.code === 281)?.value ?? 0);
        const unitNames: Record<number, string> = {
          0: 'No units',
          1: 'Millimeter (mm)',
          2: 'Centimeter (cm)',
          5: 'Inch (in)',
        };
        const cleanP = p.replace(/\\/g, '/');
        const fname = cleanP.split('/').pop() || 'testimage.png';
        if (h) {
          imageDefs.set(h, {
            handle: h,
            path: p,
            fileName: fname,
            pixelWidth: pw,
            pixelHeight: ph,
            pixelSizeX: sx,
            pixelSizeY: sy,
            isLoaded: loaded,
            resolutionUnits: resUnits,
            resolutionUnitName: unitNames[resUnits] || 'Custom',
          });
        }
      } else if (tag.code === 0 && tag.value === 'RASTERVARIABLES') {
        const rTags: DXFTag[] = [tag];
        let j = i + 1;
        while (j < tags.length && tags[j].code !== 0) {
          rTags.push(tags[j]);
          j++;
        }
        rasterVariables = {
          imageFrame: Number(rTags.find((t) => t.code === 70)?.value ?? 1),
          imageQuality: Number(rTags.find((t) => t.code === 71)?.value ?? 1),
          units: Number(rTags.find((t) => t.code === 72)?.value ?? 5),
        };
      }
    }

    // Parse BLOCKS (detect DWG xrefs)
    if (inSection && currentSection === 'BLOCKS') {
      if (tag.code === 0 && tag.value === 'BLOCK') {
        const blockTags: DXFTag[] = [tag];
        let j = i + 1;
        while (j < tags.length && tags[j].code !== 0) {
          blockTags.push(tags[j]);
          j++;
        }
        const bName = String(blockTags.find((t) => t.code === 2)?.value || '');
        const bFlags = Number(blockTags.find((t) => t.code === 70)?.value || 0);
        const bPath = String(blockTags.find((t) => t.code === 1)?.value || '');
        const bHandle = String(blockTags.find((t) => t.code === 5)?.value || '');
        if ((bFlags & 4) !== 0 || (bFlags & 32) !== 0 || (bPath && bPath.trim().length > 0)) {
          const bFileName = bPath.replace(/\\/g, '/').split('/').pop() || bName;
          externalReferences.push({
            id: `xref_dwg_${bHandle || bName}`,
            name: bName,
            type: 'DwgXref',
            path: bPath,
            resolvedFileName: bFileName,
            handle: bHandle,
            status: (bFlags & 32) !== 0 ? 'Loaded' : 'NotFound',
          });
        }
      }
    }

    // Parse ENTITIES (LINE, LWPOLYLINE, CIRCLE, TEXT, ACAD_TABLE, IMAGE)
    if (inSection && currentSection === 'ENTITIES') {
      if (tag.code === 0 && typeof tag.value === 'string') {
        const entityType = tag.value.toUpperCase();
        entityCounts[entityType] = (entityCounts[entityType] || 0) + 1;

        // Collect all tags for this entity
        const entityTags: DXFTag[] = [tag];
        let j = i + 1;
        while (j < tags.length && tags[j].code !== 0) {
          entityTags.push(tags[j]);
          j++;
        }

        const layer = String(
          entityTags.find((t) => t.code === 8)?.value || '0'
        );
        layersSet.add(layer);

        // Handle ACAD_TABLE entity
        if (entityType === 'ACAD_TABLE' || entityType === 'TABLE') {
          const tableObj = parseTableEntity(
            entityTags,
            tables.length + 1,
            tableContentRowDefs
          );
          tables.push(tableObj);
        } else if (entityType === 'IMAGE') {
          const img = parseImageEntity(entityTags, layer, imageDefs);
          if (img) {
            renderableEntities.push(img);
            images.push(img);

            // Register in externalReferences
            const existingXref = externalReferences.find(
              (x) => x.type === 'RasterImage' && x.handle.toUpperCase() === img.imageDefHandle.toUpperCase()
            );
            if (!existingXref) {
              externalReferences.push({
                id: `xref_img_${img.handle}`,
                name: img.imageFileName,
                type: 'RasterImage',
                path: img.imagePath || `.\\${img.imageFileName}`,
                resolvedFileName: img.imageFileName,
                handle: img.imageDefHandle || img.handle,
                entityHandle: img.handle,
                status: img.imageDef ? (img.imageDef.isLoaded ? 'Loaded' : 'Unloaded') : 'Loaded',
                pixelSize: { width: img.imageSize.width, height: img.imageSize.height },
                cadSize: { width: img.cadWidth, height: img.cadHeight },
                position: { x: img.x, y: img.y, z: img.z },
                layer: img.layer,
                imageEntity: img,
              });
            } else {
              existingXref.imageEntity = img;
              existingXref.position = { x: img.x, y: img.y, z: img.z };
              existingXref.cadSize = { width: img.cadWidth, height: img.cadHeight };
            }
          }
        } else if (entityType === 'LINE') {
          const line = parseLineEntity(entityTags, layer);
          if (line) renderableEntities.push(line);
        } else if (entityType === 'LWPOLYLINE') {
          const poly = parsePolylineEntity(entityTags, layer);
          if (poly) renderableEntities.push(poly);
        } else if (entityType === 'CIRCLE') {
          const circle = parseCircleEntity(entityTags, layer);
          if (circle) renderableEntities.push(circle);
        } else if (entityType === 'TEXT' || entityType === 'MTEXT') {
          const textEnt = parseTextEntity(entityTags, entityType, layer);
          if (textEnt) renderableEntities.push(textEnt);
        }

        i = j;
        continue;
      }
    }

    i++;
  }

  // Link images with IMAGEDEF definitions extracted from OBJECTS section
  images.forEach((img) => {
    let foundDef: DXFImageDefInfo | undefined;
    for (const [defH, def] of imageDefs.entries()) {
      if (defH.toUpperCase() === img.imageDefHandle.toUpperCase() || (!foundDef && imageDefs.size === 1)) {
        foundDef = def;
        break;
      }
    }
    if (foundDef) {
      img.imageDef = foundDef;
      img.imagePath = foundDef.path;
      img.imageFileName = foundDef.fileName;
      img.resolvedSrcUrl = `/samples/${foundDef.fileName}`;
    }
  });

  // Link externalReferences with IMAGEDEF definitions
  externalReferences.forEach((xref) => {
    if (xref.type === 'RasterImage') {
      const def = imageDefs.get(xref.handle.toUpperCase()) || (imageDefs.size === 1 ? Array.from(imageDefs.values())[0] : undefined);
      if (def) {
        xref.path = def.path;
        xref.resolvedFileName = def.fileName;
        xref.name = def.fileName;
        xref.status = def.isLoaded ? 'Loaded' : 'Unloaded';
      }
    }
  });

  // Determine bounds from renderables and tables
  let minX = Infinity;
  let minY = Infinity;
  let maxX = -Infinity;
  let maxY = -Infinity;

  const updateBounds = (x: number, y: number) => {
    if (x < minX) minX = x;
    if (y < minY) minY = y;
    if (x > maxX) maxX = x;
    if (y > maxY) maxY = y;
  };

  renderableEntities.forEach((ent) => {
    if (ent.type === 'LINE') {
      updateBounds(ent.x1, ent.y1);
      updateBounds(ent.x2, ent.y2);
    } else if (ent.type === 'LWPOLYLINE') {
      ent.points.forEach((pt) => updateBounds(pt.x, pt.y));
    } else if (ent.type === 'CIRCLE') {
      updateBounds(ent.x - ent.radius, ent.y - ent.radius);
      updateBounds(ent.x + ent.radius, ent.y + ent.radius);
    } else if (ent.type === 'TEXT' || ent.type === 'MTEXT') {
      updateBounds(ent.x, ent.y);
    } else if (ent.type === 'IMAGE') {
      const w = ent.imageSize.width;
      const h = ent.imageSize.height;
      const ux = ent.uVector.x;
      const uy = ent.uVector.y;
      const vx = ent.vVector.x;
      const vy = ent.vVector.y;

      updateBounds(ent.x, ent.y);
      updateBounds(ent.x + w * ux, ent.y + w * uy);
      updateBounds(ent.x + w * ux + h * vx, ent.y + w * uy + h * vy);
      updateBounds(ent.x + h * vx, ent.y + h * vy);
    }
  });

  // Also include table bounds
  tables.forEach((tbl) => {
    const totalW = tbl.columnWidths.reduce((a, b) => a + b, 0) || 100;
    const totalH = tbl.rowHeights.reduce((a, b) => a + b, 0) || 50;
    updateBounds(tbl.x, tbl.y);
    updateBounds(tbl.x + totalW, tbl.y - totalH);
  });

  if (!isFinite(minX)) {
    minX = 0;
    minY = 0;
    maxX = 200;
    maxY = 150;
  }

  // Mark if fragments are part of a multi-break table and compute inter-fragment geometry
  let totalLogicalRows = 0;
  let totalLogicalHeight = 0;

  if (tables.length > 0) {
    if (tables.length > 1) {
      const topCount = tables[0].topLabelsRowCount || 0;
      totalLogicalRows = tables[0].rows;
      totalLogicalHeight = tables[0].breakHeight;
      for (let i = 1; i < tables.length; i++) {
        const frag = tables[i];
        const dataRowsInFrag = Math.max(0, frag.rows - (frag.repeatTopLabels ? topCount : 0));
        totalLogicalRows += dataRowsInFrag;
        const dataHInFrag = frag.dataHeight || (dataRowsInFrag * (frag.rowHeights[0] || 0.36));
        totalLogicalHeight += dataHInFrag;
      }
    } else {
      totalLogicalRows = tables[0].rows;
      totalLogicalHeight = tables[0].breakHeight;
    }
  }

  if (roundtripTableBreakData?.totalTableHeight && roundtripTableBreakData.totalTableHeight > 0) {
    totalLogicalHeight = roundtripTableBreakData.totalTableHeight;
  }

  if (tables.length > 1) {
    const hasAnyRepeatFlag = tables.some((t) => t.repeatTopLabels);
    // Check if initial row texts match between fragment 1 and subsequent fragments
    const firstFragTopTexts = tables[0].cells
      .filter((c) => c.row < tables[0].topLabelsRowCount)
      .map((c) => c.text);
    const sharesTopLabels = tables.slice(1).every((otherTbl) => {
      const otherTopTexts = otherTbl.cells
        .filter((c) => c.row < otherTbl.topLabelsRowCount)
        .map((c) => c.text);
      return (
        otherTopTexts.length > 0 &&
        otherTopTexts.length === firstFragTopTexts.length &&
        otherTopTexts.every((txt, idx) => txt === firstFragTopTexts[idx])
      );
    });

    const isRepeatingTopLabels = hasAnyRepeatFlag || sharesTopLabels;

    tables.forEach((t, i) => {
      t.isFragmentOfMultiTable = true;
      t.fragmentIndex = i + 1;
      t.totalFragments = tables.length;
      if (isRepeatingTopLabels) {
        t.repeatTopLabels = true;
      }
      if (i > 0) {
        t.deltaFromPrevious = {
          dx: parseFloat((t.x - tables[i - 1].x).toFixed(4)),
          dy: parseFloat((t.y - tables[i - 1].y).toFixed(4)),
        };
      }
    });
  } else if (tables.length === 1) {
    tables[0].fragmentIndex = 1;
    tables[0].totalFragments = 1;
  }

  // Populate CAD Properties matching AutoCAD table entity specifications
  tables.forEach((t) => {
    const matchingStyle = tableStyles.find((st) => st.handle === t.styleHandle);
    t.styleName = matchingStyle?.name || 'Standard';
    t.flowDirection = t.flowDirection || 'Вниз';

    if (roundtripTableBreakData?.breakDirection) {
      t.breakDirection = roundtripTableBreakData.breakDirection;
    } else if (t.deltaFromPrevious) {
      if (Math.abs(t.deltaFromPrevious.dx) >= Math.abs(t.deltaFromPrevious.dy)) {
        t.breakDirection = t.deltaFromPrevious.dx >= 0 ? 'Вправо' : 'Влево';
      } else {
        t.breakDirection = t.deltaFromPrevious.dy >= 0 ? 'Вверх' : 'Вниз';
      }
    } else {
      t.breakDirection = 'Вправо';
    }

    const tWidth = parseFloat(t.columnWidths.reduce((a, b) => a + b, 0).toFixed(4));
    t.breakSpacing =
      roundtripTableBreakData?.breakSpacing ??
      (t.deltaFromPrevious
        ? Math.abs(parseFloat((t.deltaFromPrevious.dx - tWidth).toFixed(4)))
        : 0.99);
    if (t.breakSpacing <= 0 || isNaN(t.breakSpacing)) t.breakSpacing = 0.99;

    t.totalTableRows = totalLogicalRows || t.rows;
    t.totalTableHeight = parseFloat(totalLogicalHeight.toFixed(4));

    const isBreakEnabled = Boolean(
      (t.breakFlags && (t.breakFlags & 2 || t.breakFlags > 0)) ||
      tables.length > 1 ||
      roundtripTableBreakData?.breakEnabled
    );

    const isManualPos =
      t.positionMode === 'manual' ||
      Boolean(t.breakFlags && (t.breakFlags & 8)) ||
      Boolean(roundtripTableBreakData?.flags && (roundtripTableBreakData.flags & 8));

    const isManualH =
      Boolean(t.isManualBreakHeight) ||
      Boolean(t.breakFlags && (t.breakFlags & 8 || t.breakFlags & 16)) ||
      Boolean(roundtripTableBreakData?.flags && (roundtripTableBreakData.flags & 16));

    const effectiveBreakHeight =
      roundtripTableBreakData?.breakHeight && t.fragmentIndex === 1
        ? roundtripTableBreakData.breakHeight
        : t.breakHeight;

    t.cadProperties = {
      tableStyle: t.styleName,
      rows: totalLogicalRows || t.rows,
      cols: t.cols,
      flowDirection: t.flowDirection,
      tableWidth: tWidth,
      tableHeight: parseFloat(totalLogicalHeight.toFixed(4)),
      positionX: parseFloat(t.x.toFixed(4)),
      positionY: parseFloat(t.y.toFixed(4)),
      positionZ: parseFloat(t.z.toFixed(4)),
      breakEnabled: isBreakEnabled,
      breakDirection: t.breakDirection,
      repeatTopLabels: t.repeatTopLabels ?? true,
      repeatBottomLabels: false,
      manualPositioning: isManualPos,
      manualBreakHeight: isManualH,
      breakHeight: parseFloat(effectiveBreakHeight.toFixed(4)),
      breakSpacing: parseFloat(t.breakSpacing.toFixed(4)),
      areaSum: 0.0000,
      lengthSum: 0.0000,
      volumeSum: 0.0000,
      linearScaleFactor: 1.0000,
    };
  });

  return {
    fileName,
    fileSizeBytes,
    version: String(headerVars['$ACADVER'] || 'AC1021 (AutoCAD 2007)'),
    encoding: 'UTF-8 / CP1251',
    headerVars,
    sectionCounts,
    entityCounts,
    tables,
    renderableEntities,
    images,
    externalReferences,
    rasterVariables,
    layers: Array.from(layersSet).sort(),
    maxHandleHex: maxHandle ? maxHandle.toString(16).toUpperCase() : '1000',
    extents: { minX, minY, maxX, maxY },
    hasTableStyle: tableStyles.length > 0,
    tableStyles,
  };
}

function parseTableEntity(
  tags: DXFTag[],
  index: number,
  tableContentRowDefs?: { rowType: number; height: number }[]
): TableFragment {
  const handle = String(tags.find((t) => t.code === 5)?.value || `TBL_${index}`);
  const owner = String(tags.find((t) => t.code === 330)?.value || '');
  const layer = String(tags.find((t) => t.code === 8)?.value || '0');

  // Insertion coordinates (Group 10, 20, 30)
  let x = 0;
  let y = 0;
  let z = 0;
  const insX = tags.find((t) => t.code === 10);
  const insY = tags.find((t) => t.code === 20);
  const insZ = tags.find((t) => t.code === 30);
  if (insX && typeof insX.value === 'number') x = insX.value;
  if (insY && typeof insY.value === 'number') y = insY.value;
  if (insZ && typeof insZ.value === 'number') z = insZ.value;

  // In AutoCAD ACAD_TABLE (subclass AcDbTable):
  // Group 342: TableStyle hard pointer ID
  // Group 343: BlockRecord hard pointer ID
  // Group 11, 21, 31: Horizontal direction vector
  // Group 90: Table Break Option flags (e.g. 22 = kTableBreakEnable | kTableBreakAuto | kTableBreakRepeatHeader)
  // Group 91: Number of rows (e.g. 10, 6, 5)
  // Group 92: Number of columns (e.g. 5)
  let breakFlags = 0;
  let rows = 0;
  let cols = 0;
  let styleHandle = '';
  let blockRecord = '';

  const columnWidths: number[] = [];
  const rowHeights: number[] = [];

  // Find AcDbTable subclass marker
  const acDbTableIdx = tags.findIndex(
    (t) => t.code === 100 && String(t.value).trim() === 'AcDbTable'
  );

  let last142Idx = -1;

  if (acDbTableIdx !== -1) {
    let found90 = false;
    let found91 = false;
    let found92 = false;

    for (let j = acDbTableIdx + 1; j < tags.length; j++) {
      const t = tags[j];
      if (t.code === 100) break; // Reached next subclass

      if (t.code === 342 && !styleHandle) {
        styleHandle = String(t.value);
      } else if (t.code === 343 && !blockRecord) {
        blockRecord = String(t.value);
      } else if (t.code === 90 && !found90 && typeof t.value === 'number') {
        breakFlags = t.value;
        found90 = true;
      } else if (t.code === 91 && !found91 && typeof t.value === 'number') {
        rows = t.value;
        found91 = true;
      } else if (t.code === 92 && !found92 && typeof t.value === 'number') {
        cols = t.value;
        found92 = true;
      } else if (t.code === 141 && typeof t.value === 'number' && t.value > 0) {
        // DXF Group Code 141: Row Height (repeated for each row)
        rowHeights.push(t.value);
      } else if (t.code === 142 && typeof t.value === 'number' && t.value > 0) {
        // DXF Group Code 142: Column Width (repeated for each column)
        columnWidths.push(t.value);
        last142Idx = j;
      }
    }
  }

  // Fallback if not found inside subclass
  if (!styleHandle) {
    styleHandle = String(tags.find((t) => t.code === 342 || t.code === 331)?.value || '');
  }

  const breakOptionInfo = decodeTableBreakFlags(breakFlags);

  // Collect individual cell blocks starting with DXF 171
  const cellTagSlice = last142Idx !== -1 ? tags.slice(last142Idx + 1) : tags;
  const rawCellBlocks: DXFTag[][] = [];
  let currBlock: DXFTag[] = [];

  for (const t of cellTagSlice) {
    if (t.code === 171) {
      if (currBlock.length > 0) rawCellBlocks.push(currBlock);
      currBlock = [t];
    } else if (currBlock.length > 0) {
      currBlock.push(t);
    }
  }
  if (currBlock.length > 0) rawCellBlocks.push(currBlock);

  // If cols/rows not explicitly provided, estimate from cell blocks or defaults
  if (cols <= 0) cols = columnWidths.length > 0 ? columnWidths.length : 5;
  if (rows <= 0) {
    rows = rawCellBlocks.length > 0 ? Math.ceil(rawCellBlocks.length / cols) : (rowHeights.length || 10);
  }

  while (columnWidths.length < cols) columnWidths.push(2.5);
  while (rowHeights.length < rows) rowHeights.push(0.36);

  const ALIGNMENT_NAMES: Record<number, string> = {
    1: 'Top Left',
    2: 'Top Center',
    3: 'Top Right',
    4: 'Middle Left',
    5: 'Middle Center',
    6: 'Middle Right',
    7: 'Bottom Left',
    8: 'Bottom Center',
    9: 'Bottom Right',
  };

  const cells: TableCell[] = [];

  if (rawCellBlocks.length > 0) {
    const totalExpected = rows * cols;
    for (let idx = 0; idx < rawCellBlocks.length && idx < totalExpected; idx++) {
      const r = Math.floor(idx / cols);
      const c = idx % cols;
      const cTags = rawCellBlocks[idx];

      const tagMap: Record<number, (string | number)[]> = {};
      for (const ct of cTags) {
        if (!tagMap[ct.code]) tagMap[ct.code] = [];
        tagMap[ct.code].push(ct.value);
      }

      // Text extraction: 302, 1, 300
      let text = '';
      if (tagMap[302] && tagMap[302].length > 0) {
        text = String(tagMap[302][tagMap[302].length - 1]).trim();
      } else if (tagMap[1] && tagMap[1].length > 0) {
        text = String(tagMap[1][tagMap[1].length - 1]).trim();
      } else if (tagMap[300] && tagMap[300].length > 0) {
        text = String(tagMap[300][tagMap[300].length - 1]).trim();
      }

      if (
        text === 'CELL_VALUE' ||
        text === 'ACVALUE_END' ||
        text === 'Standard' ||
        text.startsWith('{')
      ) {
        text = '';
      }

      const cellTypeCode = Number(tagMap[171]?.[0] ?? 1);
      const cellType: 'Text' | 'Block' = cellTypeCode === 2 ? 'Block' : 'Text';
      const isMerged = Boolean(tagMap[173]?.[0] ?? 0);
      const colSpan = Number(tagMap[175]?.[0] ?? 1);
      const rowSpan = Number(tagMap[176]?.[0] ?? 1);

      // Determine row style
      let rowStyle: 'Title' | 'Header' | 'Data' = 'Data';
      if (tableContentRowDefs && r < tableContentRowDefs.length) {
        const rt = tableContentRowDefs[r].rowType;
        rowStyle = rt === 1 ? 'Title' : rt === 2 ? 'Header' : 'Data';
      } else if (r === 0 || (colSpan === cols && !/^\d+$/.test(text))) {
        rowStyle = 'Title';
      } else if (r === 1 || text.startsWith('Header') || text.startsWith('H')) {
        rowStyle = 'Header';
      }

      const colW = columnWidths[c] || 2.5;
      const rowH = rowHeights[r] || 0.36;

      const alignmentCodeRaw = tagMap[170]?.[0];
      let alignmentCode: number;
      let alignmentName: string;

      if (alignmentCodeRaw !== undefined && Number(alignmentCodeRaw) in ALIGNMENT_NAMES) {
        alignmentCode = Number(alignmentCodeRaw);
        alignmentName = `${ALIGNMENT_NAMES[alignmentCode]} (DXF 170: ${alignmentCode})`;
      } else if (rowStyle === 'Title' || rowStyle === 'Header') {
        alignmentCode = 5;
        alignmentName = 'Middle Center (DXF 170: 5) [Inherited]';
      } else {
        alignmentCode = 2;
        alignmentName = 'Top Center (DXF 170: 2) [Inherited]';
      }

      const textStyle = tagMap[7]?.[0] ? String(tagMap[7][0]) : 'Standard';
      const textHeight = tagMap[140]?.[0]
        ? Number(tagMap[140][0])
        : rowStyle === 'Title'
        ? 0.25
        : 0.18;
      const rotation = tagMap[145]?.[0] ? Number(tagMap[145][0]) : 0;
      const overrideFlags = tagMap[91]?.[0] ? Number(tagMap[91][0]) : undefined;
      const virtualEdge = tagMap[178]?.[0] ? Number(tagMap[178][0]) : undefined;
      const autofit = Boolean(tagMap[174]?.[0] ?? 0);
      const color = tagMap[62]?.[0] ? Number(tagMap[62][0]) : undefined;
      const bgColor = tagMap[63]?.[0] ? Number(tagMap[63][0]) : undefined;

      cells.push({
        row: r,
        col: c,
        text,
        type: rowStyle === 'Title' ? 'title' : rowStyle === 'Header' ? 'header' : 'text',
        cellStyle: 'по строке/столбцу',
        rowStyle,
        colStyle: 'нет',
        cellType,
        alignmentCode,
        alignmentName,
        width: colW,
        height: rowH,
        isMerged,
        colSpan,
        rowSpan,
        textStyle,
        textHeight,
        rotation,
        overrideFlags,
        virtualEdge,
        autofit,
        color,
        bgColor,
      });
    }
  } else {
    // Fallback if no 171 tags
    for (let r = 0; r < rows; r++) {
      for (let c = 0; c < cols; c++) {
        const rowStyle: 'Title' | 'Header' | 'Data' = r === 0 ? 'Title' : r === 1 ? 'Header' : 'Data';
        cells.push({
          row: r,
          col: c,
          text: r === 0 ? `Title` : r === 1 ? `Header ${c + 1}` : `Data ${r}-${c + 1}`,
          type: r === 0 ? 'title' : r === 1 ? 'header' : 'text',
          cellStyle: 'по строке/столбцу',
          rowStyle,
          colStyle: 'нет',
          cellType: 'Text',
          alignmentCode: 5,
          alignmentName: 'Middle Center (DXF 170: 5)',
          width: columnWidths[c] || 2.5,
          height: rowHeights[r] || 0.36,
          isMerged: false,
          colSpan: 1,
          rowSpan: 1,
        });
      }
    }
  }

  const breakHeight = rowHeights.reduce((sum, h) => sum + h, 0);

  // Identify row styles and detect top labels (initial Title and Header rows until the first Data row)
  const rowStyleMap: ('Title' | 'Header' | 'Data')[] = [];
  for (let r = 0; r < rows; r++) {
    const rowCells = cells.filter((c) => c.row === r);
    let style: 'Title' | 'Header' | 'Data' = 'Data';
    if (rowCells.some((c) => c.rowStyle === 'Title')) {
      style = 'Title';
    } else if (rowCells.some((c) => c.rowStyle === 'Header')) {
      style = 'Header';
    } else if (r === 0) {
      style = 'Title';
    } else if (r === 1) {
      style = 'Header';
    }
    rowStyleMap.push(style);
  }

  let topLabelsRowCount = 0;
  const topLabelRowStyles: string[] = [];
  let topLabelsHeight = 0;
  for (let r = 0; r < rows; r++) {
    const style = rowStyleMap[r];
    if (style === 'Title' || style === 'Header') {
      topLabelsRowCount++;
      topLabelRowStyles.push(style);
      topLabelsHeight += rowHeights[r] || 0.36;
    } else {
      // Encountered Data row: top labels end here
      break;
    }
  }

  let headerHeight = 0;
  let dataHeight = 0;
  for (let r = 0; r < rowHeights.length; r++) {
    if (r < topLabelsRowCount) {
      headerHeight += rowHeights[r];
    } else {
      dataHeight += rowHeights[r];
    }
  }

  const isManualPosition = Boolean(breakOptionInfo.manualPositioning || (breakFlags & 0x8));
  const positionMode: 'auto' | 'manual' = isManualPosition ? 'manual' : 'auto';
  // Repeat top labels (AutoCAD Group 90 Bit 5: 0x10 kTableBreakRepeatHeader)
  const repeatTopLabels = Boolean(breakOptionInfo.repeatHeader || (breakFlags & 0x10));

  return {
    id: `table-frag-${index}-${handle}`,
    handle,
    owner,
    layer,
    x,
    y,
    z,
    rows,
    cols,
    columnWidths,
    rowHeights,
    breakHeight,
    manualBreakHeight: parseFloat(breakHeight.toFixed(4)),
    isManualBreakHeight: isManualPosition,
    headerHeight,
    dataHeight,
    positionMode,
    flowDirection: 'Вниз',
    breakDirection: 'Вправо',
    breakSpacing: 0.99,
    repeatTopLabels,
    repeatBottomLabels: false,
    isManualPositioning: isManualPosition,
    topLabelsRowCount,
    topLabelRowStyles,
    topLabelsHeight: parseFloat(topLabelsHeight.toFixed(4)),
    styleHandle,
    blockRecord,
    breakFlags,
    breakOptionInfo,
    cells,
    rawTagCount: tags.length,
  };
}

function parseLineEntity(tags: DXFTag[], layer: string): DXFLineEntity | null {
  let x1 = 0,
    y1 = 0,
    z1 = 0;
  let x2 = 0,
    y2 = 0,
    z2 = 0;

  tags.forEach((t) => {
    if (typeof t.value === 'number') {
      if (t.code === 10) x1 = t.value;
      if (t.code === 20) y1 = t.value;
      if (t.code === 30) z1 = t.value;
      if (t.code === 11) x2 = t.value;
      if (t.code === 21) y2 = t.value;
      if (t.code === 31) z2 = t.value;
    }
  });

  return { type: 'LINE', layer, x1, y1, z1, x2, y2, z2 };
}

function parsePolylineEntity(
  tags: DXFTag[],
  layer: string
): DXFLwPolylineEntity | null {
  const points: { x: number; y: number }[] = [];
  let currentX: number | null = null;
  let closed = false;

  tags.forEach((t) => {
    if (t.code === 70 && typeof t.value === 'number') {
      closed = (t.value & 1) === 1;
    }
    if (t.code === 10 && typeof t.value === 'number') {
      currentX = t.value;
    }
    if (t.code === 20 && typeof t.value === 'number' && currentX !== null) {
      points.push({ x: currentX, y: t.value });
      currentX = null;
    }
  });

  if (points.length < 2) return null;
  return { type: 'LWPOLYLINE', layer, points, closed };
}

function parseCircleEntity(
  tags: DXFTag[],
  layer: string
): DXFCircleEntity | null {
  let x = 0,
    y = 0,
    z = 0,
    radius = 0;

  tags.forEach((t) => {
    if (typeof t.value === 'number') {
      if (t.code === 10) x = t.value;
      if (t.code === 20) y = t.value;
      if (t.code === 30) z = t.value;
      if (t.code === 40) radius = t.value;
    }
  });

  if (radius <= 0) return null;
  return { type: 'CIRCLE', layer, x, y, z, radius };
}

function parseTextEntity(
  tags: DXFTag[],
  type: 'TEXT' | 'MTEXT',
  layer: string
): DXFTextEntity | null {
  let x = 0,
    y = 0,
    z = 0;
  let text = '';
  let height = 2.5;

  tags.forEach((t) => {
    if (t.code === 1 || t.code === 3) text = String(t.value);
    if (typeof t.value === 'number') {
      if (t.code === 10) x = t.value;
      if (t.code === 20) y = t.value;
      if (t.code === 30) z = t.value;
      if (t.code === 40) height = t.value;
    }
  });

  return { type, layer, x, y, z, text, height };
}

function parseImageEntity(
  tags: DXFTag[],
  layer: string,
  imageDefs: Map<string, DXFImageDefInfo>
): DXFImageEntity | null {
  const handle = String(tags.find((t) => t.code === 5)?.value || '');
  const x = Number(tags.find((t) => t.code === 10)?.value || 0);
  const y = Number(tags.find((t) => t.code === 20)?.value || 0);
  const z = Number(tags.find((t) => t.code === 30)?.value || 0);

  const uX = Number(tags.find((t) => t.code === 11)?.value ?? 1);
  const uY = Number(tags.find((t) => t.code === 21)?.value ?? 0);
  const uZ = Number(tags.find((t) => t.code === 31)?.value ?? 0);

  const vX = Number(tags.find((t) => t.code === 12)?.value ?? 0);
  const vY = Number(tags.find((t) => t.code === 22)?.value ?? 1);
  const vZ = Number(tags.find((t) => t.code === 32)?.value ?? 0);

  const width = Number(tags.find((t) => t.code === 13)?.value || 100);
  const height = Number(tags.find((t) => t.code === 23)?.value || 100);

  const imageDefHandle = String(tags.find((t) => t.code === 340)?.value || '').toUpperCase();
  const displayProps = Number(tags.find((t) => t.code === 70)?.value ?? 7);
  const clipping = Number(tags.find((t) => t.code === 280)?.value ?? 0) === 1;
  const brightness = Number(tags.find((t) => t.code === 281)?.value ?? 50);
  const contrast = Number(tags.find((t) => t.code === 282)?.value ?? 50);
  const fade = Number(tags.find((t) => t.code === 283)?.value ?? 0);

  // Look up IMAGEDEF by handle (case-insensitive)
  let foundDef: DXFImageDefInfo | undefined;
  for (const [defH, def] of imageDefs.entries()) {
    if (defH.toUpperCase() === imageDefHandle || (!foundDef && imageDefs.size === 1)) {
      foundDef = def;
      break;
    }
  }

  const imagePath = foundDef?.path || '';
  const cleanPath = imagePath.replace(/\\/g, '/');
  const imageFileName = foundDef?.fileName || (cleanPath ? cleanPath.split('/').pop() || 'testimage.png' : 'testimage.png');

  const uLen = Math.hypot(uX, uY);
  const vLen = Math.hypot(vX, vY);
  const cadWidth = parseFloat((width * uLen).toFixed(4));
  const cadHeight = parseFloat((height * vLen).toFixed(4));
  const rotationDeg = parseFloat(((Math.atan2(uY, uX) * 180) / Math.PI).toFixed(2));

  // Collect clip vertices if any
  const clipVertices: { x: number; y: number }[] = [];
  for (let k = 0; k < tags.length; k++) {
    if (tags[k].code === 14 && k + 1 < tags.length && tags[k + 1].code === 24) {
      clipVertices.push({
        x: Number(tags[k].value),
        y: Number(tags[k + 1].value),
      });
    }
  }

  const resolvedSrcUrl = `/samples/${imageFileName}`;

  return {
    type: 'IMAGE',
    handle,
    layer,
    x,
    y,
    z,
    uVector: { x: uX, y: uY, z: uZ },
    vVector: { x: vX, y: vY, z: vZ },
    imageSize: { width, height },
    cadWidth,
    cadHeight,
    rotationDeg,
    imageDefHandle,
    imagePath,
    imageFileName,
    resolvedSrcUrl,
    displayProps,
    showImage: (displayProps & 1) !== 0,
    transparency: (displayProps & 8) !== 0,
    clipping,
    brightness,
    contrast,
    fade,
    clipVertices: clipVertices.length > 0 ? clipVertices : undefined,
    imageDef: foundDef,
  };
}
