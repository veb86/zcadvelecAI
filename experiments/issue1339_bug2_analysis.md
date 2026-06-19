# Issue #1339 — Bug 2 root cause: table cell text invisible in AutoCAD until regen

## Symptom (reported)
After ZCAD re-saves a split (`AcadTableBreakEnabled=true`) ACAD_TABLE and the
file is reopened in AutoCAD, the table grid draws correctly but **cell text is
not displayed**. Any transform (move / rotate / scale) forces a regen and the
text appears immediately. So the data loads, but the initial display is empty.

## Empirically confirmed root cause
Comparing the AutoCAD-saved originals against the ZCAD re-saved files (all
present in `cad_source/test/`, `acadtablerazdel2007_N.dxf` vs
`zcadtablerazdel2007_N.dxf`):

| object        | AutoCAD original | ZCAD re-saved |
|---------------|------------------|---------------|
| `TABLECONTENT`| 1                | **0 (dropped)** |
| `TABLEGEOMETRY`| 1               | **0 (dropped)** |
| `XRECORD`     | 3                | 1             |
| `DICTIONARY`  | 17               | 10            |
| `MTEXT`       | 55               | 55 (kept)     |

ZCAD drops the ACAD_TABLE entity's **extension-dictionary content subtree**:

```
ACAD_TABLE (entity, handle EB)
  └─ 360 xdict ─▶ DICTIONARY (357)
                    └─ ACAD_XREC_ROUNDTRIP ─▶ XRECORD (358)
                                                └─ TABLECONTENT (2B5)   ← AcDbTableContent: the cell data AutoCAD renders
                    (and)                       TABLEGEOMETRY (2B6)      ← AcDbTableGeometry: cached cell geometry
```

AutoCAD 2010+ renders table cell content from `AcDbTableContent`. With that
object missing, AutoCAD has the grid (from the entity / anonymous block) but no
content object to render text from on initial open; a regen rebuilds the
display and the text appears. This matches the reported symptom exactly.

`uzeacadtable_dxf_write.WriteRawEntityText` currently **deletes** the entity's
`{ACAD_XDICTIONARY ... 360 ... }` block on save (see comment at the proc:
"блок расширенного словаря ... удаляется ... иначе оставляет висячую ссылку"),
precisely because the subtree it points to is not round-tripped. Fixing Bug 2
means round-tripping that subtree instead of dropping it.

Reproduce: `python3 experiments/issue1339_bug2_subtree.py` (asserts the
AutoCAD files contain the subtree and the ZCAD files currently drop it).

## Scope of the subtree (measured on `acadtablerazdel2007_1.dxf`)
- 4 objects: `DICTIONARY(357)`, `XRECORD(358)`, `TABLECONTENT(2B5, ~7117 groups)`,
  `TABLEGEOMETRY(2B6, ~806 groups)`.
- **No binary (group 310) data** anywhere in the subtree — pure text DXF.
- External handle references that must be remapped on save:

| code | target | meaning | count | remap on save |
|------|--------|---------|-------|---------------|
| 330  | EB / 2B7 / 307 | owner backptrs to the 3 ACAD_TABLE entities | 3 | **identity** — ZCAD already preserves these entity handles |
| 340  | 11 | text `STYLE "Standard"` | 5 | new text-style handle (ZCAD renumbers styles) |
| 340  | 87 | `TABLESTYLE "Standard"` | 1 | new table-style handle (`TableStyleNameHandleMap` already exists) |

- Internal handles `357 / 358 / 2B5 / 2B6` and the entity's `360` link must be
  remapped consistently to fresh anonymous handles (or preserved if collision
  with ZCAD's renumbering is provably avoided).

## Proposed fix (needs compile + AutoCAD verification — see PR request)
1. **Read** (`uzeffdxf` / model): capture the subtree text for each ACAD_TABLE
   from `RawObjectsSection` by following the entity's `360` xdict, store it on
   `GDBObjAcadTable` (e.g. `FRawExtDictSubtree` + original xdict handle).
2. **Save context** (`uzeffdxfsupport` / `uzeffdxfout`): add a text-style
   *name→new-handle* map, populated the same way
   `PreallocateTableStyleHandles` populates `TableStyleNameHandleMap`.
3. **Write** (`uzeacadtable_dxf_write`):
   - In `WriteRawEntityText`, **keep** the `360` xdict and remap it to the new
     `DICTIONARY` handle (instead of deleting the block).
   - In `WriteAcadTableRoundTripObjectsToDXF`, after the roundtrip XRECORD,
     emit the captured subtree with all handles remapped:
     internal `357/358/2B5/2B6` → fresh; `340→11` → new text-style handle;
     `340→87` → `TableStyleNameHandleMap["Standard"]`; `330→EB/2B7/307`
     identity.
   - **Fail-safe:** only emit (and keep the entity's `360`) when capture
     succeeded *and* every required style handle resolves; otherwise fall back
     to today's drop behavior so the result is never worse than current.

## Why this is being requested rather than committed blind
No FPC/Lazarus compiler is available in this environment and AutoCAD cannot be
run here, so a handle-remapping mistake in the ~8000-group subtree would
produce a file that looks plausible but is silently broken, with no way to
detect it locally. The diagnosis above is verified against real files; the
implementation needs a build + an AutoCAD open-test to confirm.
