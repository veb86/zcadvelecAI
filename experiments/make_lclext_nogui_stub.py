#!/usr/bin/env python3
"""Генерация заглушек include-файлов lclextensions для виджетсета nogui.

Нужно только для проверки компиляции модулей с laz.VirtualTrees
(experiments/compile_spell_units.sh): реализации функций пустые.
Использование: make_lclext_nogui_stub.py LAZARUS_DIR OUT_DIR
"""
import re
import sys
from pathlib import Path

laz, out = Path(sys.argv[1]), Path(sys.argv[2])
out.mkdir(parents=True, exist_ok=True)
src = (laz / 'components/lclextensions/delphicompat.pas').read_text(encoding='latin-1')
interface = src[src.index('interface'):src.index('implementation')]
decls = re.findall(r'^function\s+\w+\s*(?:\([^)]*\))?\s*:\s*\w+\s*;(?:\s*stdcall;)?', interface, re.M)
body = ['type\n  TTimerList = class\n  end;\n\nvar\n  FTimerList: TTimerList;\n']
for d in decls:
    body.append(d + '\nbegin\n  FillChar(Result, SizeOf(Result), 0);\nend;\n')
(out / 'delphicompat.inc').write_text('\n'.join(body))
(out / 'uses.inc').write_text('  LCLIntf, Graphics, math,\n')
(out / 'uses_lclext.inc').write_text('')
(out / 'lclext.inc').write_text('''
function CreateBitmapMask(BitmapDC: HDC; Width, Height: Integer; TransparentColor: TColor): HBITMAP;
begin
  Result := 0;
end;

function DirectMaskBlt(DestDC: HDC; X, Y, Width, Height: Integer; SrcDC: HDC; XSrc, YSrc: Integer; Mask: HBITMAP): Boolean;
begin
  Result := False;
end;

function OptimalPixelFormat: TPixelFormat;
begin
  Result := pfDevice;
end;

function OSSupportsUTF16: Boolean;
begin
  Result := False;
end;
''')
# Заглушки laz.VirtualTrees (include/intf/<виджетсет>)
vt = laz / 'components/virtualtreeview/include/intf'
(out / 'laz.olemethods.inc').write_text(f"{{$i {vt}/laz.dummyolemethods.inc}}\n")
(out / 'laz.vtvdragmanager.inc').write_text(f"{{$i {vt}/laz.dummydragmanager.inc}}\n")
(out / 'laz.vtgraphicsi.inc').write_text('''
procedure AlphaBlend(Source, Destination: HDC; const R: TRect; const Target: TPoint; Mode: TBlendMode; ConstantAlpha, Bias: Integer);
begin
end;

function CalculateScanline(Bits: Pointer; Width, Height, Row: Integer): Pointer;
begin
  Result := nil;
end;

function GetBitmapBitsFromBitmap(Bitmap: HBITMAP): Pointer;
begin
  Result := nil;
end;
''')
print(f'{len(decls)} stubs written to {out}')
