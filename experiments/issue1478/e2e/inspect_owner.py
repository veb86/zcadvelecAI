"""Диагностика: как объекты ZCAD (устройства) раскладываются в DXF.

Примитивы устройства пишутся отдельными объектами с XDATA
_OWNERHANDLE=<handle владельца>; верхнеуровневые объекты ZCAD -
это объекты без владельца (или с владельцем вне файла).

python -I inspect_owner.py <file.dxf>
"""
import sys
from collections import Counter

import ezdxf


def owner(entity):
    """Возвращает _OWNERHANDLE из XDATA или None."""
    if not entity.xdata:
        return None
    for tags in entity.xdata.data.values():
        for tag in tags:
            value = str(tag.value)
            if tag.code == 1000 and value.startswith("_OWNERHANDLE="):
                return value.split("=", 1)[1]
    return None


doc = ezdxf.readfile(sys.argv[1])
msp = list(doc.modelspace())
handles = {e.dxf.handle.upper().lstrip("0") for e in msp}
owners = Counter(owner(e) for e in msp)
top = [e for e in msp if owner(e) is None or owner(e).lstrip("0") not in handles]
print("dxf entities:", len(msp), "owners:", len(owners))
print("top-level   :", len(top), Counter(e.dxftype() for e in top))
print("owner values:", owners.most_common(5))
