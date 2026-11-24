"""Проверяет файлы сквозного теста #1478 через ezdxf (как MCP-клиент).

python -I verify_dxf.py <sample.dxf> <workdir>
"""
import json
import os
import sys
from collections import Counter

import ezdxf


def summary(doc):
    """Возвращает типы, слои, блоки и число объектов с XDATA."""
    msp = doc.modelspace()
    ents = list(msp)
    used_blocks = {e.dxf.name for e in ents if e.dxftype() == "INSERT"}
    return {
        "count": len(ents),
        "types": Counter(e.dxftype() for e in ents),
        "layers": {e.dxf.layer for e in ents},
        "used_blocks": used_blocks,
        "xdata": sum(1 for e in ents if e.xdata),
    }


def local_path(work, reply):
    """Путь из ответа ZCAD (внутри контейнера) -> файл в рабочем каталоге."""
    marker = "/exports/"
    tail = reply["path"].split(marker, 1)[1]
    return os.path.join(work, "exports", tail)


def main(sample_path, work):
    """Сравнивает выгрузку с исходным чертежом."""
    result = json.load(open(f"{work}/summary.json", encoding="utf-8"))
    sample = summary(ezdxf.readfile(sample_path))
    sel_doc = ezdxf.readfile(local_path(work, result["selection"]))
    sel = summary(sel_doc)
    print("sample   :", sample["count"], dict(sample["types"]), "xdata", sample["xdata"])
    print("selection:", sel["count"], dict(sel["types"]), "xdata", sel["xdata"])
    # entity_count - объекты ZCAD (устройство = один объект), dxf_entity_count -
    # объекты в файле: устройство записывается как INSERT + его примитивы
    assert sel["count"] == result["selection"]["dxf_entity_count"], "dxf_entity_count"
    assert sel["count"] >= result["selection"]["entity_count"], "entity_count <= file"
    missing_layers = sel["layers"] - {layer.dxf.name for layer in sel_doc.layers}
    assert not missing_layers, f"layers present in table: {missing_layers}"
    missing_blocks = sel["used_blocks"] - {b.name for b in sel_doc.blocks}
    assert not missing_blocks, f"block definitions present: {missing_blocks}"
    assert sel["used_blocks"], "sample selection contains INSERTs"
    assert sel["xdata"] > 0, "XDATA exported by default"
    print("layers   :", sorted(sel["layers"]))
    print("blocks   :", sorted(sel["used_blocks"]))
    all_doc = summary(ezdxf.readfile(local_path(work, result["all"])))
    assert all_doc["count"] == sel["count"] + 1, "scope=all has the extra LINE"
    assert all_doc["count"] == result["all"]["dxf_entity_count"], "all: dxf_entity_count"
    nox = summary(ezdxf.readfile(local_path(work, result["noxdata"])))
    assert nox["count"] == sel["count"], "same entities without XDATA"
    assert nox["xdata"] == 0, "include_xdata=false removes XDATA"
    print("PASS ezdxf verification")


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2])
