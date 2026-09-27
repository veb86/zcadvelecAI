#!/usr/bin/env python3
"""Регрессионный тест записи таблицы LAYER в DXF (issue 1436).

После выноса записи слоёв в uzestyleslayerdxf.pas AutoCAD перестал открывать
сохранённые файлы с ошибкой:

    Ошибка в таблице LAYER
    Ожидался 0 LAYER или 0 ENDTAB, получено 0 TABLE в строке 1172.

Причина: savedxf20XX копирует заголовок таблицы (0 TABLE / 2 LAYER / 5 /
330 / 100 / 70) из шаблона, а SaveLayerToDXF записывал его повторно.
"""

from pathlib import Path


ROOT = Path(__file__).resolve().parent
LAYER_DXF = ROOT / "cad_source" / "zengine" / "styles" / "uzestyleslayerdxf.pas"
DXF_OUT = ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxfout.pas"
DXF_IN = ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxf.pas"
DXF_SUPPORT = (
    ROOT / "cad_source" / "zengine" / "fileformats" / "uzeffdxfsupport.pas"
)
OLD_DXF = ROOT / "cad_source" / "oldsavelayer.dxf"
BROKEN_DXF = ROOT / "cad_source" / "newsavelayer3.dxf"


def read_pas(path: Path) -> str:
    return path.read_bytes().decode("utf-8", errors="replace")


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def extract_procedure(source: str, marker: str, next_marker: str) -> str:
    start = source.index(marker)
    end = source.index(next_marker, start + len(marker))
    return source[start:end]


def read_pairs(path: Path):
    """Читает DXF как список (номер строки, код группы, значение)."""
    lines = path.read_bytes().decode("cp1251", errors="replace").splitlines()
    pairs = []
    for i in range(0, len(lines) - 1, 2):
        pairs.append((i + 1, int(lines[i].strip()), lines[i + 1].strip()))
    return pairs


def layer_table_errors(path: Path):
    """Проверяет структуру таблицы LAYER так же, как это делает AutoCAD:
    после заголовка таблицы допустимы только записи 0 LAYER и 0 ENDTAB."""
    pairs = read_pairs(path)
    errors = []
    tables = 0
    records = []
    idx = 0
    while idx < len(pairs) - 1:
        _, code, value = pairs[idx]
        _, next_code, next_value = pairs[idx + 1]
        if (code, value) == (0, "TABLE") and (next_code, next_value) == (2, "LAYER"):
            tables += 1
            idx += 2
            while idx < len(pairs):
                line, code, value = pairs[idx]
                if code == 0:
                    if value == "ENDTAB":
                        break
                    if value != "LAYER":
                        errors.append(
                            f"line {line}: expected 0 LAYER or 0 ENDTAB, got 0 {value}"
                        )
                    else:
                        records.append(line)
                idx += 1
        idx += 1
    if tables != 1:
        errors.append(f"expected exactly one LAYER table, found {tables}")
    if not records:
        errors.append("LAYER table contains no layer records")
    return errors


def test_reference_file_before_refactoring_is_valid():
    assert layer_table_errors(OLD_DXF) == []


def test_file_saved_by_broken_writer_is_detected():
    """Воспроизведение: файл из issue содержит вложенный 0 TABLE."""
    errors = layer_table_errors(BROKEN_DXF)
    assert any("got 0 TABLE" in e for e in errors), errors


def test_writer_does_not_write_table_header():
    body = compact(
        extract_procedure(
            read_pas(LAYER_DXF),
            "procedure SaveLayerToDXF(",
            "initialization",
        )
    )
    assert "'table'" not in body
    assert "'acdbsymboltable'" not in body
    assert "dxfgroupcode(330)" not in body
    assert "layers^.count" not in body


def test_writer_closes_table_once():
    body = compact(
        extract_procedure(
            read_pas(LAYER_DXF),
            "procedure SaveLayerToDXF(",
            "initialization",
        )
    )
    assert body.count("dxfname_endtab") + body.count("'endtab'") == 1
    # ENDTAB пишется после цикла по слоям, а не внутри него
    assert body.index("untilplp=nil;") < body.index("dxfname_endtab")


def test_writer_uses_one_handle_per_layer():
    body = compact(
        extract_procedure(
            read_pas(LAYER_DXF),
            "procedure SaveLayerToDXF(",
            "initialization",
        )
    )
    # MyGetOrCreateValue сам увеличивает handle; вместе с Inc() хэндлы
    # расходовались бы дважды
    assert "mygetorcreatevalue" not in body
    assert body.count("inc(iodxfcontext.handle)") == 1


def test_plot_style_handle_comes_from_template():
    body = compact(
        extract_procedure(
            read_pas(LAYER_DXF),
            "procedure SaveLayerToDXF(",
            "initialization",
        )
    )
    assert "$f;" not in body
    assert "inttohex(iodxfcontext.layerplotstylehandle,0)" in body

    support = compact(read_pas(DXF_SUPPORT))
    assert "layerplotstylehandle:tdwghandle;" in support
    assert "layerplotstylehandle:=0;" in support

    out = compact(read_pas(DXF_OUT))
    assign = out.index("iodxfcontext.layerplotstylehandle:=plottablefansdle;")
    call = out.index(
        "styleinfo^.saveproc(@drawing.layertable,@outstream,iodxfcontext);"
    )
    assert assign < call


def test_modern_loader_uses_registry_and_r12_loader_is_unchanged():
    source = compact(read_pas(DXF_IN))
    r12 = compact(
        extract_procedure(
            read_pas(DXF_IN), "procedure AddFromDXF12(", "procedure ReadTextstyles("
        )
    )
    assert "finddxfstyle" not in r12
    assert (
        "zcdctx.pdrawing^.layertable.addlayer(layername,layercolor,-3,true,false,true,'',tloload);"
        in r12
    )
    assert "styleinfo^.loadproc(s,clayer,rdr,exitstring,zcdctx,context)" in source


if __name__ == "__main__":
    import sys

    failed = 0
    for name, fn in list(globals().items()):
        if name.startswith("test_") and callable(fn):
            try:
                fn()
                print(f"PASS {name}")
            except Exception as exc:  # noqa: BLE001
                failed += 1
                print(f"FAIL {name}: {exc}")
    sys.exit(1 if failed else 0)
