#!/usr/bin/env python3
"""Регрессионный тест кодировки строк при сохранении в DXF2000 (issue 1438).

После чтения DWG/DXF через LibreDWG и сохранения в DXF2000 весь текст
превращался в "??????". Причина: загрузчик LibreDWG не задавал
DXFCodePage чертежа (оставалась ZCCPINVALID), а при сохранении
ZCCP2Str(ZCCPINVALID) давал $DWGCODEPAGE=ANSI_1251, тогда как строки
перекодировались в ZCCodePage2SysCP(ZCCPINVALID)=1252, где нет кириллицы.

Если в системе есть fpc, дополнительно собирается и запускается
experiments/issue1438/test_dwg_codepage.lpr
(experiments/issue1438/run_test.sh).
"""

from pathlib import Path
import os
import shutil
import subprocess
import tempfile


ROOT = Path(__file__).resolve().parent
FILEFORMATS = ROOT / "cad_source" / "zengine" / "fileformats"
LIBREDWG = FILEFORMATS / "uzefflibredwg.pas"
DXF_OUT = FILEFORMATS / "uzeffdxfout.pas"
DXF_SUPPORT = FILEFORMATS / "uzeffdxfsupport.pas"
DWG_CODEPAGE = FILEFORMATS / "dwg" / "uzedwgcodepage.pas"
DWG_TEXT = ROOT / "cad_source" / "components" / "fpdwg" / "uzedwgtext.pas"
EXPERIMENTS = ROOT / "experiments" / "issue1438"


def read_pas(path: Path) -> str:
    return path.read_bytes().decode("utf-8", errors="replace")


def compact(text: str) -> str:
    return "".join(text.split()).lower()


def strip_line_comments(text: str) -> str:
    return "\n".join(line.split("//", 1)[0] for line in text.splitlines())


def extract_block(source: str, marker: str, next_marker: str) -> str:
    start = source.index(marker)
    end = source.index(next_marker, start + len(marker))
    return source[start:end]


def test_libredwg_loaders_set_drawing_codepage():
    source = compact(read_pas(LIBREDWG))
    assert "uzedwgcodepage" in source
    apply_proc = extract_block(
        source, "procedureapplydwgcodepagetodrawing(", "end;"
    )
    assert "dxfcodepage:=dwgheadercodepagetozccodepage(" in apply_proc
    assert "dwg.header.codepage" in apply_proc
    implementation = source[source.index("implementation"):]
    dwg_loader = extract_block(
        implementation, "procedureaddfromdwg(", "procedureaddfromdxf("
    )
    dxf_loader = implementation[
        implementation.index("procedureaddfromdxf("):
    ]
    for body in (dwg_loader, dxf_loader):
        assert "applydwgcodepagetodrawing(zcdctx,dwg);" in body


def test_dxf_writer_uses_same_codepage_for_header_and_strings():
    source = compact(strip_line_comments(read_pas(DXF_OUT)))
    assert (
        "varsdict.add('$dwgcodepage',"
        "zccp2str(zccodepageordefault(drawing.dxfcodepage)));"
    ) in source
    assert (
        "header.dwgcodepage:=zccodepage2acdwgcodepage("
        "zccodepageordefault(drawing.dxfcodepage))"
    ) in source
    assert (
        "header.idwgcodepage:=zccodepage2syscp("
        "zccodepageordefault(drawing.dxfcodepage));"
    ) in source
    assert "zccodepage2syscp(drawing.dxfcodepage)" not in source


def test_support_declares_codepage_helpers():
    source = compact(read_pas(DXF_SUPPORT))
    interface = source[: source.index("implementation")]
    assert "functionsyscp2zccodepage(scp:tsystemcodepage):tzccodepage;" in interface
    assert "functionzccodepageordefault(zccp:tzccodepage):tzccodepage;" in interface
    helper = extract_block(
        source, "functionzccodepageordefault(zccp:tzccodepage):tzccodepage;"
        "begin", "end;"
    )
    assert "sysvarsysdwg_codepage" in helper


def test_dwg_codepage_mapping_unit():
    source = compact(read_pas(DWG_CODEPAGE))
    assert "unituzedwgcodepage;" in source
    assert "dwglibrecodepagetosystem(dwgcodepage,systemcodepage)" in source
    assert "result:=sysvarsysdwg_codepage" in source
    text_interface = compact(read_pas(DWG_TEXT))
    text_interface = text_interface[: text_interface.index("implementation")]
    assert "functiondwglibrecodepagetosystem(" in text_interface


def test_pascal_regression_program():
    """Собирает и запускает Pascal-тест, если доступен fpc."""
    if shutil.which("fpc") is None:
        print("fpc not found, Pascal regression program skipped")
        return
    with tempfile.TemporaryDirectory() as out:
        run = subprocess.run(
            [str(EXPERIMENTS / "run_test.sh")],
            cwd=ROOT, env=dict(os.environ, OUT=out), capture_output=True,
            text=True, errors="replace", timeout=900,
        )
        print(run.stdout[-2000:])
        assert run.returncode == 0, run.stdout[-4000:] + run.stderr
        assert "all checks passed" in run.stdout

if __name__ == "__main__":
    test_libredwg_loaders_set_drawing_codepage()
    test_dxf_writer_uses_same_codepage_for_header_and_strings()
    test_support_declares_codepage_helpers()
    test_dwg_codepage_mapping_unit()
    test_pascal_regression_program()
    print("issue 1438 DWG codepage checks passed")
