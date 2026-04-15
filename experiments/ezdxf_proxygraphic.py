from __future__ import annotations

from pathlib import Path


def extract_proxy_hex(file_name: str, entity_name: str) -> str:
    lines = Path(file_name).read_text(encoding="utf-8", errors="ignore").splitlines()
    inside_entity = False
    expect_hex_value = False
    chunks: list[str] = []

    i = 0
    while i < len(lines):
        line = lines[i].strip()
        if not inside_entity:
            if line == "0" and i + 1 < len(lines) and lines[i + 1].strip() == entity_name:
                inside_entity = True
                i += 2
                continue
        else:
            if expect_hex_value:
                chunks.append(line)
                expect_hex_value = False
            elif line == "310":
                expect_hex_value = True
            elif line == "100" and i + 1 < len(lines) and lines[i + 1].strip() == "AcDbMLeader":
                break
        i += 1
    return "".join(chunks)


def main() -> None:
    hex_data = extract_proxy_hex("cad_source/test/mleaderblock.dxf", "MULTILEADER")
    if not hex_data:
        raise SystemExit("proxy graphic not found")

    try:
        import ezdxf
        from ezdxf.proxygraphic import ProxyGraphic
    except ImportError as exc:
        raise SystemExit(f"ezdxf is required for this experiment: {exc}") from exc

    proxy = ProxyGraphic(bytes.fromhex(hex_data), doc=None)
    for index, entity in enumerate(proxy.virtual_entities()):
        print(index, entity.dxftype(), entity.dxfattribs())


if __name__ == "__main__":
    main()
