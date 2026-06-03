#!/usr/bin/env python3
"""Static regression check for issue #1274 arrow-style combo names."""

from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def read_repo(path: str) -> str:
    return (ROOT / path).read_text(encoding="utf-8")


def require(condition: bool, message: str) -> None:
    if not condition:
        raise AssertionError(message)


def main() -> None:
    arrows = read_repo("cad_source/zcad/gui/uzcgui2arrows.pas")
    dimedit = read_repo("cad_source/zcad/gui/forms/uzcfdimedit.pas")
    tab_arrows = read_repo("cad_source/zcad/gui/uzcui_dimstyleedit_tab_arrows.inc")
    reg_leader = read_repo("cad_source/zcad/register/uzcregleader.pas")
    reg_zscript = read_repo("cad_source/zcad/register/uzcregzscript.pas")

    for expected in [
        "Closed filled",
        "Closed blank",
        "Closed",
        "Dot",
        "Architectural tick",
        "Oblique",
        "Open",
        "Origin indicator",
        "Origin indicator 2",
        "Right angle",
        "Open 30",
        "Dot small",
        "Dot blank",
        "Dot small blank",
        "Box",
        "Box filled",
        "Datum triangle",
        "Datum triangle filled",
        "Integral",
        "User Arrow...",
    ]:
        require(expected in arrows, f"missing arrow display name: {expected}")

    require("resourcestring" in arrows, "arrow display names must be resourcestrings")
    require("GetArrowStyleName" in arrows, "missing reusable arrow-style name helper")
    require("FillItems" in arrows, "missing reusable arrow combo fill helper")

    require(
        "GetEnumName(TypeInfo(TArrowStyle)" not in dimedit,
        "dimension edit form still exposes raw TArrowStyle enum identifiers",
    )

    require(
        tab_arrows.count("TSupportArrowStyleCombo.FillItems") == 3,
        "programmatic dimension-style dialog must fill all three arrow combos via the helper",
    )
    require(
        tab_arrows.count("TSupportArrowStyleCombo.Setup") == 3,
        "programmatic dimension-style dialog must owner-draw all three arrow combos",
    )
    require(
        "CArrowLeaderNames" not in reg_leader,
        "leader registration still uses a duplicate hardcoded arrow-name list",
    )
    require(
        "GetArrowStyleName(ias)" in reg_leader,
        "leader registration must reuse resource-string-backed arrow names",
    )
    require(
        "uzcgui2arrows" in reg_zscript,
        "TArrowStyle descriptor registration must import the arrow-name helper",
    )
    require(
        "TSUserDef'],[FNProgram]" in reg_zscript,
        "TArrowStyle program names must stay separate from user-facing names",
    )
    require(
        reg_zscript.count("GetArrowStyleName(TS") == 20,
        "TArrowStyle user names must be populated from the resource-string helper",
    )


if __name__ == "__main__":
    main()
