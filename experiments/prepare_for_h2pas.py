#!/usr/bin/env python3
"""
Prepare dwg_new.h for h2pas conversion.

Performs the basic preparation described in issue #1029:
1. Comment out all #-preprocessor directives (#include, #define, #ifdef,
   #ifndef, #if, #else, #elif, #endif, #undef, #error, #pragma) so h2pas
   does not trip on them.
2. Multi-line directives that use trailing backslashes are commented across
   their full extent.
3. Insert typedef stubs at the top of the file (so h2pas knows the BITCODE_*
   types that would normally come from <stdint.h>).
4. Lines inside /* ... */ block comments are left alone (already comments
   in C).

The input file is read and written in place.
"""
import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_SRC = REPO_ROOT / "cad_source/components/fpdwg/libredwg/dwg_new.h"

# Typedef stubs from the issue body. These are the types h2pas needs to know
# before it sees them used in struct fields.
STUBS = """// === h2pas preparation stubs (issue #1029) ===
typedef int    BITCODE_RC;
typedef double BITCODE_BD;
typedef int    BITCODE_RS;
typedef int    BITCODE_RL;
typedef void*  BITCODE_H;
typedef double BITCODE_RD;
typedef int    BITCODE_BL;
// === end of stubs ===

"""

# A preprocessor directive line: optional whitespace, then '#', then anything.
PREPROC_RE = re.compile(r"^[ \t]*#")


def update_block_comment_state(state: str, line: str) -> str:
    """Track whether we end the line still inside a /* ... */ block comment.

    `state` is "code" or "comment" at the *start* of the line; the returned
    value is the state at the *end* of the line.
    """
    i = 0
    n = len(line)
    while i < n:
        if state == "code":
            two = line[i:i + 2]
            if two == "/*":
                state = "comment"
                i += 2
                continue
            if two == "//":
                # Rest of line is a single-line comment; state can't change.
                return state
            i += 1
        else:  # state == "comment"
            if line[i:i + 2] == "*/":
                state = "code"
                i += 2
                continue
            i += 1
    return state


def transform(text: str) -> str:
    lines = text.splitlines(keepends=True)
    out: list[str] = []

    state = "code"           # block-comment state at the start of the next line
    in_continuation = False  # are we inside a backslash-continued directive?
    inserted_stubs = False

    for raw in lines:
        starts_in_comment = state == "code"  # actually means "we are not inside /* */"
        in_block_comment_start = state == "comment"
        end_state = update_block_comment_state(state, raw)

        # Insert stubs once: right before the first non-blank, non-comment line.
        if not inserted_stubs and not in_block_comment_start:
            stripped = raw.lstrip()
            if stripped and not stripped.startswith("//") and not stripped.startswith("/*"):
                out.append(STUBS)
                inserted_stubs = True

        commented = False

        if in_continuation:
            # Continuation of a previously-commented preprocessor directive.
            if not raw.lstrip().startswith("//"):
                out.append("// " + raw)
            else:
                out.append(raw)
            commented = True
            # Continuation persists if the line still ends with backslash
            # (ignoring the trailing newline).
            in_continuation = raw.rstrip("\n").rstrip("\r").endswith("\\")
        elif not in_block_comment_start and PREPROC_RE.match(raw):
            # A new preprocessor directive — comment it out.
            out.append("// " + raw)
            commented = True
            in_continuation = raw.rstrip("\n").rstrip("\r").endswith("\\")
        else:
            out.append(raw)

        state = end_state

    return "".join(out)


def main(argv: list[str]) -> int:
    src = Path(argv[1]) if len(argv) > 1 else DEFAULT_SRC
    text = src.read_text(encoding="utf-8")
    new_text = transform(text)
    src.write_text(new_text, encoding="utf-8")
    print(f"Wrote {src} ({len(new_text)} bytes)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
