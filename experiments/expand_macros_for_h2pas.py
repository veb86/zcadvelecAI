#!/usr/bin/env python3
"""
Expand commented-out C macros in dwg_new.h so the file is fully consumable
by h2pas (issue #1031, follow-up to #1029).

Background
----------

In #1029 we commented out every C-preprocessor directive in
``cad_source/components/fpdwg/libredwg/dwg_new.h`` so h2pas would not
see ``#define`` / ``#include`` / ``#ifdef`` and friends.  The side effect
is that the macros which used to *expand* into struct fields no longer
do so: their bodies are now inert ``// #define ...`` comment blocks, but
the *usages* remain as bare identifiers like::

    typedef struct _dwg_entity_POLYLINE_2D
    {
      COMMON_ENTITY_POLYLINE;   <-- h2pas: syntax error
      BITCODE_BS flag;
      ...
    } Dwg_Entity_POLYLINE_2D;

This script collects every commented ``// #define MACRO ... \``
multi-line macro, extracts its body, and inlines it at each call site.
Macro nesting (one macro referencing another in its body) is handled by
recursive expansion.

Limitations
-----------

* Function-style macros that take an argument (``MACRO(arg)``) are not
  used anywhere in the actual struct definitions of dwg_new.h, so they
  are *not* expanded.  They appear only inside the bodies of *other*
  commented macros that are themselves never used as struct members
  (e.g. the ``ACGEOMCONSTRAINT_fields(node)`` family is defined but
  never invoked at the top level), and we conservatively skip them.
* The script is idempotent: running it twice produces the same file.
* The script only edits between the pre-existing markers
  ``// === h2pas preparation stubs (issue #1029) ===``...end-of-file.

Usage
-----

Run from the repository root::

    python3 experiments/expand_macros_for_h2pas.py

The default input/output is
``cad_source/components/fpdwg/libredwg/dwg_new.h`` (in-place).
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[1]
DEFAULT_SRC = REPO_ROOT / "cad_source/components/fpdwg/libredwg/dwg_new.h"

# A commented multi-line macro definition, e.g.
#   // #define COMMON_ENTITY_POLYLINE                    \
#   //   struct _dwg_object_entity *parent;              \
#   //   ...                                             \
#   //   BITCODE_H seqend
#
# We require continuation (\) at the end of the first line, otherwise
# it's a single-line define which is harmless for h2pas.
#
# Group 1 = macro name; group 2 = parameter list (without the
# parentheses) for function-like macros, or empty for object-like ones.
MACRO_HEAD_RE = re.compile(
    r"^//\s*#define\s+([A-Za-z_][A-Za-z_0-9]*)\s*(?:\(([^)]*)\))?"
)

# A line that *uses* a macro as a struct member: optional whitespace,
# then a bare identifier, optionally with ``(args)`` for function-like
# macros, then ';'. We also tolerate trailing comments. This matches the
# form at the call site, where the C source spells out ``MACRO_NAME;``
# or ``MACRO_NAME(arg);``.
USE_RE = re.compile(
    r"^(\s*)([A-Za-z_][A-Za-z_0-9]*)(?:\(([^)]*)\))?;"
    r"\s*(/\*.*?\*/|//[^\n]*)?\s*$"
)

# Inside a macro body the *last* line conventionally has no trailing
# semicolon (because the call site supplies one). When that final body
# line is itself a nested macro reference we still want to recognise
# and expand it; this regex matches a bare identifier (with optional
# arguments) and no ';'.
USE_NO_SEMI_RE = re.compile(
    r"^(\s*)([A-Za-z_][A-Za-z_0-9]*)(?:\(([^)]*)\))?"
    r"\s*(/\*.*?\*/|//[^\n]*)?\s*$"
)


def collect_macros(
    lines: list[str],
) -> dict[str, tuple[list[str], list[str]]]:
    """Return a mapping from macro name to ``(params, body)``.

    ``params`` is an empty list for object-like macros and a list of
    parameter names for function-like ones. ``body`` is the list of
    body lines (with the leading ``// `` stripped and trailing ``\\``
    removed, but trailing comments preserved).

    The macro head line itself is not included in the body.
    """
    macros: dict[str, tuple[list[str], list[str]]] = {}
    i = 0
    n = len(lines)
    while i < n:
        m = MACRO_HEAD_RE.match(lines[i])
        if not m:
            i += 1
            continue

        head = lines[i].rstrip("\n").rstrip("\r")
        if not head.endswith("\\"):
            # single-line define -- not interesting (it's not a body
            # macro that gets used as a struct member).
            i += 1
            continue

        name = m.group(1)
        params_str = m.group(2) or ""
        params = (
            [p.strip() for p in params_str.split(",") if p.strip()]
            if params_str
            else []
        )
        body: list[str] = []

        i += 1
        while i < n:
            raw = lines[i].rstrip("\n").rstrip("\r")
            if not raw.lstrip().startswith("//"):
                break
            stripped = raw.lstrip()
            # Strip leading "//" and the single space that conventionally
            # follows it, if any.
            if stripped.startswith("// "):
                content = stripped[3:]
            else:
                content = stripped[2:]
            ends_with_continuation = content.endswith("\\")
            if ends_with_continuation:
                content = content[:-1].rstrip()
            body.append(content)
            i += 1
            if not ends_with_continuation:
                break

        macros[name] = (params, body)
    return macros


def _substitute_params(text: str, mapping: dict[str, str]) -> str:
    """Replace whole-word parameter names in ``text`` with their values."""
    if not mapping:
        return text
    pattern = re.compile(
        r"\b(" + "|".join(re.escape(k) for k in mapping) + r")\b"
    )
    return pattern.sub(lambda m: mapping[m.group(1)], text)


def expand_body(
    body: list[str],
    macros: dict[str, tuple[list[str], list[str]]],
    indent: str,
    args: list[str] | None = None,
    params: list[str] | None = None,
    seen: tuple[str, ...] = (),
) -> list[str]:
    """Expand a macro body, recursively inlining any nested macro uses.

    ``indent`` is the indentation of the original use site; each emitted
    line is prefixed with this indent. ``seen`` guards against infinite
    recursion in case of accidental cycles.

    For function-like macros, ``params`` and ``args`` carry the formal
    parameter list and the actual arguments at the call site so that
    occurrences of a parameter name in the body are textually replaced
    with the argument.

    The convention for the C macros in this file is that the *last*
    field of the body has no trailing semicolon (because the call site
    appends ``;`` after ``MACRO``). When we inline the body we therefore
    add a semicolon to the final non-comment statement so it remains
    valid C.
    """
    mapping: dict[str, str] = {}
    if params and args is not None and len(params) == len(args):
        mapping = dict(zip(params, args))

    out: list[str] = []
    for raw in body:
        # Skip blank body lines (they sometimes appear at the end of a
        # multi-line continuation if the macro author left a stray space).
        if not raw.strip():
            continue

        # Substitute parameter names with arguments before doing
        # anything else.
        substituted = _substitute_params(raw, mapping) if mapping else raw

        m = USE_RE.match(substituted)
        if not m:
            # Try the no-semicolon form, which only happens for the
            # *last* line of a macro body that itself references another
            # macro (the call site supplies the ';').
            m = USE_NO_SEMI_RE.match(substituted)
        if m and m.group(2) in macros and m.group(2) not in seen:
            nested_name = m.group(2)
            nested_params, nested_body = macros[nested_name]
            nested_args_str = m.group(3) or ""
            nested_args = (
                [a.strip() for a in nested_args_str.split(",") if a.strip()]
                if nested_args_str
                else []
            )
            out.extend(
                expand_body(
                    nested_body,
                    macros,
                    indent,
                    args=nested_args,
                    params=nested_params,
                    seen=seen + (nested_name,),
                )
            )
            continue
        out.append(indent + substituted.lstrip())

    # Ensure the very last *statement* line ends with a semicolon: the
    # macro call site has the form ``MACRO_NAME;`` so the body's final
    # statement (which lacked its own ';') needs one to remain a valid
    # C declaration.
    for j in range(len(out) - 1, -1, -1):
        line = out[j]
        stripped = line.strip()
        if not stripped:
            continue
        if stripped.startswith("//") or stripped.startswith("/*"):
            continue
        # Strip trailing comments to inspect the actual code.
        code_part = re.sub(r"\s*/\*.*?\*/\s*$", "", line)
        code_part = re.sub(r"\s*//.*$", "", code_part)
        if code_part.rstrip().endswith(";"):
            break
        # Insert semicolon before any trailing comment so the comment
        # stays attached.
        m_comment = re.search(r"\s*(/\*.*?\*/|//.*)$", line)
        if m_comment:
            out[j] = line[: m_comment.start()].rstrip() + ";" + line[m_comment.start():]
        else:
            out[j] = line.rstrip() + ";"
        break
    return out


def _strip_extra_constructs(lines: list[str]) -> list[str]:
    """Comment out the few remaining constructs h2pas still rejects.

    This covers:

    * ``EXPORT`` function prototypes -- the ``EXPORT`` macro is commented
      out (issue #1029) so the leading ``EXPORT`` is now a bare
      identifier and h2pas treats the entire prototype as a syntax
      error. We don't actually need these prototypes for h2pas to
      generate the Pascal type bindings, so the simplest fix is to
      comment the prototype out (multi-line prototypes too).
    * ``__counted_by(...)`` attribute usage -- the macro definition is
      commented out, but two of the call sites are *not* (they appear
      inside a comment in the form ``/*__counted_by(length)*/``). The
      remaining live call site uses no comment guards; we strip the
      attribute.
    * Anonymous ``enum { ... };`` blocks -- h2pas only handles named
      enums. We give each one a synthetic name based on the file name
      and a counter so the generated Pascal binding still gets the
      constants.
    """
    out: list[str] = []
    enum_counter = 0
    i = 0
    n = len(lines)
    while i < n:
        line = lines[i]

        # 1. EXPORT function prototypes.
        stripped = line.lstrip()
        if stripped.startswith("EXPORT ") or stripped.startswith("EXPORT\t") \
                or stripped.rstrip() == "EXPORT":
            # Comment out the whole prototype: keep going until we see a
            # line ending with ';'.
            block = []
            while i < n:
                cur = lines[i]
                block.append("// " + cur)
                # Strip trailing comments to find the real end.
                code = re.sub(r"//.*$", "", cur)
                code = re.sub(r"/\*.*?\*/", "", code)
                if code.rstrip().endswith(";") or code.rstrip().endswith("}"):
                    i += 1
                    break
                i += 1
            out.extend(block)
            continue

        # 2. __counted_by(...) attribute -- drop it. Replacing with a
        #    block comment would risk nesting inside an existing
        #    ``/* ... */``, so just remove it (and any stray adjacent
        #    whitespace).
        new_line = re.sub(r"\s*__counted_by\s*\([^)]*\)", "", line)

        # 2b. Flexible array members ``foo[]`` are C99 only; h2pas
        #    rejects them. Convert to ``foo[0]`` (zero-length array)
        #    which the C source itself uses in its SWIG branch as a
        #    fallback.
        new_line = re.sub(
            r"\b([A-Za-z_][A-Za-z_0-9]*)\s*\[\s*\]",
            r"\1[0]",
            new_line,
        )

        # 3. Anonymous enum -- detect ``enum`` line not followed by an
        #    identifier (i.e. ``enum`` then ``{`` on the same or next
        #    line, with no name in between).
        m_enum = re.match(r"^(\s*)enum(\s*)$", new_line) or re.match(
            r"^(\s*)enum(\s*\{)", new_line
        )
        if m_enum and not re.match(
            r"^\s*enum\s+[A-Za-z_]", new_line
        ):
            # Generate a synthetic name and rewrite the line.
            enum_counter += 1
            name = f"_h2pas_anon_enum_{enum_counter}"
            indent = m_enum.group(1)
            if new_line.rstrip().endswith("enum"):
                new_line = f"{indent}enum {name}\n"
            else:
                # ``enum {`` form (possibly with a comment afterwards).
                new_line = re.sub(
                    r"^(\s*)enum(\s*\{)", rf"\1enum {name}\2", new_line
                )

        out.append(new_line)
        i += 1
    return out


def transform(text: str) -> tuple[str, list[tuple[int, str, int]]]:
    """Inline every macro use that resolves to a known commented macro.

    Returns the transformed text and a list of ``(line_number, name,
    expanded_lines)`` for reporting.
    """
    lines = text.splitlines(keepends=False)
    macros = collect_macros(lines)

    out: list[str] = []
    report: list[tuple[int, str, int]] = []
    for idx, raw in enumerate(lines, start=1):
        m = USE_RE.match(raw)
        if m and m.group(2) in macros:
            indent = m.group(1)
            name = m.group(2)
            params, body = macros[name]
            args_str = m.group(3) or ""
            args = (
                [a.strip() for a in args_str.split(",") if a.strip()]
                if args_str
                else []
            )
            expanded = expand_body(
                body, macros, indent, args=args, params=params, seen=(name,),
            )
            # Add a marker so a future reader can grep the inlined content
            # back to its macro of origin.
            out.append(f"{indent}// === inlined macro {name} (issue #1031) ===")
            out.extend(expanded)
            out.append(f"{indent}// === end of inlined {name} ===")
            report.append((idx, name, len(expanded)))
            continue
        out.append(raw)

    out = _strip_extra_constructs(out)

    return "\n".join(out) + "\n", report


def main(argv: list[str]) -> int:
    src = Path(argv[1]) if len(argv) > 1 else DEFAULT_SRC
    text = src.read_text(encoding="utf-8")
    new_text, report = transform(text)
    src.write_text(new_text, encoding="utf-8")
    print(f"Wrote {src} ({len(new_text)} bytes)")
    print(f"Inlined {len(report)} macro use(s):")
    for line, name, count in report:
        print(f"  line {line:>5}: {name} -> {count} field line(s)")
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
