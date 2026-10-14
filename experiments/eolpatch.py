#!/usr/bin/env python3
"""Apply exact text replacements while preserving per-region CRLF/LF endings.

Usage: eolpatch.py FILE PATCHFILE
PATCHFILE contains blocks separated by lines '<<<<', '====', '>>>>'.
"""
import re
import sys


def apply(text, old, new):
    pattern = r'\r?\n'.join(re.escape(line) for line in old.split('\n'))
    matches = list(re.finditer(pattern, text))
    if len(matches) != 1:
        raise SystemExit(f"expected 1 match, got {len(matches)} for:\n{old}")
    m = matches[0]
    eol = '\r\n' if '\r\n' in m.group(0) else '\n'
    return text[:m.start()] + new.replace('\n', eol) + text[m.end():]


def main():
    path, patch = sys.argv[1], sys.argv[2]
    with open(path, encoding='utf-8', newline='') as f:
        text = f.read()
    with open(patch, encoding='utf-8') as f:
        blocks = f.read().split('<<<<\n')[1:]
    for block in blocks:
        old, rest = block.split('====\n', 1)
        new = rest.split('>>>>\n', 1)[0]
        text = apply(text, old.rstrip('\n'), new.rstrip('\n'))
    with open(path, 'w', encoding='utf-8', newline='') as f:
        f.write(text)


if __name__ == '__main__':
    main()
