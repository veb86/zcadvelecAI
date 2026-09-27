"""Помощник: замена фрагмента в файле с сохранением CRLF/LF."""
import sys
from pathlib import Path


def replace(path, old, new):
    p = Path(path)
    s = p.read_bytes().decode('utf-8')
    crlf = '\r\n' in s
    if crlf:
        s = s.replace('\r\n', '\n')
    assert s.count(old) == 1, (path, s.count(old))
    s = s.replace(old, new, 1)
    if crlf:
        s = s.replace('\n', '\r\n')
    p.write_bytes(s.encode('utf-8'))
