#!/usr/bin/env python3
# Длины функций/процедур Pascal (от заголовка до закрывающего end;)
import re, sys
for p in sys.argv[1:]:
    lines = open(p, encoding='utf-8', errors='replace').read().split('\n')
    impl = next((i for i, l in enumerate(lines) if l.strip().lower() == 'implementation'), 0)
    i = impl
    while i < len(lines):
        m = re.match(r'^(procedure|function|constructor|destructor)\s+([\w.]+)', lines[i])
        if m:
            depth = 0; started = False; j = i
            while j < len(lines):
                t = re.sub(r"'[^']*'", '', lines[j].split('//')[0]).lower()
                for w in re.findall(r'\b(begin|end|case|try|record|asm)\b', t):
                    if w == 'end': depth -= 1
                    else: depth += 1; started = True
                if started and depth == 0: break
                j += 1
            n = j - i + 1
            if n > 30: print(f'{p}:{i+1}: {m.group(2)} {n}')
            i = j
        i += 1
