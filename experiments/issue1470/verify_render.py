#!/usr/bin/env python3
"""Проверка заполнения куба при переключении реальной сетки.

Автор: OpenAI Codex, 2026-10-27. Зависимости: render_capture, Python stdlib.
"""

from pathlib import Path
import sys

from render_capture import HEIGHT, WIDTH, render

MIN_FILLED_PIXELS = 30000


def verify(directory):
    """Сравнивает внутренние пиксели и неизменность исходных рёбер."""
    wireframe = render(directory / 'wireframe.txt', directory / 'wireframe.png')
    solid = render(directory / 'solid.txt', directory / 'solid.png')
    center = ((HEIGHT // 2) * WIDTH + WIDTH // 2) * 3
    assert wireframe[center:center + 3] != solid[center:center + 3], 'interior was not filled'
    changed_pixels = sum(wireframe[i:i + 3] != solid[i:i + 3]
                         for i in range(0, len(wireframe), 3))
    assert changed_pixels > MIN_FILLED_PIXELS, f'incomplete cube coverage: {changed_pixels} pixels'
    before = (directory / 'wireframe.txt').read_text().splitlines()
    after = (directory / 'solid.txt').read_text().splitlines()
    assert not any(line.startswith('triangle ') for line in before)
    assert sum(line.startswith('triangle ') for line in after) == 12
    assert before == [line for line in after if line.startswith('line ')]
    print(f'Visual regression passed: {changed_pixels} interior pixels filled; edges preserved')


if __name__ == '__main__':
    verify(Path(sys.argv[1]))
