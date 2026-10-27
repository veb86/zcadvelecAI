#!/usr/bin/env python3
"""Воспроизводимый OpenGL-рендер команд настоящего PolyFaceMesh.DrawGeometry.

Автор: OpenAI Codex, 2026-10-27. Зависимости: Python stdlib, libEGL.so.1,
libGL.so.1, Mesa. Используются треугольники, нормали и рёбра как в OpenGL drawer.
Запуск: render_capture.py capture.txt output.png
"""

from contextlib import contextmanager
import ctypes as ct
from pathlib import Path
import struct
import sys
import zlib

WIDTH, HEIGHT = 640, 480
EGL_SURFACELESS_MESA, EGL_OPENGL_API = 0x31DD, 0x30A2
EGL_NONE, EGL_WIDTH, EGL_HEIGHT = 0x3038, 0x3057, 0x3056
EGL_SURFACE_TYPE, EGL_PBUFFER_BIT = 0x3033, 1
EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT = 0x3040, 8
EGL_RED_SIZE, EGL_GREEN_SIZE, EGL_BLUE_SIZE = 0x3024, 0x3023, 0x3022
EGL_ALPHA_SIZE, EGL_DEPTH_SIZE = 0x3021, 0x3025
GL_COLOR_BUFFER_BIT, GL_DEPTH_BUFFER_BIT = 0x4000, 0x0100
GL_DEPTH_TEST, GL_POLYGON_OFFSET_FILL = 0x0B71, 0x8037
GL_PROJECTION, GL_MODELVIEW = 0x1701, 0x1700
GL_TRIANGLES, GL_LINES = 0x0004, 0x0001
GL_RGB, GL_UNSIGNED_BYTE = 0x1907, 0x1401
POINTER, INTEGER, UNSIGNED = ct.c_void_p, ct.c_int, ct.c_uint


def bind(library, name, restype, *argtypes):
    """Задаёт точные типы ABI, включая 64-битные указатели EGL."""
    function = getattr(library, name)
    function.restype = restype
    function.argtypes = argtypes
    return function


def choose_config(egl, display):
    """Выбирает RGB-буфер с глубиной для корректного скрытия задних граней."""
    attributes = (INTEGER * 15)(
        EGL_SURFACE_TYPE, EGL_PBUFFER_BIT, EGL_RENDERABLE_TYPE, EGL_OPENGL_BIT,
        EGL_RED_SIZE, 8, EGL_GREEN_SIZE, 8, EGL_BLUE_SIZE, 8,
        EGL_ALPHA_SIZE, 8, EGL_DEPTH_SIZE, 24, EGL_NONE)
    config, count = POINTER(), INTEGER()
    choose = bind(egl, 'eglChooseConfig', UNSIGNED, POINTER, POINTER,
                  POINTER, INTEGER, POINTER)
    assert choose(display, attributes, ct.byref(config), 1, ct.byref(count))
    assert count.value == 1, 'no EGL configuration'
    return config


@contextmanager
def gl_context():
    """Освобождает контекст и поверхность даже при ошибке регрессии."""
    egl = ct.CDLL('libEGL.so.1')
    get_display = bind(egl, 'eglGetPlatformDisplay', POINTER, UNSIGNED, POINTER, POINTER)
    display = get_display(EGL_SURFACELESS_MESA, None, None)
    initialize = bind(egl, 'eglInitialize', UNSIGNED, POINTER, POINTER, POINTER)
    assert initialize(display, None, None), 'eglInitialize failed'
    surface = context = None
    make_current = bind(egl, 'eglMakeCurrent', UNSIGNED, *([POINTER] * 4))
    try:
        assert bind(egl, 'eglBindAPI', UNSIGNED, UNSIGNED)(EGL_OPENGL_API)
        config = choose_config(egl, display)
        dimensions = (INTEGER * 5)(EGL_WIDTH, WIDTH, EGL_HEIGHT, HEIGHT, EGL_NONE)
        surface = bind(egl, 'eglCreatePbufferSurface', POINTER, *([POINTER] * 3))(
            display, config, dimensions)
        end = (INTEGER * 1)(EGL_NONE)
        context = bind(egl, 'eglCreateContext', POINTER, *([POINTER] * 4))(
            display, config, None, end)
        assert surface and context, 'EGL allocation failed'
        assert make_current(display, surface, surface, context), 'eglMakeCurrent failed'
        yield ct.CDLL('libGL.so.1')
    finally:
        make_current(display, None, None, None)
        if context:
            bind(egl, 'eglDestroyContext', UNSIGNED, POINTER, POINTER)(display, context)
        if surface:
            bind(egl, 'eglDestroySurface', UNSIGNED, POINTER, POINTER)(display, surface)
        bind(egl, 'eglTerminate', UNSIGNED, POINTER)(display)


def configure_view(gl):
    """Одинаковая ортографическая камера для каркаса и заполненного куба."""
    bind(gl, 'glViewport', None, *([INTEGER] * 4))(0, 0, WIDTH, HEIGHT)
    clear_color = bind(gl, 'glClearColor', None, *([ct.c_float] * 4))
    clear_color(0.055, 0.075, 0.105, 1)
    bind(gl, 'glClear', None, UNSIGNED)(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT)
    bind(gl, 'glEnable', None, UNSIGNED)(GL_DEPTH_TEST)
    matrix_mode = bind(gl, 'glMatrixMode', None, UNSIGNED)
    identity = bind(gl, 'glLoadIdentity', None)
    matrix_mode(GL_PROJECTION)
    identity()
    bind(gl, 'glOrtho', None, *([ct.c_double] * 6))(-2.5, 2.5, -1.875, 1.875, -10, 10)
    matrix_mode(GL_MODELVIEW)
    identity()
    rotate = bind(gl, 'glRotated', None, *([ct.c_double] * 4))
    rotate(25, 1, 0, 0)
    rotate(-35, 0, 1, 0)


def draw_triangles(gl, commands):
    """Сохраняет нормали и порядок вершин, переданные сущностью в drawer."""
    begin = bind(gl, 'glBegin', None, UNSIGNED)
    finish = bind(gl, 'glEnd', None)
    vertex = bind(gl, 'glVertex3dv', None, POINTER)
    normal = bind(gl, 'glNormal3dv', None, POINTER)
    color = bind(gl, 'glColor3f', None, *([ct.c_float] * 3))
    bind(gl, 'glEnable', None, UNSIGNED)(GL_POLYGON_OFFSET_FILL)
    bind(gl, 'glPolygonOffset', None, ct.c_float, ct.c_float)(1, 1)
    for command in commands:
        if command[0] != 'triangle':
            continue
        data = [float(value) for value in command[1:]]
        brightness = 0.5 + 0.3 * abs(data[2]) + 0.15 * abs(data[0])
        color(0.22 * brightness, 0.68 * brightness, 0.94 * brightness)
        begin(GL_TRIANGLES)
        normal((ct.c_double * 3)(*data[:3]))
        for offset in range(3, 12, 3):
            vertex((ct.c_double * 3)(*data[offset:offset + 3]))
        finish()
    bind(gl, 'glDisable', None, UNSIGNED)(GL_POLYGON_OFFSET_FILL)


def draw_lines(gl, commands):
    """Рисует исходные рёбра без диагоналей триангуляции."""
    bind(gl, 'glColor3f', None, *([ct.c_float] * 3))(0.68, 0.84, 0.96)
    vertex = bind(gl, 'glVertex3dv', None, POINTER)
    bind(gl, 'glBegin', None, UNSIGNED)(GL_LINES)
    for command in commands:
        if command[0] == 'line':
            data = [float(value) for value in command[1:]]
            vertex((ct.c_double * 3)(*data[:3]))
            vertex((ct.c_double * 3)(*data[3:]))
    bind(gl, 'glEnd', None)()


def png_chunk(kind, payload):
    """Формирует PNG-блок без дополнительных графических библиотек."""
    return (struct.pack('!I', len(payload)) + kind + payload
            + struct.pack('!I', zlib.crc32(kind + payload)))


def write_png(output_path, pixels):
    """Переворачивает нижнюю строку OpenGL в верхнюю строку PNG."""
    rows = b''.join(b'\0' + pixels[y * WIDTH * 3:(y + 1) * WIDTH * 3]
                    for y in reversed(range(HEIGHT)))
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_bytes(
        b'\x89PNG\r\n\x1a\n'
        + png_chunk(b'IHDR', struct.pack('!2I5B', WIDTH, HEIGHT, 8, 2, 0, 0, 0))
        + png_chunk(b'IDAT', zlib.compress(rows)) + png_chunk(b'IEND', b''))


def render(capture_path, output_path):
    """Выполняет захваченные команды в Mesa и возвращает RGB для сравнения."""
    commands = [line.split() for line in capture_path.read_text().splitlines()]
    with gl_context() as gl:
        configure_view(gl)
        draw_triangles(gl, commands)
        draw_lines(gl, commands)
        pixels = (ct.c_ubyte * (WIDTH * HEIGHT * 3))()
        bind(gl, 'glReadPixels', None, INTEGER, INTEGER, INTEGER, INTEGER,
             UNSIGNED, UNSIGNED, POINTER)(0, 0, WIDTH, HEIGHT, GL_RGB, GL_UNSIGNED_BYTE, pixels)
        assert bind(gl, 'glGetError', UNSIGNED)() == 0, 'OpenGL error'
        pixels = bytes(pixels)
    write_png(output_path, pixels)
    triangle_count = sum(command[0] == 'triangle' for command in commands)
    print(f'{output_path}: {triangle_count} triangles, '
          f'{sum(command[0] == "line" for command in commands)} edges')
    return pixels


if __name__ == '__main__':
    render(Path(sys.argv[1]), Path(sys.argv[2]))
