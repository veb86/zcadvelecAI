"""Проверка зон в DXF, выгруженном из ZCAD после e2e_check.mjs (#1480).

python -I verify_dxf.py <zones.dxf>
Без сторонних библиотек: читает пары «код группы / значение» секции ENTITIES.
"""
import sys

CLOSED_FLAG = 1
EXPECTED_ROOMS = {'101', '102', '103'}


def read_pairs(path):
    """Пары (код, значение) файла DXF."""
    with open(path, encoding='utf-8') as stream:
        lines = stream.read().splitlines()
    return [(lines[i].strip(), lines[i + 1]) for i in range(0, len(lines) - 1, 2)]


def entity_pairs(pairs):
    """Пары секции ENTITIES до ENDSEC."""
    start = pairs.index(('2', 'ENTITIES'))
    end = pairs.index(('0', 'ENDSEC'), start)
    return pairs[start + 1:end]


def polylines(pairs):
    """Полилинии: слой, цвет, флаги, переменные XDATA и вершины."""
    result, current = [], None
    for code, value in entity_pairs(pairs):
        if code == '0':
            if value == 'POLYLINE':
                current = {'vars': {}, 'ext': [], 'vertices': []}
                result.append(current)
            elif value == 'VERTEX' and current is not None:
                current['vertices'].append([])
            elif value != 'VERTEX':
                current = None if value != 'SEQEND' else current
            continue
        if current is None:
            continue
        add_pair(current, code, value)
    return result


def add_pair(poly, code, value):
    """Разбор одной пары полилинии или её вершины."""
    if poly['vertices']:
        if code in ('10', '20'):
            poly['vertices'][-1].append(float(value))
        return
    if code in ('8', '62', '70'):
        poly.setdefault(code, value)
    elif code == '1000' and value.startswith('#'):
        name, kind, data, _ = value.split('=', 1)[1].split('|', 3)
        poly['vars'][name] = (kind, data)
    elif code == '1000' and value.endswith('EXTENDER='):
        poly['ext'].append(value[:-1])


def check(polys):
    """Проверки зон; возвращает список ошибок."""
    errors = []
    rooms = [p for p in polys if p.get('8') == 'spaceRoom']
    floors = [p for p in polys if p.get('8') == 'spaceFloor']
    numbers = {p['vars'].get('Space_RoomPos', ('', ''))[1] for p in rooms}
    if numbers != EXPECTED_ROOMS:
        errors.append(f'room numbers {sorted(numbers)}')
    for poly in rooms + floors:
        if int(poly.get('70', '0')) & CLOSED_FLAG == 0:
            errors.append(f'{poly["8"]} polyline is not closed')
        if len(poly['vertices']) != 4:
            errors.append(f'{poly["8"]} has {len(poly["vertices"])} vertices')
        if 'INCLUDINGVOLUMEEXTENDER' not in poly['ext']:
            errors.append(f'{poly["8"]} has no INCLUDINGVOLUMEEXTENDER')
    if any(p.get('62') != '5' for p in rooms):
        errors.append('room color is not 5')
    errors += check_room_101(rooms)
    if len(floors) != 1 or floors[0].get('62') != '4':
        errors.append('expected one floor zone with color 4')
    elif floors[0]['vars'].get('space_Floor') != ('String', '1'):
        errors.append(f'floor vars {floors[0]["vars"]}')
    return errors


def check_room_101(rooms):
    """Переменные и вершины помещения 101."""
    room = [p for p in rooms if p['vars'].get('Space_RoomPos', ('', ''))[1] == '101']
    if not room:
        return ['room 101 not found']
    variables = room[0]['vars']
    errors = []
    if variables.get('Space_RoomName') != ('String', 'Кабинет'):
        errors.append(f'room 101 name {variables.get("Space_RoomName")}')
    if variables.get('Space_RoomCategory') != ('String', 'Д'):
        errors.append(f'room 101 category {variables.get("Space_RoomCategory")}')
    expected = [[0, 0], [6000, 0], [6000, 4000], [0, 4000]]
    if room[0]['vertices'] != expected:
        errors.append(f'room 101 vertices {room[0]["vertices"]}')
    return errors


def main():
    """Точка входа: печать сводки и код завершения."""
    polys = polylines(read_pairs(sys.argv[1]))
    for poly in polys:
        print(poly.get('8'), 'color', poly.get('62'), 'flags', poly.get('70'),
              'vertices', len(poly['vertices']), poly['ext'], poly['vars'])
    errors = check(polys)
    for error in errors:
        print('FAIL', error)
    print('PASS DXF zones' if not errors else 'FAIL DXF zones')
    return 1 if errors else 0


if __name__ == '__main__':
    sys.exit(main())
