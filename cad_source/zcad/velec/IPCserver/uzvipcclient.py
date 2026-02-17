#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
IPC Client for ZCAD

Простой Python-клиент для удаленного управления ZCAD через TCP + JSON.

Примеры использования:
    # Проверка доступности
    python uzvipcclient.py ping

    # Сохранение файла
    python uzvipcclient.py save /path/to/file.dxf

    # Создание линии (одной)
    python uzvipcclient.py line 0 0 100 100

    # Создание 1000 линий с рандомными координатами
    python uzvipcclient.py random_lines 1000

    # Создание окружности
    python uzvipcclient.py circle 50 50 25

    # Создание текста
    python uzvipcclient.py text 10 10 "Hello ZCAD" 5
"""

import socket
import json
import sys
import argparse
import random
from typing import List, Optional


class ZCADIPCClient:
    """Клиент для взаимодействия с ZCAD через IPC."""
    
    def __init__(self, host: str = '127.0.0.1', port: int = 7777, token: str = ''):
        self.host = host
        self.port = port
        self.token = token
        self._counter = 0
    
    def _generate_id(self) -> str:
        """Генерация уникального ID команды."""
        self._counter += 1
        return f"cmd-{self._counter:04d}"
    
    def _send_command(self, cmd: str, args: List = None) -> dict:
        """Отправка команды на сервер и получение ответа."""
        if args is None:
            args = []
        
        request = {
            'id': self._generate_id(),
            'cmd': cmd.upper(),
            'args': args
        }
        
        if self.token:
            request['token'] = self.token
        
        request_json = json.dumps(request)
        
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
                sock.settimeout(30)
                sock.connect((self.host, self.port))
                sock.sendall(request_json.encode('utf-8'))
                
                response_data = b''
                while True:
                    chunk = sock.recv(4096)
                    if not chunk:
                        break
                    response_data += chunk
                
                response = json.loads(response_data.decode('utf-8'))
                return response
                
        except socket.timeout:
            return {'id': request['id'], 'status': 'error', 'error': 'Connection timeout'}
        except ConnectionRefusedError:
            return {'id': request['id'], 'status': 'error', 'error': 'Connection refused - is ZCAD running?'}
        except Exception as e:
            return {'id': request['id'], 'status': 'error', 'error': str(e)}
    
    def ping(self) -> dict:
        """Проверка доступности ZCAD."""
        return self._send_command('PING')
    
    def save(self, filename: Optional[str] = None) -> dict:
        """Сохранение чертежа."""
        args = [filename] if filename else []
        return self._send_command('SAVE', args)
    
    def export(self, filename: str) -> dict:
        """Экспорт чертежа."""
        return self._send_command('EXPORT', [filename])
    
    def line(self, x1: float, y1: float, x2: float, y2: float) -> dict:
        """Создание линии."""
        return self._send_command('LINE', [x1, y1, x2, y2])
    
    def circle(self, x: float, y: float, radius: float) -> dict:
        """Создание окружности."""
        return self._send_command('CIRCLE', [x, y, radius])
    
    def text(self, x: float, y: float, content: str, height: float = 2.5) -> dict:
        """Создание текста."""
        return self._send_command('TEXT', [x, y, content, height])

    def begin_batch(self) -> dict:
        """Начало пакетной вставки примитивов."""
        return self._send_command('BEGIN_BATCH')

    def end_batch(self) -> dict:
        """Завершение пакетной вставки и фиксация в чертеже."""
        return self._send_command('END_BATCH')

    def random_lines(self, count: int = 1000, min_coord: float = -100, max_coord: float = 100) -> List[dict]:
        """Создание множества линий с рандомными координатами в пакетном режиме."""
        results = []
        
        # Начинаем пакетный режим
        result = self.begin_batch()
        if result.get('status') != 'ok':
            return [{'status': 'error', 'error': 'Failed to start batch mode'}]
        results.append(result)
        
        # Отправляем все линии
        for i in range(count):
            x1 = random.uniform(min_coord, max_coord)
            y1 = random.uniform(min_coord, max_coord)
            x2 = random.uniform(min_coord, max_coord)
            y2 = random.uniform(min_coord, max_coord)
            result = self.line(x1, y1, x2, y2)
            results.append(result)
            
            # Вывод прогресса каждые 100 линий
            if (i + 1) % 100 == 0:
                print(f"Progress: {i + 1}/{count} lines queued", file=sys.stderr)
        
        # Завершаем пакетный режим
        result = self.end_batch()
        results.append(result)
        
        return results


def main():
    parser = argparse.ArgumentParser(
        description='IPC Client for ZCAD',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  %(prog)s ping                           # Check ZCAD availability
  %(prog)s save                           # Save current drawing
  %(prog)s save /path/to/file.dxf         # Save to specific file
  %(prog)s export /path/to/export.dxf     # Export drawing
  %(prog)s line 0 0 100 100               # Draw line
  %(prog)s circle 50 50 25                # Draw circle
  %(prog)s text 10 10 "Hello" 5           # Add text
        """
    )
    
    parser.add_argument('command', choices=[
        'ping', 'save', 'export', 'line', 'random_lines', 'circle', 'text'
    ], help='Command to execute')
    
    parser.add_argument('args', nargs='*', help='Command arguments')
    parser.add_argument('--host', default='127.0.0.1', help='Server host (default: 127.0.0.1)')
    parser.add_argument('--port', type=int, default=7777, help='Server port (default: 7777)')
    parser.add_argument('--token', default='', help='Auth token')
    
    args = parser.parse_args()
    
    client = ZCADIPCClient(host=args.host, port=args.port, token=args.token)
    
    # Выполнение команды
    if args.command == 'ping':
        result = client.ping()
    
    elif args.command == 'save':
        filename = args.args[0] if args.args else None
        result = client.save(filename)
    
    elif args.command == 'export':
        if not args.args:
            print("Error: export requires filename argument")
            sys.exit(1)
        result = client.export(args.args[0])
    
    elif args.command == 'line':
        if len(args.args) < 4:
            print("Error: line requires 4 arguments: x1 y1 x2 y2")
            sys.exit(1)
        try:
            coords = [float(x) for x in args.args[:4]]
            result = client.line(*coords)
        except ValueError:
            print("Error: coordinates must be numbers")
            sys.exit(1)

    elif args.command == 'random_lines':
        count = int(args.args[0]) if args.args else 1000
        print(f"Sending {count} random lines to ZCAD (coordinates from -100 to 100)...", file=sys.stderr)
        results = client.random_lines(count=count, min_coord=-100, max_coord=100)
        # Вывод статистики - последний результат это END_BATCH
        if results:
            last_result = results[-1]
            if last_result.get('status') == 'ok':
                print(f"\nCompleted: {last_result.get('result', 'Batch completed')}", file=sys.stderr)
                result = {'status': 'ok', 'message': last_result.get('result', 'Batch completed')}
            else:
                print(f"\nError: {last_result.get('error', 'Unknown error')}", file=sys.stderr)
                result = last_result
        else:
            result = {'status': 'error', 'error': 'No results'}

    elif args.command == 'circle':
        if len(args.args) < 3:
            print("Error: circle requires 3 arguments: x y radius")
            sys.exit(1)
        try:
            params = [float(x) for x in args.args[:3]]
            result = client.circle(*params)
        except ValueError:
            print("Error: parameters must be numbers")
            sys.exit(1)
    
    elif args.command == 'text':
        if len(args.args) < 3:
            print("Error: text requires at least 3 arguments: x y text [height]")
            sys.exit(1)
        try:
            x = float(args.args[0])
            y = float(args.args[1])
            text = args.args[2]
            height = float(args.args[3]) if len(args.args) > 3 else 2.5
            result = client.text(x, y, text, height)
        except ValueError:
            print("Error: x, y and height must be numbers")
            sys.exit(1)
    
    else:
        print(f"Unknown command: {args.command}")
        sys.exit(1)
    
    # Вывод результата
    print(json.dumps(result, indent=2, ensure_ascii=False))
    
    # Возврат кода ошибки
    if result.get('status') != 'ok':
        sys.exit(1)


if __name__ == '__main__':
    main()
