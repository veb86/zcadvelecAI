#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Тесты для IPC-сервера ZCAD

Запуск:
    python test_ipc.py

Требования:
    - Запущенный ZCAD с активным IPC-сервером (команда IPCStart)
"""

import unittest
import sys
import time
from uzvipcclient import ZCADIPCClient


class TestZCADIPC(unittest.TestCase):
    """Тесты IPC-клиента ZCAD."""
    
    @classmethod
    def setUpClass(cls):
        """Настройка перед всеми тестами."""
        cls.client = ZCADIPCClient(host='127.0.0.1', port=7777)
        # Проверяем доступность сервера
        result = cls.client.ping()
        if result.get('status') != 'ok':
            raise unittest.SkipTest("ZCAD IPC server not available. Run 'IPCStart' in ZCAD first.")
    
    def test_01_ping(self):
        """Тест PING команды."""
        result = self.client.ping()
        self.assertEqual(result['status'], 'ok')
        self.assertEqual(result['result'], 'pong')
        print(f"✓ PING: {result}")
    
    def test_02_line(self):
        """Тест создания линии."""
        result = self.client.line(0, 0, 100, 100)
        self.assertEqual(result['status'], 'ok')
        self.assertIn('Line created', result['result'])
        print(f"✓ LINE: {result}")
    
    def test_03_circle(self):
        """Тест создания окружности."""
        result = self.client.circle(50, 50, 25)
        self.assertEqual(result['status'], 'ok')
        self.assertIn('Circle created', result['result'])
        print(f"✓ CIRCLE: {result}")
    
    def test_04_text(self):
        """Тест создания текста."""
        result = self.client.text(10, 10, "Test", 5)
        self.assertEqual(result['status'], 'ok')
        self.assertIn('Text created', result['result'])
        print(f"✓ TEXT: {result}")
    
    def test_05_line_invalid_args(self):
        """Тест LINE с недостаточным количеством аргументов."""
        # Создаем клиент с прямым доступом к _send_command
        result = self.client._send_command('LINE', [0, 0, 100])  # Не хватает y2
        self.assertEqual(result['status'], 'error')
        print(f"✓ LINE (invalid args): {result}")
    
    def test_06_unknown_command(self):
        """Тест неизвестной команды."""
        result = self.client._send_command('UNKNOWN_CMD', [])
        self.assertEqual(result['status'], 'error')
        print(f"✓ UNKNOWN_CMD: {result}")
    
    def test_07_multiple_commands(self):
        """Тест нескольких команд подряд."""
        for i in range(3):
            result = self.client.line(i*10, 0, i*10+50, 50)
            self.assertEqual(result['status'], 'ok')
            time.sleep(0.1)  # Небольшая задержка между командами
        print("✓ Multiple commands executed")


class TestZCADIPCWithAuth(unittest.TestCase):
    """Тесты с авторизацией."""
    
    def test_auth_required(self):
        """Тест с токеном авторизации."""
        # Этот тест требует настройки сервера с токеном
        client = ZCADIPCClient(host='127.0.0.1', port=7777, token='wrong_token')
        result = client.ping()
        # Если сервер настроен с токеном - должна быть ошибка
        # Если без токена - должно работать
        print(f"Auth test (wrong token): {result}")


def run_demo():
    """Демонстрация работы IPC-клиента."""
    print("=" * 50)
    print("ZCAD IPC Client Demo")
    print("=" * 50)
    
    client = ZCADIPCClient(host='127.0.0.1', port=7777)
    
    # Проверка доступности
    print("\n1. Checking ZCAD availability...")
    result = client.ping()
    print(f"   Result: {result}")
    
    if result.get('status') != 'ok':
        print("   ERROR: ZCAD IPC server not available!")
        print("   Run 'IPCStart' command in ZCAD first.")
        return
    
    # Создание геометрии
    print("\n2. Creating geometry...")
    
    print("   - Line (0,0) -> (100,100)")
    result = client.line(0, 0, 100, 100)
    print(f"     {result}")
    
    print("   - Circle at (50,50), radius=25")
    result = client.circle(50, 50, 25)
    print(f"     {result}")
    
    print("   - Text 'Hello ZCAD' at (10,10)")
    result = client.text(10, 10, "Hello ZCAD", 5)
    print(f"     {result}")
    
    # Сохранение
    print("\n3. Saving drawing...")
    result = client.save()
    print(f"   {result}")
    
    print("\n" + "=" * 50)
    print("Demo completed!")
    print("=" * 50)


if __name__ == '__main__':
    import argparse
    
    parser = argparse.ArgumentParser(description='Test ZCAD IPC client')
    parser.add_argument('--demo', action='store_true', help='Run demo instead of tests')
    parser.add_argument('-v', '--verbose', action='store_true', help='Verbose output')
    
    args = parser.parse_args()
    
    if args.demo:
        run_demo()
    else:
        # Запуск тестов
        verbosity = 2 if args.verbose else 1
        unittest.main(verbosity=verbosity, exit=False)
