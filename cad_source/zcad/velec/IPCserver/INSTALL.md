# Установка и настройка IPC-сервера ZCAD

## Сборка

### 1. Требования
- Free Pascal Compiler 3.2+
- Lazarus 2.0+
- ZCAD с поддержкой модулей velec

### 2. Добавление в проект

#### Шаг 1: Обновление путей поиска (zcad.lpi)

В файл `zcad.lpi` добавьте путь к модулю IPCserver:

```xml
<SearchPaths>
  ...
  <OtherUnitFiles Value="...;zcad/velec/IPCserver;..."/>
  ...
</SearchPaths>
```

Или откройте проект в Lazarus:
1. Project → Project Options
2. Compiler Options → Paths
3. Добавьте `zcad/velec/IPCserver` в Other unit files

#### Шаг 2: Добавление модулей в zcad.pas

В секцию `uses` файла `cad_source/zcad.pas` добавьте:

```pascal
uses
  ...
  //**IPC Server for remote control**//
  uzvipcserver,
  uzvipcintegration,
  //**//
  ...
```

Добавьте эти модули в секцию `{$IFDEF ELECTROTECH}` после других модулей velec.

### 3. Компиляция

```bash
cd cad_source
lazbuild zcad.lpi
```

Или через IDE Lazarus: Run → Build

## Использование

### Запуск IPC-сервера

В командной строке ZCAD выполните:

```
IPCStart                    # Запуск с параметрами по умолчанию (127.0.0.1:7777)
IPCStart 127.0.0.1 7777     # Указание хоста и порта
IPCStart 127.0.0.1 7777 secret  # С токеном авторизации
```

### Проверка статуса

```
IPCStatus
```

### Остановка сервера

```
IPCStop
```

## Тестирование

### Python-клиент

```bash
cd cad_source/zcad/velec/IPCserver

# Проверка доступности
python uzvipcclient.py ping

# Создание линии
python uzvipcclient.py line 0 0 100 100

# Создание окружности
python uzvipcclient.py circle 50 50 25

# Добавление текста
python uzvipcclient.py text 10 10 "Hello ZCAD" 5

# Сохранение
python uzvipcclient.py save
```

### Запуск тестов

```bash
python test_ipc.py --demo    # Демонстрация
python test_ipc.py -v        # Полные тесты
```

### Flask REST API

```bash
pip install flask flask-restful
python flask_example.py
```

API будет доступен по адресу: http://localhost:5000/

## Интеграция с внешними сервисами

### Пример на Python

```python
from uzvipcclient import ZCADIPCClient

client = ZCADIPCClient(host='127.0.0.1', port=7777)

# Проверка доступности
result = client.ping()
print(result)  # {'id': 'cmd-0001', 'status': 'ok', 'result': 'pong'}

# Создание линии
result = client.line(0, 0, 100, 100)
print(result)

# Сохранение
result = client.save('/path/to/file.dxf')
print(result)
```

### Прямое TCP-соединение

```python
import socket
import json

sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
sock.connect(('127.0.0.1', 7777))

request = {
    'id': 'cmd-001',
    'cmd': 'LINE',
    'args': [0, 0, 100, 100]
}

sock.sendall(json.dumps(request).encode())
response = sock.recv(4096)
print(json.loads(response.decode()))

sock.close()
```

## Безопасность

### Настройка токена авторизации

1. При запуске сервера укажите токен:
   ```
   IPCStart 127.0.0.1 7777 my_secret_token
   ```

2. В запросах укажите токен:
   ```json
   {
     "id": "cmd-001",
     "token": "my_secret_token",
     "cmd": "LINE",
     "args": [0, 0, 100, 100]
   }
   ```

### Ограничение доступа

По умолчанию сервер слушает только `127.0.0.1` (localhost). 
**Не изменяйте это на `0.0.0.0` без настройки файрвола!**

## Отладка

Включите режим отладки в коде:
```pascal
IPCServerManager.SetDebugMode(True);
```

Или через переменную в ZCAD (если добавлена):
```
SetVar IPCDebugMode 1
```

## Устранение неполадок

### Сервер не запускается
- Проверьте, что порт 7777 не занят другим приложением
- Проверьте права доступа к сети

### Команды не выполняются
- Убедитесь, что чертеж загружен
- Проверьте корректность аргументов

### Ошибки соединения
- Проверьте, что ZCAD запущен и IPC-сервер активен (`IPCStatus`)
- Проверьте настройки фаервола
- Убедитесь, что используется правильный порт

## Поддержка

Для добавления новых команд см. раздел "Расширение функциональности" в README.md
