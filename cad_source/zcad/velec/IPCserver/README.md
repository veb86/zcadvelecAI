# IPC Server for ZCAD

Модуль межпроцессного взаимодействия (IPC) для удаленного управления ZCAD через TCP + JSON.

## Архитектура

```
Внешний сервис (Flask / REST API / Python script)
         |
        TCP (JSON)
         |
    IPC-сервер (uzvipcserver.pas)
         |
    Очередь команд
         |
    Обработчик в GUI-потоке (uzvipcintegration.pas)
         |
    Исполнение команд CAD
```

## Компоненты

### 1. uzvipcserver.pas
Основной модуль IPC-сервера:
- TCP-сервер в отдельном потоке
- Парсинг JSON-запросов
- Очередь команд
- Базовые команды (PING, SAVE, EXPORT, LINE, CIRCLE, TEXT)

### 2. uzvipcintegration.pas
Интеграция с главным циклом ZCAD:
- Обработка команд из очереди в GUI-потоке
- Таймер для проверки очереди (100 мс)
- Выполнение CAD-команд с корректным контекстом

### 3. uzvipcclient.py
Python-клиент для тестирования и взаимодействия с ZCAD.

## Протокол IPC

### Запрос
```json
{
  "id": "cmd-001",
  "token": "optional_secret",
  "cmd": "LINE",
  "args": [0, 0, 1000, 0]
}
```

### Успешный ответ
```json
{
  "id": "cmd-001",
  "status": "ok",
  "result": "line_created"
}
```

### Ошибка
```json
{
  "id": "cmd-001",
  "status": "error",
  "error": "Unknown command"
}
```

## Команды

### PING
Проверка доступности ZCAD.
```json
{"id": "1", "cmd": "PING"}
```

### SAVE
Сохранение чертежа.
```json
{"id": "2", "cmd": "SAVE", "args": ["/path/to/file.dxf"]}
```

### EXPORT
Экспорт чертежа.
```json
{"id": "3", "cmd": "EXPORT", "args": ["/path/to/export.dxf"]}
```

### LINE
Создание линии.
```json
{"id": "4", "cmd": "LINE", "args": [x1, y1, x2, y2]}
```

### CIRCLE
Создание окружности.
```json
{"id": "5", "cmd": "CIRCLE", "args": [x, y, radius]}
```

### TEXT
Создание текста.
```json
{"id": "6", "cmd": "TEXT", "args": [x, y, "text", height]}
```

## Команды ZCAD

После загрузки модуля доступны команды ZCAD:

### IPCStart
Запуск IPC-сервера.
```
IPCStart [host] [port] [token]
IPCStart                    # Запуск с параметрами по умолчанию
IPCStart 127.0.0.1 7777     # Указание хоста и порта
IPCStart 127.0.0.1 7777 secret  # С токеном авторизации
```

### IPCStop
Остановка IPC-сервера.
```
IPCStop
```

### IPCStatus
Проверка статуса сервера.
```
IPCStatus
```

## Использование Python-клиента

### Установка
Python 3.6+ не требует дополнительных зависимостей.

### Примеры

```bash
# Проверка доступности
python uzvipcclient.py ping

# Сохранение файла
python uzvipcclient.py save /path/to/file.dxf

# Создание линии
python uzvipcclient.py line 0 0 100 100

# Создание окружности
python uzvipcclient.py circle 50 50 25

# Добавление текста
python uzvipcclient.py text 10 10 "Hello ZCAD" 5

# С указанием хоста и порта
python uzvipcclient.py --host 192.168.1.100 --port 8888 ping

# С токеном авторизации
python uzvipcclient.py --token secret save /path/to/file.dxf
```

### Использование как библиотека

```python
from uzvipcclient import ZCADIPCClient

client = ZCADIPCClient(host='127.0.0.1', port=7777, token='secret')

# Проверка доступности
result = client.ping()
print(result)  # {'id': 'cmd-0001', 'status': 'ok', 'result': 'pong'}

# Создание линии
result = client.line(0, 0, 100, 100)
print(result)

# Создание окружности
result = client.circle(50, 50, 25)
print(result)

# Сохранение
result = client.save('/path/to/file.dxf')
print(result)
```

## Интеграция с Flask

```python
from flask import Flask, request, jsonify
from uzvipcclient import ZCADIPCClient

app = Flask(__name__)
zcad = ZCADIPCClient(host='127.0.0.1', port=7777)

@app.route('/api/ping', methods=['GET'])
def ping():
    result = zcad.ping()
    return jsonify(result)

@app.route('/api/line', methods=['POST'])
def draw_line():
    data = request.json
    result = zcad.line(
        data['x1'], data['y1'],
        data['x2'], data['y2']
    )
    return jsonify(result)

@app.route('/api/save', methods=['POST'])
def save():
    data = request.json
    result = zcad.save(data.get('filename'))
    return jsonify(result)

if __name__ == '__main__':
    app.run(port=5000)
```

## Безопасность

1. **Только localhost**: Сервер по умолчанию слушает только 127.0.0.1
2. **Токен авторизации**: Опциональный токен для проверки запросов
3. **Белый список команд**: Возможность ограничить доступные команды
4. **Логирование**: Все команды логируются

## Конфигурация

### Переменные окружения
```bash
export ZCAD_IPC_HOST=127.0.0.1
export ZCAD_IPC_PORT=7777
export ZCAD_IPC_TOKEN=secret
export ZCAD_IPC_DEBUG=1
```

### Настройки по умолчанию
- Хост: 127.0.0.1
- Порт: 7777
- Таймаут: 30 секунд
- Макс. размер запроса: 64 KB

## Расширение функциональности

### Добавление новой команды

1. Добавьте тип команды в `TIPCCommandType`:
```pascal
TIPCCommandType = (..., ictMyNewCommand);
```

2. Добавьте распознавание команды в `GetCommandType`:
```pascal
else if SameText(ACmdName, 'MYNEWCMD') then
  Result := ictMyNewCommand
```

3. Добавьте обработчик в `ProcessQueue`:
```pascal
ictMyNewCommand: ExecuteMyNewCommand;
```

4. Реализуйте обработчик:
```pascal
procedure ExecuteMyNewCommand;
begin
  // Ваша логика
  CmdResult.Status := 'ok';
  CmdResult.Result := 'Success';
end;
```

## Отладка

Включите режим отладки:
```pascal
IPCServerManager.SetDebugMode(True);
```

Или через команду ZCAD:
```
SetVar IPCDebugMode 1
```

## Требования

- Free Pascal Compiler 3.2+
- Lazarus 2.0+
- ZCAD с поддержкой модулей
- Python 3.6+ (для клиента)

## Лицензия

См. основную лицензию ZCAD (GPLv2).
