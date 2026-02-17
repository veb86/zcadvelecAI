#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Пример Flask REST API для управления ZCAD через IPC.

Установка зависимостей:
    pip install flask flask-restful

Запуск:
    python flask_example.py

API Endpoints:
    GET  /api/status          - Проверка статуса
    POST /api/line            - Создание линии
    POST /api/circle          - Создание окружности
    POST /api/text            - Создание текста
    POST /api/save            - Сохранение файла
    POST /api/export          - Экспорт файла
"""

from flask import Flask, request, jsonify
from flask_restful import Api, Resource
from uzvipcclient import ZCADIPCClient
import os

app = Flask(__name__)
api = Api(app)

# Конфигурация
ZCAD_HOST = os.getenv('ZCAD_IPC_HOST', '127.0.0.1')
ZCAD_PORT = int(os.getenv('ZCAD_IPC_PORT', '7777'))
ZCAD_TOKEN = os.getenv('ZCAD_IPC_TOKEN', '')

# Глобальный клиент
zcad_client = ZCADIPCClient(host=ZCAD_HOST, port=ZCAD_PORT, token=ZCAD_TOKEN)


class StatusResource(Resource):
    """Проверка статуса ZCAD."""
    
    def get(self):
        """GET /api/status"""
        result = zcad_client.ping()
        return jsonify(result)


class LineResource(Resource):
    """Создание линии."""
    
    def post(self):
        """POST /api/line
        
        Body:
            {
                "x1": 0,
                "y1": 0,
                "x2": 100,
                "y2": 100
            }
        """
        data = request.get_json()
        
        # Валидация
        required = ['x1', 'y1', 'x2', 'y2']
        for field in required:
            if field not in data:
                return jsonify({
                    'status': 'error',
                    'error': f'Missing required field: {field}'
                }), 400
        
        result = zcad_client.line(
            float(data['x1']),
            float(data['y1']),
            float(data['x2']),
            float(data['y2'])
        )
        return jsonify(result)


class CircleResource(Resource):
    """Создание окружности."""
    
    def post(self):
        """POST /api/circle
        
        Body:
            {
                "x": 50,
                "y": 50,
                "radius": 25
            }
        """
        data = request.get_json()
        
        required = ['x', 'y', 'radius']
        for field in required:
            if field not in data:
                return jsonify({
                    'status': 'error',
                    'error': f'Missing required field: {field}'
                }), 400
        
        result = zcad_client.circle(
            float(data['x']),
            float(data['y']),
            float(data['radius'])
        )
        return jsonify(result)


class TextResource(Resource):
    """Создание текста."""
    
    def post(self):
        """POST /api/text
        
        Body:
            {
                "x": 10,
                "y": 10,
                "text": "Hello",
                "height": 5
            }
        """
        data = request.get_json()
        
        required = ['x', 'y', 'text']
        for field in required:
            if field not in data:
                return jsonify({
                    'status': 'error',
                    'error': f'Missing required field: {field}'
                }), 400
        
        height = data.get('height', 2.5)
        
        result = zcad_client.text(
            float(data['x']),
            float(data['y']),
            str(data['text']),
            float(height)
        )
        return jsonify(result)


class SaveResource(Resource):
    """Сохранение чертежа."""
    
    def post(self):
        """POST /api/save
        
        Body (optional):
            {
                "filename": "/path/to/file.dxf"
            }
        """
        data = request.get_json() or {}
        filename = data.get('filename')
        
        result = zcad_client.save(filename)
        return jsonify(result)


class ExportResource(Resource):
    """Экспорт чертежа."""
    
    def post(self):
        """POST /api/export
        
        Body:
            {
                "filename": "/path/to/export.dxf"
            }
        """
        data = request.get_json()
        
        if 'filename' not in data:
            return jsonify({
                'status': 'error',
                'error': 'Missing required field: filename'
            }), 400
        
        result = zcad_client.export(data['filename'])
        return jsonify(result)


class BatchResource(Resource):
    """Пакетное выполнение команд."""
    
    def post(self):
        """POST /api/batch
        
        Body:
            {
                "commands": [
                    {"cmd": "line", "args": [0, 0, 100, 100]},
                    {"cmd": "circle", "args": [50, 50, 25]},
                    {"cmd": "text", "args": [10, 10, "Hello", 5]}
                ]
            }
        """
        data = request.get_json()
        
        if 'commands' not in data:
            return jsonify({
                'status': 'error',
                'error': 'Missing required field: commands'
            }), 400
        
        results = []
        for cmd in data['commands']:
            cmd_type = cmd.get('cmd', '').lower()
            args = cmd.get('args', [])
            
            if cmd_type == 'line' and len(args) >= 4:
                result = zcad_client.line(*args[:4])
            elif cmd_type == 'circle' and len(args) >= 3:
                result = zcad_client.circle(*args[:3])
            elif cmd_type == 'text' and len(args) >= 3:
                result = zcad_client.text(*args[:4] if len(args) >= 4 else args[:3])
            else:
                result = {'status': 'error', 'error': f'Unknown or invalid command: {cmd_type}'}
            
            results.append(result)
        
        return jsonify({
            'status': 'ok',
            'results': results
        })


# Регистрация ресурсов
api.add_resource(StatusResource, '/api/status')
api.add_resource(LineResource, '/api/line')
api.add_resource(CircleResource, '/api/circle')
api.add_resource(TextResource, '/api/text')
api.add_resource(SaveResource, '/api/save')
api.add_resource(ExportResource, '/api/export')
api.add_resource(BatchResource, '/api/batch')


@app.route('/')
def index():
    """Главная страница с документацией API."""
    return '''
    <!DOCTYPE html>
    <html>
    <head>
        <title>ZCAD IPC API</title>
        <style>
            body { font-family: Arial, sans-serif; margin: 40px; }
            h1 { color: #333; }
            h2 { color: #666; margin-top: 30px; }
            code { background: #f4f4f4; padding: 2px 6px; border-radius: 3px; }
            pre { background: #f4f4f4; padding: 15px; border-radius: 5px; overflow-x: auto; }
            .endpoint { margin: 20px 0; padding: 15px; border: 1px solid #ddd; border-radius: 5px; }
            .method { font-weight: bold; color: #007bff; }
        </style>
    </head>
    <body>
        <h1>ZCAD IPC REST API</h1>
        <p>REST API для удаленного управления ZCAD через IPC.</p>
        
        <h2>Endpoints</h2>
        
        <div class="endpoint">
            <span class="method">GET</span> <code>/api/status</code>
            <p>Проверка доступности ZCAD.</p>
        </div>
        
        <div class="endpoint">
            <span class="method">POST</span> <code>/api/line</code>
            <p>Создание линии.</p>
            <pre>{
    "x1": 0,
    "y1": 0,
    "x2": 100,
    "y2": 100
}</pre>
        </div>
        
        <div class="endpoint">
            <span class="method">POST</span> <code>/api/circle</code>
            <p>Создание окружности.</p>
            <pre>{
    "x": 50,
    "y": 50,
    "radius": 25
}</pre>
        </div>
        
        <div class="endpoint">
            <span class="method">POST</span> <code>/api/text</code>
            <p>Создание текста.</p>
            <pre>{
    "x": 10,
    "y": 10,
    "text": "Hello ZCAD",
    "height": 5
}</pre>
        </div>
        
        <div class="endpoint">
            <span class="method">POST</span> <code>/api/save</code>
            <p>Сохранение чертежа.</p>
            <pre>{
    "filename": "/path/to/file.dxf"
}</pre>
        </div>
        
        <div class="endpoint">
            <span class="method">POST</span> <code>/api/batch</code>
            <p>Пакетное выполнение команд.</p>
            <pre>{
    "commands": [
        {"cmd": "line", "args": [0, 0, 100, 100]},
        {"cmd": "circle", "args": [50, 50, 25]},
        {"cmd": "text", "args": [10, 10, "Hello", 5]}
    ]
}</pre>
        </div>
        
        <h2>Примеры использования</h2>
        
        <h3>cURL</h3>
        <pre>curl -X POST http://localhost:5000/api/line \\
    -H "Content-Type: application/json" \\
    -d '{"x1":0,"y1":0,"x2":100,"y2":100}'</pre>
        
        <h3>Python</h3>
        <pre>import requests

# Создание линии
response = requests.post('http://localhost:5000/api/line', json={
    'x1': 0, 'y1': 0, 'x2': 100, 'y2': 100
})
print(response.json())</pre>
        
        <h2>Конфигурация</h2>
        <p>Переменные окружения:</p>
        <ul>
            <li><code>ZCAD_IPC_HOST</code> - хост ZCAD (по умолчанию: 127.0.0.1)</li>
            <li><code>ZCAD_IPC_PORT</code> - порт ZCAD (по умолчанию: 7777)</li>
            <li><code>ZCAD_IPC_TOKEN</code> - токен авторизации (опционально)</li>
        </ul>
    </body>
    </html>
    '''


if __name__ == '__main__':
    print("=" * 50)
    print("ZCAD IPC Flask API Server")
    print("=" * 50)
    print(f"ZCAD connection: {ZCAD_HOST}:{ZCAD_PORT}")
    print(f"API documentation: http://localhost:5000/")
    print("=" * 50)
    
    app.run(host='0.0.0.0', port=5000, debug=True)
