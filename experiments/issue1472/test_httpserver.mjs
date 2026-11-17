// Регрессия #1472: заголовки на проводе и штатный Node.js fetch без обхода проверок.
// Автор: OpenAI Codex, 2026-11-17. Зависимости: Node.js >= 18, FPC + FCL-Web.
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import net from 'node:net';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawn, spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { setTimeout as delay } from 'node:timers/promises';

const directory = path.dirname(fileURLToPath(import.meta.url));
const sourcePath = process.argv[2] ?? path.join(directory,
  '../../cad_source/zcad/velec/IPCserver/uzvhttpserver.pas');
const source = readFileSync(sourcePath, 'utf8');
const output = mkdtempSync(path.join(tmpdir(), 'zcad-http1472-'));
const cors = {
  'Access-Control-Allow-Origin': '*',
  'Access-Control-Allow-Methods': 'GET, POST, OPTIONS',
  'Access-Control-Allow-Headers': 'Content-Type',
};

// Получает метод целиком, включая вложенные begin/end, из текущего исходника.
function extractMethod(name) {
  const method = source.match(new RegExp(
    `^procedure TZCADHTTPServerManager\\.${name}\\([\\s\\S]*?^end;`, 'm'));
  assert.ok(method, `Missing production method ${name}`);
  return method[0];
}

// Выбирает свободный локальный порт без фиксированного номера для CI.
async function availablePort() {
  const listener = net.createServer();
  listener.listen(0, '127.0.0.1');
  await once(listener, 'listening');
  const port = listener.address().port;
  await new Promise(resolve => listener.close(resolve));
  return port;
}

// Получает настоящий HTTP-ответ без нормализации имён и значений заголовков.
async function rawResponse(port, method, route, body = '') {
  const socket = net.createConnection(port, '127.0.0.1');
  socket.setTimeout(5000, () => socket.destroy(new Error('HTTP response timeout')));
  const chunks = [];
  socket.on('data', chunk => chunks.push(chunk));
  socket.on('connect', () => socket.end(
    `${method} ${route} HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n` +
    `Content-Type: application/json\r\nContent-Length: ${Buffer.byteLength(body)}\r\n\r\n` +
    body));
  await once(socket, 'end');
  return Buffer.concat(chunks).toString('utf8');
}

// Проверяет ровно один экземпляр каждого CORS-заголовка и допустимые HTTP-имена.
function checkWireHeaders(response) {
  const lines = response.split('\r\n\r\n')[0].split('\r\n').slice(1);
  for (const line of lines) {
    assert.match(line, /^[!#$%&'*+.^_`|~0-9A-Za-z-]+: [^\r\n]*$/);
  }
  for (const [name, value] of Object.entries(cors)) {
    const matches = lines.filter(line => line.toLowerCase().startsWith(`${name.toLowerCase()}:`));
    assert.deepEqual(matches, [`${name}: ${value}`]);
  }
}

// Ждёт готовности сокета; ошибка HTTP-парсера не скрывается повторным запросом.
async function waitForServer(port, server) {
  for (let attempt = 0; attempt < 100; attempt++) {
    assert.equal(server.exitCode, null, 'FPC HTTP server exited early');
    try {
      const socket = net.createConnection(port, '127.0.0.1');
      await once(socket, 'connect');
      socket.destroy();
      return;
    } catch (error) {
      if (error.code !== 'ECONNREFUSED') throw error;
      await delay(20);
    }
  }
  throw new Error('FPC HTTP server did not start');
}

let server;
let serverLog = '';
try {
  writeFileSync(path.join(output, 'cors.inc'), extractMethod('SetCORSHeaders'));
  writeFileSync(path.join(output, 'request.inc'), extractMethod('HandleRequest'));
  const compile = spawnSync('fpc', [`-Fi${output}`, `-FU${output}`, `-FE${output}`,
    path.join(directory, 'httpserver_fixture.lpr')], { encoding: 'utf8' });
  assert.equal(compile.status, 0, compile.error?.message ?? compile.stdout + compile.stderr);
  const port = await availablePort();
  server = spawn(path.join(output, 'httpserver_fixture'), [String(port)]);
  server.stdout.on('data', chunk => { serverLog += chunk; });
  server.stderr.on('data', chunk => { serverLog += chunk; });
  await waitForServer(port, server);

  const cases = [
    ['GET', '/ipc', '', 200, '{"status":"ok","message":"ZCAD HTTP server is available"}'],
    ['POST', '/ipc', '{"command":"test","args":[1,"text"]}', 200,
      '{"command":"test","args":[1,"text"]}'],
    ['OPTIONS', '/ipc', '', 204, ''],
    ['OPTIONS', '/missing', '', 204, ''],
    ['POST', '/ipc', '', 400, '{"status":"error","error":"Empty request body"}'],
    ['POST', '/ipc', '  ', 400, '{"status":"error","error":"Empty request body"}'],
    ['PUT', '/ipc', '', 405, '{"status":"error","error":"Method not allowed"}'],
    ['GET', '/missing', '', 404, '{"status":"error","error":"Endpoint not found"}'],
    ['POST', '/ipc', '{"raise":true}', 500,
      '{"status":"error","error":"Internal server error"}'],
    ['GET', '/widgets/example', '', 200, '{"handler":"widgets"}'],
    ['GET', '/grist/poll', '', 200, '{"handler":"grist"}'],
  ];
  for (const [method, route, body, status, expected] of cases) {
    const raw = await rawResponse(port, method, route, body);
    assert.match(raw, new RegExp(`^HTTP/1\\.1 ${status} `));
    const response = await fetch(`http://127.0.0.1:${port}${route}`, {
      method,
      headers: { 'Content-Type': 'application/json', Origin: 'http://localhost:3000' },
      ...(method === 'GET' ? {} : { body }),
      signal: AbortSignal.timeout(5000),
    }).catch(error => {
      console.error(raw.split('\r\n\r\n')[0]);
      throw error;
    });
    checkWireHeaders(raw);
    assert.equal(response.status, status);
    for (const [name, value] of Object.entries(cors)) {
      assert.equal(response.headers.get(name), value);
    }
    assert.equal((await response.text()).trimEnd(), expected);
    if (route === '/ipc' && status !== 204) {
      assert.equal(response.headers.get('Content-Type'), 'application/json; charset=utf-8');
    }
    console.log(`PASS ${method} ${route} -> ${status}: wire headers and Node.js fetch`);
  }
} finally {
  if (server && server.exitCode === null && server.signalCode === null) {
    const exited = once(server, 'exit');
    server.kill();
    await exited;
  }
  if (server?.exitCode) console.error(serverLog);
  rmSync(output, { recursive: true, force: true });
}
