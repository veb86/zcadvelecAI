// Регрессия #1478: POST /api/export_selection через настоящий HTTP-сервер FPC.
// Реальные HandleRequest/SetCORSHeaders, очередь uzvhttpipc и модули маршрута;
// заглушены только журнал и запись чертежа (см. route_fixture.lpr).
// Зависимости: Node.js >= 18, FPC 3.2.2 + FCL (fcl-web, fcl-json).
import assert from 'node:assert/strict';
import { once } from 'node:events';
import { existsSync, mkdtempSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import net from 'node:net';
import { tmpdir } from 'node:os';
import path from 'node:path';
import { spawn, spawnSync } from 'node:child_process';
import { fileURLToPath } from 'node:url';
import { setTimeout as delay } from 'node:timers/promises';

const directory = path.dirname(fileURLToPath(import.meta.url));
const repo = path.join(directory, '../..');
const ipcDir = path.join(repo, 'cad_source/zcad/velec/IPCserver');
const source = readFileSync(path.join(ipcDir, 'uzvhttpserver.pas'), 'utf8');
const output = mkdtempSync(path.join(tmpdir(), 'zcad-http1478-'));
const exportDir = path.join(output, 'exports');
const ROUTE = '/api/export_selection';
const TIMEOUT_SEC = 1;

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

// Ждёт готовности сокета; ранний выход сервера считается ошибкой.
async function waitForServer(port, server) {
  for (let attempt = 0; attempt < 200; attempt++) {
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

// Отправляет запрос и возвращает статус, Content-Type и разобранный JSON.
async function call(port, method, route, body) {
  const response = await fetch(`http://127.0.0.1:${port}${route}`, {
    method,
    headers: { 'Content-Type': 'application/json' },
    ...(body === undefined ? {} : { body }),
    signal: AbortSignal.timeout(10000),
  });
  const text = await response.text();
  return {
    status: response.status,
    type: response.headers.get('Content-Type'),
    cors: response.headers.get('Access-Control-Allow-Origin'),
    text,
    json: JSON.parse(text),
  };
}

// Проверяет конверт ошибки из ТЗ: ok=false, command и error.code.
function assertError(result, code) {
  assert.equal(result.status, 200, result.text);
  assert.equal(result.json.ok, false, result.text);
  assert.equal(result.json.command, 'export_selection');
  assert.equal(result.json.error.code, code, result.text);
  assert.equal(typeof result.json.error.message, 'string');
}

// Проверяет обязательные поля успешного ответа из ТЗ.
function assertSuccess(result, expectedPath) {
  assert.equal(result.status, 200, result.text);
  assert.equal(result.type, 'application/json; charset=utf-8');
  assert.equal(result.cors, '*');
  const json = result.json;
  assert.equal(json.ok, true, result.text);
  assert.equal(json.command, 'export_selection');
  assert.equal(json.path, expectedPath);
  assert.equal(json.scope, 'selection');
  assert.equal(json.entity_count, 1);
  assert.deepEqual(json.bbox, [0, 0, 10, 5]);
  assert.match(result.text, /"bbox" ?: ?\[0\.0, 0\.0, 10\.0, 5\.0\]/);
  assert.equal(json.units, 'mm');
  assert.equal(typeof json.message, 'string');
  assert.ok(existsSync(expectedPath), `DXF not written: ${expectedPath}`);
  assert.match(readFileSync(expectedPath, 'utf8'), /ENTITIES[\s\S]*LINE[\s\S]*EOF/);
}

// Набор сценариев маршрута; порядок важен для FILE_EXISTS и TIMEOUT.
async function runCases(port) {
  const defaultPath = path.join(exportDir, 'selection.dxf');
  assertSuccess(await call(port, 'POST', ROUTE, '{}'), defaultPath);
  console.log('PASS POST {} -> default ZCAD_EXPORT_DIR/selection.dxf');

  assertSuccess(await call(port, 'POST', ROUTE, ''), defaultPath);
  console.log('PASS POST with empty body -> defaults, idempotent overwrite');

  const cyrillic = path.join(exportDir, 'Проект', 'выделение.dxf');
  assertSuccess(await call(port, 'POST', `${ROUTE}?source=mcp`,
    JSON.stringify({ out_path: cyrillic, scope: 'selection', overwrite: true,
      include_xdata: true })), cyrillic);
  console.log('PASS POST Cyrillic out_path with query string -> parent dir created');

  assertError(await call(port, 'POST', ROUTE,
    JSON.stringify({ out_path: cyrillic, overwrite: false })), 'FILE_EXISTS');
  console.log('PASS overwrite=false on existing file -> FILE_EXISTS');

  assertError(await call(port, 'POST', ROUTE,
    JSON.stringify({ out_path: path.join(output, 'outside.dxf') })), 'INVALID_PATH');
  assert.ok(!existsSync(path.join(output, 'outside.dxf')));
  console.log('PASS out_path outside allowlist -> INVALID_PATH, nothing written');

  assertError(await call(port, 'POST', ROUTE,
    JSON.stringify({ out_path: path.join(exportDir, '..', 'escape.dxf') })), 'INVALID_PATH');
  console.log('PASS path traversal -> INVALID_PATH');

  assertError(await call(port, 'POST', ROUTE, '{not json'), 'INVALID_REQUEST');
  assertError(await call(port, 'POST', ROUTE, '{"overwrite":"yes"}'), 'INVALID_REQUEST');
  console.log('PASS malformed body -> INVALID_REQUEST');

  assertError(await call(port, 'POST', ROUTE,
    JSON.stringify({ out_path: path.join(exportDir, 'plain-error.dxf') })), 'INTERNAL_ERROR');
  console.log('PASS non-JSON handler failure -> INTERNAL_ERROR envelope');

  const started = Date.now();
  const slow = await call(port, 'POST', ROUTE,
    JSON.stringify({ out_path: path.join(exportDir, 'slow.dxf') }));
  assertError(slow, 'TIMEOUT');
  assert.ok(Date.now() - started < 2400, 'TIMEOUT must not wait for the slow export');
  assert.match(slow.json.error.message, /within 1 s/);
  console.log('PASS slow export -> TIMEOUT JSON after ZCAD_EXPORT_TIMEOUT');

  // Главный поток ещё занят медленной выгрузкой: следующая команда ждёт
  // в очереди, получает TIMEOUT и отменяется, не начав запись файла.
  const queuedPath = path.join(exportDir, 'queued.dxf');
  assertError(await call(port, 'POST', ROUTE,
    JSON.stringify({ out_path: queuedPath })), 'TIMEOUT');
  const slowPath = path.join(exportDir, 'slow.dxf');
  for (let attempt = 0; attempt < 100 && !existsSync(slowPath); attempt++) await delay(50);
  assert.ok(existsSync(slowPath), 'slow export should still finish on the main thread');
  await delay(100);
  assert.ok(!existsSync(queuedPath), 'cancelled command must not run');
  console.log('PASS command queued behind a busy main thread -> TIMEOUT, cancelled');

  assertSuccess(await call(port, 'POST', ROUTE, '{}'), defaultPath);
  console.log('PASS server keeps working after TIMEOUT');

  const ipcPath = path.join(exportDir, 'ipc.dxf');
  const viaIpc = await call(port, 'POST', '/ipc', JSON.stringify(
    { id: 'export-1', cmd: 'EXPORT_SELECTION', args: [{ out_path: ipcPath }] }));
  assert.equal(viaIpc.json.status, 'ok', viaIpc.text);
  assert.equal(JSON.parse(viaIpc.json.result).path, ipcPath);
  assert.ok(existsSync(ipcPath));
  const ipcError = await call(port, 'POST', '/ipc', JSON.stringify(
    { id: 'export-2', cmd: 'EXPORT_SELECTION', args: [{ out_path: '/etc/x.dxf' }] }));
  assert.equal(ipcError.json.status, 'error');
  assert.equal(JSON.parse(ipcError.json.error).error.code, 'INVALID_PATH');
  console.log('PASS POST /ipc cmd=EXPORT_SELECTION -> same JSON inside the IPC envelope');

  const get = await call(port, 'GET', ROUTE);
  assert.equal(get.status, 405);
  assert.equal(get.json.ok, false);
  assert.equal(get.json.error.code, 'INVALID_REQUEST');
  console.log('PASS GET -> 405 JSON');

  const ipc = await call(port, 'GET', '/ipc');
  assert.equal(ipc.json.status, 'ok');
  const missing = await call(port, 'GET', '/api/export_selection_extra');
  assert.equal(missing.status, 404);
  console.log('PASS existing /ipc and 404 routes unchanged');
}

let server;
let serverLog = '';
try {
  writeFileSync(path.join(output, 'cors.inc'), extractMethod('SetCORSHeaders'));
  writeFileSync(path.join(output, 'request.inc'), extractMethod('HandleRequest'));
  const compile = spawnSync('fpc', ['-vewn', `-Fi${output}`,
    `-Fi${path.join(repo, 'cad_source/zengine')}`,
    `-Fu${path.join(directory, 'stubs')}`, `-Fu${ipcDir}`,
    `-Fu${path.join(repo, 'cad_source/components/zbaseutils/src')}`,
    `-FU${output}`, `-FE${output}`, path.join(directory, 'route_fixture.lpr')],
  { encoding: 'utf8' });
  assert.equal(compile.status, 0, compile.error?.message ?? compile.stdout + compile.stderr);
  const port = await availablePort();
  server = spawn(path.join(output, 'route_fixture'), [String(port)], {
    env: { ...process.env, ZCAD_EXPORT_DIR: exportDir,
      ZCAD_EXPORT_TIMEOUT: String(TIMEOUT_SEC), ZCAD_EXPORT_ROOT: '',
      ZCAD_MCP_WORKSPACE: '', ZCAD_EXPORT_FILENAME: '' },
  });
  server.stdout.on('data', chunk => { serverLog += chunk; });
  server.stderr.on('data', chunk => { serverLog += chunk; });
  await waitForServer(port, server);
  await runCases(port);
  assert.match(serverLog, /Command cancelled before start: .*EXPORT_SELECTION/);
  console.log('PASS all export_selection route checks');
} catch (error) {
  console.error(serverLog);
  throw error;
} finally {
  if (server && server.exitCode === null && server.signalCode === null) {
    const exited = once(server, 'exit');
    server.kill();
    await exited;
  }
  rmSync(output, { recursive: true, force: true });
}
