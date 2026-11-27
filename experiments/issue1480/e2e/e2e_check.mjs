// Сквозная проверка #1480 против настоящего ZCAD: SPACE_ADD_ROOM / SPACE_ADD_POLY.
// node e2e_check.mjs <port> <workdir> [draw|undo]
//   draw - чертит зоны и выгружает чертёж в <work>/exports/zones.dxf;
//   undo - после Ctrl+Z (run_e2e.sh) проверяет, что пакет отменён одним шагом.
import assert from 'node:assert/strict';
import { existsSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';

const [port, work, phase = 'draw'] = process.argv.slice(2);
const base = `http://127.0.0.1:${port}`;
const log = [];

const IPC_RETRIES = 2;

// Отправляет команду в /ipc и возвращает разобранный конверт ответа.
// "Command timeout" обычно значит, что главный поток не взял команду за
// таймаут и она отменена до запуска (uzvhttpipc запрещает запоздалое
// выполнение), - такой запрос повторяется; повтор виден в requests.log, а
// лишняя зона, если команда всё же выполнилась, видна по entity_count и
// verify_dxf.py.
async function ipc(cmd, args = []) {
  for (let attempt = 0; ; attempt++) {
    const body = { id: String(log.length + 1), cmd, args };
    const response = await fetch(`${base}/ipc`, {
      method: 'POST', body: JSON.stringify(body),
      headers: { 'Content-Type': 'application/json' },
      signal: AbortSignal.timeout(60000),
    });
    const text = await response.text();
    const line = `${cmd} ${JSON.stringify(args)} -> ${response.status} ${text}`;
    console.log(line);
    log.push(line);
    const reply = JSON.parse(text);
    if (reply.error !== 'Command timeout' || attempt >= IPC_RETRIES) return reply;
  }
}

// Ждёт, пока ZCAD поднимет HTTP-сервер.
async function waitForZcad() {
  for (let attempt = 0; attempt < 600; attempt++) {
    try {
      const response = await fetch(`${base}/ipc`, { signal: AbortSignal.timeout(2000) });
      if (response.ok) return;
    } catch { /* ещё не запущен */ }
    await delay(500);
  }
  throw new Error('ZCAD HTTP server did not start');
}

// Ждёт, пока главный поток ZCAD начнёт выполнять команды из очереди IPC:
// сервер отвечает раньше, чем заканчивается запуск, и первая команда может
// получить "Command timeout". Пробный запрос без номера чертёж не меняет.
async function waitForMainThread() {
  for (let attempt = 0; attempt < 10; attempt++) {
    const response = await fetch(`${base}/ipc`, {
      method: 'POST',
      body: JSON.stringify({ id: 'warmup', cmd: 'SPACE_ADD_ROOM', args: [{}] }),
      signal: AbortSignal.timeout(60000),
    });
    const reply = JSON.parse(await response.text());
    if (reply.error !== 'Command timeout') return;
    await delay(2000);
  }
  throw new Error('ZCAD main thread does not process IPC commands');
}

// Результат успешной команды: строка JSON внутри конверта.
function okResult(reply) {
  assert.equal(reply.status, 'ok', JSON.stringify(reply));
  const result = typeof reply.result === 'string' ? JSON.parse(reply.result) : reply.result;
  assert.equal(result.ok, true);
  return result;
}

// Код ошибки неуспешной команды.
function errorCode(reply) {
  assert.notEqual(reply.status, 'ok', JSON.stringify(reply));
  const error = typeof reply.error === 'string' ? JSON.parse(reply.error) : reply.error;
  assert.equal(error.ok, false);
  return error.error.code;
}

const room = [[0, 0], [6000, 0], [6000, 4000], [0, 4000]];

await waitForZcad();
await waitForMainThread();
if (phase === 'undo') {
  // Ctrl+Z отменяет весь пакет END_BATCH (помещения 102 и 103) одним шагом
  const after = okResult(await ipc('EXPORT_SELECTION', [{ scope: 'all',
    out_path: path.join(work, 'exports', 'undo.dxf'), overwrite: true }]));
  assert.equal(after.entity_count, 2, 'room 101 and floor remain after one undo');
  console.log('PASS e2e undo');
  process.exit(0);
}

const r1 = okResult(await ipc('SPACE_ADD_ROOM',
  [{ points: room, number: '101', name: 'Кабинет', category: 'Д' }]));
assert.equal(r1.layer, 'spaceRoom');
assert.equal(r1.area, 24000000);
assert.equal(r1.batch, false);

assert.equal(errorCode(await ipc('SPACE_ADD_ROOM', [{ points: room }])), 'MISSING_NUMBER');
assert.equal(errorCode(await ipc('SPACE_ADD_ROOM',
  [{ points: [[0, 0], [10, 10], [10, 0], [0, 10]], number: '1' }])), 'SELF_INTERSECTING');

okResult(await ipc('SPACE_ADD_POLY', [{ kind: 'floor', points:
  [[-100, -100], [20100, -100], [20100, 10100], [-100, 10100]], values: { space_Floor: '1' } }]));

assert.equal((await ipc('BEGIN_BATCH')).status, 'ok'); // ответ - текст, не JSON
const b1 = okResult(await ipc('SPACE_ADD_ROOM',
  [{ points: [[6000, 0], [12000, 0], [12000, 4000], [6000, 4000]], number: '102' }]));
assert.equal(b1.batch, true);
okResult(await ipc('SPACE_ADD_ROOM', [JSON.stringify(
  { points: [[12000, 0], [18000, 0], [18000, 4000], [12000, 4000]], number: '103' })]));
assert.equal((await ipc('END_BATCH')).status, 'ok');

const out = path.join(work, 'exports', 'zones.dxf');
const exported = okResult(await ipc('EXPORT_SELECTION',
  [{ scope: 'all', out_path: out, overwrite: true }]));
assert.ok(existsSync(out));
assert.equal(exported.entity_count, 4, 'room 101, floor, rooms 102 and 103');

writeFileSync(path.join(work, 'requests.log'), log.join('\n') + '\n');
console.log('PASS e2e SPACE_ADD_ROOM');
