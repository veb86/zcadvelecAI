// Проверки одной фазы сквозного теста #1478 против настоящего ZCAD.
// node e2e_check.mjs <nodoc|nosel|sel> <port> <workdir>
import assert from 'node:assert/strict';
import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import path from 'node:path';
import { setTimeout as delay } from 'node:timers/promises';

const [phase, port, work] = process.argv.slice(2);
const base = `http://127.0.0.1:${port}`;
const exportDir = path.join(work, 'exports');

// Отправляет POST с JSON-телом и возвращает разобранный ответ.
async function post(route, body) {
  const response = await fetch(base + route, {
    method: 'POST', body: JSON.stringify(body),
    headers: { 'Content-Type': 'application/json' },
    signal: AbortSignal.timeout(90000),
  });
  const text = await response.text();
  console.log(`${phase} ${route} ${JSON.stringify(body)} -> ${response.status} ${text}`);
  return JSON.parse(text);
}

// Ждёт, пока ZCAD загрузит чертёж и поднимет HTTP-сервер.
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

// Проверяет код ошибки в конверте ТЗ.
function expectError(reply, code) {
  assert.equal(reply.ok, false);
  assert.equal(reply.command, 'export_selection');
  assert.equal(reply.error.code, code);
}

// Фаза с выделением: основные сценарии и файлы для проверки ezdxf.
async function selectedPhase() {
  const summary = {};
  const first = await post('/api/export_selection', {});
  assert.equal(first.ok, true);
  assert.equal(first.path, path.join(exportDir, 'selection.dxf'));
  assert.ok(first.entity_count > 0);
  assert.ok(first.dxf_entity_count >= first.entity_count, 'devices expand in DXF');
  assert.equal(first.bbox.length, 4);
  assert.ok(existsSync(first.path));
  readFileSync(first.path); // файл не заблокирован после ответа
  summary.selection = first;

  const line = await post('/ipc', { id: 'l', cmd: 'LINE', args: [-500, -500, -400, -400] });
  assert.equal(line.status, 'ok');
  const again = await post('/api/export_selection', {});
  assert.equal(again.entity_count, first.entity_count, 'new unselected LINE is not exported');
  const all = await post('/api/export_selection',
    { scope: 'all', out_path: path.join(exportDir, 'all.dxf') });
  assert.equal(all.entity_count, first.entity_count + 1, 'scope=all includes the new LINE');
  assert.ok(all.bbox[0] <= -500 && all.bbox[1] <= -500);
  summary.all = all;

  const cyr = path.join(exportDir, 'Проект', 'фрагмент без xdata.dxf');
  const noX = await post('/api/export_selection', { out_path: cyr, include_xdata: false });
  assert.equal(noX.ok, true);
  assert.equal(noX.path, cyr);
  summary.noxdata = noX;

  expectError(await post('/api/export_selection', { out_path: cyr, overwrite: false }),
    'FILE_EXISTS');
  expectError(await post('/api/export_selection', { out_path: '/etc/zcad.dxf' }),
    'INVALID_PATH');
  writeFileSync(path.join(work, 'summary.json'), JSON.stringify(summary, null, 2));
}

// Фаза без чертежа: DWGClose показывает модальный вопрос о сохранении,
// модальный цикл обслуживает очередь IPC, и пока run_e2e.sh не нажмёт «No»,
// выгрузка видит пустой новый чертёж (EMPTY_SELECTION). После закрытия
// чертежей нет - ждём NO_DOCUMENT; файл при этом не создаётся.
async function noDocumentPhase() {
  for (let attempt = 0; attempt < 120; attempt++) {
    const reply = await post('/api/export_selection', {});
    if (reply.error?.code === 'NO_DOCUMENT') {
      expectError(reply, 'NO_DOCUMENT');
      assert.ok(!existsSync(path.join(exportDir, 'selection.dxf')), 'nothing written');
      return;
    }
    expectError(reply, 'EMPTY_SELECTION');
    await delay(500);
  }
  throw new Error('drawing was not closed: NO_DOCUMENT never returned');
}

await waitForZcad();
if (phase === 'nodoc') {
  await noDocumentPhase();
} else if (phase === 'nosel') {
  expectError(await post('/api/export_selection', {}), 'EMPTY_SELECTION');
  const all = await post('/api/export_selection',
    { scope: 'all', out_path: path.join(exportDir, 'nosel-all.dxf') });
  assert.equal(all.ok, true);
} else {
  await selectedPhase();
}
console.log(`PASS e2e phase ${phase}`);
