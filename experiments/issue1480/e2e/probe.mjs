// Диагностика: последовательность IPC-команд из JSON-файла против ZCAD.
// node probe.mjs <port> <steps.json>  (steps: [[cmd, args], ...])
// Шаг ["@UNDO"] вводит команду Undo в командной строке ZCAD через xdotool.
// Пример: steps_export_undo.json (Undo после EXPORT_SELECTION, #1480).
import { execSync } from 'node:child_process';
import { readFileSync } from 'node:fs';
import { setTimeout as delay } from 'node:timers/promises';

const [port, stepsFile] = process.argv.slice(2);
const base = `http://127.0.0.1:${port}`;
const steps = JSON.parse(readFileSync(stepsFile, 'utf-8'));

// Ждёт, пока ZCAD поднимет HTTP-сервер.
async function waitForZcad() {
  for (let attempt = 0; attempt < 600; attempt++) {
    try {
      const r = await fetch(`${base}/ipc`, { signal: AbortSignal.timeout(2000) });
      if (r.ok) return;
    } catch { /* ещё не запущен */ }
    await delay(500);
  }
  throw new Error('ZCAD HTTP server did not start');
}

// Ждёт, пока главный поток ZCAD начнёт брать команды из очереди IPC
// (во время запуска первая команда может получить "Command timeout").
async function waitForMainThread() {
  for (let attempt = 0; attempt < 10; attempt++) {
    const r = await fetch(`${base}/ipc`, {
      method: 'POST',
      body: JSON.stringify({ id: 'warmup', cmd: 'SPACE_ADD_ROOM', args: [{}] }),
      signal: AbortSignal.timeout(60000),
    });
    if (!(await r.text()).includes('Command timeout')) return;
    await delay(2000);
  }
  throw new Error('ZCAD main thread does not process IPC commands');
}

await waitForZcad();
await waitForMainThread();
for (const [i, [cmd, args]] of steps.entries()) {
  if (cmd === '@UNDO') {
    execSync("xdotool windowactivate --sync $(xdotool search --onlyvisible " +
      "--name '^Command line$' | head -1) && sleep 1 && " +
      "xdotool type --delay 50 Undo && xdotool key Return && sleep 2");
    console.log('@UNDO done');
    continue;
  }
  const r = await fetch(`${base}/ipc`, {
    method: 'POST', body: JSON.stringify({ id: String(i + 1), cmd, args }),
    signal: AbortSignal.timeout(60000),
  });
  console.log(cmd, '->', (await r.text()).slice(0, 160));
}
await delay(3000);
const alive = await fetch(`${base}/ipc`, { signal: AbortSignal.timeout(5000) })
  .then(() => true, () => false);
console.log('ALIVE', alive);
process.exit(alive ? 0 : 1);
