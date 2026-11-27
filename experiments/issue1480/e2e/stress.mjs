// Нагрузка на HTTP-сервер ZCAD: параллельные запросы, затем проверка, жив ли ZCAD.
// node stress.mjs <port> <потоков> <запросов на поток> [cmd]
// Воспроизводит падение GTK2 «split_segment reached end of line!», если
// обработчик HTTP-запроса пишет в историю командной строки из рабочего потока.
import { setTimeout as delay } from 'node:timers/promises';

const [port, streams, perStream, cmd = 'SPACE_ADD_ROOM'] = process.argv.slice(2);
const base = `http://127.0.0.1:${port}`;

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

let timeouts = 0;
let maxMs = 0;

// Один запрос; запрос без номера не меняет чертёж (MISSING_NUMBER).
// Возвращает true, если главный поток не взял команду ("Command timeout").
async function send(id) {
  const body = { id, cmd, args: [{ points: [[0, 0], [1, 0], [1, 1]] }] };
  const t0 = Date.now();
  const r = await fetch(`${base}/ipc`, { method: 'POST', body: JSON.stringify(body),
    signal: AbortSignal.timeout(60000) });
  const timedOut = (await r.text()).includes('Command timeout');
  maxMs = Math.max(maxMs, Date.now() - t0);
  return timedOut;
}

// Один поток запросов; считает ответы "Command timeout".
async function stream(id) {
  for (let i = 0; i < Number(perStream); i++)
    if (await send(`${id}-${i}`)) timeouts++;
}

await waitForZcad();
// Пока ZCAD запускается, главный поток может не успевать за таймаут IPC
for (let i = 0; i < 10 && await send('warmup'); i++) await delay(2000);
const started = Date.now();
const results = await Promise.allSettled(
  Array.from({ length: Number(streams) }, (_, i) => stream(i)));
const rejected = results.filter((r) => r.status === 'rejected');
const failed = rejected.length;
for (const r of rejected.slice(0, 3)) console.log('rejected:', r.reason?.cause?.code ?? r.reason?.message);
await delay(3000);
const alive = await fetch(`${base}/ipc`, { signal: AbortSignal.timeout(5000) })
  .then(() => true, () => false);
// Сбросы соединения (UND_ERR_SOCKET) - повторное использование сокета клиентом
// после закрытия соединения сервером; на результат влияет только ALIVE.
console.log(`streams=${streams} per=${perStream} socket_errors=${failed} ` +
  `timeouts=${timeouts} max_ms=${maxMs} ms=${Date.now() - started} ALIVE ${alive}`);
process.exit(alive ? 0 : 1);
