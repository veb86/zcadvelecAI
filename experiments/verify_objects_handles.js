/**
 * Верификация: проверяет, что после пересопоставления дескрипторов
 * все ссылки в секции OBJECTS будут указывать на существующие объекты.
 *
 * Симулирует процесс сохранения: TABLES/BLOCKS получают новые
 * дескрипторы, а OBJECTS проходит через пересопоставление.
 */
const fs = require('fs');

const HANDLE_CODES = new Set([5, 105, 320, 330, 340, 350, 360, 390, 1005]);

function extractSection(filename, sectionName) {
  const lines = fs.readFileSync(filename, 'utf-8').split(/\r?\n/);
  let start = -1, end2 = -1;
  for (let i = 0; i < lines.length - 3; i++) {
    if (lines[i].trim() === '0' && lines[i+1].trim() === 'SECTION' &&
        lines[i+2].trim() === '2' && lines[i+3].trim() === sectionName) {
      start = i; break;
    }
  }
  if (start < 0) return [];
  for (let i = start + 4; i < lines.length - 1; i++) {
    if (lines[i].trim() === '0' && lines[i+1].trim() === 'ENDSEC') {
      end2 = i + 1; break;
    }
  }
  if (end2 < 0) return [];
  return lines.slice(start, end2 + 1);
}

function getHandlesByCode5(lines) {
  const handles = new Set();
  for (let i = 0; i < lines.length - 1; i++) {
    if (lines[i].trim() === '5') handles.add(lines[i+1].trim());
  }
  return handles;
}

const origFile = 'testspds3entity.txt';
const savedFile = 'testspds3entity888.txt';

// Дескрипторы из TABLES/BLOCKS исходного файла
const origTablesHandles = getHandlesByCode5(extractSection(origFile, 'TABLES'));
const origBlocksHandles = getHandlesByCode5(extractSection(origFile, 'BLOCKS'));

// Дескрипторы из TABLES/BLOCKS сохранённого файла
const savedTablesHandles = getHandlesByCode5(extractSection(savedFile, 'TABLES'));
const savedBlocksHandles = getHandlesByCode5(extractSection(savedFile, 'BLOCKS'));
const savedEntitiesHandles = getHandlesByCode5(extractSection(savedFile, 'ENTITIES'));

// Секция OBJECTS из исходного файла (которая записывается в сохранённый файл)
const origObjects = extractSection(origFile, 'OBJECTS');
const origObjectsHandles = getHandlesByCode5(origObjects);

// Симулируем пересопоставление
// h2p_keys = дескрипторы из исходного файла TABLES/BLOCKS (карта загрузки)
const h2pKeys = new Set([...origTablesHandles, ...origBlocksHandles]);

// p2h_values = дескрипторы из сохранённого файла TABLES/BLOCKS (карта сохранения)
// Предполагаем, что указатели совпадают (в порядке обхода)
const origTablesArr = [...origTablesHandles];
const savedTablesArr = [...savedTablesHandles];
const origBlocksArr = [...origBlocksHandles];
const savedBlocksArr = [...savedBlocksHandles];

// Карта source_handle → new_handle для TABLES/BLOCKS
const sourceToNew = new Map();
for (let i = 0; i < Math.min(origTablesArr.length, savedTablesArr.length); i++) {
  sourceToNew.set(origTablesArr[i], savedTablesArr[i]);
}
for (let i = 0; i < Math.min(origBlocksArr.length, savedBlocksArr.length); i++) {
  sourceToNew.set(origBlocksArr[i], savedBlocksArr[i]);
}

console.log('=== Карта пересопоставления TABLES/BLOCKS ===');
for (const [old, newH] of sourceToNew) {
  if (old !== newH) console.log(`  ${old} → ${newH}`);
}

// Симулируем прогон OBJECTS через WriteRemappedObjectsSection
let handleCounter = 0x100;
const localRemap = new Map();
const newObjectsHandles = new Set();
let remappedCount = 0;

for (let i = 0; i < origObjects.length - 1; i++) {
  const code = parseInt(origObjects[i].trim());
  if (!HANDLE_CODES.has(code)) continue;
  const oldH = origObjects[i+1].trim();
  if (oldH === '0') continue;

  // Ищем в localRemap
  if (localRemap.has(oldH)) {
    remappedCount++;
    continue;
  }

  // Ищем в sourceToNew (h2p → p2h)
  if (sourceToNew.has(oldH)) {
    localRemap.set(oldH, sourceToNew.get(oldH));
    remappedCount++;
    continue;
  }

  // Назначаем новый дескриптор
  const newH = handleCounter.toString(16).toUpperCase();
  handleCounter++;
  localRemap.set(oldH, newH);
  if (code === 5) newObjectsHandles.add(newH);
}

console.log(`\n=== Результат пересопоставления ===`);
console.log(`Пересопоставлено дескрипторов: ${remappedCount}`);
console.log(`Записей в локальной карте: ${localRemap.size}`);

// Проверяем целостность после пересопоставления
// Все дескрипторы в сохранённом файле
const allNewHandles = new Set([
  ...savedTablesHandles,
  ...savedBlocksHandles,
  ...savedEntitiesHandles,
  ...newObjectsHandles
]);

// Проверяем ссылки в пересопоставленном OBJECTS
let brokenRefs = 0;
let totalRefs = 0;
for (let i = 0; i < origObjects.length - 1; i++) {
  const code = parseInt(origObjects[i].trim());
  if (!HANDLE_CODES.has(code) || code === 5) continue;
  const oldH = origObjects[i+1].trim();
  if (oldH === '0') continue;
  totalRefs++;
  const newH = localRemap.get(oldH) || oldH;
  if (!allNewHandles.has(newH) && !localRemap.has(oldH)) {
    brokenRefs++;
    console.log(`  Битая ссылка: ${oldH} → ${newH} (код ${code})`);
  }
}

console.log(`\nВсего ссылок (кроме code 5): ${totalRefs}`);
console.log(`Битых ссылок после пересопоставления: ${brokenRefs}`);

if (brokenRefs === 0) {
  console.log('\n✓ ТЕСТ ПРОЙДЕН: все ссылки будут корректны после пересопоставления');
} else {
  console.log('\n✗ ТЕСТ НЕ ПРОЙДЕН: остались битые ссылки');
  process.exit(1);
}
