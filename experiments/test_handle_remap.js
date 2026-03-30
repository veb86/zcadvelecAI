/**
 * Тест: проверяет, что дескрипторы в секции OBJECTS
 * пересопоставляются корректно при сохранении DXF.
 *
 * Сравнивает исходный файл (testspds3entity.txt) и файл,
 * сохранённый ZCAD (testspds3entity888.txt), и показывает
 * какие дескрипторы должны быть пересопоставлены.
 */
const fs = require('fs');

// Коды групп, содержащие дескрипторы
const HANDLE_CODES = new Set([5, 105, 320, 330, 340, 350, 360, 390, 1005]);

/**
 * Извлекает секцию из DXF-файла
 */
function extractSection(filename, sectionName) {
  const lines = fs.readFileSync(filename, 'utf-8').split(/\r?\n/);
  let start = -1, end2 = -1;
  for (let i = 0; i < lines.length - 3; i++) {
    if (lines[i].trim() === '0' && lines[i+1].trim() === 'SECTION' &&
        lines[i+2].trim() === '2' && lines[i+3].trim() === sectionName) {
      start = i;
      break;
    }
  }
  if (start < 0) return null;
  for (let i = start + 4; i < lines.length - 1; i++) {
    if (lines[i].trim() === '0' && lines[i+1].trim() === 'ENDSEC') {
      end2 = i + 1;
      break;
    }
  }
  if (end2 < 0) return null;
  return lines.slice(start, end2 + 1);
}

/**
 * Извлекает все дескрипторы из секции
 * Возвращает Map: handle -> type для дескрипторов с кодом 5
 */
function extractHandles(lines) {
  const handles = new Map();
  let currentType = '?';
  for (let i = 0; i < lines.length - 1; i++) {
    const code = lines[i].trim();
    const value = lines[i+1].trim();
    if (code === '0') currentType = value;
    if (code === '5') handles.set(value, currentType);
  }
  return handles;
}

/**
 * Извлекает все ссылки на дескрипторы из секции OBJECTS
 */
function extractObjectsRefs(lines) {
  const refs = new Set();
  for (let i = 0; i < lines.length - 1; i++) {
    const code = parseInt(lines[i].trim());
    if (HANDLE_CODES.has(code)) {
      refs.add(lines[i+1].trim());
    }
  }
  return refs;
}

// Загружаем файлы
const origFile = 'testspds3entity.txt';
const savedFile = 'testspds3entity888.txt';

// Извлекаем секции TABLES из обоих файлов
const origTables = extractSection(origFile, 'TABLES');
const savedTables = extractSection(savedFile, 'TABLES');

// Извлекаем секции BLOCKS из обоих файлов
const origBlocks = extractSection(origFile, 'BLOCKS');
const savedBlocks = extractSection(savedFile, 'BLOCKS');

// Извлекаем дескрипторы из TABLES
const origTableHandles = extractHandles(origTables);
const savedTableHandles = extractHandles(savedTables);

// Извлекаем дескрипторы из BLOCKS
const origBlockHandles = extractHandles(origBlocks);
const savedBlockHandles = extractHandles(savedBlocks);

// Извлекаем секцию OBJECTS из исходного файла
const origObjects = extractSection(origFile, 'OBJECTS');
const objectsRefs = extractObjectsRefs(origObjects);

console.log('=== Дескрипторы TABLES+BLOCKS, на которые ссылается OBJECTS ===\n');

// Все дескрипторы из TABLES и BLOCKS исходного файла
const allOrigHandles = new Map([...origTableHandles, ...origBlockHandles]);

let needsRemap = 0;
let alreadyOk = 0;
for (const ref of objectsRefs) {
  if (ref === '0') continue;
  if (allOrigHandles.has(ref)) {
    const type = allOrigHandles.get(ref);
    // Этот дескриптор нужно пересопоставить, потому что он из TABLES/BLOCKS
    needsRemap++;
    console.log(`  Нужно пересопоставить: ${ref} (${type})`);
  }
}

console.log(`\nИтого: ${needsRemap} дескрипторов TABLES/BLOCKS нужно пересопоставить`);
console.log(`Всего уникальных ссылок в OBJECTS: ${objectsRefs.size}`);

// Проверяем, что все ссылки OBJECTS указывают на существующие объекты
const allOrigFileHandles = new Map([
  ...extractHandles(extractSection(origFile, 'TABLES')),
  ...extractHandles(extractSection(origFile, 'BLOCKS')),
  ...extractHandles(extractSection(origFile, 'ENTITIES')),
  ...extractHandles(extractSection(origFile, 'OBJECTS'))
]);

console.log('\n=== Проверка целостности ссылок в OBJECTS ===');
let broken = 0;
for (const ref of objectsRefs) {
  if (ref === '0') continue;
  if (!allOrigFileHandles.has(ref)) {
    broken++;
    console.log(`  Битая ссылка: ${ref}`);
  }
}
console.log(`Битых ссылок: ${broken}`);
