# Руководство по внедрению новой архитектуры Proxy Graphic

## Файлы новой архитектуры

Созданы/обновлены следующие файлы:

```
zcad\velec\uzeentproxy\
├── uzeentproxytypes.pas       ← НОВЫЙ (типы данных, OPCODE)
├── uzeentproxyparser.pas      ← ОБНОВЛЁН (парсер AcGiWorldDraw)
├── uzeentacdproxy.pas         ← ОБНОВЛЁН (основной класс)
├── ARCHITECTURE.md            ← НОВЫЙ (документация)
└── IMPLEMENTATION_GUIDE.md    ← НОВЫЙ (это руководство)
```

## Изменения в существующих файлах

### uzeentproxyparser.pas

**Полностью переписан:**
- Удалён: `TProxyObjectParser` (ACIS SAT / Mesh подход)
- Удалён: `TProxyMeshData` (данные меша)
- Добавлен: `TProxyByteStream` (чтение бинарных данных)
- Добавлен: `TProxyCommandParser` (базовый класс парсера)
- Добавлен: `TProxyGraphicParser` (главный парсер)
- Добавлены парсеры: Circle, Arc, Polyline, Text, и т.д.

### uzeentacdproxy.pas

**Обновлён:**
- Удалено: `FMeshData: TProxyMeshData`
- Добавлено: `FVirtualEntities: array of PGDBObjEntity`
- Обновлён: `LoadFromDXF` (парсинг через TProxyGraphicParser)
- Обновлён: `FormatEntity` (отрисовка виртуальных сущностей)
- Добавлен: `ExplodeToVirtualEntities` (взрыв прокси)
- Добавлен: `ConvertResultToEntity` (конвертация в сущности ZCAD)

## Пошаговое внедрение

### Шаг 1: Проверка компиляции

```bash
# Скомпилируйте модуль
fpc -Shzcad\velec\uzeentproxy\uzeentacdproxy.pas
```

**Ожидаемый результат**: Ошибок нет

### Шаг 2: Обновление uses

Добавьте в другие модули, которые работают с прокси:

```pascal
uses
  ...
  uzeentproxytypes,  // Новый модуль
  uzeentproxyparser, // Обновлённый модуль
  ...
```

### Шаг 3: Тестирование на тестовых файлах

#### Тест 1: Загрузка testspds3entity.dxf

```pascal
procedure TestProxyLoading;
var
  Doc: TDrawingDef;
  Entity: PGDBObjEntity;
  Proxy: PGDBObjAcdProxy;
begin
  Doc := TDrawingDef.Create;
  try
    Doc.LoadFromFile('testspds3entity.dxf');
    
    // Ищем прокси-объекты
    for Entity in Doc.Entities do begin
      if Entity^.GetObjType = GDBAcdProxyID then begin
        Proxy := PGDBObjAcdProxy(Entity);
        
        Writeln('Proxy found:');
        Writeln('  Entities: ', Proxy^.GetVirtualEntityCount);
        Writeln('  BBox: ', 
          Proxy^.vp.BoundingBox.LBN.X:0:2, ',',
          Proxy^.vp.BoundingBox.LBN.Y:0:2);
        
        // Проверка отрисовки
        Proxy^.FormatEntity(Doc, DC, EFDraw);
      end;
    end;
  finally
    Doc.Free;
  end;
end;
```

**Ожидаемый результат**:
- В логе: `uzeentacdproxy: LoadFromDXF Parsed 2 entities`
- Отображаются 2 круга с текстом

#### Тест 2: Взрыв прокси-объекта

```pascal
procedure TestProxyExplode;
var
  Proxy: PGDBObjAcdProxy;
  Layout: PGDBLayout;
begin
  Proxy := ...; // Найти прокси
  Layout := Proxy^.GetLayout;
  
  Proxy^.ExplodeToVirtualEntities(Layout);
  
  // Прокси удалён, сущности добавлены в layout
  Writeln('Exploded!');
end;
```

**Ожидаемый результат**: Прокси заменён на круг + текст

### Шаг 4: Проверка производительности

```pascal
procedure TestPerformance;
var
  StartTime, EndTime: Int64;
begin
  StartTime := GetTickCount64;
  
  // Загрузка
  Doc.LoadFromFile('testspds3entity.dxf');
  
  EndTime := GetTickCount64;
  Writeln('Load time: ', EndTime - StartTime, ' ms');
  
  // Ожидаемое время: < 50 ms
end;
```

## Обратная совместимость

### Старый код (работает)

```pascal
// Старый подход с TProxyObjectParser больше не работает
// parser := TProxyObjectParser.Create; // ← УДАЛЕНО
```

### Новый код

```pascal
// Новый подход
Parser := TProxyGraphicParser.Create;
try
  if Parser.InitFromHex(HexData) then begin
    if Parser.Parse then begin
      // Работа с результатами
    end;
  end;
finally
  Parser.Free;
end;
```

## Отладка

### Включение логирования

В uzeentacdproxy.pas уже включено логирование:

```pascal
programlog.LogOutFormatStr(
  'uzeentacdproxy: LoadFromDXF Parsed %d entities',
  [FEntityCount], LM_Info);
```

### Просмотр логов

```
zcad\logs\zcad.log
```

**Ищите строки**:
- `uzeentacdproxy: LoadFromDXF START`
- `uzeentacdproxy: LoadFromDXF Parsed X entities`
- `uzeentacdproxy: FormatEntity drawing X virtual entities`

### Частые ошибки

#### Ошибка 1: "ReadInt32: End of stream"

**Причина**: Неправильный формат бинарных данных

**Решение**: Проверьте, что код 310 содержит hex-строку

#### Ошибка 2: "Invalid command size: 0"

**Причина**: Повреждённые данные

**Решение**: Пропустите команду, продолжите парсинг

#### Ошибка 3: "No valid results"

**Причина**: Неизвестный формат прокси (не AcGiWorldDraw)

**Решение**: Используйте fallback (BBox из кодов 10/11)

## Расширение функциональности

### Добавление поддержки нового OPCODE

1. Добавьте в `uzeentproxytypes.pas`:
   ```pascal
   pgcNewCommand = 99
   ```

2. Создайте парсер:
   ```pascal
   TProxyNewCommandParser = class(TProxyCommandParser)
     function Parse: TProxyCommandResult; override;
   end;
   ```

3. Добавьте обработчик в `TProxyGraphicParser.HandleNewCommand`

4. Добавьте конвертацию в `GDBObjAcdProxy.ConvertResultToEntity`

### Добавление поддержки СПДС параметров

Для полной поддержки СПДС GraphiCS:

1. Читайте текстовые параметры (коды 300/301/40)
2. Сопоставьте с бинарными данными
3. Используйте Shape для определения типа примитива

## Миграция со старой архитектуры

### Что делать со старым кодом

| Старый код | Новый код |
|-----------|-----------|
| `TProxyObjectParser` | `TProxyGraphicParser` |
| `TProxyMeshData` | `FVirtualEntities` |
| `ParseACISData` | `HandleCircle`, `HandleArc`, ... |
| `GetMeshForDisplay` | `GetVirtualEntity` |
| `DrawProxyMesh` | `DrawVirtualEntities` |

### Алгоритм миграции

1. Найдите все использования `TProxyObjectParser`
2. Замените на `TProxyGraphicParser`
3. Обновите работу с результатами
4. Протестируйте

## Контрольный список внедрения

- [ ] Файлы скопированы в проект
- [ ] Модуль компилируется без ошибок
- [ ] Тест 1: Загрузка testspds3entity.dxf
- [ ] Тест 2: Отрисовка прокси-объектов
- [ ] Тест 3: Взрыв прокси-объекта
- [ ] Логи показывают успешную загрузку
- [ ] Производительность в норме (< 50 ms)
- [ ] Документация прочитана

## Поддержка

При возникновении проблем:

1. Проверьте логи (`zcad\logs\zcad.log`)
2. Проверьте версию формата (должна быть 24 для СПДС)
3. Убедитесь, что бинарные данные в hex-формате
4. Обратитесь к документации `ARCHITECTURE.md`

---

*Руководство по внедрению новой архитектуры Proxy Graphic*
