# Техническое задание: Модульная архитектура для Proxy примитивов ZCAD

## Проблема текущей реализации

В текущей версии все парсеры Proxy примитивов находятся в одном модуле `uzeentproxyparser.pas`:
- Все парсеры в одном файле (1500+ строк)
- Сложно отключить неработающий примитив
- Трудно развивать и тестировать отдельные примитивы
- Высокая связность кода

## Цель

Создать модульную архитектуру по аналогии с основными сущностями ZCAD (uzeentarc.pas, uzeentcircle.pas и т.д.), где каждый примитив:
- Находится в отдельном модуле
- Сам регистрируется в менеджере при инициализации
- Умеет самостоятельно парсить свои данные из Proxy Graphic
- Умеет создавать сущность ZCAD для отрисовки
- Может быть легко отключен исключением файла из проекта

## Архитектурные принципы

### 1. Менеджер Proxy примитивов

Создать центральный менеджер `TProxyPrimitiveManager` (аналог `uzeentityfactory.pas`):

```pascal
unit uzeentproxymanager;

type
  // Функция создания парсера
  TCreateProxyParserFunc = function: IProxyPrimitiveParser;
  
  // Информация о зарегистрированном примитиве
  TProxyPrimitiveInfo = record
    PrimitiveType: TProxyPrimitiveType;  // pptCircle, pptArc и т.д.
    DXFName: string;                      // 'CIRCLE', 'ARC'
    CreateParserFunc: TCreateProxyParserFunc;
  end;
  
  // Менеджер
  TProxyPrimitiveManager = class
    class var
      FPrimitives: TDictionary<TProxyPrimitiveType, TProxyPrimitiveInfo>;
      FNeedInit: Boolean;
    
    class procedure RegisterPrimitive(
      const PrimitiveType: TProxyPrimitiveType;
      const DXFName: string;
      const CreateParserFunc: TCreateProxyParserFunc
    );
    
    class function GetParser(
      const PrimitiveType: TProxyPrimitiveType
    ): IProxyPrimitiveParser;
    
    class function IsPrimitiveRegistered(
      const PrimitiveType: TProxyPrimitiveType
    ): Boolean;
  end;
```

### 2. Интерфейс парсера примитива

Создать унифицированный интерфейс для всех парсеров:

```pascal
unit uzeentproxytypes;

type
  IProxyPrimitiveParser = interface
    ['{GUID}']
    // Чтение данных из потока
    function ParseFromStream(Stream: TProxyByteStream; CommandSize: Integer): Boolean;
    
    // Проверка валидности данных
    function IsValid: Boolean;
    
    // Получение ошибки
    function GetErrorMsg: string;
    
    // Создание сущности ZCAD
    function CreateZCDEntity(const Drawing: TDrawingDef; const State: TProxyGraphicState): PGDBObjEntity;
    
    // Расширение BBox
    procedure ExpandBoundingBox(var MinPt, MaxPt: TzePoint3d);
    
    // Тип примитива
    function GetPrimitiveType: TProxyPrimitiveType;
  end;
```

### 3. Базовый класс парсера

Создать базовый класс для упрощения реализации:

```pascal
unit uzeentproxybaseparser;

type
  TProxyBaseParser = class(TInterfacedObject, IProxyPrimitiveParser)
  protected
    FValid: Boolean;
    FErrorMsg: string;
    FPrimitiveType: TProxyPrimitiveType;
    
    function IsValid: Boolean; virtual;
    function GetErrorMsg: string; virtual;
    function GetPrimitiveType: TProxyPrimitiveType; virtual;
    
    // Абстрактные методы - переопределяются в потомках
    function DoParseFromStream(Stream: TProxyByteStream; CommandSize: Integer): Boolean; virtual; abstract;
    function DoCreateZCDEntity(const Drawing: TDrawingDef; const State: TProxyGraphicState): PGDBObjEntity; virtual; abstract;
    procedure DoExpandBoundingBox(var MinPt, MaxPt: TzePoint3d); virtual; abstract;
    
  public
    function ParseFromStream(Stream: TProxyByteStream; CommandSize: Integer): Boolean;
    function CreateZCDEntity(const Drawing: TDrawingDef; const State: TProxyGraphicState): PGDBObjEntity;
    procedure ExpandBoundingBox(var MinPt, MaxPt: TzePoint3d);
  end;
```

### 4. Структура модулей

Каждый примитив в отдельном файле по аналогии с uzeentarc.pas:

```
zcad\velec\uzeentproxy\
├── uzeentproxymanager.pas          # Менеджер примитивов
├── uzeentproxybaseparser.pas       # Базовый класс
├── uzeentproxyparsercircle.pas     # Парсер круга
├── uzeentproxyparserarc.pas        # Парсер дуги
├── uzeentproxyparserellipse.pas    # Парсер эллипса
├── uzeentproxyparserpolyline.pas   # Парсер полилинии
├── uzeentproxyparserpolygon.pas    # Парсер полигона
├── uzeentproxyparserline.pas       # Парсер линии
├── uzeentproxyparsertext.pas       # Парсер текста
└── uzeentacdproxy.pas              # Основной ACAD Proxy объект
```

### 5. Пример реализации модуля (круг)

```pascal
unit uzeentproxyparsercircle;

interface

uses
  uzeentproxytypes, uzeentproxybaseparser, uzeentity, uzedrawingdef;

type
  TProxyCircleParser = class(TProxyBaseParser)
  private
    FCenter: TzePoint3d;
    FRadius: Double;
    FNormal: TzePoint3d;
    
  protected
    function DoParseFromStream(Stream: TProxyByteStream; CommandSize: Integer): Boolean; override;
    function DoCreateZCDEntity(const Drawing: TDrawingDef; const State: TProxyGraphicState): PGDBObjEntity; override;
    procedure DoExpandBoundingBox(var MinPt, MaxPt: TzePoint3d); override;
    
  public
    constructor Create;
    function GetPrimitiveType: TProxyPrimitiveType; override;
  end;

// Функция создания для регистрации
function CreateCircleParser: IProxyPrimitiveParser;

implementation

uses
  uzeentproxymanager, uzeentcircle, uzegeometry;

constructor TProxyCircleParser.Create;
begin
  inherited Create;
  FPrimitiveType := pptCircle;
  FValid := False;
end;

function TProxyCircleParser.GetPrimitiveType: TProxyPrimitiveType;
begin
  Result := pptCircle;
end;

function TProxyCircleParser.DoParseFromStream(Stream: TProxyByteStream; CommandSize: Integer): Boolean;
begin
  try
    // Формат: Center (3d) + Radius (d) + Normal (3d)
    FCenter := Stream.ReadVertex;
    FRadius := Stream.ReadDouble;
    FNormal := Stream.ReadVector;
    FValid := True;
    Result := True;
  except
    on E: Exception do
    begin
      FErrorMsg := 'Circle parse error: ' + E.Message;
      Result := False;
    end;
  end;
end;

function TProxyCircleParser.DoCreateZCDEntity(const Drawing: TDrawingDef; const State: TProxyGraphicState): PGDBObjEntity;
var
  CircleObj: PGDBObjCircle;
begin
  CircleObj := GDBObjCircle.CreateInstance;
  CircleObj^.initnul;
  CircleObj^.vp.Color := State.Color;
  CircleObj^.vp.Layer := State.Layer;
  CircleObj^.Local.p_insert := FCenter;
  CircleObj^.Radius := FRadius;
  CircleObj^.CalcObjMatrix(@Drawing);
  Result := CircleObj;
end;

procedure TProxyCircleParser.DoExpandBoundingBox(var MinPt, MaxPt: TzePoint3d);
begin
  ExpandBBoxWithPoint(CreateVertex(FCenter.x - FRadius, FCenter.y - FRadius, FCenter.z), MinPt, MaxPt);
  ExpandBBoxWithPoint(CreateVertex(FCenter.x + FRadius, FCenter.y + FRadius, FCenter.z), MinPt, MaxPt);
end;

// Функция создания для регистрации
function CreateCircleParser: IProxyPrimitiveParser;
begin
  Result := TProxyCircleParser.Create;
end;

initialization
  // Регистрация при загрузке модуля
  TProxyPrimitiveManager.RegisterPrimitive(pptCircle, 'CIRCLE', @CreateCircleParser);
  
end.
```

### 6. Обновление основного модуля uzeentacdproxy.pas

Изменить `FormatEntity` для использования менеджера:

```pascal
procedure GDBObjAcdProxy.FormatEntity(var drawing: TDrawingDef; var DC: TDrawContext; Stage: TEFStages);
var
  I: Integer;
  Parser: IProxyPrimitiveParser;
  Entity: PGDBObjEntity;
begin
  if FResultCount > 0 then
  begin
    for I := 0 to FResultCount - 1 do
    begin
      if FResults[I].Valid and TProxyPrimitiveManager.IsPrimitiveRegistered(FResults[I].PrimitiveType) then
      begin
        // Получаем парсер из менеджера
        Parser := TProxyPrimitiveManager.GetParser(FResults[I].PrimitiveType);
        
        // Парсим данные из результата (нужно доработать хранение)
        // Parser.ParseFromStream(...);
        
        if Parser.IsValid then
        begin
          Entity := Parser.CreateZCDEntity(drawing, FCurrentState);
          if Entity <> nil then
          begin
            Entity^.FormatEntity(drawing, DC, Stage);
            Entity^.done;
            FreeMem(Pointer(Entity));
          end;
        end;
      end;
    end;
  end;
end;
```

### 7. Хранение данных примитивов

Вместо `TProxyCommandResult` с вариантной записью использовать интерфейс:

```pascal
type
  IParsedPrimitive = interface
    ['{GUID}']
    function GetParser: IProxyPrimitiveParser;
    function GetPrimitiveType: TProxyPrimitiveType;
    function GetData: Pointer; // Или конкретный тип через as
  end;
```

### 8. Преимущества новой архитектуры

1. **Модульность**: Каждый примитив в отдельном файле
2. **Отключаемость**: Исключить файл из проекта = отключить примитив
3. **Тестируемость**: Можно тестировать парсеры отдельно
4. **Расширяемость**: Легко добавить новый примитив
5. **Поддержка**: Легко найти и исправить баги в конкретном примитиве
6. **Единый стиль**: Как основные сущности ZCAD

### 9. План миграции

**Этап 1: Подготовка**
- [ ] Создать `uzeentproxymanager.pas`
- [ ] Создать `uzeentproxybaseparser.pas`
- [ ] Создать интерфейс `IProxyPrimitiveParser`

**Этап 2: Перенос парсеров**
- [ ] Создать `uzeentproxyparsercircle.pas` (перенести из uzeentproxyparser.pas)
- [ ] Создать `uzeentproxyparserarc.pas`
- [ ] Создать `uzeentproxyparserellipse.pas` (исправить 7 параметров!)
- [ ] Создать `uzeentproxyparserpolyline.pas`
- [ ] Создать `uzeentproxyparserpolygon.pas`
- [ ] Создать `uzeentproxyparsertext.pas`

**Этап 3: Интеграция**
- [ ] Обновить `uzeentacdproxy.pas` для работы с менеджером
- [ ] Обновить `CalcBBoxFromParserResults` для использования `ExpandBoundingBox`
- [ ] Обновить `ConvertResultToEntity` для использования `CreateZCDEntity`

**Этап 4: Тестирование**
- [ ] Протестировать на testproxy.dxf
- [ ] Проверить отключение отдельных примитивов
- [ ] Проверить корректность отрисовки

### 10. Критерии приемки

1. Все примитивы работают как до рефакторинга
2. Можно отключить примитив удалением файла из проекта
3. Код компилируется без ошибок
4. Логирование работает корректно
5. Производительность не ухудшилась

## Примечания

- Сохранить обратную совместимость со старым кодом
- Использовать интерфейсы для управления временем жизни объектов
- Добавить подробное логирование для отладки
- Документировать каждый публичный метод
