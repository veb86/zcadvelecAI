# Концепция добавления OpenGL 3.3 в ZCAD

## Краткое резюме

Данный документ описывает концепцию и план внедрения поддержки OpenGL 3.3 Core Profile в проект ZCAD. В проекте уже существует базовая структура для поддержки "современного" OpenGL (каталог `openglmodern`), однако текущая реализация неполная и использует устаревшие подходы.

## Текущее состояние проекта

### Архитектура рендеринга

Проект использует модульную архитектуру с поддержкой различных бэкендов рендеринга:

1. **Абстрактный слой** (`uzgldrawerabstract.pas`, `uzglviewareaabstract.pas`)
   - Определяет интерфейсы для всех операций рисования
   - Независим от конкретной реализации

2. **Существующие бэкенды**:
   - `opengl` - OpenGL 1.x с immediate mode (glBegin/glEnd)
   - `openglmodern` - Начальная поддержка VBO (неполная)
   - `gdi` - Windows GDI рендеринг
   - `dx` - DirectX (в разработке)
   - `canvas` - Canvas-based рендеринг

3. **Система регистрации бэкендов** (`uzglbackendmanager.pas`)
   - Позволяет динамически переключаться между бэкендами
   - `openglmodern` бэкенд регистрируется только при `UseExperimentalFeatures=true`

### Текущая реализация OpenGL 1.x

**Файлы**: `cad_source/zengine/zgl/opengl/`

Основные компоненты:
- `uzgldrawerogl.pas` - Реализация рендерера через immediate mode
- `uzglviewareaogl.pas` - Управление OpenGL контекстом
- `uzgloglstatemanager.pas` - Оптимизация состояния OpenGL (минимизация вызовов)

Используемые возможности OpenGL 1.x:
- Immediate mode: `glBegin()`, `glVertex*()`, `glEnd()`
- Fixed function pipeline
- Matrix stack: `glPushMatrix()`, `glPopMatrix()`, `glLoadMatrix()`
- Встроенные матрицы: `GL_MODELVIEW`, `GL_PROJECTION`
- Fixed lighting
- Texture state machine
- Line stipple, polygon stipple

### Текущая реализация "openglmodern"

**Файлы**: `cad_source/zengine/zgl/openglmodern/`

Что уже реализовано:
1. **VBO аллокация** (`uzglviewareaoglmodern.pas:84-87`)
   ```pascal
   glGenBuffers(1,@VBO.vboID);
   glBindBuffer(GL_ARRAY_BUFFER,VBO.vboID);
   glBufferData(GL_ARRAY_BUFFER,CVBOSize,nil,GL_STATIC_DRAW);
   VBO.vboAllocator.init(CVBOSize);
   ```

2. **Частичная реализация примитивов** (`uzgprimitivescreatoroglmodern.pas`)
   - Только линии (TLLVBOLine)
   - Использует VBO для хранения вершин
   - Загружает данные через `glBufferSubData`

3. **Детекция версии OpenGL** (`uzglviewareaoglmodern.pas:46-83`)
   - Последовательная проверка версий от 1.2 до 4.3

Что НЕ реализовано:
- ❌ Шейдерная программа
- ❌ VAO (Vertex Array Objects)
- ❌ Uniform переменные для матриц
- ❌ Отрисовка треугольников, квадов
- ❌ Batch rendering (все примитивы в один VBO)
- ❌ Цветовые атрибуты
- ❌ Текстуры
- ❌ Освещение
- ❌ Line stipple (не поддерживается в Core Profile)
- ❌ Stencil буфер операции

## OpenGL 3.3 Core Profile - Ключевые отличия

### Что удалено из OpenGL 3.3 Core

1. **Immediate mode** - `glBegin/glEnd` полностью удалены
2. **Fixed function pipeline** - нет встроенных шейдеров
3. **Matrix stack** - `glPushMatrix/glPopMatrix` удалены
4. **Fixed lighting** - `GL_LIGHTING`, `GL_LIGHT0` удалены
5. **Built-in variables** - `gl_Vertex`, `gl_Color` удалены
6. **Default VAO** - обязательно создавать VAO явно
7. **Line stipple** - `glLineStipple` удалён (нужен fragment shader)
8. **glColor*, glVertex*** - все функции установки атрибутов удалены

### Что требуется для OpenGL 3.3 Core

1. **Vertex Array Objects (VAO)**
   - Хранят состояние vertex attributes
   - Минимум один VAO должен быть привязан

2. **Vertex Buffer Objects (VBO)**
   - Для хранения вершин, цветов, текстурных координат
   - Обязательное использование

3. **Шейдерная программа**
   - Vertex shader - обработка вершин
   - Fragment shader - раскраска пикселей
   - Компиляция и линковка шейдеров

4. **Uniform переменные**
   - Для передачи матриц (model, view, projection)
   - Для глобальных параметров

5. **Vertex attributes**
   - Position (vec3)
   - Color (vec4)
   - Normal (vec3) - для освещения
   - TexCoord (vec2) - для текстур

## Предлагаемая архитектура

### 1. Шейдерная система

#### 1.1 Базовые шейдеры

**Vertex Shader (basic.vert)**:
```glsl
#version 330 core

layout(location = 0) in vec3 position;
layout(location = 1) in vec4 color;

uniform mat4 modelMatrix;
uniform mat4 viewMatrix;
uniform mat4 projectionMatrix;

out vec4 fragColor;

void main() {
    gl_Position = projectionMatrix * viewMatrix * modelMatrix * vec4(position, 1.0);
    fragColor = color;
}
```

**Fragment Shader (basic.frag)**:
```glsl
#version 330 core

in vec4 fragColor;
out vec4 finalColor;

void main() {
    finalColor = fragColor;
}
```

#### 1.2 Дополнительные шейдеры

1. **Line stipple shader** - эмуляция `glLineStipple`
2. **Lighting shader** - Phong/Blinn-Phong освещение
3. **Texture shader** - для текстурированных примитивов
4. **Selection shader** - для stencil buffer эмуляции

#### 1.3 Shader Manager

Новый модуль: `uzglshadermanager.pas`

```pascal
type
  TShaderType = (ST_Basic, ST_Stipple, ST_Lighting, ST_Texture, ST_Selection);

  TShaderProgram = record
    ProgramID: GLuint;
    VertexShaderID: GLuint;
    FragmentShaderID: GLuint;
    // Uniform locations
    ModelMatrixLoc: GLint;
    ViewMatrixLoc: GLint;
    ProjectionMatrixLoc: GLint;
    ColorLoc: GLint;
  end;

  TShaderManager = class
    function CompileShader(source: string; shaderType: GLenum): GLuint;
    function LinkProgram(vertShader, fragShader: GLuint): GLuint;
    function GetShaderProgram(shaderType: TShaderType): TShaderProgram;
    procedure UseProgram(shaderType: TShaderType);
  end;
```

### 2. Система буферов

#### 2.1 Vertex Buffer Manager

Расширить существующий `TVBOAllocator`:

```pascal
type
  TVertexAttribute = (VA_Position, VA_Color, VA_Normal, VA_TexCoord);

  TVertexFormat = record
    Position: GDBVertex3S;  // 12 bytes
    Color: TRGBA;          // 4 bytes
    Normal: GDBVertex3S;    // 12 bytes (optional)
    TexCoord: GDBVertex2S;  // 8 bytes (optional)
  end; // Total: 36 bytes per vertex

  TVBOManager = class
    VAOID: GLuint;
    VBOPosition: GLuint;
    VBOColor: GLuint;
    VBOIndex: GLuint;  // Element buffer

    procedure CreateBuffers;
    procedure BindVertexAttributes;
    procedure UpdateBuffer(data: pointer; size: integer);
  end;
```

#### 2.2 Batch Rendering System

Новый модуль: `uzglbatchrenderer.pas`

```pascal
type
  TPrimitiveType = (PT_Lines, PT_Triangles, PT_TriangleFan, PT_TriangleStrip);

  TRenderBatch = record
    PrimitiveType: TPrimitiveType;
    VertexCount: integer;
    IndexOffset: integer;
    ShaderType: TShaderType;
  end;

  TBatchRenderer = class
    CurrentBatch: array of TRenderBatch;
    VertexData: array of TVertexFormat;
    IndexData: array of GLuint;

    procedure BeginBatch(primitiveType: TPrimitiveType);
    procedure AddVertex(const pos: GDBVertex3S; const color: TRGB);
    procedure EndBatch;
    procedure Flush;  // Отрисовка всех батчей
  end;
```

### 3. Matrix Management

Поскольку matrix stack удалён, нужна собственная реализация:

Новый модуль: `uzglmatrixstack.pas`

```pascal
type
  TMatrixStack = class
    Stack: array of DMatrix4D;
    CurrentIndex: integer;

    procedure Push(const matrix: DMatrix4D);
    procedure Pop;
    function GetCurrent: DMatrix4D;
    procedure LoadIdentity;
    procedure Multiply(const matrix: DMatrix4D);
  end;

  TMatrixManager = class
    ModelMatrix: TMatrixStack;
    ViewMatrix: DMatrix4D;
    ProjectionMatrix: DMatrix4D;

    procedure UpdateUniforms(shaderProgram: TShaderProgram);
  end;
```

### 4. State Manager для OpenGL 3.3

Обновить `uzgloglstatemanager.pas`:

```pascal
type
  TOGLModernStateManager = class
    CurrentShaderProgram: GLuint;
    CurrentVAO: GLuint;
    CurrentVBO: GLuint;

    // Эмуляция старых состояний
    CurrentColor: TRGBA;
    LineWidth: GLfloat;
    PointSize: GLfloat;
    DepthTest: boolean;
    Blending: boolean;

    procedure SetColor(const color: TRGB);
    procedure SetLineWidth(width: single);
    procedure FlushState;  // Применить изменения к шейдерам
  end;
```

### 5. Обновление примитивов

Обновить `uzgprimitivescreatoroglmodern.pas`:

```pascal
type
  TLLVBOLine33 = object(TLLLine)
    BatchIndex: integer;
    procedure AddToBatch(batchRenderer: TBatchRenderer);
  end;

  TLLVBOTriangle33 = object(TLLTriangle)
    BatchIndex: integer;
    procedure AddToBatch(batchRenderer: TBatchRenderer);
  end;

  // Аналогично для других примитивов
```

### 6. Обновление Drawer

Обновить `uzgldraweroglmodern.pas`:

```pascal
type
  TZGLOpenGL33Drawer = class(TZGLOpenGLDrawer)
    ShaderManager: TShaderManager;
    BatchRenderer: TBatchRenderer;
    MatrixManager: TMatrixManager;
    VBOManager: TVBOManager;
    StateManager: TOGLModernStateManager;

    procedure SetOGLMatrix(const cam:GDBObjCamera; const w,h:integer); override;
    procedure startrender(const mode:TRenderMode; var matrixs:tmatrixs); override;
    procedure endrender; override;

    // Все методы DrawLine, DrawTriangle и т.д. должны использовать батчинг
    procedure DrawLine(const PVertexBuffer:PZGLVertex3Sarray; const i1,i2:TLLVertexIndex); override;
  end;
```

## Алгоритмы и изменения по модулям

### Модуль 1: Shader Manager (`uzglshadermanager.pas`) - НОВЫЙ

**Назначение**: Управление компиляцией, линковкой и использованием шейдеров

**Алгоритмы**:
1. **CompileShader**
   - Загрузить исходный код шейдера
   - Создать shader object (`glCreateShader`)
   - Скомпилировать (`glCompileShader`)
   - Проверить ошибки (`glGetShaderiv`, `glGetShaderInfoLog`)
   - Вернуть ID или сообщить об ошибке

2. **LinkProgram**
   - Создать program object (`glCreateProgram`)
   - Присоединить шейдеры (`glAttachShader`)
   - Связать (`glLinkProgram`)
   - Проверить ошибки (`glGetProgramiv`, `glGetProgramInfoLog`)
   - Получить uniform locations (`glGetUniformLocation`)
   - Вернуть program ID

3. **UseProgram**
   - Переключиться на нужную программу (`glUseProgram`)
   - Кешировать текущую программу чтобы избежать лишних вызовов

### Модуль 2: Matrix Stack (`uzglmatrixstack.pas`) - НОВЫЙ

**Назначение**: Эмуляция matrix stack из OpenGL 1.x

**Алгоритмы**:
1. **Push**
   - Скопировать текущую матрицу
   - Добавить копию в стек
   - Увеличить счётчик

2. **Pop**
   - Уменьшить счётчик
   - Восстановить матрицу из стека
   - Пометить матрицы как "грязные" для обновления uniform

3. **Multiply**
   - Умножить текущую матрицу на переданную
   - Использовать существующие функции матричной алгебры из проекта

### Модуль 3: Batch Renderer (`uzglbatchrenderer.pas`) - НОВЫЙ

**Назначение**: Группировка примитивов для эффективной отрисовки

**Алгоритмы**:
1. **AddVertex**
   - Добавить вершину в текущий буфер
   - Преобразовать координаты если нужно (LCS)
   - Сохранить цвет, нормаль и другие атрибуты

2. **EndBatch**
   - Зафиксировать текущий батч
   - Сохранить информацию о типе примитива и количестве вершин

3. **Flush**
   - Загрузить все вершины в VBO (`glBufferData` или `glBufferSubData`)
   - Для каждого батча:
     - Установить нужную шейдерную программу
     - Обновить uniform переменные (матрицы, цвет)
     - Вызвать `glDrawArrays` или `glDrawElements`
   - Очистить буферы для следующего кадра

### Модуль 4: VBO Manager (`uzglvbomanager.pas`) - НОВЫЙ/РАСШИРЕНИЕ

**Назначение**: Управление VAO и VBO

**Алгоритмы**:
1. **CreateBuffers**
   ```
   - glGenVertexArrays(1, &VAOID)
   - glBindVertexArray(VAOID)
   - glGenBuffers(1, &VBOPosition)
   - glGenBuffers(1, &VBOColor)
   - glGenBuffers(1, &VBOIndex)
   ```

2. **BindVertexAttributes**
   ```
   - glBindBuffer(GL_ARRAY_BUFFER, VBOPosition)
   - glEnableVertexAttribArray(0)  // Position attribute
   - glVertexAttribPointer(0, 3, GL_FLOAT, false, stride, offset)

   - glBindBuffer(GL_ARRAY_BUFFER, VBOColor)
   - glEnableVertexAttribArray(1)  // Color attribute
   - glVertexAttribPointer(1, 4, GL_UNSIGNED_BYTE, true, stride, offset)
   ```

3. **UpdateBuffer**
   ```
   - glBindBuffer(GL_ARRAY_BUFFER, buffer)
   - glBufferData(GL_ARRAY_BUFFER, size, data, GL_DYNAMIC_DRAW)
   ```

### Модуль 5: OpenGL33 Drawer (`uzgldraweroglmodern.pas`) - СУЩЕСТВЕННЫЕ ИЗМЕНЕНИЯ

**Изменения**:
1. **Инициализация** - Создать все менеджеры (shader, matrix, batch, vbo)
2. **startrender**
   - Очистить батч рендерер
   - Установить матрицы view и projection через MatrixManager
   - Активировать нужную шейдерную программу

3. **DrawLine, DrawTriangle и т.д.**
   - НЕ рисовать немедленно
   - Добавить вершины в BatchRenderer
   - Батч рендерер накапливает примитивы

4. **endrender**
   - Вызвать `BatchRenderer.Flush()`
   - Это отрисует все накопленные примитивы одним вызовом

5. **SetColor**
   - Сохранить цвет в StateManager
   - Цвет будет использован при добавлении следующих вершин

6. **SetOGLMatrix**
   - Обновить MatrixManager
   - Матрицы будут переданы в шейдер при следующем flush

### Модуль 6: OpenGL33 ViewArea (`uzglviewareaoglmodern.pas`) - РАСШИРЕНИЕ

**Изменения**:
1. **getareacaps**
   - Проверить, что OpenGL >= 3.3
   - Если нет - вернуть ошибку или fallback на OpenGL 1.x
   - Инициализировать расширения: `Load_GL_version_3_3`

2. **CreateDrawer**
   - Создать `TZGLOpenGL33Drawer` вместо текущего
   - Инициализировать все подсистемы

### Модуль 7: Primitives Creator (`uzgprimitivescreatoroglmodern.pas`) - РАСШИРЕНИЕ

**Изменения**:
1. Добавить поддержку всех типов примитивов:
   - Lines (уже есть, доработать)
   - Triangles
   - Triangle Fans
   - Triangle Strips
   - Quads (эмулировать через два треугольника)
   - Points

2. Каждый примитив при отрисовке должен:
   - Получить вершины из геометрии
   - Добавить их в BatchRenderer
   - Сохранить ссылку на батч для оптимизации

### Модуль 8: State Manager (`uzgloglstatemanager.pas`) - ЧАСТИЧНОЕ ОБНОВЛЕНИЕ

**Что сохранить**:
- Кеширование состояний (linewidth, pointsize, colors)
- Логику минимизации GL вызовов

**Что изменить**:
- `myglBegin/myglEnd` - заменить на начало/конец батча
- `myglVertex*` - перенаправить в BatchRenderer
- `myglColor*` - сохранять в StateManager для следующих вершин
- Matrix функции - делегировать в MatrixManager

**Что удалить/адаптировать**:
- Immediate mode код
- Функции работы с matrix stack
- Line stipple (заменить на shader-based решение)

## Эмуляция удалённых возможностей

### 1. Line Stipple

OpenGL 3.3 Core не поддерживает `glLineStipple`. Решение:

**Вариант A**: Geometry Shader (OpenGL 3.3+)
- Генерировать точки вдоль линии
- Fragment shader проверяет позицию и отбрасывает пиксели

**Вариант B**: Текстура 1D
- Создать 1D текстуру с паттерном stipple
- В fragment shader семплировать текстуру по длине линии

**Вариант C**: Множественные сегменты (простой)
- Разбить пунктирную линию на короткие сегменты
- Рисовать только видимые сегменты

### 2. Polygon Stipple

Аналогично line stipple:
- Использовать 2D текстуру с паттерном
- В fragment shader отбрасывать пиксели по паттерну
- Uniform для включения/выключения

### 3. Selection с Stencil Buffer

Stencil операции работают в OpenGL 3.3, но нужна адаптация:
- `glStencilFunc` и `glStencilOp` остаются
- Рисование с проверкой stencil работает как раньше
- Изменения минимальны

### 4. Lighting

Полностью переделать на shader-based:

**Basic Phong Lighting в Vertex Shader**:
```glsl
uniform vec3 lightPosition;
uniform vec3 cameraPosition;

vec3 normal = normalize(inNormal);
vec3 lightDir = normalize(lightPosition - fragPosition);
vec3 viewDir = normalize(cameraPosition - fragPosition);

float diff = max(dot(normal, lightDir), 0.0);
vec3 diffuse = diff * lightColor;

vec3 reflectDir = reflect(-lightDir, normal);
float spec = pow(max(dot(viewDir, reflectDir), 0.0), shininess);
vec3 specular = spec * lightColor;

fragColor = vec4((ambient + diffuse + specular) * objectColor, 1.0);
```

## План поэтапного внедрения

### Фаза 1: Базовая инфраструктура (1-2 недели)

1. Создать shader manager
   - Базовые vertex/fragment шейдеры
   - Компиляция и линковка
   - Обработка ошибок

2. Создать matrix manager
   - Matrix stack эмуляция
   - Передача uniform в шейдеры

3. Создать VBO manager
   - VAO создание и binding
   - VBO для позиций и цветов
   - Vertex attribute setup

### Фаза 2: Batch Rendering (1-2 недели)

1. Реализовать batch renderer
   - Аккумуляция вершин
   - Flush механизм

2. Интегрировать с drawer
   - Переделать DrawLine
   - Переделать DrawTriangle
   - Базовая отрисовка работает

3. Тестирование базового рендеринга
   - Простые линии
   - Простые треугольники

### Фаза 3: Полные примитивы (1 неделя)

1. Реализовать все типы примитивов
   - Triangle fans
   - Triangle strips
   - Quads (как 2 треугольника)
   - Points

2. Обновить primitives creator
   - Поддержка всех типов
   - Оптимизация батчинга

### Фаза 4: Продвинутые возможности (2-3 недели)

1. Line stipple эмуляция
   - Geometry shader или текстурный подход
   - Интеграция с существующим кодом

2. Polygon stipple
   - Fragment shader с текстурой
   - Uniform для паттернов

3. Lighting
   - Vertex shader Phong lighting
   - Uniform для параметров света
   - Интеграция с существующей системой

4. Textures
   - Текстурные координаты в VBO
   - Sampler в fragment shader
   - Текстурный буфер для сохранения экрана

### Фаза 5: Оптимизация и полировка (1-2 недели)

1. Профилирование
   - Найти узкие места
   - Оптимизировать батчинг

2. Обработка ошибок
   - Fallback на OpenGL 1.x если 3.3 недоступен
   - Информативные сообщения об ошибках

3. Документация
   - Комментарии в коде
   - Руководство пользователя

4. Тестирование
   - Разные GPU
   - Разные ОС
   - Граничные случаи

## Карта изменений по файлам

### Новые файлы

1. `cad_source/zengine/zgl/openglmodern/uzglshadermanager.pas`
2. `cad_source/zengine/zgl/openglmodern/uzglmatrixstack.pas`
3. `cad_source/zengine/zgl/openglmodern/uzglbatchrenderer.pas`
4. `cad_source/zengine/zgl/openglmodern/uzglvbomanager.pas`
5. `cad_source/zengine/zgl/openglmodern/shaders/basic.vert`
6. `cad_source/zengine/zgl/openglmodern/shaders/basic.frag`
7. `cad_source/zengine/zgl/openglmodern/shaders/stipple.vert`
8. `cad_source/zengine/zgl/openglmodern/shaders/stipple.frag`
9. `cad_source/zengine/zgl/openglmodern/shaders/lighting.vert`
10. `cad_source/zengine/zgl/openglmodern/shaders/lighting.frag`

### Существенные изменения

1. `cad_source/zengine/zgl/openglmodern/uzgldraweroglmodern.pas`
   - Полная переработка
   - Добавить менеджеры
   - Батчинг вместо immediate mode

2. `cad_source/zengine/zgl/openglmodern/uzglviewareaoglmodern.pas`
   - Проверка версии OpenGL >= 3.3
   - Инициализация новых подсистем
   - Обработка ошибок

3. `cad_source/zengine/zgl/openglmodern/uzgprimitivescreatoroglmodern.pas`
   - Расширить типы примитивов
   - Интеграция с batch renderer

### Минорные изменения

1. `cad_source/zengine/zgl/common/uzgloglstatemanager.pas`
   - Адаптировать под новую архитектуру
   - Опциональное использование для OpenGL 3.3

2. `cad_source/zengine/zgl/common/uzgldrawerabstract.pas`
   - Возможно добавить новые методы
   - Обратная совместимость

## Потенциальные проблемы и решения

### Проблема 1: Производительность при большом количестве примитивов

**Решение**:
- Использовать persistent mapped buffers (OpenGL 4.4+) или buffer orphaning
- Разделить геометрию на пространственные блоки
- Использовать instancing для повторяющихся объектов

### Проблема 2: Совместимость со старым кодом

**Решение**:
- Сохранить API абстрактного drawer
- Внутренняя реализация другая, но интерфейс тот же
- Существующий код не нужно менять

### Проблема 3: Отладка шейдеров

**Решение**:
- Детальное логирование ошибок компиляции
- Возможность загрузки шейдеров из файлов для быстрого тестирования
- Fallback шейдеры при ошибке

### Проблема 4: Line stipple качество

**Решение**:
- Использовать geometry shader для точного контроля
- Если производительность плохая - fallback на texture-based решение
- Настраиваемое качество в зависимости от hardware

## Тестирование

### Unit Tests

1. Shader compilation
2. Matrix operations
3. Batch accumulation
4. VBO allocation

### Integration Tests

1. Отрисовка простых сцен
2. Сравнение результатов с OpenGL 1.x бэкендом
3. Stress test - большое количество примитивов

### Visual Tests

1. Скриншоты известных сцен
2. Сравнение попиксельно с reference
3. Проверка stipple паттернов
4. Проверка освещения

## Заключение

Внедрение OpenGL 3.3 в ZCAD требует существенной переработки подсистемы рендеринга, но благодаря модульной архитектуре проекта это выполнимо без нарушения существующего кода. Ключевые компоненты:

1. **Shader Manager** - для управления шейдерами
2. **Matrix Manager** - замена удалённого matrix stack
3. **Batch Renderer** - группировка примитивов
4. **VBO Manager** - управление буферами

Основные преимущества после внедрения:
- Лучшая производительность за счёт батчинга
- Поддержка современных GPU
- Возможность использования продвинутых эффектов
- Совместимость с будущими версиями OpenGL

Общая оценка трудозатрат: **6-10 недель** для полной реализации и тестирования.
