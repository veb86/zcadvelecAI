// uexpsvggeometry.pas
unit uexpsvggeometry;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uzegeometrytypes, uzegeometry, uexpsvgtypes, math;

type
  // Класс для преобразования координат ZCAD в SVG
  TSVGTransformer = class
  private
    FOffsetX, FOffsetY: Double;
    FScale: Double;
    FFlipY: Boolean; // В SVG Y направлена вниз, в ZCAD - вверх
  public
    constructor Create;
    // Установка трансформации из матрицы вставки блока
    procedure SetFromBlockInsert(const Matrix: TzeTypedMatrix4D);
    // Преобразование точки
    function Transform(const Pt: TzePoint3d): TSVGPoint;
    // Преобразование дуги: центр, радиус, начальный угол, конечный угол (в радианах)
    // Возвращает начальную точку, конечную точку и флаги для SVG Path
    procedure TransformArc(const Center: TzePoint3d; Radius: Double;
                          StartAngle, EndAngle: Double;
                          out StartPt, EndPt: TSVGPoint;
                          out LargeArcFlag, SweepFlag: Integer);
    // Свойство для доступа к масштабу
    property Scale: Double read FScale;
  end;

implementation

constructor TSVGTransformer.Create;
begin
  inherited;
  FOffsetX := 0;
  FOffsetY := 0;
  FScale := 1.0;
  FFlipY := True; // Инвертируем Y для SVG
end;

procedure TSVGTransformer.SetFromBlockInsert(const Matrix: TzeTypedMatrix4D);
begin
  // Извлекаем смещение из матрицы 4x4
  // TzeMatrix4d.v - это массив строк (TzeVector4d), каждая строка имеет поля x,y,z,w
  FOffsetX := Matrix.mtr.v[3].x;
  FOffsetY := Matrix.mtr.v[3].y;
  // Масштаб можно извлечь из матрицы, если нужно
  FScale := 1.0; // Упрощенно, можно доработать для неравномерного масштаба
end;

function TSVGTransformer.Transform(const Pt: TzePoint3d): TSVGPoint;
begin
  // Применяем матрицу трансформации блока
  Result.X := (Pt.x + FOffsetX) * FScale;
  if FFlipY then
    Result.Y := -(Pt.y + FOffsetY) * FScale
  else
    Result.Y := (Pt.y + FOffsetY) * FScale;
end;

procedure TSVGTransformer.TransformArc(const Center: TzePoint3d; Radius: Double;
  StartAngle, EndAngle: Double; out StartPt, EndPt: TSVGPoint;
  out LargeArcFlag, SweepFlag: Integer);
var
  C: TSVGPoint;
  AngleDiff: Double;
begin
  C := Transform(Center);
  
  // Начальная точка дуги
  StartPt.X := C.X + Radius * Cos(StartAngle);
  StartPt.Y := C.Y + Radius * Sin(StartAngle) * (-1); // Учет flip Y
  
  // Конечная точка дуги  
  EndPt.X := C.X + Radius * Cos(EndAngle);
  EndPt.Y := C.Y + Radius * Sin(EndAngle) * (-1); // Учет flip Y
  
  // Определяем флаги для SVG Path
  AngleDiff := EndAngle - StartAngle;
  // Нормализация угла
  while AngleDiff < 0 do AngleDiff := AngleDiff + 2*Pi;
  while AngleDiff > 2*Pi do AngleDiff := AngleDiff - 2*Pi;
  
  LargeArcFlag := IfThen(Abs(AngleDiff) > Pi, 1, 0);
  
  // В ZCAD и SVG разное направление углов (CW vs CCW)
  // В SVG sweep-flag=1 - по часовой стрелке
  // В ZCAD обычно против часовой от оси X
  SweepFlag := 1; // Нужно проверить на практике, возможно 0
end;

end.