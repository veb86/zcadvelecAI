// uexpsvgwriter.pas
unit uexpsvgwriter;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uexpsvgtypes;

type
  // Простой writer для SVG без интерфейсов
  TSVGWriter = class
  private
    FContent: TStringList;
    FBounds: TSVGRect;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Установка границ для viewBox
    procedure SetBounds(const Bounds: TSVGRect);
    
    // Примитивы SVG
    procedure AddLine(const X1, Y1, X2, Y2: Double; const Stroke: string = 'black');
    procedure AddCircle(const CX, CY, R: Double; const Stroke: string = 'black'; Fill: string = 'none');
    
    // Дуга через Path (A command)
    // rx, ry - радиусы, x-axis-rotation - поворот эллипса,
    // large-arc-flag, sweep-flag, x, y - конечная точка
    procedure AddArcPath(const StartX, StartY, Radius: Double;
                        const LargeArcFlag, SweepFlag: Integer;
                        const EndX, EndY: Double;
                        const Stroke: string = 'black');
    
    // Полилиния
    procedure AddPolyline(const Points: array of TSVGPoint; const Stroke: string = 'black');
    
    // Сохранение
    function SaveToFile(const FileName: string): Boolean;
    function GetSVGText: string;
  end;

implementation

constructor TSVGWriter.Create;
begin
  inherited;
  FContent := TStringList.Create;
  FBounds.MinX := 0; FBounds.MinY := 0;
  FBounds.MaxX := 100; FBounds.MaxY := 100;
end;

destructor TSVGWriter.Destroy;
begin
  FContent.Free;
  inherited;
end;

procedure TSVGWriter.SetBounds(const Bounds: TSVGRect);
begin
  FBounds := Bounds;
  // Добавляем небольшой отступ
  FBounds.MinX := FBounds.MinX - 1;
  FBounds.MinY := FBounds.MinY - 1;
  FBounds.MaxX := FBounds.MaxX + 1;
  FBounds.MaxY := FBounds.MaxY + 1;
end;

procedure TSVGWriter.AddLine(const X1, Y1, X2, Y2: Double; const Stroke: string);
begin
  FContent.Add(Format('    <line x1="%f" y1="%f" x2="%f" y2="%f" stroke="%s" />',
    [X1, Y1, X2, Y2, Stroke]));
end;

procedure TSVGWriter.AddCircle(const CX, CY, R: Double; const Stroke: string; Fill: string);
begin
  FContent.Add(Format('    <circle cx="%f" cy="%f" r="%f" stroke="%s" fill="%s" />',
    [CX, CY, R, Stroke, Fill]));
end;

procedure TSVGWriter.AddArcPath(const StartX, StartY, Radius: Double;
  const LargeArcFlag, SweepFlag: Integer; const EndX, EndY: Double;
  const Stroke: string);
var
  PathData: string;
begin
  // M - move to start, A - arc to end
  PathData := Format('M %f %f A %f %f 0 %d %d %f %f',
    [StartX, StartY, Radius, Radius, LargeArcFlag, SweepFlag, EndX, EndY]);
    
  FContent.Add(Format('    <path d="%s" stroke="%s" fill="none" />',
    [PathData, Stroke]));
end;

procedure TSVGWriter.AddPolyline(const Points: array of TSVGPoint; const Stroke: string);
var
  i: Integer;
  PointsStr: string;
begin
  if Length(Points) < 2 then Exit;
  
  PointsStr := '';
  for i := 0 to High(Points) do
  begin
    if i > 0 then PointsStr := PointsStr + ' ';
    PointsStr := PointsStr + Format('%f,%f', [Points[i].X, Points[i].Y]);
  end;
  
  FContent.Add(Format('    <polyline points="%s" stroke="%s" fill="none" />',
    [PointsStr, Stroke]));
end;

function TSVGWriter.SaveToFile(const FileName: string): Boolean;
var
  TempList: TStringList;
  i: Integer;
begin
  try
    TempList := TStringList.Create;
    try
      // Добавляем header
      TempList.Add('<?xml version="1.0" encoding="UTF-8" standalone="no"?>');
      TempList.Add('<svg xmlns="http://www.w3.org/2000/svg" xmlns:zcad="https://github.com/zamtmn/zcad" version="1.1">');
      TempList.Add(Format('  <g transform="translate(%f, %f)">',
        [-FBounds.MinX, -FBounds.MinY]));

      // Добавляем содержимое
      for i := 0 to FContent.Count - 1 do
        TempList.Add(FContent[i]);

      // Добавляем footer
      TempList.Add('  </g>');
      TempList.Add('</svg>');

      TempList.SaveToFile(FileName);
    finally
      TempList.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TSVGWriter.GetSVGText: string;
begin
  Result := FContent.Text;
end;

end.