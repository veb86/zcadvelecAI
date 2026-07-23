// uexpsvgblock.pas
unit uexpsvgblock;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uzeentity, uzeentblockinsert, uzeentline, uzeentcircle,
  uzeentarc, uzeentpolyline, uzeentlwpolyline, uzeentellipse, uzeentdevice, uzegeometrytypes, uzegeometry,
  uexpsvgtypes, uexpsvggeometry, uexpsvgwriter, uzcdrawings, uzeTypes,
  uzeblockdef, uzeconsts, uzcinterface;

type
  // Класс для экспорта блока в SVG
  TBlockSVGExporter = class
  private
    FWriter: TSVGWriter;
    FTransformer: TSVGTransformer;
    FGeometry: TSVGGeometryBuilder;

    // Обработка конкретных примитивов
    procedure ProcessLine(const Line: PGDBObjLine);
    procedure ProcessCircle(const Circle: PGDBObjCircle);
    procedure ProcessArc(const Arc: PGDBObjArc);
    procedure ProcessEllipse(const Ellipse: PGDBObjEllipse);
    procedure ProcessPolyline(const PL: PGDBObjPolyline);
    procedure ProcessLWPolyline(const LW: PGDBObjLWPolyline);
    procedure ProcessDevice(const Dev: PGDBObjDevice);

    // Проверка видимости слоя
    function IsEntityVisible(const Entity: PGDBObjEntity): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    // Основной метод экспорта
    function ExportBlock(const BlockInsert: PGDBObjBlockInsert;
                        const OutputFile: string): Boolean;
    function ExportDevice(const Device: PGDBObjDevice;
                        const OutputFile: string): Boolean;
  end;

implementation

constructor TBlockSVGExporter.Create;
begin
  inherited;
  FWriter := TSVGWriter.Create;
  FTransformer := TSVGTransformer.Create;
  FGeometry := TSVGGeometryBuilder.Create;
end;

destructor TBlockSVGExporter.Destroy;
begin
  FWriter.Free;
  FTransformer.Free;
  FGeometry.Free;
  inherited;
end;

function TBlockSVGExporter.IsEntityVisible(const Entity: PGDBObjEntity): Boolean;
begin
  // Проверка видимости слоя
  if Entity^.vp.Layer^._on = False then
    Result := False
  else
    Result := True;
end;

procedure TBlockSVGExporter.ProcessLine(const Line: PGDBObjLine);
var
  P1, P2: TSVGPoint;
begin
  if not IsEntityVisible(Line) then Exit;

  P1 := FTransformer.Transform(Line^.CoordInOCS.lBegin);
  P2 := FTransformer.Transform(Line^.CoordInOCS.lEnd);

  FWriter.AddLine(P1.X, P1.Y, P2.X, P2.Y);
  FGeometry.AddPoint(P1.X, P1.Y);
  FGeometry.AddPoint(P2.X, P2.Y);
end;

procedure TBlockSVGExporter.ProcessCircle(const Circle: PGDBObjCircle);
var
  Center: TSVGPoint;
  Radius: Double;
begin
  if not IsEntityVisible(Circle) then Exit;

  Center := FTransformer.Transform(Circle^.Local.P_insert);
  // Радиус масштабируем (упрощенно, без учета неравномерного масштаба)
  Radius := Circle^.Radius * FTransformer.Scale;

  FWriter.AddCircle(Center.X, Center.Y, Radius);
  FGeometry.AddPoint(Center.X - Radius, Center.Y - Radius);
  FGeometry.AddPoint(Center.X + Radius, Center.Y + Radius);
end;

procedure TBlockSVGExporter.ProcessArc(const Arc: PGDBObjArc);
var
  StartPt, EndPt: TSVGPoint;
  LargeArcFlag, SweepFlag: Integer;
  Center: TzePoint3d;
  StartAngle, EndAngle: Double;
  Radius: Double;
begin
  if not IsEntityVisible(Arc) then Exit;

  Center := Arc^.Local.P_insert;
  Radius := Arc^.R;
  StartAngle := Arc^.StartAngle;
  EndAngle := Arc^.EndAngle;

  // Получаем точки и флаги для SVG
  FTransformer.TransformArc(Center, Radius, StartAngle, EndAngle,
    StartPt, EndPt, LargeArcFlag, SweepFlag);

  FWriter.AddArcPath(StartPt.X, StartPt.Y, Radius,
    LargeArcFlag, SweepFlag, EndPt.X, EndPt.Y);

  FGeometry.AddPoint(StartPt.X, StartPt.Y);
  FGeometry.AddPoint(EndPt.X, EndPt.Y);
end;

procedure TBlockSVGExporter.ProcessEllipse(const Ellipse: PGDBObjEllipse);
var
  Center: TSVGPoint;
  MajorRadius, MinorRadius: Double;
  Rotation: Double;
  StartPt, EndPt: TSVGPoint;
  LargeArcFlag, SweepFlag: Integer;
begin
  if not IsEntityVisible(Ellipse) then Exit;

  // Центр эллипса в локальных координатах
  Center := FTransformer.Transform(Ellipse^.Local.P_insert);
  
  // Большая полуось (длина вектора MajorAxis)
  MajorRadius := oneVertexlength(Ellipse^.MajorAxis);
  // Малая полуось (через Ratio)
  MinorRadius := MajorRadius * Ellipse^.Ratio;
  
  // Угол поворота эллипса (угол большой оси)
  Rotation := ArcTan(Ellipse^.MajorAxis.y / Ellipse^.MajorAxis.x);
  
  // Для эллипса используем упрощенный подход - рисуем как path
  // SVG эллиптическая дуга требует сложных вычислений, поэтому
  // экспортируем как эллипс через scale transform
  
  // Если это полный эллипс (StartAngle=0, EndAngle=2*Pi)
  if (Abs(Ellipse^.StartAngle) < 0.0001) and (Abs(Ellipse^.EndAngle - 2*Pi) < 0.0001) then
  begin
    FWriter.AddEllipse(Center.X, Center.Y, MajorRadius * FTransformer.Scale, 
                       MinorRadius * FTransformer.Scale, Rotation);
  end
  else
  begin
    // Дуга эллипса - используем упрощенный подход через ArcPath
    FTransformer.TransformArc(Ellipse^.Local.P_insert, MajorRadius, 
      Ellipse^.StartAngle, Ellipse^.EndAngle,
      StartPt, EndPt, LargeArcFlag, SweepFlag);
    
    FWriter.AddArcPath(StartPt.X, StartPt.Y, MajorRadius * FTransformer.Scale,
      LargeArcFlag, SweepFlag, EndPt.X, EndPt.Y);
  end;
  
  FGeometry.AddPoint(Center.X - MajorRadius, Center.Y - MinorRadius);
  FGeometry.AddPoint(Center.X + MajorRadius, Center.Y + MinorRadius);
end;

procedure TBlockSVGExporter.ProcessPolyline(const PL: PGDBObjPolyline);
var
  Points: array of TSVGPoint;
  i: Integer;
  Vertex: PzePoint3d;
begin
  if not IsEntityVisible(PL) then Exit;

  if PL^.VertexArrayInOCS.Count < 2 then Exit;

  SetLength(Points, PL^.VertexArrayInOCS.Count);
  for i := 0 to PL^.VertexArrayInOCS.Count - 1 do
  begin
    Vertex := PL^.VertexArrayInOCS.getDataMutable(i);
    Points[i] := FTransformer.Transform(Vertex^);
    FGeometry.AddPoint(Points[i].X, Points[i].Y);
  end;

  FWriter.AddPolyline(Points);
end;

procedure TBlockSVGExporter.ProcessLWPolyline(const LW: PGDBObjLWPolyline);
var
  Points: array of TSVGPoint;
  i: Integer;
  Vertex2D: TzePoint2d;
  Vertex3D: TzePoint3d;
begin
  if not IsEntityVisible(LW) then Exit;

  if LW^.Vertex2D_in_OCS_Array.Count < 2 then Exit;

  SetLength(Points, LW^.Vertex2D_in_OCS_Array.Count);
  for i := 0 to LW^.Vertex2D_in_OCS_Array.Count - 1 do
  begin
    Vertex2D := LW^.Vertex2D_in_OCS_Array.getData(i);
    // Преобразуем 2D точку в 3D (Z=0)
    Vertex3D := CreateVertex(Vertex2D.x, Vertex2D.y, 0);
    Points[i] := FTransformer.Transform(Vertex3D);
    FGeometry.AddPoint(Points[i].X, Points[i].Y);
  end;

  FWriter.AddPolyline(Points);
  
  // Если полилиния замкнута, добавляем линию от последней к первой точке
  if LW^.Closed then
  begin
    FWriter.AddLine(Points[High(Points)].X, Points[High(Points)].Y,
                    Points[0].X, Points[0].Y);
  end;
end;

procedure TBlockSVGExporter.ProcessDevice(const Dev: PGDBObjDevice);
var
  BlockDef: PGDBObjBlockdef;
  Entity: PGDBObjEntity;
  i: Integer;
  EntityType: TObjID;
  OldTransformer: TSVGTransformer;
begin
  if not IsEntityVisible(Dev) then Exit;

  // Получаем определение блока устройства
  BlockDef := Dev^.PDef;
  if not Assigned(BlockDef) then Exit;

  // Сохраняем текущий трансформер
  OldTransformer := FTransformer;
  try
    // Настраиваем трансформер из матрицы устройства
    FTransformer := TSVGTransformer.Create;
    try
      FTransformer.SetFromBlockInsert(Dev^.objMatrix);

      // Перебираем примитивы в определении блока устройства
      for i := 0 to BlockDef^.ObjArray.Count - 1 do
      begin
        Entity := PGDBObjEntity(BlockDef^.ObjArray.getData(i));
        if not Assigned(Entity) then Continue;

        // Проверяем тип примитива
        EntityType := Entity^.GetObjType;

        case EntityType of
          GDBLineID:
            ProcessLine(PGDBObjLine(Entity));
          GDBCircleID:
            ProcessCircle(PGDBObjCircle(Entity));
          GDBArcID:
            ProcessArc(PGDBObjArc(Entity));
          GDBPolylineID:
            ProcessPolyline(PGDBObjPolyline(Entity));
          GDBLWPolylineID:
            ProcessLWPolyline(PGDBObjLWPolyline(Entity));
        end;
      end;
    finally
      FTransformer.Free;
      FTransformer := OldTransformer;
    end;
  except
    FTransformer := OldTransformer;
    raise;
  end;
end;

function TBlockSVGExporter.ExportBlock(const BlockInsert: PGDBObjBlockInsert;
  const OutputFile: string): Boolean;
var
  BlockDef: PGDBObjBlockdef;
  Entity: PGDBObjEntity;
  i: Integer;
  EntityType: Integer;
begin
  Result := False;

  try
    // Получаем определение блока по имени
    BlockDef := BlockInsert^.PDef;
    if not Assigned(BlockDef) then
    begin
      zcUI.TextMessage('Ошибка: определение блока не найдено', TMWOHistoryOut);
      Exit;
    end;

    // Сбрасываем трансформер в identity - экспортируем в локальных координатах блока
    FTransformer.SetFromBlockInsert(cOneMatrix);

    // Перебираем примитивы в определении блока (как в getPointConnector из uzvcom)
    // BlockDef.ObjArray содержит примитивы блока
    for i := 0 to BlockDef^.ObjArray.Count - 1 do
    begin
      Entity := PGDBObjEntity(BlockDef^.ObjArray.getData(i));
      if not Assigned(Entity) then Continue;

      // Проверяем тип примитива
      EntityType := Entity^.GetObjType;

      case EntityType of
        GDBLineID:
          ProcessLine(PGDBObjLine(Entity));
        GDBCircleID:
          ProcessCircle(PGDBObjCircle(Entity));
        GDBArcID:
          ProcessArc(PGDBObjArc(Entity));
        GDBEllipseID:
          ProcessEllipse(PGDBObjEllipse(Entity));
        GDBPolylineID:
          ProcessPolyline(PGDBObjPolyline(Entity));
        GDBLWPolylineID:
          ProcessLWPolyline(PGDBObjLWPolyline(Entity));
        GDBDeviceID:
          ProcessDevice(PGDBObjDevice(Entity));
        GDBBlockInsertID:
          begin
            // Вложенные блоки игнорируем согласно ТЗ
            // Можно добавить логирование
          end;
      else
        // Другие типы игнорируем
      end;
    end;
    
    // Устанавливаем границы и сохраняем
    if FGeometry.HasGeometry then
      FWriter.SetBounds(FGeometry.GetBounds)
    else
    begin
      // Пустой SVG с комментарием
      FWriter.SetBounds(FGeometry.GetBounds);
    end;

    Result := FWriter.SaveToFile(OutputFile);

  except
    on E: Exception do
    begin
      zcUI.TextMessage('Ошибка экспорта: ' + E.Message, TMWOHistoryOut);
      Result := False;
    end;
  end;
end;

function TBlockSVGExporter.ExportDevice(const Device: PGDBObjDevice;
  const OutputFile: string): Boolean;
var
  BlockDef: PGDBObjBlockdef;
  Entity: PGDBObjEntity;
  i: Integer;
  EntityType: TObjID;
begin
  Result := False;

  try
    // Получаем определение блока устройства
    BlockDef := Device^.PDef;
    if not Assigned(BlockDef) then
    begin
      zcUI.TextMessage('Ошибка: определение блока устройства не найдено', TMWOHistoryOut);
      Exit;
    end;

    // Сбрасываем трансформер в identity - экспортируем в локальных координатах устройства
    FTransformer.SetFromBlockInsert(cOneMatrix);

    // 1. Перебираем примитивы в определении блока устройства
    for i := 0 to BlockDef^.ObjArray.Count - 1 do
    begin
      Entity := PGDBObjEntity(BlockDef^.ObjArray.getData(i));
      if not Assigned(Entity) then Continue;

      // Проверяем тип примитива
      EntityType := Entity^.GetObjType;

      case EntityType of
        GDBLineID:
          ProcessLine(PGDBObjLine(Entity));
        GDBCircleID:
          ProcessCircle(PGDBObjCircle(Entity));
        GDBArcID:
          ProcessArc(PGDBObjArc(Entity));
        GDBEllipseID:
          ProcessEllipse(PGDBObjEllipse(Entity));
        GDBPolylineID:
          ProcessPolyline(PGDBObjPolyline(Entity));
        GDBLWPolylineID:
          ProcessLWPolyline(PGDBObjLWPolyline(Entity));
        GDBDeviceID:
          // Вложенные устройства игнорируем
          ;
        GDBBlockInsertID:
          begin
            // Вложенные блоки игнорируем согласно ТЗ
            // Можно добавить логирование
          end;
      else
        // Другие типы игнорируем
      end;
    end;

    // 2. Перебираем переменные объекты устройства (VarObjArray)
    for i := 0 to Device^.VarObjArray.Count - 1 do
    begin
      Entity := PGDBObjEntity(Device^.VarObjArray.getDataMutable(i));
      if not Assigned(Entity) then Continue;

      // Проверяем тип примитива
      EntityType := Entity^.GetObjType;

      case EntityType of
        GDBLineID:
          ProcessLine(PGDBObjLine(Entity));
        GDBCircleID:
          ProcessCircle(PGDBObjCircle(Entity));
        GDBArcID:
          ProcessArc(PGDBObjArc(Entity));
        GDBEllipseID:
          ProcessEllipse(PGDBObjEllipse(Entity));
        GDBPolylineID:
          ProcessPolyline(PGDBObjPolyline(Entity));
        GDBLWPolylineID:
          ProcessLWPolyline(PGDBObjLWPolyline(Entity));
      end;
    end;

    // Устанавливаем границы и сохраняем
    if FGeometry.HasGeometry then
      FWriter.SetBounds(FGeometry.GetBounds)
    else
    begin
      // Пустой SVG с комментарием
      FWriter.SetBounds(FGeometry.GetBounds);
    end;

    Result := FWriter.SaveToFile(OutputFile);

  except
    on E: Exception do
    begin
      zcUI.TextMessage('Ошибка экспорта: ' + E.Message, TMWOHistoryOut);
      Result := False;
    end;
  end;
end;

end.
