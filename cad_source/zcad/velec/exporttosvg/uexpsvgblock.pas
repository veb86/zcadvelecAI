// uexpsvgblock.pas
unit uexpsvgblock;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uzeentity, uzeentblockinsert, uzeentline, uzeentcircle,
  uzeentarc, uzeentpolyline, uzegeometrytypes, uzegeometry,
  uexpsvgtypes, uexpsvggeometry, uexpsvgwriter, uzcdrawings;

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
    procedure ProcessPolyline(const PL: PGDBObjPolyline);
    
    // Проверка видимости слоя
    function IsEntityVisible(const Entity: PGDBObjEntity): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    
    // Основной метод экспорта
    function ExportBlock(const BlockInsert: PGDBObjBlockInsert; 
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
  if Entity.vp.Layer^._on = False then
    Result := False
  else
    Result := True;
end;

procedure TBlockSVGExporter.ProcessLine(const Line: PGDBObjLine);
var
  P1, P2: TSVGPoint;
begin
  if not IsEntityVisible(Line) then Exit;
  
  P1 := FTransformer.Transform(Line.CoordInWCS.lBegin);
  P2 := FTransformer.Transform(Line.CoordInWCS.lEnd);
  
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

  Center := FTransformer.Transform(Circle.Center);
  // Радиус масштабируем (упрощенно, без учета неравномерного масштаба)
  Radius := Circle.Radius * FTransformer.Scale;

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

  Center := Arc.Center;
  Radius := Arc.Radius;
  StartAngle := Arc.StartAngle;
  EndAngle := Arc.EndAngle;
  
  // Получаем точки и флаги для SVG
  FTransformer.TransformArc(Center, Radius, StartAngle, EndAngle,
    StartPt, EndPt, LargeArcFlag, SweepFlag);
    
  FWriter.AddArcPath(StartPt.X, StartPt.Y, Radius, 
    LargeArcFlag, SweepFlag, EndPt.X, EndPt.Y);
    
  FGeometry.AddPoint(StartPt.X, StartPt.Y);
  FGeometry.AddPoint(EndPt.X, EndPt.Y);
end;

procedure TBlockSVGExporter.ProcessPolyline(const PL: PGDBObjPolyline);
var
  Points: array of TSVGPoint;
  i: Integer;
  Vertex: PzePoint3d;
begin
  if not IsEntityVisible(PL) then Exit;

  if PL.VertexArrayInWCS.Count < 2 then Exit;

  SetLength(Points, PL.VertexArrayInWCS.Count);
  for i := 0 to PL.VertexArrayInWCS.Count - 1 do
  begin
    Vertex := PL.VertexArrayInWCS.getDataMutable(i);
    Points[i] := FTransformer.Transform(Vertex^);
    FGeometry.AddPoint(Points[i].X, Points[i].Y);
  end;

  FWriter.AddPolyline(Points);
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
    BlockDef := BlockInsert.BlockDef;
    if not Assigned(BlockDef) then
    begin
      ZCMsgCallBackInterface.TextMessage('Ошибка: определение блока не найдено', TMsgType.SMError);
      Exit;
    end;
    
    // Настраиваем трансформер из матрицы вставки
    FTransformer.SetFromBlockInsert(BlockInsert.Matrix);
    
    // Перебираем примитивы в определении блока (как в getPointConnector из uzvcom)
    // BlockDef.ObjArray содержит примитивы блока
    for i := 0 to BlockDef.ObjArray.Count - 1 do
    begin
      Entity := PGDBObjEntity(BlockDef.ObjArray.getData(i));
      if not Assigned(Entity) then Continue;
      
      // Проверяем тип примитива
      EntityType := Entity.GetType;
      
      case EntityType of
        GDBLineID: 
          ProcessLine(PGDBObjLine(Entity));
        GDBCircleID: 
          ProcessCircle(PGDBObjCircle(Entity));
        GDBArcID: 
          ProcessArc(PGDBObjArc(Entity));
        GDBPolylineID: 
          ProcessPolyline(PGDBObjPolyline(Entity));
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
      FWriter.FContent.Add('    <!-- Нет поддерживаемых примитивов для экспорта -->');
    end;
    
    Result := FWriter.SaveToFile(OutputFile);
    
  except
    on E: Exception do
    begin
      ZCMsgCallBackInterface.TextMessage('Ошибка экспорта: ' + E.Message, TMsgType.SMError);
      Result := False;
    end;
  end;
end;

end.
