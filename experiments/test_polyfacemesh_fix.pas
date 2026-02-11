program test_polyfacemesh_fix;

{$MODE OBJFPC}{$H+}

uses
  Classes, SysUtils,
  uzeentpolyfacemesh,
  uzeentity,
  uzegeometrytypes,
  uzegeometry,
  uzctnrVectorBytes,
  uzMVReader,
  uzedrawingdef,
  uzeffdxfsupport,
  uzclog;

var
  MeshObj: GDBObjPolyFaceMesh;
  Drawing: TDrawingDef;
  Context: TIODXFLoadContext;
  Reader: TZMemReader;
  Stream: TMemoryStream;
  dxfData: string;
  i: Integer;
begin
  // Создаем тестовые данные DXF для PolyFaceMesh
  dxfData := 
    '  0' + #13#10 +
    'POLYLINE' + #13#10 +
    '  8' + #13#10 +
    '0' + #13#10 +
    ' 70' + #13#10 +
    '    64' + #13#10 +
    ' 71' + #13#10 +
    '    4' + #13#10 +
    ' 72' + #13#10 +
    '    1' + #13#10 +
    '  0' + #13#10 +
    'VERTEX' + #13#10 +
    '  8' + #13#10 +
    '0' + #13#10 +
    ' 10' + #13#10 +
    '2460.4724' + #13#10 +
    ' 20' + #13#10 +
    '388.9392' + #13#10 +
    ' 30' + #13#10 +
    '818.7112' + #13#10 +
    '  0' + #13#10 +
    'VERTEX' + #13#10 +
    '  8' + #13#10 +
    '0' + #13#10 +
    ' 10' + #13#10 +
    '2460.4724' + #13#10 +
    ' 20' + #13#10 +
    '1908.9392' + #13#10 +
    ' 30' + #13#10 +
    '925.0' + #13#10 +
    '  0' + #13#10 +
    'VERTEX' + #13#10 +
    '  8' + #13#10 +
    '0' + #13#10 +
    ' 10' + #13#10 +
    '360.4724' + #13#10 +
    ' 20' + #13#10 +
    '1908.9392' + #13#10 +
    ' 30' + #13#10 +
    '925.0' + #13#10 +
    '  0' + #13#10 +
    'VERTEX' + #13#10 +
    '  8' + #13#10 +
    '0' + #13#10 +
    ' 10' + #13#10 +
    '360.4724' + #13#10 +
    ' 20' + #13#10 +
    '388.9392' + #13#10 +
    ' 30' + #13#10 +
    '818.7112' + #13#10 +
    '  0' + #13#10 +
    'VERTEX' + #13#10 +
    '  8' + #13#10 +
    '0' + #13#10 +
    ' 70' + #13#10 +
    '   128' + #13#10 +
    ' 71' + #13#10 +
    '    1' + #13#10 +
    ' 72' + #13#10 +
    '    2' + #13#10 +
    ' 73' + #13#10 +
    '    3' + #13#10 +
    ' 74' + #13#10 +
    '    4' + #13#10 +
    '  0' + #13#10 +
    'SEQEND' + #13#10;

  try
    writeln('Тестирование исправления PolyFaceMesh...');
    writeln;
    
    // Создаем поток и ридер
    Stream := TMemoryStream.Create;
    Stream.Write(dxfData[1], Length(dxfData));
    Stream.Position := 0;
    
    Reader := TZMemReader.Create(Stream);
    
    // Инициализируем объекты
    MeshObj.init(nil, nil, 0);
    
    // Тестируем загрузку
    writeln('Загрузка PolyFaceMesh из DXF данных...');
    MeshObj.LoadFromDXF(Reader, nil, Drawing, Context);
    
    writeln;
    writeln('Результаты загрузки:');
    writeln('Количество вершин: ', MeshObj.GetVertexCount);
    writeln('Количество граней: ', MeshObj.GetFaceCount);
    
    writeln;
    writeln('Координаты вершин:');
    for i := 0 to MeshObj.VertexArrayInOCS.Count - 1 do begin
      writeln('Вершина ', i + 1, ': (', 
              MeshObj.VertexArrayInOCS.PArray[i].x:8:4, ', ',
              MeshObj.VertexArrayInOCS.PArray[i].y:8:4, ', ',
              MeshObj.VertexArrayInOCS.PArray[i].z:8:4, ')');
    end;
    
    writeln;
    writeln('Грани:');
    for i := 0 to MeshObj.GetFaceCount - 1 do begin
      var Face := MeshObj.GetFaceVertices(i);
      writeln('Грань ', i + 1, ': ', Face.VertexCount, ' вершин - ',
              Face.Vertex1, ', ', Face.Vertex2, ', ', Face.Vertex3, ', ', Face.Vertex4);
    end;
    
    // Проверяем результаты
    writeln;
    if MeshObj.GetVertexCount = 4 then
      writeln('✓ Количество вершин верное')
    else
      writeln('✗ Ошибка в количестве вершин: ожидается 4, получено ', MeshObj.GetVertexCount);
      
    if MeshObj.GetFaceCount = 1 then
      writeln('✓ Количество граней верное')
    else
      writeln('✗ Ошибка в количестве граней: ожидается 1, получено ', MeshObj.GetFaceCount);
      
    // Проверяем первую вершину
    if (MeshObj.VertexArrayInOCS.Count > 0) and
       (Abs(MeshObj.VertexArrayInOCS.PArray[0].x - 2460.4724) < 0.001) and
       (Abs(MeshObj.VertexArrayInOCS.PArray[0].y - 388.9392) < 0.001) and
       (Abs(MeshObj.VertexArrayInOCS.PArray[0].z - 818.7112) < 0.001) then
      writeln('✓ Первая вершина загружена корректно')
    else
      writeln('✗ Ошибка в координатах первой вершины');
      
  except
    on E: Exception do begin
      writeln('Ошибка при выполнении теста: ', E.Message);
    end;
  end;
  
  writeln;
  writeln('Тест завершен.');
end.