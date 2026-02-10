program test_polyfacemesh;

{$MODE OBJFPC}{$H+}

uses
  Classes, SysUtils,
  uzeLog,
  uzeentpolyfacemesh,
  uzedrawingdef,
  uzctnrVectorBytesStream,
  uzMVReader;

var
  Drawing: TDrawingDef;
  Context: TIODXFLoadContext;
  MemReader: TZMemReader;
  FileStream: TFileStream;
  ByteArray: TBytes;
  PolyFaceMesh: GDBObjPolyFaceMesh;
  i: Integer;
  face: TFaceIndices;
begin
  // Инициализация логирования
  programlog := nil;
  
  writeln('Тестирование загрузки PolyFaceMesh...');
  
  try
    // Чтение DXF файла
    FileStream := TFileStream.Create('examples/polyfacemesh_example.dxf', fmOpenRead);
    SetLength(ByteArray, FileStream.Size);
    FileStream.Read(ByteArray[0], FileStream.Size);
    FileStream.Free;
    
    // Создание читателя
    MemReader := TZMemReader.Create(@ByteArray[0], Length(ByteArray));
    
    // Создание контекста загрузки
    Context := TIODXFLoadContext.Create;
    Context.GDBVertexLoadCache.init;
    
    // Создание PolyFaceMesh объекта
    PolyFaceMesh.init(nil, nil, 0);
    
    // Загрузка из DXF
    PolyFaceMesh.LoadFromDXF(MemReader, nil, Drawing, Context);
    
    // Вывод результатов
    writeln('Загружено вершин: ', PolyFaceMesh.GetVertexCount);
    writeln('Загружено граней: ', PolyFaceMesh.GetFaceCount);
    
    for i := 0 to PolyFaceMesh.GetFaceCount - 1 do begin
      face := PolyFaceMesh.GetFaceVertices(i);
      writeln('Грань ', i + 1, ': ', face.VertexCount, ' вершин');
      writeln('  Индексы: ', face.Vertex1, ', ', face.Vertex2, ', ', face.Vertex3, ', ', face.Vertex4);
    end;
    
    writeln('Тест завершен успешно!');
    
    Context.GDBVertexLoadCache.Done;
    Context.Free;
    MemReader.Free;
    
  except
    on E: Exception do begin
      writeln('Ошибка: ', E.Message);
      ExitCode := 1;
    end;
  end;
end.