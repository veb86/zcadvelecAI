program test_polyfacemesh_fix;
{$Mode delphi}

// Минимальный тест для проверки исправления ошибки компиляции
type
  TzePoint3d = record
    x, y, z: Double;
  end;

  TZMemReader = record
    // Заглушка для теста
  end;

// Функция, которая вызывала ошибку - ожидает Single
function dxfLoadGroupCodeFloat(var rdr:TZMemReader;DXFCode,CurrentDXFCode:Integer; var v:Single):Boolean;
begin
  Result := False;
end;

// Функция, которую мы используем для исправления - работает с TzePoint3d
function dxfLoadGroupCodeVertex(var rdr:TZMemReader;const DXFCode,CurrentDXFCode:Integer; var v:TzePoint3d):Boolean;
begin
  Result := False;
end;

var
  rdr: TZMemReader;
  byt: Integer;
  currentVertex: TzePoint3d;
  x: Single;
  
begin
  writeln('Тест исправления ошибки компиляции...');
  
  // Этот код вызывал ошибку: Got "Double" expected "Single"
  // if dxfLoadGroupCodeFloat(rdr,10,byt,currentVertex.x) then begin
  
  // Наше исправление - используем dxfLoadGroupCodeVertex
  if dxfLoadGroupCodeVertex(rdr,10,byt,currentVertex) then begin
    writeln('Компиляция успешна!');
  end;
  
  writeln('Ошибка компиляции исправлена.');
end.