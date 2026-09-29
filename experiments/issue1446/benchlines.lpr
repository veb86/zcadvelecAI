program benchlines;
// issue #1446: оценка составляющих времени разбора OBJECTS: проход по строкам,
// выделение строк значений, хранение пар. Использование: benchlines <файл.dxf>
{$mode objfpc}{$H+}
uses SysUtils, Classes;
type
  TPair = record Code: Integer; Value: string; end;
var
  L: TStringList; B: TStringBuilder;
  Text, V: string; I, J, N, Start, Pos_, Len: Integer; T: QWord;
  Pairs: array of TPair;
begin
  L := TStringList.Create; L.LoadFromFile(ParamStr(1));
  J := 0;
  while (J + 3 < L.Count) and not ((Trim(L[J]) = '0') and (Trim(L[J + 1]) = 'SECTION')
    and (Trim(L[J + 3]) = 'OBJECTS')) do Inc(J);
  B := TStringBuilder.Create;
  for I := J to L.Count - 1 do B.Append(L[I]).Append(LineEnding);
  Text := B.ToString; B.Free; L.Free;
  Len := Length(Text);
  // 1. только проход по строкам
  T := GetTickCount64; N := 0; Pos_ := 1;
  while Pos_ <= Len do begin
    while (Pos_ <= Len) and (Text[Pos_] <> #10) do Inc(Pos_);
    Inc(Pos_); Inc(N);
  end;
  writeln('lines ', N, ': scan ', GetTickCount64 - T, ' ms');
  // 2. проход + Copy каждой второй строки
  T := GetTickCount64; Pos_ := 1; I := 0;
  while Pos_ <= Len do begin
    Start := Pos_;
    while (Pos_ <= Len) and (Text[Pos_] <> #10) do Inc(Pos_);
    if Odd(I) then V := Copy(Text, Start, Pos_ - Start);
    Inc(Pos_); Inc(I);
  end;
  writeln('scan+copy values ', GetTickCount64 - T, ' ms');
  // 3. проход + Copy + хранение пар
  T := GetTickCount64; Pos_ := 1; I := 0; SetLength(Pairs, 0); N := 0;
  while Pos_ <= Len do begin
    Start := Pos_;
    while (Pos_ <= Len) and (Text[Pos_] <> #10) do Inc(Pos_);
    if Odd(I) then begin
      if N >= Length(Pairs) then if Length(Pairs) < 8 then SetLength(Pairs, 8) else SetLength(Pairs, Length(Pairs) * 2);
      Pairs[N].Code := 1; Pairs[N].Value := Copy(Text, Start, Pos_ - Start); Inc(N);
    end;
    Inc(Pos_); Inc(I);
  end;
  writeln('scan+copy+store ', GetTickCount64 - T, ' ms');
  T := GetTickCount64; Pairs := nil;
  writeln('free pairs ', GetTickCount64 - T, ' ms');
end.
