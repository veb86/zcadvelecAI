{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
  Модуль: uzeentproxystream
  Назначение: Поток байтов для чтения Proxy Graphic данных
  На основе анализа ezdxf (Python) - ByteStream класс
}

unit uzeentproxystream;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzeTypes,
  uzeGeometryTypes;

type
  { Поток байтов для чтения (аналог ByteStream из ezdxf) }
  TProxyByteStream = class
  private
    FData: TBytes;
    FIndex: Integer;
    FLength: Integer;
  public
    constructor Create(const Data: TBytes);

    { Чтение примитивов }
    function ReadInt32: Integer;
    function ReadUInt32: Cardinal;
    function ReadInt16: SmallInt;
    function ReadUInt16: Word;
    function ReadDouble: Double;
    function ReadFloat: Single;
    function ReadByte: Byte;
    function ReadBoolean: Boolean;

    { Чтение вершин и векторов }
    function ReadVertex: TzePoint3d;  // 3 doubles (24 байта)
    function ReadVector: TzePoint3d;  // 3 doubles (24 байта)
    function ReadPoint2D: TzePoint2d; // 2 doubles (16 байт)

    { Чтение строк }
    function ReadString(Encoding: TEncoding): string;
    function ReadPaddedString(Encoding: TEncoding): string;
    function ReadUnicodeString: string;
    function ReadPaddedUnicodeString: string;

    { Чтение структур }
    function ReadStruct(const Format: string): TArray<Double>;

    { Пропуск байтов }
    procedure Skip(Count: Integer);

    { Проверки }
    function EndOfStream: Boolean;
    function RemainingBytes: Integer;

    { Свойства }
    property Index: Integer read FIndex;
    property Length: Integer read FLength;
    property Data: TBytes read FData;
  end;

implementation

{ === TProxyByteStream === }

constructor TProxyByteStream.Create(const Data: TBytes);
begin
  inherited Create;
  FData := Copy(Data, 0, system.Length(Data));
  FIndex := 0;
  FLength := system.Length(Data);
end;

function TProxyByteStream.ReadInt32: Integer;
begin
  if FIndex + 4 > FLength then
    raise Exception.CreateFmt('ReadInt32: End of stream (index=%d, length=%d)', [FIndex, FLength]);

  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TProxyByteStream.ReadUInt32: Cardinal;
begin
  if FIndex + 4 > FLength then
    raise Exception.Create('ReadUInt32: End of stream');

  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TProxyByteStream.ReadInt16: SmallInt;
begin
  if FIndex + 2 > FLength then
    raise Exception.Create('ReadInt16: End of stream');

  Move(FData[FIndex], Result, 2);
  Inc(FIndex, 2);
end;

function TProxyByteStream.ReadUInt16: Word;
begin
  if FIndex + 2 > FLength then
    raise Exception.Create('ReadUInt16: End of stream');

  Move(FData[FIndex], Result, 2);
  Inc(FIndex, 2);
end;

function TProxyByteStream.ReadDouble: Double;
begin
  if FIndex + 8 > FLength then
    raise Exception.Create('ReadDouble: End of stream');

  Move(FData[FIndex], Result, 8);
  Inc(FIndex, 8);
end;

function TProxyByteStream.ReadFloat: Single;
begin
  if FIndex + 4 > FLength then
    raise Exception.Create('ReadFloat: End of stream');

  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TProxyByteStream.ReadByte: Byte;
begin
  if FIndex + 1 > FLength then
    raise Exception.Create('ReadByte: End of stream');

  Result := FData[FIndex];
  Inc(FIndex);
end;

function TProxyByteStream.ReadBoolean: Boolean;
begin
  Result := ReadByte <> 0;
end;

function TProxyByteStream.ReadVertex: TzePoint3d;
begin
  Result.X := ReadDouble;
  Result.Y := ReadDouble;
  Result.Z := ReadDouble;
end;

function TProxyByteStream.ReadVector: TzePoint3d;
begin
  Result := ReadVertex;
end;

function TProxyByteStream.ReadPoint2D: TzePoint2d;
begin
  Result.X := ReadDouble;
  Result.Y := ReadDouble;
end;

function TProxyByteStream.ReadString(Encoding: TEncoding): string;
var
  I, Len: Integer;
  Bytes: TBytes;
begin
  // Читаем до нулевого байта
  Len := 0;
  while (FIndex + Len < FLength) and (FData[FIndex + Len] <> 0) do
    Inc(Len);

  if Len > 0 then begin
    SetLength(Bytes, Len);
    Move(FData[FIndex], Bytes[0], Len);
    try
      Result := Encoding.GetString(Bytes);
    except
      Result := '';
    end;
  end else
    Result := '';

  Inc(FIndex, Len + 1); // +1 для нулевого терминатора
end;

function TProxyByteStream.ReadPaddedString(Encoding: TEncoding): string;
var
  Len, PaddedLen: Integer;
  Bytes: TBytes;
begin
  // Читаем длину + паддинг (как в AutoCAD)
  Len := ReadInt32;
  PaddedLen := ReadInt32;

  if Len > 0 then begin
    SetLength(Bytes, Len);
    Move(FData[FIndex], Bytes[0], Len);
    try
      Result := Encoding.GetString(Bytes);
    except
      Result := '';
    end;
    Inc(FIndex, Len);
  end else
    Result := '';

  // Пропускаем паддинг
  if PaddedLen > Len then
    Skip(PaddedLen - Len);
end;

function TProxyByteStream.ReadUnicodeString: string;
var
  Len: Integer;
  WBytes: TBytes;
begin
  // Читаем до нулевого слова (UTF-16)
  Len := 0;
  while (FIndex + Len * 2 + 1 < FLength) and
        ((FData[FIndex + Len * 2] <> 0) or (FData[FIndex + Len * 2 + 1] <> 0)) do
    Inc(Len);

  if Len > 0 then begin
    SetLength(WBytes, Len * 2);
    Move(FData[FIndex], WBytes[0], Len * 2);
    try
      Result := TEncoding.Unicode.GetString(WBytes);
    except
      Result := '';
    end;
    Inc(FIndex, Len * 2);
  end else
    Result := '';

  Inc(FIndex, 2); // +2 для нулевого терминатора
end;

function TProxyByteStream.ReadPaddedUnicodeString: string;
var
  Len: Integer;
  WBytes: TBytes;
begin
  // Читаем UTF-16 строку с нулевым терминатором (как в AcGiWorldDraw)
  Len := 0;
  while (FIndex + Len * 2 + 1 < FLength) and
        ((FData[FIndex + Len * 2] <> 0) or (FData[FIndex + Len * 2 + 1] <> 0)) do
    Inc(Len);

  if Len > 0 then begin
    SetLength(WBytes, Len * 2);
    Move(FData[FIndex], WBytes[0], Len * 2);
    try
      Result := TEncoding.Unicode.GetString(WBytes);
    except
      Result := '';
    end;
    Inc(FIndex, Len * 2);
  end else
    Result := '';

  // Пропускаем нулевой терминатор (2 байта)
  if FIndex + 1 < FLength then
    Inc(FIndex, 2);

  // Выравниваем по 4 байтам (паддинг до границы DWORD)
  if (FIndex mod 4) <> 0 then
    Skip(4 - (FIndex mod 4));
end;

function TProxyByteStream.ReadStruct(const Format: string): TArray<Double>;
var
  I: Integer;
  Count: Integer;
begin
  Count := system.Length(Format);
  SetLength(Result, Count);

  for I := 1 to Count do begin
    case Format[I] of
      'd': Result[I-1] := ReadDouble;
      'f': Result[I-1] := ReadFloat;
      'i': Result[I-1] := ReadInt32;
      'w': Result[I-1] := ReadUInt16;
      'b': Result[I-1] := ReadByte;
    end;
  end;
end;

procedure TProxyByteStream.Skip(Count: Integer);
begin
  if FIndex + Count > FLength then
    Count := FLength - FIndex;
  Inc(FIndex, Count);
end;

function TProxyByteStream.EndOfStream: Boolean;
begin
  Result := FIndex >= FLength;
end;

function TProxyByteStream.RemainingBytes: Integer;
begin
  Result := FLength - FIndex;
end;

end.
