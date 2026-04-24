program test_parse_text_fix;
{$Mode delphi}{$H+}

{ Test to confirm: padding alignment after strings must be relative to
  the COMMAND start, not the absolute stream index. The DXF 2007 mleader
  contains LWPOLYLINE with size 53 (not multiple of 4), which breaks
  absolute alignment for subsequent commands including UNICODE_TEXT2. }

uses
  SysUtils, Classes;

type
  TBytes = array of Byte;

function HexStringToBytes(const Hex: string): TBytes;
var
  I: Integer;
begin
  SetLength(Result, Length(Hex) div 2);
  for I := 0 to High(Result) do
    Result[I] := StrToInt('$' + Copy(Hex, I * 2 + 1, 2));
end;

function ExtractProxyGraphicHex(const FileName, EntityName: string): string;
var
  Lines: TStringList;
  I: Integer;
  InsideEntity: Boolean;
  ExpectHexValue: Boolean;
begin
  Result := '';
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    InsideEntity := False;
    ExpectHexValue := False;
    I := 0;
    while I < Lines.Count do
    begin
      if not InsideEntity then
      begin
        if (Trim(Lines[I]) = '0') and (I + 1 < Lines.Count)
          and (Trim(Lines[I + 1]) = EntityName) then
        begin
          InsideEntity := True;
          Inc(I, 2);
          Continue;
        end;
      end
      else
      begin
        if ExpectHexValue then
        begin
          Result := Result + Trim(Lines[I]);
          ExpectHexValue := False;
        end
        else if Trim(Lines[I]) = '310' then
          ExpectHexValue := True
        else if (Trim(Lines[I]) = '100') and (I + 1 < Lines.Count)
          and (Trim(Lines[I + 1]) = 'AcDbMLeader') then
          Break;
      end;
      Inc(I);
    end;
  finally
    Lines.Free;
  end;
end;

var
  RelativeBase: Integer = 0;

function ReadInt32(const Data: TBytes; var Idx: Integer): Integer;
begin
  Move(Data[Idx], Result, 4);
  Inc(Idx, 4);
end;

function ReadUInt32(const Data: TBytes; var Idx: Integer): Cardinal;
begin
  Move(Data[Idx], Result, 4);
  Inc(Idx, 4);
end;

function ReadDouble(const Data: TBytes; var Idx: Integer): Double;
begin
  Move(Data[Idx], Result, 8);
  Inc(Idx, 8);
end;

function ReadPaddedUnicodeString(const Data: TBytes; var Idx: Integer): UnicodeString;
var
  Len: Integer;
  Bytes: TBytes;
  RelIdx: Integer;
begin
  Len := 0;
  while (Idx + Len * 2 + 1 < Length(Data)) and
        ((Data[Idx + Len * 2] <> 0) or (Data[Idx + Len * 2 + 1] <> 0)) do
    Inc(Len);

  SetLength(Bytes, Len * 2);
  if Len > 0 then
    Move(Data[Idx], Bytes[0], Len * 2);
  SetLength(Result, Len);
  if Len > 0 then
    Move(Bytes[0], Result[1], Len * 2);
  Inc(Idx, Len * 2 + 2);
  // Relative to RelativeBase (start of command payload)
  RelIdx := Idx - RelativeBase;
  while (RelIdx mod 4) <> 0 do
  begin
    Inc(Idx);
    Inc(RelIdx);
  end;
end;

procedure DecodeUnicodeText2(const Data: TBytes; Offset, Size: Integer);
var
  Idx: Integer;
  px, py, pz, nx, ny, nz, dx, dy, dz: Double;
  h, wf, ob, tr: Double;
  fl1, fl2, fl3, fl4, fl5, bold, italic, charset, pitch: Cardinal;
  txt, tf, fn, bf: UnicodeString;
begin
  Idx := Offset + 8;
  RelativeBase := Idx; // Base for alignment — start of payload
  px := ReadDouble(Data, Idx);
  py := ReadDouble(Data, Idx);
  pz := ReadDouble(Data, Idx);
  nx := ReadDouble(Data, Idx);
  ny := ReadDouble(Data, Idx);
  nz := ReadDouble(Data, Idx);
  dx := ReadDouble(Data, Idx);
  dy := ReadDouble(Data, Idx);
  dz := ReadDouble(Data, Idx);
  WriteLn(Format('  Insert=(%.4f,%.4f,%.4f)', [px, py, pz]));

  txt := ReadPaddedUnicodeString(Data, Idx);
  WriteLn('  Text=', string(txt));
  ReadInt32(Data, Idx); // IgnoreLen
  ReadInt32(Data, Idx); // Raw
  h := ReadDouble(Data, Idx);
  wf := ReadDouble(Data, Idx);
  ob := ReadDouble(Data, Idx);
  tr := ReadDouble(Data, Idx);
  WriteLn(Format('  H=%.3f WF=%.3f', [h, wf]));
  fl1 := ReadUInt32(Data, Idx);
  fl2 := ReadUInt32(Data, Idx);
  fl3 := ReadUInt32(Data, Idx);
  fl4 := ReadUInt32(Data, Idx);
  fl5 := ReadUInt32(Data, Idx);
  WriteLn(Format('  Flags=[%d %d %d %d %d]', [fl1, fl2, fl3, fl4, fl5]));
  bold := ReadUInt32(Data, Idx);
  italic := ReadUInt32(Data, Idx);
  charset := ReadUInt32(Data, Idx);
  pitch := ReadUInt32(Data, Idx);
  WriteLn(Format('  Bold=%d Italic=%d Charset=%d Pitch=%d',
    [bold, italic, charset, pitch]));
  tf := ReadPaddedUnicodeString(Data, Idx);
  fn := ReadPaddedUnicodeString(Data, Idx);
  bf := ReadPaddedUnicodeString(Data, Idx);
  WriteLn('  TypeFace=', string(tf));
  WriteLn('  FontName=', string(fn));
  WriteLn('  BigFont=', string(bf));
  WriteLn(Format('  Consumed=%d / %d', [Idx - Offset, Size]));
end;

var
  HexData: string;
  Data: TBytes;
  Offset: Integer;
begin
  HexData := ExtractProxyGraphicHex('../cad_source/test/mleader2007notwork.dxf',
    'MULTILEADER');
  Data := HexStringToBytes(HexData);
  WriteLn('Data length: ', Length(Data));

  // We know UNICODE_TEXT2 is at offset 649, size 200
  Offset := 649;
  WriteLn('UNICODE_TEXT2 at offset ', Offset, ' (mod 4 = ',
    Offset mod 4, ')');
  DecodeUnicodeText2(Data, Offset, 200);
end.
