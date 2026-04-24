program test_fix_inline;
{$Mode delphi}{$H+}

{ Simulates the fixed parser logic — what the real TProxyByteStream will
  do after we set PaddingBase per command. We read mleader2007notwork.dxf
  proxy graphic data and verify UNICODE_TEXT2 parses correctly (text,
  height, font). }

uses
  SysUtils, Classes;

type
  TBytes = array of Byte;

  TMyStream = class
  private
    FData: TBytes;
    FIndex: Integer;
    FLength: Integer;
    FUnicodeText: Boolean;
    FPaddingBase: Integer;
  public
    constructor Create(const Data: TBytes; Unicode: Boolean);
    function ReadInt32: Integer;
    function ReadUInt32: Cardinal;
    function ReadDouble: Double;
    function ReadVertex(out X, Y, Z: Double): Boolean;
    function ReadPaddedUnicodeString: UnicodeString;
    procedure Skip(Count: Integer);
    property Index: Integer read FIndex;
    property PaddingBase: Integer read FPaddingBase write FPaddingBase;
  end;

constructor TMyStream.Create(const Data: TBytes; Unicode: Boolean);
begin
  inherited Create;
  FData := Copy(Data, 0, Length(Data));
  FIndex := 0;
  FLength := Length(Data);
  FUnicodeText := Unicode;
  FPaddingBase := 0;
end;

function TMyStream.ReadInt32: Integer;
begin
  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TMyStream.ReadUInt32: Cardinal;
begin
  Move(FData[FIndex], Result, 4);
  Inc(FIndex, 4);
end;

function TMyStream.ReadDouble: Double;
begin
  Move(FData[FIndex], Result, 8);
  Inc(FIndex, 8);
end;

function TMyStream.ReadVertex(out X, Y, Z: Double): Boolean;
begin
  X := ReadDouble; Y := ReadDouble; Z := ReadDouble;
  Result := True;
end;

function TMyStream.ReadPaddedUnicodeString: UnicodeString;
var
  Len, RelIdx, Rem: Integer;
  Bytes: TBytes;
begin
  Result := '';
  if FUnicodeText then
  begin
    Len := 0;
    while (FIndex + Len * 2 + 1 < FLength) and
          ((FData[FIndex + Len * 2] <> 0) or (FData[FIndex + Len * 2 + 1] <> 0)) do
      Inc(Len);
    if Len > 0 then
    begin
      SetLength(Bytes, Len * 2);
      Move(FData[FIndex], Bytes[0], Len * 2);
      SetLength(Result, Len);
      Move(Bytes[0], Result[1], Len * 2);
      Inc(FIndex, Len * 2);
    end;
    if FIndex + 1 < FLength then
      Inc(FIndex, 2);
  end
  else
  begin
    Len := 0;
    while (FIndex + Len < FLength) and (FData[FIndex + Len] <> 0) do
      Inc(Len);
    if Len > 0 then
    begin
      SetLength(Result, Len);
      Move(FData[FIndex], Result[1], Len);
      Inc(FIndex, Len);
    end;
    if FIndex < FLength then
      Inc(FIndex, 1);
  end;

  { Alignment relative to FPaddingBase, not absolute }
  RelIdx := FIndex - FPaddingBase;
  Rem := RelIdx mod 4;
  if Rem <> 0 then
    Skip(4 - Rem);
end;

procedure TMyStream.Skip(Count: Integer);
begin
  if FIndex + Count > FLength then
    Count := FLength - FIndex;
  Inc(FIndex, Count);
end;

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
  InsideEntity, ExpectHexValue: Boolean;
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

procedure TestFile(const FileName: string);
var
  HexData: string;
  Data: TBytes;
  Stream: TMyStream;
  ChunkSize, CmdCount, CmdSize, OpCode, I: Integer;
  StartIdx: Integer;
  ix, iy, iz: Double;
  nx, ny, nz: Double;
  dx, dy, dz: Double;
  Height, WFactor, Oblique, Tracking: Double;
  Fl1, Fl2, Fl3, Fl4, Fl5: Cardinal;
  Bold, Italic, Charset, Pitch: Cardinal;
  IgnoreLen, Raw: Integer;
  Text, TypeFace, FontName, BigFont: UnicodeString;
  TextOpCode: Integer;
  SavedUnicode: Boolean;
begin
  WriteLn('=== ', FileName, ' ===');
  HexData := ExtractProxyGraphicHex(FileName, 'MULTILEADER');
  Data := HexStringToBytes(HexData);
  WriteLn('Data size=', Length(Data));

  // Determine text opcode and unicode flag from filename
  if Pos('2007', FileName) > 0 then
  begin
    TextOpCode := 38;
    SavedUnicode := True;
  end
  else
  begin
    TextOpCode := 11;
    SavedUnicode := False;
  end;

  Stream := TMyStream.Create(Data, SavedUnicode);
  try
    ChunkSize := Stream.ReadInt32;
    CmdCount := Stream.ReadInt32;
    for I := 1 to CmdCount do
    begin
      StartIdx := Stream.Index;
      CmdSize := Stream.ReadInt32;
      OpCode := Stream.ReadInt32;
      Stream.PaddingBase := Stream.Index; // <-- THE FIX

      if OpCode = TextOpCode then
      begin
        WriteLn(Format('  cmd#%d op=%d size=%d at offset %d (mod4=%d)',
          [I, OpCode, CmdSize, StartIdx, StartIdx mod 4]));
        Stream.ReadVertex(ix, iy, iz);
        Stream.ReadVertex(nx, ny, nz);
        Stream.ReadVertex(dx, dy, dz);
        if TextOpCode = 38 then
        begin
          Text := Stream.ReadPaddedUnicodeString;
          IgnoreLen := Stream.ReadInt32;
          Raw := Stream.ReadInt32;
        end
        else
        begin
          // OpCode=11 has length field AFTER text (ANSI, as DXF 2000)
          Text := Stream.ReadPaddedUnicodeString; // ANSI mode
          IgnoreLen := Stream.ReadInt32;
          Raw := Stream.ReadInt32;
        end;
        Height := Stream.ReadDouble;
        WFactor := Stream.ReadDouble;
        Oblique := Stream.ReadDouble;
        Tracking := Stream.ReadDouble;
        Fl1 := Stream.ReadUInt32;
        Fl2 := Stream.ReadUInt32;
        Fl3 := Stream.ReadUInt32;
        Fl4 := Stream.ReadUInt32;
        Fl5 := Stream.ReadUInt32;
        if TextOpCode = 38 then
        begin
          Bold := Stream.ReadUInt32;
          Italic := Stream.ReadUInt32;
          Charset := Stream.ReadUInt32;
          Pitch := Stream.ReadUInt32;
          TypeFace := Stream.ReadPaddedUnicodeString;
        end;
        FontName := Stream.ReadPaddedUnicodeString;
        BigFont := Stream.ReadPaddedUnicodeString;
        WriteLn('    Text=', String(Text));
        WriteLn('    IgnoreLen=', IgnoreLen, ' Raw=', Raw);
        WriteLn(Format('    Height=%.3f WFactor=%.3f', [Height, WFactor]));
        WriteLn(Format('    Flags=[%d,%d,%d,%d,%d]',
          [Fl1, Fl2, Fl3, Fl4, Fl5]));
        WriteLn('    TypeFace=', String(TypeFace));
        WriteLn('    FontName=', String(FontName));
        WriteLn('    BigFont=', String(BigFont));
        WriteLn(Format('    Consumed=%d / %d',
          [Stream.Index - StartIdx, CmdSize]));
      end;
      // Skip to next command
      while Stream.Index < StartIdx + CmdSize do
        Stream.Skip(1);
    end;
  finally
    Stream.Free;
  end;
  WriteLn;
end;

begin
  TestFile('../cad_source/test/mleader2000notwork.dxf');
  TestFile('../cad_source/test/mleader2007notwork.dxf');
end.
