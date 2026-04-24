program test_parse_text;
{$Mode delphi}{$H+}

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
  while (Idx mod 4) <> 0 do
    Inc(Idx);
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
  WriteLn(Format('  Normal=(%.4f,%.4f,%.4f)', [nx, ny, nz]));
  WriteLn(Format('  Direction=(%.4f,%.4f,%.4f)', [dx, dy, dz]));

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

procedure DecodeText2(const Data: TBytes; Offset, Size: Integer);
var
  Idx: Integer;
  px, py, pz, nx, ny, nz, dx, dy, dz: Double;
  h, wf, ob, tr: Double;
  fl1, fl2, fl3, fl4, fl5: Cardinal;
  LenField, Raw: Integer;
  Text: AnsiString;
  FontName, BigFont: AnsiString;
  SLen: Integer;
begin
  Idx := Offset + 8;
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
  // ANSI string padded to 4
  SLen := 0;
  while (Idx + SLen < Length(Data)) and (Data[Idx + SLen] <> 0) do
    Inc(SLen);
  SetLength(Text, SLen);
  if SLen > 0 then
    Move(Data[Idx], Text[1], SLen);
  Inc(Idx, SLen + 1);
  while (Idx mod 4) <> 0 do
    Inc(Idx);
  WriteLn('  Text=', Text);

  LenField := ReadInt32(Data, Idx);
  Raw := ReadInt32(Data, Idx);
  WriteLn(Format('  Length=%d Raw=%d', [LenField, Raw]));
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

  // FontFilename
  SLen := 0;
  while (Idx + SLen < Length(Data)) and (Data[Idx + SLen] <> 0) do
    Inc(SLen);
  SetLength(FontName, SLen);
  if SLen > 0 then Move(Data[Idx], FontName[1], SLen);
  Inc(Idx, SLen + 1);
  while (Idx mod 4) <> 0 do Inc(Idx);
  WriteLn('  FontFilename=', FontName);

  SLen := 0;
  while (Idx + SLen < Length(Data)) and (Data[Idx + SLen] <> 0) do
    Inc(SLen);
  SetLength(BigFont, SLen);
  if SLen > 0 then Move(Data[Idx], BigFont[1], SLen);
  Inc(Idx, SLen + 1);
  while (Idx mod 4) <> 0 do Inc(Idx);
  WriteLn('  BigFont=', BigFont);
  WriteLn(Format('  Consumed=%d / %d', [Idx - Offset, Size]));
end;

procedure AnalyzeFile(const FileName: string);
var
  HexData: string;
  Data: TBytes;
  Offset, CmdSize, CmdOp, I: Integer;
  ChunkSize, CmdCount: Integer;
begin
  WriteLn('=== ', FileName, ' ===');
  HexData := ExtractProxyGraphicHex(FileName, 'MULTILEADER');
  WriteLn('Hex length: ', Length(HexData));
  Data := HexStringToBytes(HexData);
  WriteLn('Data length: ', Length(Data));
  Offset := 0;
  ChunkSize := ReadInt32(Data, Offset);
  CmdCount := ReadInt32(Data, Offset);
  WriteLn('ChunkSize=', ChunkSize, ' CommandCount=', CmdCount);
  I := 0;
  while Offset + 8 <= Length(Data) do
  begin
    CmdSize := ReadInt32(Data, Offset);
    CmdOp := ReadInt32(Data, Offset);
    Offset := Offset - 8; // rewind to start of cmd
    Inc(I);
    WriteLn(Format('cmd#%d off=%d size=%d op=%d', [I, Offset, CmdSize, CmdOp]));
    if (CmdOp = 38) then
    begin
      WriteLn('  UNICODE_TEXT2:');
      DecodeUnicodeText2(Data, Offset, CmdSize);
    end
    else if (CmdOp = 11) then
    begin
      WriteLn('  TEXT2:');
      DecodeText2(Data, Offset, CmdSize);
    end;
    Inc(Offset, CmdSize);
  end;
  WriteLn;
end;

begin
  AnalyzeFile('../cad_source/test/mleader2000notwork.dxf');
  AnalyzeFile('../cad_source/test/mleader2007notwork.dxf');
end.
