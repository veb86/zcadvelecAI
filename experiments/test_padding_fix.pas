program test_padding_fix;
{$Mode delphi}{$H+}

{ Standalone test: verify that the fixed TProxyByteStream correctly parses
  UnicodeText2 from mleader2007notwork.dxf, even though the preceding
  LWPOLYLINE command has size=53 (not multiple of 4) which breaks absolute
  alignment. }

uses
  SysUtils, Classes,
  uzeentproxystream;

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

procedure TestUnicodeText2Parse;
var
  HexData: string;
  Data: TBytes;
  Stream: TProxyByteStream;
  CmdSize, OpCode: Integer;
  ChunkSize, CmdCount, I: Integer;
  StartIdx: Integer;
  InsertX, InsertY, InsertZ: Double;
  Height, WFactor: Double;
  Text, TypeFace, FontName: string;
  IgnoreLen, Raw: Integer;
  Fl1, Fl2, Fl3, Fl4, Fl5: Cardinal;
  Bold, Italic, Charset, Pitch: Cardinal;
  NormalX, NormalY, NormalZ: Double;
  DirX, DirY, DirZ: Double;
  Oblique, Tracking: Double;
  BigFont: string;
begin
  HexData := ExtractProxyGraphicHex('../cad_source/test/mleader2007notwork.dxf',
    'MULTILEADER');
  Data := HexStringToBytes(HexData);
  WriteLn('Total data size: ', Length(Data));

  Stream := TProxyByteStream.Create(Data, True);
  try
    ChunkSize := Stream.ReadInt32;
    CmdCount := Stream.ReadInt32;
    WriteLn('ChunkSize=', ChunkSize, ' CmdCount=', CmdCount);
    for I := 1 to CmdCount do
    begin
      StartIdx := Stream.Index;
      CmdSize := Stream.ReadInt32;
      OpCode := Stream.ReadInt32;
      Stream.PaddingBase := Stream.Index; // <-- THE FIX
      WriteLn(Format('cmd#%d off=%d size=%d op=%d base=%d', [I, StartIdx, CmdSize, OpCode, Stream.PaddingBase]));
      if OpCode = 38 then
      begin
        InsertX := Stream.ReadDouble;
        InsertY := Stream.ReadDouble;
        InsertZ := Stream.ReadDouble;
        NormalX := Stream.ReadDouble;
        NormalY := Stream.ReadDouble;
        NormalZ := Stream.ReadDouble;
        DirX := Stream.ReadDouble;
        DirY := Stream.ReadDouble;
        DirZ := Stream.ReadDouble;
        WriteLn(Format('  Insert=(%.4f,%.4f,%.4f)', [InsertX, InsertY, InsertZ]));
        WriteLn(Format('  Normal=(%.4f,%.4f,%.4f)', [NormalX, NormalY, NormalZ]));
        WriteLn(Format('  Direction=(%.4f,%.4f,%.4f)', [DirX, DirY, DirZ]));
        Text := Stream.ReadPaddedUnicodeString;
        IgnoreLen := Stream.ReadInt32;
        Raw := Stream.ReadInt32;
        Height := Stream.ReadDouble;
        WFactor := Stream.ReadDouble;
        Oblique := Stream.ReadDouble;
        Tracking := Stream.ReadDouble;
        Fl1 := Stream.ReadUInt32;
        Fl2 := Stream.ReadUInt32;
        Fl3 := Stream.ReadUInt32;
        Fl4 := Stream.ReadUInt32;
        Fl5 := Stream.ReadUInt32;
        Bold := Stream.ReadUInt32;
        Italic := Stream.ReadUInt32;
        Charset := Stream.ReadUInt32;
        Pitch := Stream.ReadUInt32;
        TypeFace := Stream.ReadPaddedUnicodeString;
        FontName := Stream.ReadPaddedUnicodeString;
        BigFont := Stream.ReadPaddedUnicodeString;
        WriteLn('  Text=', Text);
        WriteLn(Format('  IgnoreLen=%d Raw=%d', [IgnoreLen, Raw]));
        WriteLn(Format('  Height=%.3f WFactor=%.3f', [Height, WFactor]));
        WriteLn(Format('  Flags=[%d,%d,%d,%d,%d] Bold=%d Italic=%d Charset=%d Pitch=%d',
          [Fl1, Fl2, Fl3, Fl4, Fl5, Bold, Italic, Charset, Pitch]));
        WriteLn('  TypeFace=', TypeFace);
        WriteLn('  FontName=', FontName);
        WriteLn('  BigFont=', BigFont);
        WriteLn(Format('  Consumed=%d / %d', [Stream.Index - StartIdx, CmdSize]));
      end;
      // skip to end of command
      while Stream.Index < StartIdx + CmdSize do
        Stream.Skip(1);
    end;
  finally
    Stream.Free;
  end;
end;

begin
  TestUnicodeText2Parse;
end.
