program test_parse_text_trace;
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

var
  HexData: string;
  Data: TBytes;
  I, CmdStart: Integer;
  Payload: string;
begin
  HexData := ExtractProxyGraphicHex('../cad_source/test/mleader2007notwork.dxf',
    'MULTILEADER');
  Data := HexStringToBytes(HexData);
  WriteLn('Total data size: ', Length(Data));

  // We know cmd#23 op=38 starts at offset 649, size=200
  CmdStart := 649;
  WriteLn('cmd#23 UNICODE_TEXT2 starts at offset ', CmdStart);
  WriteLn('cmd header 8 bytes, payload 192 bytes');
  Payload := '';
  for I := CmdStart to CmdStart + 199 do
    Payload := Payload + IntToHex(Data[I], 2) + ' ';
  WriteLn('Raw bytes:');
  WriteLn(Payload);
  WriteLn;

  // Pull out just after header + 72 bytes (9 doubles)
  WriteLn('At text start (offset ', CmdStart + 8 + 72, '):');
  Payload := '';
  for I := CmdStart + 8 + 72 to CmdStart + 8 + 72 + 31 do
    Payload := Payload + IntToHex(Data[I], 2) + ' ';
  WriteLn(Payload);
  WriteLn;

  // Scan for null double-word
  WriteLn('Analyzing text area byte by byte starting at offset ',
    CmdStart + 8 + 72, ':');
  for I := 0 to 23 do
  begin
    WriteLn(Format('  +%d: byte 0x%s (%d) -> ',
      [I, IntToHex(Data[CmdStart + 8 + 72 + I], 2),
       Data[CmdStart + 8 + 72 + I]]),
      Char(Data[CmdStart + 8 + 72 + I]));
  end;
end.
