unit fpdwg_libredwg_utils;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils,
  dwg,
  fpdwg_types,
  fpdwg_logger;

function DWGHandleValue(const Handle: Dwg_Handle): TDWGHandle;
function HandleRefFromBitCode(const Ref: BITCODE_H): TDWGHandleRef;
function DWGObjectTypeName(ObjType: DWG_OBJECT_TYPE): string;
function DWGVersionFromLibre(Version: DWG_VERSION_TYPE): TDWGVersion; overload;
function DWGVersionFromLibre(Version, FromVersion: DWG_VERSION_TYPE): TDWGVersion; overload;
function SafeDecodeText(P: PAnsiChar; Codepage: Integer;
  Logger: IDWGLogger): string;
function SafeDecodeLibreDWGText(P: PAnsiChar; Codepage: Integer;
  Logger: IDWGLogger): string;

implementation

const
  CP1251_80_TO_BF: array[$80..$BF] of Word = (
    $0402, $0403, $201A, $0453, $201E, $2026, $2020, $2021,
    $20AC, $2030, $0409, $2039, $040A, $040C, $040B, $040F,
    $0452, $2018, $2019, $201C, $201D, $2022, $2013, $2014,
    0,     $2122, $0459, $203A, $045A, $045C, $045B, $045F,
    $00A0, $040E, $045E, $0408, $00A4, $0490, $00A6, $00A7,
    $0401, $00A9, $0404, $00AB, $00AC, $00AD, $00AE, $0407,
    $00B0, $00B1, $0406, $0456, $0491, $00B5, $00B6, $00B7,
    $0451, $2116, $0454, $00BB, $0458, $0405, $0455, $0457
  );

function DWGHandleValue(const Handle: Dwg_Handle): TDWGHandle;
begin
  Result := Handle.value;
end;

function HandleRefFromBitCode(const Ref: BITCODE_H): TDWGHandleRef;
begin
  if Ref = nil then
    Exit(TDWGHandleRef.Null);

  if Ref^.absolute_ref <> 0 then
  begin
    Result.Value := Ref^.absolute_ref;
    Result.Source := hsAbsoluteRef;
  end
  else if Ref^.handleref.value <> 0 then
  begin
    Result.Value := Ref^.handleref.value;
    Result.Source := hsHandleref;
  end
  else
    Result := TDWGHandleRef.Null;
end;

function DWGObjectTypeName(ObjType: DWG_OBJECT_TYPE): string;
begin
  WriteStr(Result, ObjType);
  if Result = '' then
    Result := Format('DWG_OBJECT_TYPE(%d)', [Ord(ObjType)]);
end;

function DWGVersionFromLibre(Version: DWG_VERSION_TYPE): TDWGVersion; overload;
begin
  case Version of
    R_13b1, R_13b2, R_13, R_13c3:
      Result := dvR13;
    R_14:
      Result := dvR14;
    R_2000b, R_2000, R_2000i, R_2002:
      Result := dvR2000;
    R_2004a, R_2004b, R_2004c, R_2004:
      Result := dvR2004;
    R_2007a, R_2007b, R_2007:
      Result := dvR2007;
    R_2010b, R_2010:
      Result := dvR2010;
    R_2013b, R_2013:
      Result := dvR2013;
    R_2018b, R_2018:
      Result := dvR2018;
    R_INVALID:
      Result := dvInvalid;
  else
    if Version > R_2018 then
      Result := dvAfter
    else
      Result := dvInvalid;
  end;
end;

function DWGVersionFromLibre(Version, FromVersion: DWG_VERSION_TYPE): TDWGVersion; overload;
begin
  Result := DWGVersionFromLibre(Version);
  if Result = dvInvalid then
    Result := DWGVersionFromLibre(FromVersion);
end;

function CStringToRaw(P: PAnsiChar): RawByteString;
var
  L: SizeUInt;
begin
  if P = nil then
    Exit('');

  L := StrLen(P);
  SetString(Result, P, SizeInt(L));
end;

function LooksLikeUTF16LE(P: PAnsiChar): Boolean;
begin
  Result := (P <> nil) and (P[0] <> #0) and (P[1] = #0);
end;

function UTF16LELength(P: PAnsiChar): Integer;
begin
  Result := 0;
  if P = nil then
    Exit;

  while (P[Result * 2] <> #0) or (P[Result * 2 + 1] <> #0) do
    Inc(Result);
end;

function RawBytesToHex(const Raw: RawByteString): string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(Raw) do
  begin
    if I > 1 then
      Result := Result + ' ';
    Result := Result + IntToHex(Ord(Raw[I]), 2);
  end;
end;

procedure WarnDecodeFallback(Logger: IDWGLogger; Codepage: Integer);
begin
  if Logger <> nil then
    Logger.Warning(1400,
      Format('Text decoding fallback used for codepage %d', [Codepage]));
end;

function HexFallback(const Raw: RawByteString; Codepage: Integer;
  Logger: IDWGLogger): string;
begin
  WarnDecodeFallback(Logger, Codepage);
  Result := '<hex:' + RawBytesToHex(Raw) + '>';
end;

function IsValidUTF8(const Raw: RawByteString): Boolean;
var
  I, L: Integer;
  B1, B2, B3, B4: Byte;
begin
  Result := False;
  I := 1;
  L := Length(Raw);

  while I <= L do
  begin
    B1 := Ord(Raw[I]);

    if B1 <= $7F then
      Inc(I)
    else if (B1 >= $C2) and (B1 <= $DF) then
    begin
      if I + 1 > L then
        Exit;
      B2 := Ord(Raw[I + 1]);
      if not ((B2 >= $80) and (B2 <= $BF)) then
        Exit;
      Inc(I, 2);
    end
    else if B1 = $E0 then
    begin
      if I + 2 > L then
        Exit;
      B2 := Ord(Raw[I + 1]);
      B3 := Ord(Raw[I + 2]);
      if not ((B2 >= $A0) and (B2 <= $BF) and
              (B3 >= $80) and (B3 <= $BF)) then
        Exit;
      Inc(I, 3);
    end
    else if ((B1 >= $E1) and (B1 <= $EC)) or
            ((B1 >= $EE) and (B1 <= $EF)) then
    begin
      if I + 2 > L then
        Exit;
      B2 := Ord(Raw[I + 1]);
      B3 := Ord(Raw[I + 2]);
      if not ((B2 >= $80) and (B2 <= $BF) and
              (B3 >= $80) and (B3 <= $BF)) then
        Exit;
      Inc(I, 3);
    end
    else if B1 = $ED then
    begin
      if I + 2 > L then
        Exit;
      B2 := Ord(Raw[I + 1]);
      B3 := Ord(Raw[I + 2]);
      if not ((B2 >= $80) and (B2 <= $9F) and
              (B3 >= $80) and (B3 <= $BF)) then
        Exit;
      Inc(I, 3);
    end
    else if B1 = $F0 then
    begin
      if I + 3 > L then
        Exit;
      B2 := Ord(Raw[I + 1]);
      B3 := Ord(Raw[I + 2]);
      B4 := Ord(Raw[I + 3]);
      if not ((B2 >= $90) and (B2 <= $BF) and
              (B3 >= $80) and (B3 <= $BF) and
              (B4 >= $80) and (B4 <= $BF)) then
        Exit;
      Inc(I, 4);
    end
    else if (B1 >= $F1) and (B1 <= $F3) then
    begin
      if I + 3 > L then
        Exit;
      B2 := Ord(Raw[I + 1]);
      B3 := Ord(Raw[I + 2]);
      B4 := Ord(Raw[I + 3]);
      if not ((B2 >= $80) and (B2 <= $BF) and
              (B3 >= $80) and (B3 <= $BF) and
              (B4 >= $80) and (B4 <= $BF)) then
        Exit;
      Inc(I, 4);
    end
    else if B1 = $F4 then
    begin
      if I + 3 > L then
        Exit;
      B2 := Ord(Raw[I + 1]);
      B3 := Ord(Raw[I + 2]);
      B4 := Ord(Raw[I + 3]);
      if not ((B2 >= $80) and (B2 <= $8F) and
              (B3 >= $80) and (B3 <= $BF) and
              (B4 >= $80) and (B4 <= $BF)) then
        Exit;
      Inc(I, 4);
    end
    else
      Exit;
  end;

  Result := True;
end;

procedure AppendUTF8CodePoint(var S: string; CodePoint: Cardinal);
begin
  if CodePoint <= $7F then
    S := S + Chr(CodePoint)
  else if CodePoint <= $7FF then
  begin
    S := S + Chr($C0 or (CodePoint shr 6));
    S := S + Chr($80 or (CodePoint and $3F));
  end
  else if CodePoint <= $FFFF then
  begin
    S := S + Chr($E0 or (CodePoint shr 12));
    S := S + Chr($80 or ((CodePoint shr 6) and $3F));
    S := S + Chr($80 or (CodePoint and $3F));
  end
  else
  begin
    S := S + Chr($F0 or (CodePoint shr 18));
    S := S + Chr($80 or ((CodePoint shr 12) and $3F));
    S := S + Chr($80 or ((CodePoint shr 6) and $3F));
    S := S + Chr($80 or (CodePoint and $3F));
  end;
end;

function UTF16LEBytesToUTF8(P: PAnsiChar; CharCount: Integer;
  out Text: string): Boolean;
var
  I: Integer;
  CodePoint: Cardinal;
begin
  Result := False;
  Text := '';
  if (P = nil) or (CharCount < 0) then
    Exit;

  for I := 0 to CharCount - 1 do
  begin
    CodePoint := Ord(P[I * 2]) or (Ord(P[I * 2 + 1]) shl 8);
    if CodePoint = 0 then
      Exit;
    AppendUTF8CodePoint(Text, CodePoint);
  end;

  Result := True;
end;

function CP1251ByteToCodePoint(B: Byte): Cardinal;
begin
  if B < $80 then
    Exit(B);
  if B <= $BF then
    Exit(CP1251_80_TO_BF[B]);
  Result := $0410 + (B - $C0);
end;

function TryCP1251ToUTF8(const Raw: RawByteString; out Text: string): Boolean;
var
  I: Integer;
  CodePoint: Cardinal;
begin
  Result := False;
  Text := '';
  for I := 1 to Length(Raw) do
  begin
    CodePoint := CP1251ByteToCodePoint(Ord(Raw[I]));
    if CodePoint = 0 then
      Exit;
    AppendUTF8CodePoint(Text, CodePoint);
  end;
  Result := True;
end;

function SafeDecodeText(P: PAnsiChar; Codepage: Integer;
  Logger: IDWGLogger): string;
var
  Raw: RawByteString;
begin
  Raw := CStringToRaw(P);
  if Raw = '' then
    Exit('');

  if Codepage = 1251 then
  begin
    if TryCP1251ToUTF8(Raw, Result) then
      Exit;
    Exit(HexFallback(Raw, Codepage, Logger));
  end;

  if (Codepage = 65001) or (Codepage = 0) then
  begin
    if IsValidUTF8(Raw) then
      Exit(string(Raw));
    Exit(HexFallback(Raw, Codepage, Logger));
  end;

  if IsValidUTF8(Raw) then
    Result := string(Raw)
  else
    Result := HexFallback(Raw, Codepage, Logger);
end;

function SafeDecodeLibreDWGText(P: PAnsiChar; Codepage: Integer;
  Logger: IDWGLogger): string;
var
  CharCount: Integer;
begin
  if P = nil then
    Exit('');

  if LooksLikeUTF16LE(P) then
  begin
    CharCount := UTF16LELength(P);
    if UTF16LEBytesToUTF8(P, CharCount, Result) then
      Exit;
  end;

  Result := SafeDecodeText(P, Codepage, Logger);
end;

end.
