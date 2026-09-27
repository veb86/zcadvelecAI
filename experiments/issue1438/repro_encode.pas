{ Воспроизведение issue #1438: чертёж, загруженный из DWG, имеет
  DXFCodePage=ZCCPINVALID. При сохранении в DXF2000 заголовок получает
  $DWGCODEPAGE=ANSI_1251 (ZCCP2Str), а строки перекодируются в 1252
  (ZCCodePage2SysCP) -> кириллица превращается в '?'. }
program repro_encode;
{$Mode objfpc}{$H+}
uses
  {$IFDEF UNIX}cwstring,{$ENDIF}SysUtils;

{ Копия логики dxfEncodeString из uzeffdxfsupport.pas }
function EncodeForDXF2000(const v:String;TargetCP:TSystemCodePage):RawByteString;
var
  ts:RawByteString;
begin
  ts:=v;
  SetCodePage(ts,CP_UTF8,false);
  SetCodePage(ts,TargetCP,true);
  Result:=ts;
end;

var
  s:String;
  r:RawByteString;
begin
  s:='Слой Текст';
  r:=EncodeForDXF2000(s,1252);
  WriteLn('ZCCPINVALID -> 1252: ', r, ' (header says ANSI_1251)');
  r:=EncodeForDXF2000(s,1251);
  SetCodePage(r,CP_UTF8,true);
  WriteLn('ZCCP1251    -> 1251: ', r);
end.
