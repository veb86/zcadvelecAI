{ Регрессионный тест issue #1438: после чтения DWG/DXF через LibreDWG
  и сохранения в DXF2000 кириллица превращалась в '?'.
  Проверяется:
  - кодовая страница из заголовка DWG переносится в DXFCodePage чертежа;
  - для чертежа без кодовой страницы (ZCCPINVALID) заголовок $DWGCODEPAGE
    и фактическая перекодировка строк совпадают;
  - строка UTF-8 кодируется в DXF2000 без потери символов.
  Сборка: MODE=objfpc experiments/issue1438/build_units.sh <этот файл> }
program test_dwg_codepage;

{$Mode objfpc}{$H+}

uses
  {$IFDEF UNIX}cwstring,{$ENDIF}
  SysUtils,
  uzeTypes,
  uzeffdxfsupport,
  uzedwgcodepage;

var
  Failures:Integer=0;

{ Выводит результат проверки и считает ошибки }
procedure Check(Cond:Boolean;const Name:String);
begin
  if Cond then
    WriteLn('PASS ',Name)
  else begin
    WriteLn('FAIL ',Name);
    Inc(Failures);
  end;
end;

{ Кодирует строку для DXF2000 так же, как uzeffdxfout для чертежа
  с кодовой страницей ACP }
function EncodeDXF2000(const S:String;ACP:TZCCodePage):RawByteString;
var
  Hdr:TDXFHeaderInfo;
begin
  Hdr.iVersion:=1015;
  Hdr.iDWGCodePage:=ZCCodePage2SysCP(ZCCodePageOrDefault(ACP));
  Result:=dxfEncodeString(S,Hdr);
end;

{ Строит ожидаемые байты строки в кодовой странице CP }
function Expected(const S:String;CP:TSystemCodePage):RawByteString;
begin
  Result:=S;
  SetCodePage(Result,CP_UTF8,false);
  SetCodePage(Result,CP,true);
end;

const
  Text='Слой Текст';
begin
  { Как в конфигурации ZCAD: новые чертежи в ANSI_1251 }
  sysvarSysDWG_CodePage:=ZCCP1251;

  Check(DWGHeaderCodePageToZCCodePage(29)=ZCCP1251,
    'LibreDWG ANSI_1251 (29) -> ZCCP1251');
  Check(DWGHeaderCodePageToZCCodePage(30)=ZCCP1252,
    'LibreDWG ANSI_1252 (30) -> ZCCP1252');
  Check(DWGHeaderCodePageToZCCodePage(1251)=ZCCP1251,
    'Windows 1251 -> ZCCP1251');
  Check(DWGHeaderCodePageToZCCodePage(0)=ZCCP1251,
    'LibreDWG UTF8 (0) -> SysDWG_CodePage');
  Check(DWGHeaderCodePageToZCCodePage(43)=ZCCP1251,
    'LibreDWG UTF16 (43) -> SysDWG_CodePage');
  Check(DWGHeaderCodePageToZCCodePage(9999)=ZCCP1251,
    'unknown codepage -> SysDWG_CodePage');

  Check(ZCCodePageOrDefault(ZCCPINVALID)=ZCCP1251,
    'ZCCodePageOrDefault(ZCCPINVALID) = SysDWG_CodePage');
  Check(ZCCodePageOrDefault(ZCCP1250)=ZCCP1250,
    'ZCCodePageOrDefault keeps valid codepage');
  Check(SysCP2ZCCodePage(CP_UTF8)=ZCCPINVALID,
    'SysCP2ZCCodePage(UTF8) = ZCCPINVALID');

  { Заголовок и перекодировка строк должны описывать одну страницу }
  Check(ZCCP2Str(ZCCodePageOrDefault(ZCCPINVALID))='ANSI_1251',
    '$DWGCODEPAGE for ZCCPINVALID drawing = ANSI_1251');
  Check(ZCCodePage2SysCP(ZCCodePageOrDefault(ZCCPINVALID))=1251,
    'string encoding for ZCCPINVALID drawing = 1251');

  { Сама ошибка: строки не должны превращаться в '?' }
  Check(Pos('?',EncodeDXF2000(Text,ZCCPINVALID))=0,
    'DXF2000 text of ZCCPINVALID drawing has no "?"');
  Check(EncodeDXF2000(Text,ZCCPINVALID)=Expected(Text,1251),
    'DXF2000 text of ZCCPINVALID drawing is cp1251');
  Check(EncodeDXF2000(Text,DWGHeaderCodePageToZCCodePage(29))=
    Expected(Text,1251),'DXF2000 text of DWG (ANSI_1251) drawing is cp1251');

  if Failures>0 then begin
    WriteLn(Failures,' check(s) failed');
    Halt(1);
  end;
  WriteLn('all checks passed');
end.
