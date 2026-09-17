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
@author(Andrey Zubarev <zamtmn@yandex.ru>)
}

unit uzestyleslayerdxf;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  sysutils, uzeffdxfsupport, uzestyleslayers, uzestylesfactory,
  uzMVReader, uzeffmanager, uzbLogIntf,gzctnrVectorTypes, uzctnrVectorBytesStream;

{ Процедура чтения таблицы LAYER из DXF }
procedure LoadLayerFromDXF(var s: ansistring; const styleParam: string;
  var rdr: TZMemReader; const exitString: String; var ZCDCtx: TZDrawingContext;
  var context: TIODXFLoadContext);

{ Процедура записи таблицы LAYER в DXF }
procedure SaveLayerToDXF(drawing: PGDBLayerArray; var outstream: TZctnrVectorBytes;
  var IODXFContext: TIODXFSaveContext);

implementation

uses
  uzeconsts, uzeTypes, UGDBNamedObjectsArray;

procedure gotodxf(var rdr:TZMemReader; fcode: Integer; const fname: String);
var
  byt: Integer;
  s: String;
begin
  if fname<>'' then begin
    while not rdr.EOF do begin
      byt:=rdr.ParseInteger;
      s := rdr.ParseString;
      if (byt = fcode) and (s = fname) then
        exit;
    end;
  end else begin
    while not rdr.EOF do begin
      byt:=rdr.ParseInteger;
      if (byt = fcode) then
        exit;
      rdr.SkipString;
    end;
  end;
end;

{==============================================================================
  Чтение таблицы LAYER из DXF (современный формат)
  Перенесено из uzeffdxf.pas (процедура ReadLayers)
==============================================================================}
procedure LoadLayerFromDXF(var s: ansistring; const styleParam: string;
  var rdr: TZMemReader; const exitString: String; var ZCDCtx: TZDrawingContext;
  var context: TIODXFLoadContext);
var
  byt: Integer;
  lname, desk: String;
  nulisread: boolean;
  player: PGDBLayerProp;
  clayer: string;
begin
  { Получаем имя текущего слоя из параметров }
  clayer := styleParam;

  nulisread := false;
  gotodxf(rdr, 0, dxfName_Layer);
  player := nil;
  while s = dxfName_Layer do
  begin
    byt := 2;
    while byt <> 0 do
    begin
      if not nulisread then
      begin
        byt := rdr.ParseInteger;
        s := rdr.ParseString;
      end
      else
        nulisread := false;
      case byt of
        2: begin
          zDebugLn('{D}[DXF_CONTENTS]Found layer  ' + s);
          s := dxfDeCodeString(s, context.Header);
          lname := s;
          player := ZCDCtx.PDrawing^.LayerTable.MergeItem(s, ZCDCtx.LoadMode);
          if player <> nil then
            player^.init(s);
        end;
        6: if player <> nil then
          player^.LT := ZCDCtx.PDrawing^.LTypeStyleTable.getAddres(dxfDeCodeString(s, context.Header));
        1001: begin
          if s = 'AcAecLayerStandard' then
          begin
            s := rdr.ParseString;
            byt := strtoint(s);
            if byt <> 0 then
            begin
              s := rdr.ParseString;
              s := rdr.ParseString;
              byt := strtoint(s);
              if byt <> 0 then
              begin
                dxfLoadString(rdr, desk, context.Header);
                //desk := rdr.ParseString;
                if player <> nil then
                  player^.desk := desk;
              end
              else
              begin
                nulisread := true;
                s := rdr.ParseString;
              end;
            end
            else
            begin
              nulisread := true;
              s := rdr.ParseString;
            end;
          end;
        end;
        else begin
          if player <> nil then
            player^.SetValueFromDxf(byt, s);
        end;
      end;
    end;
    { Определение CurrentLayer - сохраняем существующую логику }
    if ZCDCtx.PDrawing^.CurrentLayer = nil then
      ZCDCtx.PDrawing^.CurrentLayer := player
    else if lname = clayer then
      ZCDCtx.PDrawing^.CurrentLayer := player;
  end;
end;

{==============================================================================
  Запись таблицы LAYER в DXF
  Перенесено из uzeffdxfout.pas
==============================================================================}
procedure SaveLayerToDXF(drawing: PGDBLayerArray; var outstream: TZctnrVectorBytes;
  var IODXFContext: TIODXFSaveContext);
var
  plp: PGDBLayerProp;
  ir: itrec;
  attr: Integer;
  temphandle: QWord;
  plottablefansdle: Integer;
begin
  { Получаем handle для plot style (по умолчанию 0xF) }
  plottablefansdle := $F;

  plp := drawing^.beginiterate(ir);
  if plp <> nil then
    repeat
      { Выделяем handle для слоя }
      IODXFContext.p2h.MyGetOrCreateValue(plp, IODXFContext.handle, temphandle);

      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(0));
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfName_Layer);
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(5));
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(inttohex(temphandle, 0));
      Inc(IODXFContext.handle);
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(100));
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfName_AcDbSymbolTableRecord);
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(100));
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL('AcDbLayerTableRecord');
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(2));
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfEnCodeString(plp^.Name, IODXFContext.Header));

      { Атрибуты слоя (lock) }
      attr := 0;
      if plp^._lock then
        attr := attr + 4;
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(70));
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(IntToStr(attr));

      { Цвет слоя (с учётом on/off) }
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(62));
      if plp^._on then
        TZctnrVectorBytesStream(outstream).TXTAddStringEOL(IntToStr(plp^.color))
      else
        TZctnrVectorBytesStream(outstream).TXTAddStringEOL(IntToStr(-plp^.color));

      { Имя типа линии }
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(6));
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfEnCodeString(GetLTName(plp^.LT), IODXFContext.Header));

      { Флаг печати }
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(290));
      if plp^._print then
        TZctnrVectorBytesStream(outstream).TXTAddStringEOL('1')
      else
        TZctnrVectorBytesStream(outstream).TXTAddStringEOL('0');

      { Толщина линии }
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(370));
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(IntToStr(plp^.lineweight));

      { Plot style handle }
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(390));
      TZctnrVectorBytesStream(outstream).TXTAddStringEOL(inttohex(plottablefansdle, 0));

      { Описание слоя (если есть) }
      if plp^.desk <> '' then
      begin
        TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(1001));
        TZctnrVectorBytesStream(outstream).TXTAddStringEOL('AcAecLayerStandard');
        TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(1000));
        TZctnrVectorBytesStream(outstream).TXTAddStringEOL('');
        TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfGroupCode(1000));
        TZctnrVectorBytesStream(outstream).TXTAddStringEOL(dxfEnCodeString(plp^.desk, IODXFContext.Header));
      end;

      plp := drawing^.iterate(ir);
    until plp = nil;
end;

{==============================================================================
  Регистрация обработчика LAYER при инициализации модуля
==============================================================================}
initialization
  RegisterDXFStyle(
    'LAYER',
    @LoadLayerFromDXF,
    @SaveLayerToDXF
  );

end.
