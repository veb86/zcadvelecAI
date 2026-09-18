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
  uzMVReader, uzeffmanager, uzbLogIntf, gzctnrVectorTypes, uzctnrVectorBytes, uzctnrVectorBytesStream;

{ Процедура чтения таблицы LAYER из DXF }
procedure LoadLayerFromDXF(var s: ansistring; const styleParam: string;
  var rdr: TZMemReader; const exitString: String; var ZCDCtx: TZDrawingContext;
  var context: TIODXFLoadContext);

{ Процедура записи таблицы LAYER в DXF }
procedure SaveLayerToDXF(Layers: PGDBLayerArray; outstream: Pointer;
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
procedure SaveLayerToDXF(Layers: PGDBLayerArray; outstream: Pointer;
  var IODXFContext: TIODXFSaveContext);
var
  plp: PGDBLayerProp;
  ir: itrec;
  attr: Integer;
  temphandle: TDWGHandle;
  plottablefansdle: Integer;
  Bytes: TZctnrVectorBytes;
begin
  // Корректное приведение типа указателя
  Bytes := TZctnrVectorBytes(outstream^);

  { Получаем handle для plot style (по умолчанию 0xF) }
  plottablefansdle := $F;

  plp := Layers^.beginiterate(ir);
  if plp <> nil then
    repeat
      { Выделяем handle для слоя }
      IODXFContext.p2h.MyGetOrCreateValue(plp, IODXFContext.handle, temphandle);

      Bytes.TXTAddStringEOL(dxfGroupCode(0));
      Bytes.TXTAddStringEOL(dxfName_Layer);
      Bytes.TXTAddStringEOL(dxfGroupCode(5));
      Bytes.TXTAddStringEOL(inttohex(temphandle, 0));
      Inc(IODXFContext.handle);
      Bytes.TXTAddStringEOL(dxfGroupCode(100));
      Bytes.TXTAddStringEOL(dxfName_AcDbSymbolTableRecord);
      Bytes.TXTAddStringEOL(dxfGroupCode(100));
      Bytes.TXTAddStringEOL('AcDbLayerTableRecord');
      Bytes.TXTAddStringEOL(dxfGroupCode(2));
      Bytes.TXTAddStringEOL(dxfEnCodeString(plp^.Name, IODXFContext.Header));

      { Атрибуты слоя (lock) }
      attr := 0;
      if plp^._lock then
        attr := attr + 4;
      Bytes.TXTAddStringEOL(dxfGroupCode(70));
      Bytes.TXTAddStringEOL(IntToStr(attr));

      { Цвет слоя (с учётом on/off) }
      Bytes.TXTAddStringEOL(dxfGroupCode(62));
      if plp^._on then
        Bytes.TXTAddStringEOL(IntToStr(plp^.color))
      else
        Bytes.TXTAddStringEOL(IntToStr(-plp^.color));

      { Имя типа линии }
      Bytes.TXTAddStringEOL(dxfGroupCode(6));
      Bytes.TXTAddStringEOL(dxfEnCodeString(GetLTName(plp^.LT), IODXFContext.Header));

      { Флаг печати }
      Bytes.TXTAddStringEOL(dxfGroupCode(290));
      if plp^._print then
        Bytes.TXTAddStringEOL('1')
      else
        Bytes.TXTAddStringEOL('0');

      { Толщина линии }
      Bytes.TXTAddStringEOL(dxfGroupCode(370));
      Bytes.TXTAddStringEOL(IntToStr(plp^.lineweight));

      { Plot style handle }
      Bytes.TXTAddStringEOL(dxfGroupCode(390));
      Bytes.TXTAddStringEOL(inttohex(plottablefansdle, 0));

      { Описание слоя (если есть) }
      if plp^.desk <> '' then
      begin
        Bytes.TXTAddStringEOL(dxfGroupCode(1001));
        Bytes.TXTAddStringEOL('AcAecLayerStandard');
        Bytes.TXTAddStringEOL(dxfGroupCode(1000));
        Bytes.TXTAddStringEOL('');
        Bytes.TXTAddStringEOL(dxfGroupCode(1000));
        Bytes.TXTAddStringEOL(dxfEnCodeString(plp^.desk, IODXFContext.Header));
      end;

      plp := Layers^.iterate(ir);
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
