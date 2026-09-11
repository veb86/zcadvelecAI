{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************
}
{
  DXF serialization LAYER.

  Модель слоя находится в uzestyleslayers. Этот модуль содержит только
  DXF-специфическую часть чтения/записи таблицы LAYER.
}
unit uzestyleslayerdxf;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzestylesfactory;

implementation

uses
  uzestyleslayers,
  uzeffdxfsupport,
  uzMVReader,
  uzgldrawcontext,
  uzedrawingsimple,
  uzctnrVectorBytesStream,
  uzbLogIntf,
  uzclog,
  uzestrconsts;

procedure LoadLayerFromDXF(var s:ansistring;const clayer:string;
  var rdr:TZMemReader;const exitString:string;
  var ZCDCtx:TZDrawingContext;var context:TIODXFLoadContext);
var
  byt:Integer;
  lname,desk:String;
  nulisread:Boolean;
  player:PGDBLayerProp;
begin
  nulisread:=False;
  GoToDXF(rdr,0,dxfName_Layer);
  player:=nil;

  while s=dxfName_Layer do
  begin
    byt:=2;
    while byt<>0 do
    begin
      if not nulisread then
      begin
        byt:=rdr.ParseInteger;
        s:=rdr.ParseString;
      end
      else
        nulisread:=False;

      case byt of
        2:
          begin
            zDebugLn('{D}[DXF_CONTENTS]Found layer  '+s);
            s:=dxfDeCodeString(s,context.Header);
            lname:=s;
            player:=ZCDCtx.PDrawing^.LayerTable.MergeItem(s,ZCDCtx.LoadMode);
            if player<>nil then
              player^.init(s);
          end;
        5:
          begin
            if player<>nil then
              context.h2p.Add(DXFHandle(s),
                TDXFHandle2ZCObject.TPointerWithType.CreateRec(player,OT_Layer));
          end;
        6:
          if player<>nil then
            player^.LT:=ZCDCtx.PDrawing^.LTypeStyleTable.getAddres(
              dxfDeCodeString(s,context.Header));
        1001:
          begin
            if s='AcAecLayerStandard' then
            begin
              s:=rdr.ParseString;
              byt:=StrToInt(s);
              if byt<>0 then
              begin
                s:=rdr.ParseString;
                s:=rdr.ParseString;
                if player<>nil then
                  player^.desk:=dxfDeCodeString(s,context.Header);
              end;
            end;
          end;
        1000:
          begin
            { Старые файлы могут содержать пользовательские XDATA в другом
              порядке. Не изменяем существующую семантику: здесь значение
              обрабатывается как описание только после AcAecLayerStandard. }
          end;
        else
          begin
            if player<>nil then
              player^.SetValueFromDxf(byt,s);
          end;
      end;
    end;

    if ZCDCtx.PDrawing^.CurrentLayer=nil then
      ZCDCtx.PDrawing^.CurrentLayer:=player
    else if UpperCase(s)=UpperCase(clayer) then
      ZCDCtx.PDrawing^.CurrentLayer:=player;
  end;
end;

procedure SaveLayerToDXF(var outstream:TZctnrVectorBytes;
  var drawing:TSimpleDrawing;var context:TIODXFSaveContext);
var
  plp:PGDBLayerProp;
  ir:itrec;
  attr:Integer;
  plottablehandle:TDWGHandle;
begin
  { Табличная запись LAYER получает хэндл текущего PLOTSTYLE TABLE. В
    существующем writer этот хэндл вычисляется непосредственно перед
    сериализацией LAYER. Сохраняем это значение через VarsDict, если оно
    было подготовлено общим writer'ом; при отсутствии значения используем 0. }
  plottablehandle:=0;
  if not context.VarsDict.MyGetValue('$ZCAD_LAY_PLOTSTYLE_HANDLE',plottablehandle) then
    plottablehandle:=0;

  plp:=drawing.LayerTable.beginiterate(ir);
  if plp<>nil then
    repeat
      outstream.TXTAddStringEOL(dxfGroupCode(0));
      outstream.TXTAddStringEOL(dxfName_Layer);
      outstream.TXTAddStringEOL(dxfGroupCode(5));
      outstream.TXTAddStringEOL(inttohex(context.handle,0));
      Inc(context.handle);
      outstream.TXTAddStringEOL(dxfGroupCode(100));
      outstream.TXTAddStringEOL(dxfName_AcDbSymbolTableRecord);
      outstream.TXTAddStringEOL(dxfGroupCode(100));
      outstream.TXTAddStringEOL('AcDbLayerTableRecord');
      outstream.TXTAddStringEOL(dxfGroupCode(2));
      outstream.TXTAddStringEOL(dxfEnCodeString(plp^.Name,context.Header));

      attr:=0;
      if plp^._lock then
        attr:=attr+4;
      outstream.TXTAddStringEOL(dxfGroupCode(70));
      outstream.TXTAddStringEOL(IntToStr(attr));
      outstream.TXTAddStringEOL(dxfGroupCode(62));
      if plp^._on then
        outstream.TXTAddStringEOL(IntToStr(plp^.color))
      else
        outstream.TXTAddStringEOL(IntToStr(-plp^.color));
      outstream.TXTAddStringEOL(dxfGroupCode(6));
      outstream.TXTAddStringEOL(dxfEnCodeString(GetLTName(plp^.LT),context.Header));
      outstream.TXTAddStringEOL(dxfGroupCode(290));
      if plp^._print then
        outstream.TXTAddStringEOL('1')
      else
        outstream.TXTAddStringEOL('0');
      outstream.TXTAddStringEOL(dxfGroupCode(370));
      outstream.TXTAddStringEOL(IntToStr(plp^.lineweight));
      outstream.TXTAddStringEOL(dxfGroupCode(390));
      outstream.TXTAddStringEOL(inttohex(plottablehandle,0));

      if plp^.desk<>'' then
      begin
        outstream.TXTAddStringEOL(dxfGroupCode(1001));
        outstream.TXTAddStringEOL('AcAecLayerStandard');
        outstream.TXTAddStringEOL(dxfGroupCode(1000));
        outstream.TXTAddStringEOL('');
        outstream.TXTAddStringEOL(dxfGroupCode(1000));
        outstream.TXTAddStringEOL(dxfEnCodeString(plp^.desk,context.Header));
      end;

      plp:=drawing.LayerTable.iterate(ir);
    until plp=nil;
end;

initialization
  RegisterDXFStyle('LAYER',@LoadLayerFromDXF,@SaveLayerToDXF);
end.
