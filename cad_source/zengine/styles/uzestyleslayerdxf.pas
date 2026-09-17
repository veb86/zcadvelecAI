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
  uzMVReader, uzeffmanager, uzbLogIntf,gzctnrVectorTypes;

{ Процедура чтения таблицы LAYER из DXF }
procedure LoadLayerFromDXF(var s: ansistring; const styleParam: string;
  var rdr: TZMemReader; const exitString: String; var ZCDCtx: TZDrawingContext;
  var context: TIODXFLoadContext);

{ Процедура записи таблицы LAYER в DXF }
procedure SaveLayerToDXF(drawing: PGDBLayerArray; var outstream: TZctnrVectorBytes);

implementation

uses
  uzeconsts, uzeTypes, UGDBNamedObjectsArray, uzctnrVectorBytesStream;


procedure gotodxf(var rdr:TZMemReader; fcode: Integer; const fname: String);
var
  byt: Integer;
  s: String;
  //error: Integer;
begin
  if fname<>'' then begin
    while not rdr.EOF do begin
      byt:=rdr.ParseInteger;
      //s := rdr.ParseString;
      //val(s, byt, error);
      //if error <> 0 then
      //  s := s{чето тут не так};
      s := rdr.ParseString;
      if (byt = fcode) and (s = fname) then
        exit;
    end;
  end else begin
    while not rdr.EOF do begin
      byt:=rdr.ParseInteger;
      //s := rdr.ParseString;
      //val(s, byt, error);
      //if error <> 0 then
      //  s := s{чето тут не так};
      if (byt = fcode) then
        exit;
      //s:=rdr.ParseString;
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
procedure SaveLayerToDXF(drawing: PGDBLayerArray; var outstream: TZctnrVectorBytes);
var
  plp: PGDBLayerProp;
  ir: itrec;
  attr: Integer;
  IODXFContext: PTIODXFSaveContext;
  temphandle: Integer;
  plottablefansdle: Integer;
begin
  { В текущей реализации контекст должен передаваться отдельно }
  { Для сохранения обратной совместимости используем глобальный подход }
  { или изменяем сигнатуру процедуры для передачи контекста }
  { Пока оставим заглушку, которая должна быть вызвана из основного кода }
  { с правильным контекстом }
  
  { Получаем handle для plot style (по умолчанию 0xF) }
  plottablefansdle := $F;

  plp := drawing^.beginiterate(ir);
  if plp <> nil then
    repeat
      { Выделяем handle для слоя - нужен корректный контекст }
      { IODXFContext должен быть передан как параметр }
      { Временная реализация без контекста будет неполной }
      
      { TODO: Передать IODXFContext как параметр процедуры }
      { Пока эта процедура должна вызываться из uzeffdxfout.pas }
      { где есть доступ к IODXFContext }
      
      { Эта реализация требует изменения сигнатуры процедуры }
      { для передачи IODXFContext }
      
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
