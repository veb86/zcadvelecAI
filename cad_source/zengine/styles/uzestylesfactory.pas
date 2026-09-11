{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************
}
{
  Универсальный реестр DXF-таблиц стилей.

  Реестр намеренно не знает ничего о конкретных стилях. Конкретный модуль
  стиля регистрирует имя DXF-таблицы и процедуры чтения/записи.
}
unit uzestylesfactory;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  usimplegenerics,
  uzgldrawcontext,
  uzedrawingsimple,
  uzctnrVectorBytesStream,
  uzMVReader,
  uzeffdxfsupport;

type
  TStyleDXFLoadProc=procedure(var s:ansistring;const clayer:string;
                              var rdr:TZMemReader;const exitString:string;
                              var ZCDCtx:TZDrawingContext;
                              var context:TIODXFLoadContext);

  TStyleDXFSaveProc=procedure(var outstream:TZctnrVectorBytes;
                              var drawing:TSimpleDrawing;
                              var context:TIODXFSaveContext);

  TStyleDXFInfo=record
    DXFName:string;
    LoadProc:TStyleDXFLoadProc;
    SaveProc:TStyleDXFSaveProc;
  end;

  TDXFStyleName2InfoMap=GKey2DataMap<string,TStyleDXFInfo{$IFNDEF DELPHI},LessString{$ENDIF}>;

procedure RegisterDXFStyle(const ADXFName:string;
                           const ALoadProc:TStyleDXFLoadProc;
                           const ASaveProc:TStyleDXFSaveProc);
function FindDXFStyle(const ADXFName:string;out AInfo:TStyleDXFInfo):Boolean;

var
  DXFStyleName2Info:TDXFStyleName2InfoMap;

implementation

procedure RegisterDXFStyle(const ADXFName:string;
                           const ALoadProc:TStyleDXFLoadProc;
                           const ASaveProc:TStyleDXFSaveProc);
var
  Info:TStyleDXFInfo;
  Name:string;
begin
  Name:=UpperCase(ADXFName);
  Info.DXFName:=Name;
  Info.LoadProc:=ALoadProc;
  Info.SaveProc:=ASaveProc;
  DXFStyleName2Info.RegisterKey(Name,Info);
end;

function FindDXFStyle(const ADXFName:string;out AInfo:TStyleDXFInfo):Boolean;
begin
  Result:=DXFStyleName2Info.MyGetValue(UpperCase(ADXFName),AInfo);
end;

initialization
  DXFStyleName2Info:=TDXFStyleName2InfoMap.Create;

finalization
  FreeAndNil(DXFStyleName2Info);
end.
