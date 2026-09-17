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

unit uzestylesfactory;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  sysutils, usimplegenerics, uzeffdxfsupport, UGDBNamedObjectsArray,
  uzMVReader, uzeffmanager, gzctnrSTL, uzctnrVectorBytesStream, uzestyleslayers;

type

  PStyleDXFInfo = ^TStyleDXFInfo;

  { Типы процедур для чтения и записи DXF-стилей }
  TStyleDXFLoadProc = procedure(var s: ansistring; const styleParam: string;
    var rdr: TZMemReader; const exitString: String; var ZCDCtx: TZDrawingContext;
    var context: TIODXFLoadContext);

  TStyleDXFSaveProc = procedure(drawing: PGDBLayerArray; var outstream: TZctnrVectorBytes;
    var IODXFContext: TIODXFSaveContext);

  { Информация о зарегистрированном DXF-стиле }
  TStyleDXFInfo = record
    DXFName: String;
    LoadProc: TStyleDXFLoadProc;
    SaveProc: TStyleDXFSaveProc;
  end;

  { Маппинг имени стиля на информацию о нём }
  TDXFName2StyleInfoDataMap = GKey2DataMap<String, TStyleDXFInfo>;

var
  DXFName2StyleInfoData: TDXFName2StyleInfoDataMap;
  NeedInit: boolean = true;

{ Регистрация DXF-стиля }
procedure RegisterDXFStyle(const _DXFName: String;
  const _LoadProc: TStyleDXFLoadProc;
  const _SaveProc: TStyleDXFSaveProc);

{ Поиск информации о стиле по DXF-имени }
function FindDXFStyle(const _DXFName: String): PStyleDXFInfo;

implementation

var
  StyleInfoStorage: array of TStyleDXFInfo;

procedure RegisterDXFStyle(const _DXFName: String;
  const _LoadProc: TStyleDXFLoadProc;
  const _SaveProc: TStyleDXFSaveProc);
var
  StyleInfoData: TStyleDXFInfo;
begin
  if NeedInit then
  begin
    DXFName2StyleInfoData := TDXFName2StyleInfoDataMap.Create;
    NeedInit := false;
  end;

  StyleInfoData.DXFName := _DXFName;
  StyleInfoData.LoadProc := _LoadProc;
  StyleInfoData.SaveProc := _SaveProc;

  { Сохраняем в массиве для предотвращения освобождения памяти }
  SetLength(StyleInfoStorage, Length(StyleInfoStorage) + 1);
  StyleInfoStorage[High(StyleInfoStorage)] := StyleInfoData;

  DXFName2StyleInfoData.RegisterKey(_DXFName, StyleInfoData);
end;

function FindDXFStyle(const _DXFName: String): PStyleDXFInfo;
var
  i: Integer;
begin
  Result := nil;
  if not NeedInit then
  begin
    for i := 0 to High(StyleInfoStorage) do
    begin
      if StyleInfoStorage[i].DXFName = _DXFName then
      begin
        Result := @StyleInfoStorage[i];
        Exit;
      end;
    end;
  end;
end;

initialization
  if NeedInit then
  begin
    DXFName2StyleInfoData := TDXFName2StyleInfoDataMap.Create;
    NeedInit := false;
  end;

finalization
  FreeAndNil(DXFName2StyleInfoData);
  SetLength(StyleInfoStorage, 0);

end.
