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
unit uzeffDxfOut;
{$INCLUDE zengineconfig.inc}
{$MODE delphi}{$H+}
interface

uses
  uzbpaths,uzbstrproc,uzgldrawcontext,usimplegenerics,uzestylesdim,uzeentityfactory,
  {$IFNDEF DELPHI}LazUTF8,{$ENDIF}uzbUnits,
  UGDBNamedObjectsArray,uzestyleslinetypes,uzedrawingsimple,uzelongprocesssupport,
  gzctnrVectorTypes,uzglviewareadata,uzeffdxfsupport,uzestrconsts,uzestylestexts,
  uzegeometry,uzeentsubordinated,uzeentgenericsubentry,uzeTypes,
  uzegeometrytypes,SysUtils,uzeconsts,UGDBObjBlockdefArray,
  uzctnrVectorBytesStream,UGDBVisibleOpenArray,uzeentity,uzeblockdef,uzestyleslayers,
  uzeffmanager,uzbLogIntf,uzeLogIntf,
  uzMVSMemoryMappedFile,uzMVReader,uzbBaseUtils,
  uzestylestablesdxf,uzclog,uzeffdxfnodregistry;
type
  { Callback, вызываемый перед началом записи DXF. Позволяет подпиться
    на pre-save обработку чертежа (например, конвертацию ProxyEntity
    в BlockInsert). }
  TBeforeSaveDxfProc=procedure(var drawing:TSimpleDrawing);
  { Callback, вызываемый перед закрытием секции OBJECTS. Позволяет
    специализированным модулям записывать свои OBJECTS-сущности, не
    добавляя предметные зависимости в общий DXF writer. }
  TObjectsSaveDxfProc=procedure(var outstream:TZctnrVectorBytes;
                                var drawing:TSimpleDrawing;
                                var IODXFContext:TIODXFSaveContext);
  { Callback, вызываемый перед закрытием секции CLASSES. Классы
    прикладных DXF-объектов должны быть объявлены до появления их
    экземпляров в секциях ENTITIES/OBJECTS. }
  TClassesSaveDxfProc=procedure(var outstream:TZctnrVectorBytes;
                                var drawing:TSimpleDrawing;
                                var IODXFContext:TIODXFSaveContext);

{ Регистрирует pre-save callback. Все зарегистрированные callback'и
  вызываются в порядке регистрации в начале savedxf20XX. }
procedure RegisterBeforeSaveDxfProc(proc:TBeforeSaveDxfProc);
procedure RegisterObjectsSaveDxfProc(proc:TObjectsSaveDxfProc);
procedure RegisterClassesSaveDxfProc(proc:TClassesSaveDxfProc);

function savedxf20XX(const SavedFileName:string;const TemplateFileName:string;var drawing:TSimpleDrawing;AVer:TZCDxfVersion):boolean;

implementation

uses
  uzeffdxfnod,uzeffdxfnodlog;

var
  BeforeSaveDxfProcs:array of TBeforeSaveDxfProc;
  ObjectsSaveDxfProcs:array of TObjectsSaveDxfProc;
  ClassesSaveDxfProcs:array of TClassesSaveDxfProc;

procedure RegisterBeforeSaveDxfProc(proc:TBeforeSaveDxfProc);
var
  i:Integer;
begin
  i:=Length(BeforeSaveDxfProcs);
  SetLength(BeforeSaveDxfProcs,i+1);
  BeforeSaveDxfProcs[i]:=proc;
end;

procedure RegisterObjectsSaveDxfProc(proc:TObjectsSaveDxfProc);
var
  i:Integer;
begin
  i:=Length(ObjectsSaveDxfProcs);
  SetLength(ObjectsSaveDxfProcs,i+1);
  ObjectsSaveDxfProcs[i]:=proc;
end;

procedure RegisterClassesSaveDxfProc(proc:TClassesSaveDxfProc);
var
  i:Integer;
begin
  i:=Length(ClassesSaveDxfProcs);
  SetLength(ClassesSaveDxfProcs,i+1);
  ClassesSaveDxfProcs[i]:=proc;
end;

procedure RegisterAcadAppInDXF(const appname:string;outstream:PTZctnrVectorBytes;var handle:TDWGHandle);
begin
  outstream^.TXTAddStringEOL(dxfGroupCode(0));
  outstream^.TXTAddStringEOL('APPID');

  outstream^.TXTAddStringEOL(dxfGroupCode(5));
  outstream^.TXTAddStringEOL(inttohex(handle,0));
  Inc(handle);

  outstream^.TXTAddStringEOL(dxfGroupCode(100));
  outstream^.TXTAddStringEOL('AcDbSymbolTableRecord');
  outstream^.TXTAddStringEOL(dxfGroupCode(100));
  outstream^.TXTAddStringEOL('AcDbRegAppTableRecord');
  outstream^.TXTAddStringEOL(dxfGroupCode(2));
  outstream^.TXTAddStringEOL(appname);
  outstream^.TXTAddStringEOL(dxfGroupCode(70));
  outstream^.TXTAddStringEOL('0');
  {
  0
  APPID
  5
  12
  >>330
  >>9
  100
  AcDbSymbolTableRecord
  100
  AcDbRegAppTableRecord
  2
  ACAD
  70
  0
  }
end;


{ Преобразует вещественное число в строку DXF с гарантией десятичной точки }
function DXFFloatStr(Value: Double): string;
begin
  Result := FloatToStr(Value);
  if Pos('.', Result) = 0 then
    Result := Result + '.0';
end;

{ Записывает блок границ ячейки в поток (коды 274-279, 284-289, 64-69).
  Значения по умолчанию: тип линии = -2, видимость = 1, цвет = 0 }
procedure WriteCellBordersToStream(
  var outstream: TZctnrVectorBytes);
var
  BorderCode, VisCode, ColorCode: Integer;
begin
  for BorderCode := 274 to 279 do
  begin
    VisCode := BorderCode + 10;
    ColorCode := BorderCode - 210;
    outstream.TXTAddStringEOL(dxfGroupCode(BorderCode));
    outstream.TXTAddStringEOL('-2');
    outstream.TXTAddStringEOL(dxfGroupCode(VisCode));
    outstream.TXTAddStringEOL('1');
    outstream.TXTAddStringEOL(dxfGroupCode(ColorCode));
    outstream.TXTAddStringEOL('0');
  end;
end;

{ Записывает один блок стиля ячейки (title/header/data) в поток }
procedure WriteCellStyleToStream(
  var outstream: TZctnrVectorBytes;
  const CS: TGDBDXFTableCellStyle;
  const TextStyleName: string);
begin
  outstream.TXTAddStringEOL(dxfGroupCode(7));
  if TextStyleName <> '' then
    outstream.TXTAddStringEOL(TextStyleName)
  else
    outstream.TXTAddStringEOL('Standard');
  outstream.TXTAddStringEOL(dxfGroupCode(140));
  outstream.TXTAddStringEOL(DXFFloatStr(CS.TextHeight));
  outstream.TXTAddStringEOL(dxfGroupCode(170));
  outstream.TXTAddStringEOL(IntToStr(CS.Alignment));
  outstream.TXTAddStringEOL(dxfGroupCode(62));
  outstream.TXTAddStringEOL(IntToStr(CS.TextColor));
  outstream.TXTAddStringEOL(dxfGroupCode(63));
  outstream.TXTAddStringEOL(IntToStr(CS.BackgroundColor));
  outstream.TXTAddStringEOL(dxfGroupCode(283));
  if CS.BackgroundColorEnabled then
    outstream.TXTAddStringEOL('1')
  else
    outstream.TXTAddStringEOL('0');
  outstream.TXTAddStringEOL(dxfGroupCode(90));
  outstream.TXTAddStringEOL('512');
  outstream.TXTAddStringEOL(dxfGroupCode(91));
  outstream.TXTAddStringEOL('0');
  outstream.TXTAddStringEOL(dxfGroupCode(1));
  outstream.TXTAddStringEOL('');
  WriteCellBordersToStream(outstream);
end;

{ Пара «код/значение» в поток (сокращение для читаемости кода ниже) }
procedure dxfPairOut(var outstream: TZctnrVectorBytes;
                     const Code: Integer; const Value: string); inline;
begin
  outstream.TXTAddStringEOL(dxfGroupCode(Code));
  outstream.TXTAddStringEOL(Value);
end;

{ Записывает один блок CELLSTYLE карты стилей ячеек (AcDbCellStyleMap).

  Именно этот объект даёт смысл идентификаторам стиля ячейки (90) внутри
  TABLECELL_BEGIN объекта TABLECONTENT: без CELLSTYLEMAP AutoCAD отбрасывает
  индивидуальные стили ячеек и применяет встроенное правило «строка 1 — Title,
  строка 2 — Header, остальные — Data» (issue #1409).

  AStyleId  — 1=_TITLE, 2=_HEADER, 3=_DATA
  AStyleName— '_TITLE' / '_HEADER' / '_DATA'
  AStyleType— 1 для заголовочных строк (title/header), 2 для данных
  AFlags92  — 32768 для _TITLE (признак строки заголовка таблицы), иначе 0 }
procedure WriteCellStyleMapEntryToStream(
  var outstream: TZctnrVectorBytes;
  const CS: TGDBDXFTableCellStyle;
  const AStyleId, AStyleType, AFlags92: Integer;
  const AStyleName: string;
  const ATextStyleHandle: string);
var
  GridBit: Integer;
begin
  dxfPairOut(outstream, 300, 'CELLSTYLE');
  dxfPairOut(outstream, 1, 'TABLEFORMAT_BEGIN');
  dxfPairOut(outstream, 90, '5');
  dxfPairOut(outstream, 170, '1');
  dxfPairOut(outstream, 91, '0');
  dxfPairOut(outstream, 92, IntToStr(AFlags92));
  dxfPairOut(outstream, 62, '257');
  dxfPairOut(outstream, 93, '1');

  { Формат содержимого ячейки: выравнивание (94) и высота текста (144) }
  dxfPairOut(outstream, 300, 'CONTENTFORMAT');
  dxfPairOut(outstream, 1, 'CONTENTFORMAT_BEGIN');
  dxfPairOut(outstream, 90, '0');
  dxfPairOut(outstream, 91, '0');
  dxfPairOut(outstream, 92, '512');
  dxfPairOut(outstream, 93, '0');
  dxfPairOut(outstream, 300, '');
  dxfPairOut(outstream, 40, '0.0');
  dxfPairOut(outstream, 140, '1.0');
  dxfPairOut(outstream, 94, IntToStr(CS.Alignment));
  dxfPairOut(outstream, 62, IntToStr(CS.TextColor));
  { 340 = хэндл текстового стиля. AutoCAD пишет реальную ссылку STYLE в
    каждом CONTENTFORMAT карты; 0 оставляет определение CELLSTYLE неполным,
    после чего индивидуальные стили ячеек игнорируются (issue #1409). }
  dxfPairOut(outstream, 340, ATextStyleHandle);
  dxfPairOut(outstream, 144, DXFFloatStr(CS.TextHeight));
  dxfPairOut(outstream, 309, 'CONTENTFORMAT_END');

  dxfPairOut(outstream, 171, '1');
  dxfPairOut(outstream, 301, 'MARGIN');
  dxfPairOut(outstream, 1, 'CELLMARGIN_BEGIN');
  dxfPairOut(outstream, 40, '1.5');
  dxfPairOut(outstream, 40, '1.5');
  dxfPairOut(outstream, 40, '1.5');
  dxfPairOut(outstream, 40, '1.5');
  dxfPairOut(outstream, 40, '0.18');
  dxfPairOut(outstream, 40, '0.18');
  dxfPairOut(outstream, 309, 'CELLMARGIN_END');

  { Шесть описаний линий сетки ячейки (горизонтальные/вертикальные/рамка) }
  dxfPairOut(outstream, 94, '6');
  GridBit := 1;
  while GridBit <= 32 do
  begin
    dxfPairOut(outstream, 95, IntToStr(GridBit));
    dxfPairOut(outstream, 302, 'GRIDFORMAT');
    dxfPairOut(outstream, 1, 'GRIDFORMAT_BEGIN');
    dxfPairOut(outstream, 90, '0');
    dxfPairOut(outstream, 91, '1');
    dxfPairOut(outstream, 62, '0');
    dxfPairOut(outstream, 92, '-2');
    dxfPairOut(outstream, 340, '0');
    dxfPairOut(outstream, 93, '0');
    dxfPairOut(outstream, 40, '0.045');
    dxfPairOut(outstream, 309, 'GRIDFORMAT_END');
    GridBit := GridBit * 2;
  end;
  dxfPairOut(outstream, 309, 'TABLEFORMAT_END');

  dxfPairOut(outstream, 1, 'CELLSTYLE_BEGIN');
  dxfPairOut(outstream, 90, IntToStr(AStyleId));
  dxfPairOut(outstream, 91, IntToStr(AStyleType));
  dxfPairOut(outstream, 300, AStyleName);
  dxfPairOut(outstream, 309, 'CELLSTYLE_END');
end;

{ Записывает расширенный словарь стиля таблицы и объект CELLSTYLEMAP.

  Цепочка ссылок, которую ожидает AutoCAD (issue #1409):
    TABLESTYLE --102{ACAD_XDICTIONARY/360--> DICTIONARY
      --(ключ ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP)--> CELLSTYLEMAP

  DictHandle/MapHandle — заранее выделенные хэндлы этих двух объектов. }
procedure WriteCellStyleMapObjectsToStream(
  var outstream: TZctnrVectorBytes;
  Style: PTGDBDXFTableStyle;
  const StyleHandle, DictHandle, MapHandle: TDWGHandle;
  TextStyleNameHandleMap: TString2StringDictionary);
var
  CS: array[0..2] of TGDBDXFTableCellStyle;
  TextStyleHandles: array[0..2] of string;
  PCellStyle: PTGDBDXFTableCellStyle;
  CellIdx: Integer;
  Iter: itrec;
  DefaultAlignments: array[0..2] of Integer;

  function ResolveTextStyleHandle(const StyleName: string): string;
  begin
    Result := '0';
    if (StyleName <> '')
       and TextStyleNameHandleMap.MyGetValue(StyleName, Result) then
      Exit;
    { Старые/неполные TABLESTYLE могут не содержать группу 7. В таком случае
      используем реальный хэндл Standard, как делает AutoCAD. }
    TextStyleNameHandleMap.MyGetValue('Standard', Result);
  end;
begin
  { Порядок блоков ячеек в TABLESTYLE: 0=data, 1=title, 2=header }
  DefaultAlignments[0] := 2;
  DefaultAlignments[1] := 5;
  DefaultAlignments[2] := 5;
  for CellIdx := 0 to 2 do
  begin
    FillChar(CS[CellIdx], SizeOf(CS[CellIdx]), 0);
    CS[CellIdx].TextHeight := 2.5;
    CS[CellIdx].Alignment := DefaultAlignments[CellIdx];
    TextStyleHandles[CellIdx] :=
      ResolveTextStyleHandle(Style^.CellTextStyleName[CellIdx]);
  end;
  CellIdx := 0;
  PCellStyle := Style^.CellFormats.beginiterate(Iter);
  while (PCellStyle <> nil) and (CellIdx < 3) do
  begin
    CS[CellIdx] := PCellStyle^;
    Inc(CellIdx);
    PCellStyle := Style^.CellFormats.iterate(Iter);
  end;

  { Словарь-расширение стиля таблицы }
  dxfPairOut(outstream, 0, 'DICTIONARY');
  dxfPairOut(outstream, 5, inttohex(DictHandle, 0));
  dxfPairOut(outstream, 330, inttohex(StyleHandle, 0));
  dxfPairOut(outstream, 100, 'AcDbDictionary');
  { 280|1 — жёсткое владение записями словаря. AutoCAD пишет этот флаг для
    словаря-расширения стиля таблицы; без него CELLSTYLEMAP считается
    «одолженным» объектом (issue #1409). }
  dxfPairOut(outstream, 280, '1');
  dxfPairOut(outstream, 281, '1');
  dxfPairOut(outstream, 3, 'ACAD_ROUNDTRIP_2008_TABLESTYLE_CELLSTYLEMAP');
  dxfPairOut(outstream, 360, inttohex(MapHandle, 0));

  { Сама карта стилей ячеек: _TITLE=1, _HEADER=2, _DATA=3 }
  dxfPairOut(outstream, 0, 'CELLSTYLEMAP');
  dxfPairOut(outstream, 5, inttohex(MapHandle, 0));
  dxfPairOut(outstream, 102, '{ACAD_REACTORS');
  dxfPairOut(outstream, 330, inttohex(DictHandle, 0));
  dxfPairOut(outstream, 102, '}');
  dxfPairOut(outstream, 330, inttohex(DictHandle, 0));
  dxfPairOut(outstream, 100, 'AcDbCellStyleMap');
  dxfPairOut(outstream, 90, '3');
  WriteCellStyleMapEntryToStream(
    outstream, CS[1], 1, 1, 32768, '_TITLE', TextStyleHandles[1]);
  WriteCellStyleMapEntryToStream(
    outstream, CS[2], 2, 1, 0, '_HEADER', TextStyleHandles[2]);
  WriteCellStyleMapEntryToStream(
    outstream, CS[0], 3, 2, 0, '_DATA', TextStyleHandles[0]);
end;

{ Записывает один объект TABLESTYLE в выходной поток DXF.
  StyleHandle — заранее назначенный хэндл объекта.
  OwnerHandle — хэндл словаря ACAD_TABLESTYLE (владелец).
  DictHandle/MapHandle — хэндлы расширенного словаря и CELLSTYLEMAP;
  если оба равны 0, карта стилей ячеек не записывается. }
procedure WriteTableStyleObjectToStream(
  var outstream: TZctnrVectorBytes;
  Style: PTGDBDXFTableStyle;
  const StyleHandle: TDWGHandle;
  const OwnerHandle: TDWGHandle;
  TextStyleNameHandleMap: TString2StringDictionary;
  const DictHandle: TDWGHandle = 0;
  const MapHandle: TDWGHandle = 0);
var
  PCellStyle: PTGDBDXFTableCellStyle;
  DefaultCS: TGDBDXFTableCellStyle;
  DefaultAlignments: array[0..2] of Integer;
  CellIdx: Integer;
  Iter: itrec;
begin
  DefaultAlignments[0] := 2;
  DefaultAlignments[1] := 5;
  DefaultAlignments[2] := 5;

  outstream.TXTAddStringEOL(dxfGroupCode(0));
  outstream.TXTAddStringEOL('TABLESTYLE');
  outstream.TXTAddStringEOL(dxfGroupCode(5));
  outstream.TXTAddStringEOL(inttohex(StyleHandle, 0));

  { Блок ACAD_XDICTIONARY со старым хэндлом Style^.XDictHandle не пишем
    (issue #1339): такая ссылка указывала бы на несуществующий объект.
    Но если для стиля выделены хэндлы под словарь и CELLSTYLEMAP, эти
    объекты записываются нами ниже, и ссылка 360 корректна (issue #1409). }
  if (DictHandle > 0) and (MapHandle > 0) then
  begin
    outstream.TXTAddStringEOL(dxfGroupCode(102));
    outstream.TXTAddStringEOL('{ACAD_XDICTIONARY');
    outstream.TXTAddStringEOL(dxfGroupCode(360));
    outstream.TXTAddStringEOL(inttohex(DictHandle, 0));
    outstream.TXTAddStringEOL(dxfGroupCode(102));
    outstream.TXTAddStringEOL('}');
  end;

  { Блок ACAD_REACTORS — принадлежность словарю }
  outstream.TXTAddStringEOL(dxfGroupCode(102));
  outstream.TXTAddStringEOL('{ACAD_REACTORS');
  outstream.TXTAddStringEOL(dxfGroupCode(330));
  outstream.TXTAddStringEOL(inttohex(OwnerHandle, 0));
  outstream.TXTAddStringEOL(dxfGroupCode(102));
  outstream.TXTAddStringEOL('}');
  outstream.TXTAddStringEOL(dxfGroupCode(330));
  outstream.TXTAddStringEOL(inttohex(OwnerHandle, 0));

  { AcDbTableStyle }
  outstream.TXTAddStringEOL(dxfGroupCode(100));
  outstream.TXTAddStringEOL('AcDbTableStyle');
  outstream.TXTAddStringEOL(dxfGroupCode(3));
  outstream.TXTAddStringEOL(Style^.Name);
  outstream.TXTAddStringEOL(dxfGroupCode(70));
  outstream.TXTAddStringEOL(IntToStr(Style^.Flags70));
  outstream.TXTAddStringEOL(dxfGroupCode(71));
  outstream.TXTAddStringEOL(IntToStr(Style^.Flags71));
  outstream.TXTAddStringEOL(dxfGroupCode(40));
  outstream.TXTAddStringEOL(DXFFloatStr(Style^.HorzCellMargin));
  outstream.TXTAddStringEOL(dxfGroupCode(41));
  outstream.TXTAddStringEOL(DXFFloatStr(Style^.VertCellMargin));
  outstream.TXTAddStringEOL(dxfGroupCode(280));
  if Style^.TitleSuppressed then
    outstream.TXTAddStringEOL('1')
  else
    outstream.TXTAddStringEOL('0');
  outstream.TXTAddStringEOL(dxfGroupCode(281));
  if Style^.ColumnHeadingSuppressed then
    outstream.TXTAddStringEOL('1')
  else
    outstream.TXTAddStringEOL('0');

  { Записываем блоки ячеек (title, header, data) }
  CellIdx := 0;
  PCellStyle := Style^.CellFormats.beginiterate(Iter);
  while (PCellStyle <> nil) and (CellIdx < 3) do
  begin
    WriteCellStyleToStream(outstream, PCellStyle^,
      Style^.CellTextStyleName[CellIdx]);
    Inc(CellIdx);
    PCellStyle := Style^.CellFormats.iterate(Iter);
  end;
  { Добиваем до трёх блоков значениями по умолчанию }
  while CellIdx < 3 do
  begin
    FillChar(DefaultCS, SizeOf(DefaultCS), 0);
    DefaultCS.TextHeight := 2.5;
    DefaultCS.Alignment := DefaultAlignments[CellIdx];
    DefaultCS.BackgroundColor := 7;
    WriteCellStyleToStream(outstream, DefaultCS, 'Standard');
    Inc(CellIdx);
  end;

  { Карта стилей ячеек — без неё AutoCAD игнорирует индивидуальные стили
    ячеек в TABLECONTENT (issue #1409). }
  if (DictHandle > 0) and (MapHandle > 0) then
    WriteCellStyleMapObjectsToStream(outstream, Style,
      StyleHandle, DictHandle, MapHandle, TextStyleNameHandleMap);

  programlog.LogOutFormatStr(
    'uzeffdxfout: записан TABLESTYLE "%s" handle=%s',
    [Style^.Name, inttohex(StyleHandle, 0)], LM_Info);
end;

procedure saveentitiesdxf2000(pva:PGDBObjEntityOpenArray;var outStream:TZctnrVectorBytes;var drawing:TSimpleDrawing;var IODXFContext:TIODXFSaveContext);
var
  pv:pgdbobjEntity;
  ir:itrec;
  lph:TLPSHandle;
begin
  lph:=lps.StartLongProcess('saveentitiesdxf2000',@outStream,pva^.Count);
  pv:=pva^.beginiterate(ir);
  if pv<>nil then
    repeat
      lps.ProgressLongProcess(lph,ir.itc);
      IODXFContext.LocalEntityFlags:=DefaultLocalEntityFlags;
      pv^.DXFOut(outStream,drawing,IODXFContext);
      pv:=pva^.iterate(ir);
    until pv=nil;
  lps.EndLongProcess(lph);
end;

procedure MakeVariablesDict(VarsDict:TString2StringDictionary;var drawing:TSimpleDrawing);
var
  pcurrtextstyle:PGDBTextStyle;
  pcurrentdimstyle:PGDBDimStyle;
begin
  VarsDict.Add('$CLAYER',drawing.GetCurrentLayer^.Name);
  VarsDict.Add('$CELTYPE',drawing.GetCurrentLType^.Name);
  VarsDict.Add('$DWGCODEPAGE',ZCCP2Str(ZCCodePageOrDefault(drawing.DXFCodePage)));

  pcurrtextstyle:=drawing.GetCurrentTextStyle;
  if pcurrtextstyle<>nil then
    VarsDict.Add('$TEXTSTYLE',drawing.GetCurrentTextStyle^.Name)
  else
    VarsDict.Add('$TEXTSTYLE',TSNStandardStyleName);

  pcurrentdimstyle:=drawing.GetCurrentDimStyle;
  if pcurrentdimstyle<>nil then
    VarsDict.Add('$DIMSTYLE',pcurrentdimstyle^.Name)
  else
    VarsDict.Add('$DIMSTYLE','Standatd');

  VarsDict.Add('$CELWEIGHT',IntToStr(drawing.CurrentLineW));
  VarsDict.Add('$LTSCALE',floattostr(drawing.LTScale));
  VarsDict.Add('$CELTSCALE',floattostr(drawing.CLTScale));
  VarsDict.Add('$CECOLOR',IntToStr(drawing.CColor));

  if drawing.LWDisplay then
    VarsDict.Add('$LWDISPLAY',IntToStr(1))
  else
    VarsDict.Add('$LWDISPLAY',IntToStr(0));
  VarsDict.Add('$HANDSEED','FUCK OFF!');

  VarsDict.Add('$LUNITS',IntToStr(Ord(drawing.LUnits)+1));
  VarsDict.Add('$LUPREC',IntToStr(Ord(drawing.LUPrec)));
  VarsDict.Add('$AUNITS',IntToStr(Ord(drawing.AUnits)));
  VarsDict.Add('$AUPREC',IntToStr(Ord(drawing.AUPrec)));
  VarsDict.Add('$ANGDIR',IntToStr(Ord(drawing.AngDir)));
  VarsDict.Add('$ANGBASE',floattostr(drawing.AngBase));
  VarsDict.Add('$UNITMODE',IntToStr(Ord(drawing.UnitMode)));
  VarsDict.Add('$INSUNITS',IntToStr(Ord(drawing.InsUnits)));
  VarsDict.Add('$TEXTSIZE',floattostr(drawing.TextSize));
end;


function savedxf20XX(const SavedFileName:string;const TemplateFileName:string;var drawing:TSimpleDrawing;AVer:TZCDxfVersion):boolean;
var
  sysfilename:rawbytestring;
  templatefile:TZctnrVectorBytes;
  outstream:TZctnrVectorBytes;
  groups,values,ts:string;
  groupi,valuei,intable,attr:integer;
  temphandle,temphandle2,lasthandle,vporttablehandle,plottablefansdle,dimtablehandle:TDWGHandle;
  i:integer;
  OldHandele2NewHandle:TMapHandleToHandle;

  inlayertable,inblocksec,inblocktable,inlttypetable,indimstyletable,inappidtable:boolean;
  handlepos:integer;
  ignoredsource:boolean;
  instyletable:boolean;
  invporttable:boolean;

  pltp:PGDBLtypeProp;
  plp:PGDBLayerProp;
  pdsp:PGDBDimStyle;
  ir,ir2,ir3,ir4,ir5:itrec;
  TDI:PTDashInfo;
  PStroke:PDouble;
  PSP:PShapeProp;
  PTP:PTextProp;
  p:pointer;
  IODXFContext:TIODXFSaveContext;
  laststrokewrited:boolean;
  pcurrtextstyle:PGDBTextStyle;
  variablenotprocessed:boolean;
  processedvarscount:integer;
  lph:TLPSHandle;

  inobjectssec,inclassessec: boolean;
  { Внутри объекта NOD шаблона (после его группы 5) }
  innodobj: boolean;
  { Пара шаблона, прочитанная заранее и ещё не обработанная }
  pendingpair: boolean;
  pendinggroups,pendingvalues: string;
  peekgroups,peekvalues: string;
  acadTableOwnerDone: boolean;
  beforeProcIdx: integer;
  { NOD-обработчики, участвующие в сохранении (реестр uzeffdxfnodregistry) }
  NODSave: TZNODSaveSession;

  { Вызывается перед ENDSEC секции OBJECTS: сначала ветки NOD-обработчиков
    (этап 3 ТЗ NOD), затем прикладные OBJECTS-callback'и. }
  procedure RunObjectsSaveDxfProcs;
  var
    objectsProcIdx: Integer;
    nodHandle: TDWGHandle;
  begin
    { Новый хэндл NOD: корневой словарь шаблона уже записан и перемаплен }
    nodHandle:=0;
    if NODSave.TemplateNODHandle<>0 then
      nodHandle:=OldHandele2NewHandle.MyGetValue(NODSave.TemplateNODHandle);
    NODSave.WriteObjects(outstream,drawing,IODXFContext,nodHandle);
    for objectsProcIdx:=0 to High(ObjectsSaveDxfProcs) do
      if Assigned(ObjectsSaveDxfProcs[objectsProcIdx]) then
        ObjectsSaveDxfProcs[objectsProcIdx](
          outstream,drawing,IODXFContext);
  end;

  procedure RunClassesSaveDxfProcs;
  var
    classesProcIdx: Integer;
  begin
    NODSave.WriteClasses(outstream,drawing,IODXFContext);
    for classesProcIdx:=0 to High(ClassesSaveDxfProcs) do
      if Assigned(ClassesSaveDxfProcs[classesProcIdx]) then
        ClassesSaveDxfProcs[classesProcIdx](
          outstream,drawing,IODXFContext);
  end;

  { Вычисляет новый хэндл владельца (*Model_Space) для сырых сущностей
    ACAD_TABLE. Должно вызываться ДО записи секции ENTITIES (issue #1339). }
  procedure PreallocateAcadTableOwnerHandle;
  var
    msHandle: TDWGHandle;
  begin
    if acadTableOwnerDone then
      Exit;
    acadTableOwnerDone:=True;
    { Новый хэндл владельца сущностей пространства модели. В шаблоне и в
      исходных файлах AutoCAD блок *Model_Space всегда имеет хэндл 1F. }
    msHandle:=OldHandele2NewHandle.MyGetValue($1F);
    if msHandle>0 then
      IODXFContext.AcadTableOwnerHandle:=msHandle;
  end;

  { Хэндлы веток NOD-обработчиков (стили таблиц и т.п.): до ENTITIES, т.к.
    сырые ACAD_TABLE ссылаются на стили (342). Затем — перемаппинг
    словарей-веток шаблона на новые словари (этап 4 ТЗ NOD).
    Идемпотентна. }
  procedure ReserveNODHandles;
  begin
    NODSave.ReserveHandles(drawing,IODXFContext);
    NODSave.MapTemplateHandles(OldHandele2NewHandle,IODXFContext);
  end;

  { Пишет записи NOD обработчиков, которых нет в NOD шаблона и которые по
    алфавиту идут перед ключом ANextKey ('' — все оставшиеся). }
  procedure WriteNODInsertions(const ANextKey:string);
  var
    entries:TZDXFDictEntries;
    entryIdx:integer;
  begin
    entries:=NODSave.TakeNODInsertions(ANextKey);
    for entryIdx:=0 to High(entries) do begin
      outstream.TXTAddStringEOL(dxfGroupCode(3));
      outstream.TXTAddStringEOL(entries[entryIdx].Key);
      outstream.TXTAddStringEOL(dxfGroupCode(entries[entryIdx].OwnershipCode));
      outstream.TXTAddStringEOL(inttohex(entries[entryIdx].TargetHandle,0));
    end;
  end;
begin
  intable:=0;
  IODXFContext.InitRec;

  { Вызываем зарегистрированные pre-save обработчики перед началом
    записи (например, конвертация ProxyEntity -> BlockInsert). }
  for beforeProcIdx:=0 to High(BeforeSaveDxfProcs) do
    if Assigned(BeforeSaveDxfProcs[beforeProcIdx]) then
      BeforeSaveDxfProcs[beforeProcIdx](drawing);

  IODXFContext.Header.Version:=ZCDxfVer2DXF_ACVer(AVer);
  IODXFContext.Header.iVersion:=ZCDxfVer2ACVer(AVer);

  { Если кодовая страница чертежа не задана, берём SysDWG_CodePage - так же,
    как для $DWGCODEPAGE в MakeVariablesDict, иначе заголовок и
    перекодировка строк DXF2000 расходятся (issue #1438) }
  //if AVer<ZCDxf2007 then begin
    IODXFContext.Header.DWGCodePage:=ZCCodePage2ACDWGCodePage(ZCCodePageOrDefault(drawing.DXFCodePage)){SysCP2ACCP(ACodePage)};
    IODXFContext.Header.iDWGCodePage:=ZCCodePage2SysCP(ZCCodePageOrDefault(drawing.DXFCodePage));//ACodePage;
  //end else begin
  //  IODXFContext.Header.DWGCodePage:=ZCCodePage2ACDWGCodePage(drawing.DXFCodePage){SysCP2ACCP(ACodePage)};
  //  IODXFContext.Header.iDWGCodePage:=ZCCodePage2SysCP(drawing.DXFCodePage);//ACodePage;
  //end;

  DefaultFormatSettings.DecimalSeparator:='.';
  outstream.init(10*1024*1024);
  begin
    lph:=lps.StartLongProcess('Save DXF file',@outstream,drawing.pObjRoot^.ObjArray.Count);
    OldHandele2NewHandle:=TMapHandleToHandle.Create;
    templatefile.InitFromFile(TemplateFileName);
    { Обработчики NOD, подходящие по версии; хэндл NOD шаблона — из его
      секции OBJECTS (модель этапа 1) }
    NODSave:=TZNODSaveSession.Create(IODXFContext.Header.Version);
    NODSave.LoadTemplate(TemplateFileName);
    inlayertable:=False;
    inblocksec:=False;
    inblocktable:=False;
    instyletable:=False;
    ignoredsource:=False;
    invporttable:=False;
    inlttypetable:=False;
    indimstyletable:=False;
    inappidtable:=False;
    inobjectssec:=False;
    inclassessec:=False;
    innodobj:=False;
    pendingpair:=False;
    pendinggroups:='';
    pendingvalues:='';
    acadTableOwnerDone:=False;
    MakeVariablesDict(IODXFContext.VarsDict,drawing);
    processedvarscount:=IODXFContext.VarsDict.Count;
    while pendingpair or templatefile.notEOF do begin
      if pendingpair then begin
        groups:=pendinggroups;
        values:=pendingvalues;
        pendingpair:=False;
      end else begin
        groups:=templatefile.readString;
        values:=templatefile.readString;
      end;
      groupi:=StrToInt(groups);
      variablenotprocessed:=True;
      if (groupi=9)and(processedvarscount>0) then begin
        variablenotprocessed:=False;
        if IODXFContext.VarsDict.mygetvalue(values,ts) then begin
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
          groups:=templatefile.readString;
          templatefile.readString;
          outstream.TXTAddStringEOL(groups);
          if values='$HANDSEED' then
            handlepos:=outstream.Count;
          outstream.TXTAddStringEOL(dxfEnCodeString(ts,IODXFContext.Header));
          Dec(processedvarscount);
        end else
          variablenotprocessed:=True;
      end;
      if variablenotprocessed then
        if (groupi=5)  or (groupi=320)  or (groupi=330)  or (groupi=340)  or (groupi=350)  or  (groupi=1005)  or
          (groupi=390)  or (groupi=360)  or (groupi=105) then begin
          valuei:=StrToInt('$'+values);
          if valuei=0 then begin
            if not ignoredsource then begin
              outstream.TXTAddStringEOL(groups);
              outstream.TXTAddStringEOL('0');
            end;
          end else begin
            if inlayertable and (groupi=390) then
              plottablefansdle:=intable;  {поймать плоттабле}
            intable:=OldHandele2NewHandle.MyGetValue(valuei);
            if intable>0 then begin
              if not ignoredsource then begin
                outstream.TXTAddStringEOL(groups);
                outstream.TXTAddStringEOL(inttohex(intable,0));
              end;
              lasthandle:=intable;
            end else begin
              OldHandele2NewHandle.Add(valuei,IODXFContext.handle);
              if not ignoredsource then begin
                outstream.TXTAddStringEOL(groups);
                outstream.TXTAddStringEOL(inttohex(IODXFContext.handle,0));
              end;
              lasthandle:=IODXFContext.handle;
              Inc(IODXFContext.handle);
            end;
            if inlayertable and (groupi=390) then
              plottablefansdle:=lasthandle;  {поймать плоттабле}
            if indimstyletable and (groupi=5) then
              dimtablehandle:=lasthandle;  {поймать dimtable}
            { Начало объекта NOD шаблона: в него добавляются записи
              NOD-обработчиков (этап 4 ТЗ NOD) }
            if inobjectssec and (groupi=5) and (NODSave.TemplateNODHandle<>0)
               and (TDWGHandle(valuei)=NODSave.TemplateNODHandle) then
              innodobj:=True;
          end;
        end else if (groupi=2) and (values='CLASSES') then begin
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
          inclassessec:=True;
        end else if inclassessec and (groupi=1) then begin
          { Имя класса шаблона — для ClassesProc NOD-обработчиков }
          IODXFContext.TemplateClassNames.Add(Trim(values));
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
        end else if inclassessec and (groupi=0)
                    and (values=dxfName_ENDSEC) then begin
          { Прикладные модули объявляют здесь классы своих сущностей и
            объектов до закрывающего ENDSEC (issue #1409). }
          RunClassesSaveDxfProcs;
          inclassessec:=False;
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
        end else if (groupi=2) and (values='ENTITIES') then begin
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
          { Перед записью сущностей вычисляем хэндл владельца и выделяем
            хэндлы веток NOD (стилей таблиц) — сырые ACAD_TABLE ссылаются
            на них (issue #1339, этапы 3–4 ТЗ NOD). }
          PreallocateAcadTableOwnerHandle;
          ReserveNODHandles;
          saveentitiesdxf2000(@{p}drawing.pObjRoot^.ObjArray,outstream,drawing,IODXFContext);
        end else if (groupi=2) and (values='BLOCKS') then begin
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
          inblocksec:=True;
        end else if (inblocksec) and ((groupi=0) and (values=dxfName_ENDSEC)) then begin
          if drawing.BlockDefArray.Count>0 then
            for i:=0 to drawing.BlockDefArray.Count-1 do begin
              zDebugLn('{D}[DXF_CONTENTS]write BlockDef '+PBlockdefArray(drawing.BlockDefArray.parray)^[i].Name);
              outstream.TXTAddStringEOL(dxfGroupCode(0));
              outstream.TXTAddStringEOL('BLOCK');
              outstream.TXTAddStringEOL(dxfGroupCode(5));
              outstream.TXTAddStringEOL(inttohex(IODXFContext.handle{temphandle},0));
              Inc(IODXFContext.handle);
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL(dxfName_AcDbEntity);
              outstream.TXTAddStringEOL(dxfGroupCode(8));
              outstream.TXTAddStringEOL('0');
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL('AcDbBlockBegin');
              outstream.TXTAddStringEOL(dxfGroupCode(2));
              outstream.TXTAddStringEOL(dxfEnCodeString(PBlockdefArray(drawing.BlockDefArray.parray)^[i].Name,IODXFContext.Header));
              outstream.TXTAddStringEOL(dxfGroupCode(70));
              outstream.TXTAddStringEOL('2');
              outstream.TXTAddStringEOL(dxfGroupCode(10));
              outstream.TXTAddStringEOL(floattostr(PBlockdefArray({p}drawing.BlockDefArray.parray)^[i].base.x));
              outstream.TXTAddStringEOL(dxfGroupCode(20));
              outstream.TXTAddStringEOL(floattostr(PBlockdefArray({p}drawing.BlockDefArray.parray)^[i].base.y));
              outstream.TXTAddStringEOL(dxfGroupCode(30));
              outstream.TXTAddStringEOL(floattostr(PBlockdefArray({p}drawing.BlockDefArray.parray)^[i].base.z));
              outstream.TXTAddStringEOL(dxfGroupCode(3));
              outstream.TXTAddStringEOL(PBlockdefArray({p}drawing.BlockDefArray.parray)^[i].Name);
              outstream.TXTAddStringEOL(dxfGroupCode(1));
              outstream.TXTAddStringEOL('');

              saveentitiesdxf2000(@PBlockdefArray(drawing.BlockDefArray.parray)^[i].ObjArray,outstream,drawing,IODXFContext);

              outstream.TXTAddStringEOL(dxfGroupCode(0));
              outstream.TXTAddStringEOL('ENDBLK');
              outstream.TXTAddStringEOL(dxfGroupCode(5));
              outstream.TXTAddStringEOL(inttohex(IODXFContext.handle,0));
              Inc(IODXFContext.handle);
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL(dxfName_AcDbEntity);
              outstream.TXTAddStringEOL(dxfGroupCode(8));
              outstream.TXTAddStringEOL('0');
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL('AcDbBlockEnd');

              dxfStringWithoutEncodeOut(outstream,1001,ZCADAppNameInDXF);
              dxfStringWithoutEncodeOut(outstream,1002,'{');
              if assigned(PBlockdefArray(drawing.BlockDefArray.parray)^[i].EntExtensions) then
                PBlockdefArray(drawing.BlockDefArray.parray)^[i].EntExtensions.RunSaveToDxf(outstream,@PBlockdefArray(
                  drawing.BlockDefArray.parray)^[i],IODXFContext);
              dxfStringWithoutEncodeOut(outstream,1002,'}');

            end;

          outstream.TXTAddStringEOL(dxfGroupCode(0));
          outstream.TXTAddStringEOL(dxfName_ENDSEC);


          inblocksec:=False;
        end else if (invporttable) and ((groupi=0) and (values=dxfName_ENDTAB)) then begin
          invporttable:=False;
          ignoredsource:=False;

          outstream.TXTAddStringEOL(dxfGroupCode(5));
          outstream.TXTAddStringEOL(inttohex(IODXFContext.handle,0));
          vporttablehandle:=IODXFContext.handle;
          Inc(IODXFContext.handle);

          outstream.TXTAddStringEOL(dxfGroupCode(330));
          outstream.TXTAddStringEOL('0');
          outstream.TXTAddStringEOL(dxfGroupCode(100));
          outstream.TXTAddStringEOL('AcDbSymbolTable');
          outstream.TXTAddStringEOL(dxfGroupCode(70));
          outstream.TXTAddStringEOL('1');
          outstream.TXTAddStringEOL(dxfGroupCode(0));
          outstream.TXTAddStringEOL('VPORT');
          outstream.TXTAddStringEOL(dxfGroupCode(5));
          outstream.TXTAddStringEOL(inttohex(IODXFContext.handle,0));
          Inc(IODXFContext.handle);
          outstream.TXTAddStringEOL(dxfGroupCode(330));
          outstream.TXTAddStringEOL(inttohex(vporttablehandle,0));

          outstream.TXTAddStringEOL(dxfGroupCode(100));
          outstream.TXTAddStringEOL('AcDbSymbolTableRecord');
          outstream.TXTAddStringEOL(dxfGroupCode(100));
          outstream.TXTAddStringEOL('AcDbViewportTableRecord');

          outstream.TXTAddStringEOL(dxfGroupCode(2));
          outstream.TXTAddStringEOL('*Active');
          outstream.TXTAddStringEOL(dxfGroupCode(70));
          outstream.TXTAddStringEOL('0');

          outstream.TXTAddStringEOL(dxfGroupCode(10));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(20));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(11));
          outstream.TXTAddStringEOL('1.0');
          outstream.TXTAddStringEOL(dxfGroupCode(21));
          outstream.TXTAddStringEOL('1.0');

          if assigned(drawing.wa)and(drawing.wa.getviewcontrol<>nil) then begin
            outstream.TXTAddStringEOL(dxfGroupCode(12));
            outstream.TXTAddStringEOL(floattostr(drawing.wa.param.CPoint.x));
            outstream.TXTAddStringEOL(dxfGroupCode(22));
            outstream.TXTAddStringEOL(floattostr(drawing.wa.param.CPoint.y));
          end else begin
            outstream.TXTAddStringEOL(dxfGroupCode(12));
            outstream.TXTAddStringEOL('0');
            outstream.TXTAddStringEOL(dxfGroupCode(22));
            outstream.TXTAddStringEOL('0');
          end;
          outstream.TXTAddStringEOL(dxfGroupCode(13));
          outstream.TXTAddStringEOL(floattostr(drawing.Snap.Base.x));
          outstream.TXTAddStringEOL(dxfGroupCode(23));
          outstream.TXTAddStringEOL(floattostr(drawing.Snap.Base.y));
          outstream.TXTAddStringEOL(dxfGroupCode(14));
          outstream.TXTAddStringEOL(floattostr(drawing.Snap.Spacing.x));
          outstream.TXTAddStringEOL(dxfGroupCode(24));
          outstream.TXTAddStringEOL(floattostr(drawing.Snap.Spacing.y));
          outstream.TXTAddStringEOL(dxfGroupCode(15));
          outstream.TXTAddStringEOL(floattostr(drawing.GridSpacing.x));
          outstream.TXTAddStringEOL(dxfGroupCode(25));
          outstream.TXTAddStringEOL(floattostr(drawing.GridSpacing.y));
          outstream.TXTAddStringEOL(dxfGroupCode(16));
          outstream.TXTAddStringEOL(floattostr(-drawing.GetPCamera^.prop.look.x));
          outstream.TXTAddStringEOL(dxfGroupCode(26));
          outstream.TXTAddStringEOL(floattostr(-drawing.GetPCamera^.prop.look.y));
          outstream.TXTAddStringEOL(dxfGroupCode(36));
          outstream.TXTAddStringEOL(floattostr(-drawing.GetPCamera^.prop.look.z));
          outstream.TXTAddStringEOL(dxfGroupCode(17));
          outstream.TXTAddStringEOL(floattostr(0));
          outstream.TXTAddStringEOL(dxfGroupCode(27));
          outstream.TXTAddStringEOL(floattostr(0));
          outstream.TXTAddStringEOL(dxfGroupCode(37));
          outstream.TXTAddStringEOL(floattostr(0));
          outstream.TXTAddStringEOL(dxfGroupCode(40));
          if assigned(drawing.wa)and(drawing.wa.getviewcontrol<>nil) then
            outstream.TXTAddStringEOL(floattostr(drawing.wa.param.ViewHeight))
          else
            outstream.TXTAddStringEOL(IntToStr(500));
          outstream.TXTAddStringEOL(dxfGroupCode(41));
          if assigned(drawing.wa)and(drawing.wa.getviewcontrol<>nil) then
            outstream.TXTAddStringEOL(
              floattostr(drawing.wa.getviewcontrol.ClientWidth/drawing.wa.getviewcontrol.ClientHeight))
          else
            outstream.TXTAddStringEOL(IntToStr(1));
          outstream.TXTAddStringEOL(dxfGroupCode(42));
          outstream.TXTAddStringEOL('50.0');
          outstream.TXTAddStringEOL(dxfGroupCode(43));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(44));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(50));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(51));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(71));
          outstream.TXTAddStringEOL('0');
          outstream.TXTAddStringEOL(dxfGroupCode(72));
          outstream.TXTAddStringEOL('1000');
          outstream.TXTAddStringEOL(dxfGroupCode(73));
          outstream.TXTAddStringEOL('1');
          outstream.TXTAddStringEOL(dxfGroupCode(74));
          outstream.TXTAddStringEOL('3');
          outstream.TXTAddStringEOL(dxfGroupCode(75));
          if drawing.SnapGrid then
            outstream.TXTAddStringEOL('1')
          else
            outstream.TXTAddStringEOL('0');
          outstream.TXTAddStringEOL(dxfGroupCode(76));
          if drawing.DrawGrid then
            outstream.TXTAddStringEOL('1')
          else
           outstream.TXTAddStringEOL('0');
          outstream.TXTAddStringEOL(dxfGroupCode(77));
          outstream.TXTAddStringEOL('0');
          outstream.TXTAddStringEOL(dxfGroupCode(78));
          outstream.TXTAddStringEOL('0');
          outstream.TXTAddStringEOL(dxfGroupCode(281));
          outstream.TXTAddStringEOL('0');
          outstream.TXTAddStringEOL(dxfGroupCode(65));
          outstream.TXTAddStringEOL('1');
          outstream.TXTAddStringEOL(dxfGroupCode(110));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(120));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(130));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(111));
          outstream.TXTAddStringEOL('1.0');
          outstream.TXTAddStringEOL(dxfGroupCode(121));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(131));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(112));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(122));
          outstream.TXTAddStringEOL('1.0');
          outstream.TXTAddStringEOL(dxfGroupCode(132));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(79));
          outstream.TXTAddStringEOL('0');
          outstream.TXTAddStringEOL(dxfGroupCode(146));
          outstream.TXTAddStringEOL('0.0');
          outstream.TXTAddStringEOL(dxfGroupCode(0));
          outstream.TXTAddStringEOL('ENDTAB');

        end else if (inblocktable) and ((groupi=0) and (values=dxfName_ENDTAB)) then begin
          inblocktable:=False;
          if drawing.BlockDefArray.Count>0 then

            for i:=0 to drawing.BlockDefArray.Count-1 do begin
              outstream.TXTAddStringEOL(dxfGroupCode(0));
              outstream.TXTAddStringEOL(dxfName_BLOCK_RECORD);

              IODXFContext.p2h.MyGetOrCreateValue(@(PBlockdefArray(drawing.BlockDefArray.parray)^[i]),IODXFContext.handle,temphandle);
              { Запоминаем «имя блока -> новый хэндл BLOCK_RECORD», чтобы
                переписать ссылку 343 сырой сущности ACAD_TABLE на её
                анонимный блок (issue #1339). }
              if not IODXFContext.BlockNameHandleMap.MyContans(
                   PBlockdefArray(drawing.BlockDefArray.parray)^[i].Name) then
                IODXFContext.BlockNameHandleMap.Add(
                  PBlockdefArray(drawing.BlockDefArray.parray)^[i].Name,
                  inttohex(temphandle,0));
              outstream.TXTAddStringEOL(dxfGroupCode(5));
              outstream.TXTAddStringEOL(inttohex(temphandle,0));
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL(dxfName_AcDbSymbolTableRecord);
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL('AcDbBlockTableRecord');
              outstream.TXTAddStringEOL(dxfGroupCode(2));
              outstream.TXTAddStringEOL(dxfEnCodeString(PBlockdefArray(drawing.BlockDefArray.parray)^[i].Name,IODXFContext.Header));

            end;
          outstream.TXTAddStringEOL(dxfGroupCode(0));
          outstream.TXTAddStringEOL(dxfName_ENDTAB);
        end else if (inlayertable) and ((groupi=0) and (values=dxfName_ENDTAB)) then begin
          inlayertable:=False;
          ignoredsource:=False;
          plp:=drawing.layertable.beginiterate(ir);
          if plp<>nil then
            repeat
              outstream.TXTAddStringEOL(dxfGroupCode(0));
              outstream.TXTAddStringEOL(dxfName_Layer);
              outstream.TXTAddStringEOL(dxfGroupCode(5));
              outstream.TXTAddStringEOL(inttohex(IODXFContext.handle,0));
              Inc(IODXFContext.handle);
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL(dxfName_AcDbSymbolTableRecord);
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL('AcDbLayerTableRecord');
              outstream.TXTAddStringEOL(dxfGroupCode(2));
              outstream.TXTAddStringEOL(dxfEnCodeString(plp^.Name,IODXFContext.Header));
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
              outstream.TXTAddStringEOL(dxfEnCodeString(GetLTName(plp^.LT),IODXFContext.Header));
              outstream.TXTAddStringEOL(dxfGroupCode(290));
              if plp^._print then
                outstream.TXTAddStringEOL('1')
              else
                outstream.TXTAddStringEOL('0');
              outstream.TXTAddStringEOL(dxfGroupCode(370));
              outstream.TXTAddStringEOL(IntToStr(plp^.lineweight));
              outstream.TXTAddStringEOL(dxfGroupCode(390));
              outstream.TXTAddStringEOL(inttohex(plottablefansdle,0));

              if plp^.desk<>'' then begin
                outstream.TXTAddStringEOL(dxfGroupCode(1001));
                outstream.TXTAddStringEOL('AcAecLayerStandard');
                outstream.TXTAddStringEOL(dxfGroupCode(1000));
                outstream.TXTAddStringEOL('');
                outstream.TXTAddStringEOL(dxfGroupCode(1000));
                outstream.TXTAddStringEOL(dxfEnCodeString(plp^.desk,IODXFContext.Header));
              end;

              plp:=drawing.layertable.iterate(ir);
            until plp=nil;

          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
        end
        else if (inlttypetable) and ((groupi=0) and (values=dxfName_ENDTAB)) then begin
          inlttypetable:=False;
          ignoredsource:=False;
          temphandle:=IODXFContext.handle-1;
          pltp:=drawing.LTypeStyleTable.beginiterate(ir);
          if pltp<>nil then
            repeat
              zDebugLn('{D}[DXF_CONTENTS]write linetype '+pltp^.Name);
              outstream.TXTAddStringEOL(dxfGroupCode(0));
              outstream.TXTAddStringEOL(dxfName_LTYPE);
              IODXFContext.p2h.MyGetOrCreateValue(pltp,IODXFContext.handle,temphandle);
              outstream.TXTAddStringEOL(dxfGroupCode(5));
              outstream.TXTAddStringEOL(inttohex(temphandle,0));
              outstream.TXTAddStringEOL(dxfGroupCode(330));
              outstream.TXTAddStringEOL(inttohex(temphandle,0));
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL(dxfName_AcDbSymbolTableRecord);
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL('AcDbLinetypeTableRecord');
              outstream.TXTAddStringEOL(dxfGroupCode(2));
              outstream.TXTAddStringEOL(dxfEnCodeString(pltp^.Name,IODXFContext.Header));
              outstream.TXTAddStringEOL(dxfGroupCode(70));
              outstream.TXTAddStringEOL('0');
              outstream.TXTAddStringEOL(dxfGroupCode(3));
              outstream.TXTAddStringEOL(dxfEnCodeString(pltp^.desk,IODXFContext.Header));
              outstream.TXTAddStringEOL(dxfGroupCode(72));
              outstream.TXTAddStringEOL('65');
              i:=pltp^.strokesarray.GetRealCount;
              outstream.TXTAddStringEOL(dxfGroupCode(73));
              outstream.TXTAddStringEOL(IntToStr(i));
              outstream.TXTAddStringEOL(dxfGroupCode(40));
              outstream.TXTAddStringEOL(floattostr(pltp^.LengthDXF));
              if i>0 then begin
                TDI:=pltp^.dasharray.beginiterate(ir2);
                PStroke:=pltp^.strokesarray.beginiterate(ir3);
                PSP:=pltp^.shapearray.beginiterate(ir4);
                PTP:=pltp^.textarray.beginiterate(ir5);
                laststrokewrited:=False;
                if PStroke<>nil then
                  repeat
                    case TDI^ of
                      TDIDash:begin
                        if laststrokewrited then begin
                          outstream.TXTAddStringEOL(dxfGroupCode(74));
                          outstream.TXTAddStringEOL('0');
                        end;
                        outstream.TXTAddStringEOL(dxfGroupCode(49));
                        outstream.TXTAddStringEOL(floattostr(PStroke^));
                        PStroke:=pltp^.strokesarray.iterate(ir3);
                        laststrokewrited:=True;
                      end;
                      TDIShape:if PSP^.param.PStyle<>nil then begin
                          laststrokewrited:=False;
                          outstream.TXTAddStringEOL(dxfGroupCode(74));
                          outstream.TXTAddStringEOL('4');
                          outstream.TXTAddStringEOL(dxfGroupCode(75));
                          outstream.TXTAddStringEOL(IntToStr(PSP^.ShapeNum));

                          IODXFContext.p2h.MyGetOrCreateValue(PSP^.param.PStyle,IODXFContext.handle,temphandle);
                          outstream.TXTAddStringEOL(dxfGroupCode(340));
                          outstream.TXTAddStringEOL(inttohex(temphandle,0));
                          outstream.TXTAddStringEOL(dxfGroupCode(46));
                          outstream.TXTAddStringEOL(floattostr(PSP^.param.Height));
                          outstream.TXTAddStringEOL(dxfGroupCode(50));
                          outstream.TXTAddStringEOL(floattostr(PSP^.param.Angle));
                          outstream.TXTAddStringEOL(dxfGroupCode(44));
                          outstream.TXTAddStringEOL(floattostr(PSP^.param.X));
                          outstream.TXTAddStringEOL(dxfGroupCode(45));
                          outstream.TXTAddStringEOL(floattostr(PSP^.param.Y));
                          PSP:=pltp^.shapearray.iterate(ir4);
                        end;
                      TDIText:begin
                        laststrokewrited:=False;
                        outstream.TXTAddStringEOL(dxfGroupCode(74));
                        outstream.TXTAddStringEOL('2');
                        outstream.TXTAddStringEOL(dxfGroupCode(75));
                        outstream.TXTAddStringEOL('0');

                        IODXFContext.p2h.MyGetOrCreateValue(PTP^.param.PStyle,IODXFContext.handle,temphandle);
                        outstream.TXTAddStringEOL(dxfGroupCode(340));
                        outstream.TXTAddStringEOL(inttohex(temphandle,0));
                        outstream.TXTAddStringEOL(dxfGroupCode(46));
                        outstream.TXTAddStringEOL(floattostr(PTP^.param.Height));
                        outstream.TXTAddStringEOL(dxfGroupCode(50));
                        outstream.TXTAddStringEOL(floattostr(PTP^.param.Angle));
                        outstream.TXTAddStringEOL(dxfGroupCode(44));
                        outstream.TXTAddStringEOL(floattostr(PTP^.param.X));
                        outstream.TXTAddStringEOL(dxfGroupCode(45));
                        outstream.TXTAddStringEOL(floattostr(PTP^.param.Y));
                        outstream.TXTAddStringEOL(dxfGroupCode(9));
                        outstream.TXTAddStringEOL(PTP^.Text);
                        PTP:=pltp^.textarray.iterate(ir5);
                      end;
                    end;
                    TDI:=pltp^.dasharray.iterate(ir2);
                  until TDI=nil;
                if laststrokewrited then begin
                  outstream.TXTAddStringEOL(dxfGroupCode(74));
                  outstream.TXTAddStringEOL('0');
                end;

              end;
              pltp:=drawing.LTypeStyleTable.iterate(ir);
            until pltp=nil;
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
        end else if (indimstyletable) and ((groupi=0) and (values=dxfName_ENDTAB)) then begin
          { TODO :  надо писать заголовок таблицы руками, а не из шаблона DXF, т.к. там есть перечень стилей который проебывается}
          indimstyletable:=False;
          ignoredsource:=False;
          //дальше идут стили
          pdsp:=drawing.DimStyleTable.beginiterate(ir);
          if pdsp<>nil then
            repeat
              outstream.TXTAddStringEOL(dxfGroupCode(0));
              outstream.TXTAddStringEOL('DIMSTYLE');
              outstream.TXTAddStringEOL(dxfGroupCode(105));
              outstream.TXTAddStringEOL(inttohex(IODXFContext.handle,0));
              Inc(IODXFContext.handle);

              outstream.TXTAddStringEOL(dxfGroupCode(330));
              outstream.TXTAddStringEOL(inttohex(dimtablehandle,0));

              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL('AcDbSymbolTableRecord');
              outstream.TXTAddStringEOL(dxfGroupCode(100));
              outstream.TXTAddStringEOL('AcDbDimStyleTableRecord');
              outstream.TXTAddStringEOL(dxfGroupCode(2));
              outstream.TXTAddStringEOL(dxfEncodeString(pdsp^.Name,IODXFContext.Header));
              outstream.TXTAddStringEOL(dxfGroupCode(3));
              outstream.TXTAddStringEOL(pdsp^.Units.DIMPOST);
              outstream.TXTAddStringEOL(dxfGroupCode(70));
              outstream.TXTAddStringEOL('0');

              //тут сами настройки
              outstream.TXTAddStringEOL(dxfGroupCode(40));
              outstream.TXTAddStringEOL(floattostr(pdsp^.Units.DIMSCALE));
              outstream.TXTAddStringEOL(dxfGroupCode(44));
              outstream.TXTAddStringEOL(floattostr(pdsp^.Lines.DIMEXE));
              outstream.TXTAddStringEOL(dxfGroupCode(42));
              outstream.TXTAddStringEOL(floattostr(pdsp^.Lines.DIMEXO));
              outstream.TXTAddStringEOL(dxfGroupCode(46));
              outstream.TXTAddStringEOL(floattostr(pdsp^.Lines.DIMDLE));

              outstream.TXTAddStringEOL(dxfGroupCode(41));
              outstream.TXTAddStringEOL(floattostr(pdsp^.Arrows.DIMASZ));

              outstream.TXTAddStringEOL(dxfGroupCode(173));
              if pdsp^.Arrows.DIMBLK1<>pdsp^.Arrows.DIMBLK2 then begin
                outstream.TXTAddStringEOL('1');
              end else begin
                outstream.TXTAddStringEOL('0');
              end;

              if pdsp^.Arrows.DIMLDRBLK<>TSClosedFilled then begin
                IODXFContext.p2h.MyGetOrCreateValue(drawing.BlockDefArray.getblockdef(pdsp^.GetDimBlockParam(-1).Name),
                  IODXFContext.handle,temphandle);
                outstream.TXTAddStringEOL(dxfGroupCode(341));
                outstream.TXTAddStringEOL(inttohex(temphandle,0));
              end;


              if pdsp^.Arrows.DIMBLK1<>pdsp^.Arrows.DIMBLK2 then begin
                if pdsp^.Arrows.DIMBLK1<>TSClosedFilled then begin
                  IODXFContext.p2h.MyGetOrCreateValue(
                    drawing.BlockDefArray.getblockdef(pdsp^.GetDimBlockParam(0).Name),IODXFContext.handle,temphandle);
                  if temphandle<>0 then begin
                    outstream.TXTAddStringEOL(dxfGroupCode(343));
                    outstream.TXTAddStringEOL(inttohex(temphandle,0));
                  end;
                end;
                if pdsp^.Arrows.DIMBLK2<>TSClosedFilled then begin
                  IODXFContext.p2h.MyGetOrCreateValue(
                    drawing.BlockDefArray.getblockdef(pdsp^.GetDimBlockParam(1).Name),IODXFContext.handle,temphandle);
                  if temphandle<>0 then begin
                    outstream.TXTAddStringEOL(dxfGroupCode(344));
                    outstream.TXTAddStringEOL(inttohex(temphandle,0));
                  end;
                end;
              end else begin
                if pdsp^.Arrows.DIMBLK1<>TSClosedFilled then begin
                  IODXFContext.p2h.MyGetOrCreateValue(drawing.BlockDefArray.getblockdef(
                    pdsp^.GetDimBlockParam(0).Name),IODXFContext.handle,temphandle);
                  if temphandle<>0 then begin
                    outstream.TXTAddStringEOL(dxfGroupCode(342));
                    outstream.TXTAddStringEOL(inttohex(temphandle,0));
                  end;
                end;
              end;

              outstream.TXTAddStringEOL(dxfGroupCode(140));
              outstream.TXTAddStringEOL(floattostr(pdsp^.Text.DIMTXT));

              outstream.TXTAddStringEOL(dxfGroupCode(141));
              outstream.TXTAddStringEOL(floattostr(pdsp^.Lines.DIMCEN));

              outstream.TXTAddStringEOL(dxfGroupCode(73));
              if pdsp^.Text.DIMTIH then
                outstream.TXTAddStringEOL('1')
              else
                outstream.TXTAddStringEOL('0');
              outstream.TXTAddStringEOL(dxfGroupCode(74));
              if pdsp^.Text.DIMTOH then
                outstream.TXTAddStringEOL('1')
              else
                outstream.TXTAddStringEOL('0');
              outstream.TXTAddStringEOL(dxfGroupCode(147));
              outstream.TXTAddStringEOL(floattostr(pdsp^.Text.DIMGAP));

              outstream.TXTAddStringEOL(dxfGroupCode(77));
              case pdsp^.Text.DIMTAD of
                DTVPCenters:outstream.TXTAddStringEOL('0');
                DTVPAbove:outstream.TXTAddStringEOL('1');
                DTVPOutside:outstream.TXTAddStringEOL('2');
                DTVPJIS:outstream.TXTAddStringEOL('3');
                DTVPBellov:outstream.TXTAddStringEOL('4');
              end;{case}

              outstream.TXTAddStringEOL(dxfGroupCode(144));
              outstream.TXTAddStringEOL(floattostr(pdsp^.Units.DIMLFAC));
              outstream.TXTAddStringEOL(dxfGroupCode(271));
              outstream.TXTAddStringEOL(IntToStr(pdsp^.Units.DIMDEC));
              outstream.TXTAddStringEOL(dxfGroupCode(45));
              outstream.TXTAddStringEOL(floattostr(pdsp^.Units.DIMRND));

              outstream.TXTAddStringEOL(dxfGroupCode(277));
              case pdsp^.Units.DIMLUNIT of
                DUScientific:outstream.TXTAddStringEOL('1');
                DUDecimal:outstream.TXTAddStringEOL('2');
                DUEngineering:outstream.TXTAddStringEOL('3');
                DUArchitectural:outstream.TXTAddStringEOL('4');
                DUFractional:outstream.TXTAddStringEOL('5');
                DUSystem:outstream.TXTAddStringEOL('6');
              end;{case}
              outstream.TXTAddStringEOL(dxfGroupCode(278));
              case pdsp^.Units.DIMDSEP of
                DDSDot:outstream.TXTAddStringEOL('46');
                DDSComma:outstream.TXTAddStringEOL('44');
                DDSSpace:outstream.TXTAddStringEOL('32');
              end;{case}
              outstream.TXTAddStringEOL(dxfGroupCode(279));
              case pdsp^.Placing.DIMTMOVE of
                DTMMoveDimLine:outstream.TXTAddStringEOL('0');
                DTMCreateLeader:outstream.TXTAddStringEOL('1');
                DTMnothung:outstream.TXTAddStringEOL('2');
              end;{case}

              if pdsp^.Lines.DIMLWD<>DIMLWDDefaultValue then begin
                outstream.TXTAddStringEOL(dxfGroupCode(371));
                outstream.TXTAddStringEOL(IntToStr(pdsp^.Lines.DIMLWD));
              end;
              if pdsp^.Lines.DIMLWE<>DIMLWEDefaultValue then begin
                outstream.TXTAddStringEOL(dxfGroupCode(372));
                outstream.TXTAddStringEOL(IntToStr(pdsp^.Lines.DIMLWE));
              end;

              if pdsp^.Lines.DIMCLRD<>DIMCLRDDefaultValue then begin
                outstream.TXTAddStringEOL(dxfGroupCode(176));
                outstream.TXTAddStringEOL(IntToStr(pdsp^.Lines.DIMCLRD));
              end;
              if pdsp^.Lines.DIMCLRE<>DIMCLREDefaultValue then begin
                outstream.TXTAddStringEOL(dxfGroupCode(177));
                outstream.TXTAddStringEOL(IntToStr(pdsp^.Lines.DIMCLRE));
              end;
              if pdsp^.Text.DIMCLRT<>DIMCLRTDefaultValue then begin
                outstream.TXTAddStringEOL(dxfGroupCode(178));
                outstream.TXTAddStringEOL(IntToStr(pdsp^.Text.DIMCLRT));
              end;

              outstream.TXTAddStringEOL(dxfGroupCode(340));
              p:=pdsp^.Text.DIMTXSTY;

              IODXFContext.p2h.MyGetOrCreateValue(p,IODXFContext.handle,temphandle);

              outstream.TXTAddStringEOL(inttohex(temphandle,0));

              pltp:=drawing.LTypeStyleTable.GetSystemLT(TLTByBlock);
              if (pdsp^.Lines.DIMLTYPE<>pltp)and(pdsp^.Lines.DIMLTYPE<>nil) then begin
                outstream.TXTAddStringEOL(dxfGroupCode(1001));
                outstream.TXTAddStringEOL('ACAD_DSTYLE_DIM_LINETYPE');
                outstream.TXTAddStringEOL(dxfGroupCode(1070));
                outstream.TXTAddStringEOL('380');
                outstream.TXTAddStringEOL(dxfGroupCode(1005));
                IODXFContext.p2h.MyGetOrCreateValue(pdsp^.Lines.DIMLTYPE,IODXFContext.handle,temphandle);
                outstream.TXTAddStringEOL(inttohex(temphandle,0));
              end;
              if (pdsp^.Lines.DIMLTEX1<>pltp)and(pdsp^.Lines.DIMLTEX1<>nil) then begin
                outstream.TXTAddStringEOL(dxfGroupCode(1001));
                outstream.TXTAddStringEOL('ACAD_DSTYLE_DIM_EXT1_LINETYPE');
                outstream.TXTAddStringEOL(dxfGroupCode(1070));
                outstream.TXTAddStringEOL('381');
                outstream.TXTAddStringEOL(dxfGroupCode(1005));
                IODXFContext.p2h.MyGetOrCreateValue(pdsp^.Lines.DIMLTEX1,IODXFContext.handle,temphandle);
                outstream.TXTAddStringEOL(inttohex(temphandle,0));
              end;
              if (pdsp^.Lines.DIMLTEX2<>pltp)and(pdsp^.Lines.DIMLTEX2<>nil) then begin
                outstream.TXTAddStringEOL(dxfGroupCode(1001));
                outstream.TXTAddStringEOL('ACAD_DSTYLE_DIM_EXT2_LINETYPE');
                outstream.TXTAddStringEOL(dxfGroupCode(1070));
                outstream.TXTAddStringEOL('382');
                outstream.TXTAddStringEOL(dxfGroupCode(1005));
                IODXFContext.p2h.MyGetOrCreateValue(pdsp^.Lines.DIMLTEX2,IODXFContext.handle,temphandle);
                outstream.TXTAddStringEOL(inttohex(temphandle,0));
              end;

              pdsp:=drawing.DimStyleTable.iterate(ir);
            until pdsp=nil;
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);

        end else if (groupi=0) and (values=dxfName_ENDTAB)and inappidtable then begin
          inappidtable:=False;
          ignoredsource:=False;

          RegisterAcadAppInDXF('ACAD',@outstream,IODXFContext.handle);
          RegisterAcadAppInDXF('ACAD_PSEXT',@outstream,IODXFContext.handle);
          RegisterAcadAppInDXF('AcAecLayerStandard',@outstream,IODXFContext.handle);
          RegisterAcadAppInDXF(ZCADAppNameInDXF,@outstream,IODXFContext.handle);
          RegisterAcadAppInDXF('ACAD_DSTYLE_DIM_LINETYPE',@outstream,IODXFContext.handle);
          RegisterAcadAppInDXF('ACAD_DSTYLE_DIM_EXT1_LINETYPE',@outstream,IODXFContext.handle);
          RegisterAcadAppInDXF('ACAD_DSTYLE_DIM_EXT2_LINETYPE',@outstream,IODXFContext.handle);

          outstream.TXTAddStringEOL(dxfGroupCode(0));
          outstream.TXTAddStringEOL('ENDTAB');
        end else if (instyletable) and ((groupi=0) and (values=dxfName_ENDTAB)) then begin
          instyletable:=False;
          ignoredsource:=False;
          temphandle2:=IODXFContext.handle-2;
          if drawing.TextStyleTable.GetRealCount>0 then begin
            pcurrtextstyle:=drawing.TextStyleTable.beginiterate(ir);
            if pcurrtextstyle<>nil then
              repeat
                if pcurrtextstyle^.UsedInLTYPE then begin
                  outstream.TXTAddStringEOL(dxfGroupCode(0));
                  outstream.TXTAddStringEOL(dxfName_Style);
                  p:=pcurrtextstyle;

                  IODXFContext.p2h.MyGetOrCreateValue(pcurrtextstyle,IODXFContext.handle,temphandle);
                  outstream.TXTAddStringEOL(dxfGroupCode(5));
                  outstream.TXTAddStringEOL(inttohex(temphandle,0));
                  Inc(IODXFContext.handle);
                  outstream.TXTAddStringEOL(dxfGroupCode(330));
                  outstream.TXTAddStringEOL(inttohex(temphandle2,0));
                  outstream.TXTAddStringEOL(dxfGroupCode(100));
                  outstream.TXTAddStringEOL(dxfName_AcDbSymbolTableRecord);
                  outstream.TXTAddStringEOL(dxfGroupCode(100));
                  outstream.TXTAddStringEOL('AcDbTextStyleTableRecord');
                  outstream.TXTAddStringEOL(dxfGroupCode(2));
                  outstream.TXTAddStringEOL('');
                  outstream.TXTAddStringEOL(dxfGroupCode(70));
                  outstream.TXTAddStringEOL('1');

                  outstream.TXTAddStringEOL(dxfGroupCode(40));
                  outstream.TXTAddStringEOL(floattostr(pcurrtextstyle^.prop.size));

                  outstream.TXTAddStringEOL(dxfGroupCode(41));
                  outstream.TXTAddStringEOL(floattostr(pcurrtextstyle^.prop.wfactor));

                  outstream.TXTAddStringEOL(dxfGroupCode(50));
                  outstream.TXTAddStringEOL(floattostr(pcurrtextstyle^.prop.oblique*180/pi));

                  outstream.TXTAddStringEOL(dxfGroupCode(71));
                  outstream.TXTAddStringEOL('0');

                  outstream.TXTAddStringEOL(dxfGroupCode(42));
                  outstream.TXTAddStringEOL('2.5');

                  outstream.TXTAddStringEOL(dxfGroupCode(3));
                  outstream.TXTAddStringEOL(pcurrtextstyle^.FontFile);

                  outstream.TXTAddStringEOL(dxfGroupCode(4));
                  outstream.TXTAddStringEOL('');

                end else begin
                  outstream.TXTAddStringEOL(dxfGroupCode(0));
                  outstream.TXTAddStringEOL(dxfName_Style);
                  outstream.TXTAddStringEOL(dxfGroupCode(5));

                  p:=pcurrtextstyle;
                  IODXFContext.p2h.MyGetOrCreateValue(p,IODXFContext.handle,temphandle);
                  if not IODXFContext.TextStyleNameHandleMap.MyContans(
                    pcurrtextstyle^.Name) then
                    IODXFContext.TextStyleNameHandleMap.Add(
                      pcurrtextstyle^.Name, inttohex(temphandle,0));
                  outstream.TXTAddStringEOL(inttohex(temphandle,0));

                  outstream.TXTAddStringEOL(dxfGroupCode(330));
                  outstream.TXTAddStringEOL(inttohex(temphandle2,0));
                  outstream.TXTAddStringEOL(dxfGroupCode(100));
                  outstream.TXTAddStringEOL(dxfName_AcDbSymbolTableRecord);
                  outstream.TXTAddStringEOL(dxfGroupCode(100));
                  outstream.TXTAddStringEOL('AcDbTextStyleTableRecord');
                  outstream.TXTAddStringEOL(dxfGroupCode(2));
                  outstream.TXTAddStringEOL(dxfEncodeString(pcurrtextstyle^.Name,IODXFContext.Header));
                  outstream.TXTAddStringEOL(dxfGroupCode(70));
                  outstream.TXTAddStringEOL('0');

                  outstream.TXTAddStringEOL(dxfGroupCode(40));
                  outstream.TXTAddStringEOL(floattostr(pcurrtextstyle^.prop.size));

                  outstream.TXTAddStringEOL(dxfGroupCode(41));
                  outstream.TXTAddStringEOL(floattostr(pcurrtextstyle^.prop.wfactor));

                  outstream.TXTAddStringEOL(dxfGroupCode(50));
                  outstream.TXTAddStringEOL(floattostr(pcurrtextstyle^.prop.oblique*180/pi));

                  outstream.TXTAddStringEOL(dxfGroupCode(71));
                  outstream.TXTAddStringEOL('0');

                  outstream.TXTAddStringEOL(dxfGroupCode(42));
                  outstream.TXTAddStringEOL('2.5');

                  outstream.TXTAddStringEOL(dxfGroupCode(3));
                  outstream.TXTAddStringEOL(pcurrtextstyle^.FontFile);

                  outstream.TXTAddStringEOL(dxfGroupCode(4));
                  outstream.TXTAddStringEOL('');
                  if pcurrtextstyle^.FontFamily<>'' then begin
                    outstream.TXTAddStringEOL(dxfGroupCode(1001));
                    outstream.TXTAddStringEOL('ACAD');
                    outstream.TXTAddStringEOL(dxfGroupCode(1000));
                    outstream.TXTAddStringEOL(pcurrtextstyle^.FontFamily);
                  end;
                end;
                pcurrtextstyle:=drawing.TextStyleTable.iterate(ir);
              until pcurrtextstyle=nil;
          end;
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
        end
        else if (groupi=0) and (values=dxfName_TABLE) then begin
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
          groups:=templatefile.readString;
          values:=templatefile.readString;
          groupi:=StrToInt(groups);
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
          if (groupi=2) and (values=dxfName_Layer) then begin
            inlayertable:=True;
          end else if (groupi=2) and (values=dxfName_BLOCK_RECORD) then begin
            inblocktable:=True;
          end else if (groupi=2) and (values=dxfName_Style) then begin
            instyletable:=True;
          end else if (groupi=2) and (values=dxfName_LType) then begin
            inlttypetable:=True;
          end else if (groupi=2) and (values='DIMSTYLE') then begin
            indimstyletable:=True;
          end else if (groupi=2) and (values='APPID') then begin
            inappidtable:=True;
          end else if (groupi=2) and (values='VPORT') then begin
            invporttable:=True;
            IgnoredSource:=True;
          end;

        end else if (groupi=0) and (values=dxfName_Layer)and inlayertable then begin
          IgnoredSource:=True;
        end else if (groupi=0) and (values='APPID')and inappidtable then begin
          IgnoredSource:=True;
        end else if (groupi=0) and (values=dxfName_Style)and instyletable then begin
          IgnoredSource:=True;
        end else if (groupi=0) and (values=dxfName_LType)and inlttypetable then begin
          IgnoredSource:=True;
        end else if (groupi=0) and (values=dxfName_DIMSTYLE)and indimstyletable then begin
          IgnoredSource:=True;

        { === Секция OBJECTS: NOD шаблона и ветки NOD-обработчиков
              (этап 4 ТЗ NOD) === }
        end else if (groupi=2) and (values='OBJECTS') then begin
          inobjectssec:=True;
          { Обычно уже вызваны перед ENTITIES; вызовы идемпотентны }
          PreallocateAcadTableOwnerHandle;
          ReserveNODHandles;
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
        end else if inobjectssec and innodobj and (groupi=3) then begin
          { Запись NOD шаблона: перед ней — недостающие записи обработчиков,
            которые по алфавиту идут раньше. Пары 350 записей, ветки
            которых заменяет обработчик, перемаплены на его словарь
            (MapTemplateHandles). }
          WriteNODInsertions(values);
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
        end else if inobjectssec and (groupi=0) and (values=dxfName_ENDSEC) then begin
          if innodobj then begin
            WriteNODInsertions('');
            innodobj:=False;
          end;
          { Ветки NOD-обработчиков, затем прикладные OBJECTS-callback'и }
          RunObjectsSaveDxfProcs;
          inobjectssec:=False;
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
        end else if inobjectssec and (groupi=0) then begin
          { Начало следующего объекта шаблона: NOD закончился }
          if innodobj then begin
            WriteNODInsertions('');
            innodobj:=False;
          end;
          if NODSave.HasTemplateSkips and templatefile.notEOF then begin
            { Объект ветки шаблона, которую заменяет обработчик,
              пропускается целиком — по множеству хэндлов, построенному
              моделью этапа 1 при LoadTemplate }
            peekgroups:=templatefile.readString;
            peekvalues:=templatefile.readString;
            if (StrToInt(peekgroups)=5)
               and NODSave.IsTemplateHandleSkipped(StrToInt('$'+Trim(peekvalues))) then begin
              NODLogTraceFormatStr('uzeffdxfout: template %s %s skipped',
                [values,Trim(peekvalues)]);
              while templatefile.notEOF do begin
                peekgroups:=templatefile.readString;
                peekvalues:=templatefile.readString;
                if StrToInt(peekgroups)=0 then begin
                  { Начало следующего объекта (или ENDSEC) — обработать
                    на следующей итерации }
                  pendinggroups:=peekgroups;
                  pendingvalues:=peekvalues;
                  pendingpair:=True;
                  Break;
                end;
              end;
            end else begin
              outstream.TXTAddStringEOL(groups);
              outstream.TXTAddStringEOL(values);
              pendinggroups:=peekgroups;
              pendingvalues:=peekvalues;
              pendingpair:=True;
            end;
          end else begin
            outstream.TXTAddStringEOL(groups);
            outstream.TXTAddStringEOL(values);
          end;

        end else begin
          if not ignoredsource then begin
            outstream.TXTAddStringEOL(groups);
            outstream.TXTAddStringEOL(values);
          end;
        end;
    end;
    i:=outstream.Count;
    outstream.Count:=handlepos;
    outstream.TXTAddStringEOL(inttohex(IODXFContext.handle+$100000000,9){'100000013'});
    outstream.Count:=i;
    OldHandele2NewHandle.Destroy;
    NODSave.Free;
    templatefile.done;

    sysfilename:={$IFNDEF DELPHI}utf8tosys{$ENDIF}(SavedFileName);
    if FileExists(sysfilename) then begin
      deletefile(sysfilename+'.bak');
      if not renamefile(sysfilename,sysfilename+'.bak') then
        zDebugLn('{WH}'+rsUnableRenameFileToBak,[SavedFileName]);
    end;

    if outstream.SaveToFile(SavedFileName)<=0 then begin
      zDebugLn('{EM}'+rsUnableToWriteFile,[SavedFileName]);
      Result:=False;
    end else
      Result:=True;
    lps.EndLongProcess(lph);

  end;
  outstream.done;
  IODXFContext.done;
end;

{ === Временный NOD-обработчик ACAD_TABLESTYLE (этап 4 ТЗ NOD) ===
  Адаптер переносит запись стилей таблиц из бывшего автомата
  savedxf20XX на реестр NOD: ReserveHandlesProc — бывший
  PreallocateTableStyleHandles, SaveProc — словарь-ветка и
  WriteTableStyleObjectToStream (+ CELLSTYLEMAP). Чтение стилей через
  реестр и перенос обработчика в отдельный модуль — этап 5.
  Если стилей в чертеже нет, ReserveHandlesProc возвращает 0 и ветка
  ACAD_TABLESTYLE шаблона копируется как есть.
  Хэндлы, выделенные ReserveHandlesProc, хранятся до SaveProc того же
  сохранения (savedxf20XX не реентерабельна). }

var
  TSNODStyles:array of PTGDBDXFTableStyle;
  TSNODHandles:array of TDWGHandle;
  TSNODXDictHandles:array of TDWGHandle;
  TSNODMapHandles:array of TDWGHandle;

function TableStyleNODReserveHandles(var ADrawing:TSimpleDrawing;
  var AIODXFContext:TIODXFSaveContext):TDWGHandle;
var
  style:PTGDBDXFTableStyle;
  iter:itrec;
  n:integer;
begin
  SetLength(TSNODStyles,0);
  SetLength(TSNODHandles,0);
  SetLength(TSNODXDictHandles,0);
  SetLength(TSNODMapHandles,0);
  if ADrawing.DXFTableStyleTable.Count=0 then
    Exit(0);
  { Словарь-ветка, затем на каждый стиль: TABLESTYLE, его расширенный
    словарь и CELLSTYLEMAP (issue #1409) }
  Result:=AIODXFContext.handle;
  Inc(AIODXFContext.handle);
  SetLength(TSNODStyles,ADrawing.DXFTableStyleTable.Count);
  SetLength(TSNODHandles,Length(TSNODStyles));
  SetLength(TSNODXDictHandles,Length(TSNODStyles));
  SetLength(TSNODMapHandles,Length(TSNODStyles));
  n:=0;
  style:=ADrawing.DXFTableStyleTable.beginiterate(iter);
  while (style<>nil) and (n<Length(TSNODStyles)) do begin
    if AIODXFContext.TableStyleNameHandleMap.MyContans(style^.Name) then
      NODLogWarningFormatStr(
        'uzeffdxfout: table style "%s" is duplicated, skipped',[style^.Name])
    else begin
      TSNODStyles[n]:=style;
      TSNODHandles[n]:=AIODXFContext.handle;
      TSNODXDictHandles[n]:=AIODXFContext.handle+1;
      TSNODMapHandles[n]:=AIODXFContext.handle+2;
      Inc(AIODXFContext.handle,3);
      AIODXFContext.TableStyleNameHandleMap.Add(
        style^.Name,inttohex(TSNODHandles[n],0));
      Inc(n);
    end;
    style:=ADrawing.DXFTableStyleTable.iterate(iter);
  end;
  SetLength(TSNODStyles,n);
  SetLength(TSNODHandles,n);
  SetLength(TSNODXDictHandles,n);
  SetLength(TSNODMapHandles,n);
  programlog.LogOutFormatStr(
    'uzeffdxfout: выделены хэндлы для %d стилей таблиц (словарь %s)',
    [n,inttohex(Result,0)],LM_Info);
end;

procedure TableStyleNODSave(var AOutStream:TZctnrVectorBytes;
  var ADrawing:TSimpleDrawing;var AIODXFContext:TIODXFSaveContext;
  ADictHandle,ANODHandle:TDWGHandle);
var
  i:integer;
begin
  if ADictHandle=0 then
    Exit;
  if ANODHandle=0 then
    NODLogWarningFormatStr(
      'uzeffdxfout: ACAD_TABLESTYLE dictionary %s is written without NOD',
      [inttohex(ADictHandle,0)]);
  { Словарь-ветка — как в шаблоне 2007: владелец и реактор — NOD }
  AOutStream.TXTAddStringEOL(dxfGroupCode(0));
  AOutStream.TXTAddStringEOL('DICTIONARY');
  AOutStream.TXTAddStringEOL(dxfGroupCode(5));
  AOutStream.TXTAddStringEOL(inttohex(ADictHandle,0));
  AOutStream.TXTAddStringEOL(dxfGroupCode(102));
  AOutStream.TXTAddStringEOL('{ACAD_REACTORS');
  AOutStream.TXTAddStringEOL(dxfGroupCode(330));
  AOutStream.TXTAddStringEOL(inttohex(ANODHandle,0));
  AOutStream.TXTAddStringEOL(dxfGroupCode(102));
  AOutStream.TXTAddStringEOL('}');
  AOutStream.TXTAddStringEOL(dxfGroupCode(330));
  AOutStream.TXTAddStringEOL(inttohex(ANODHandle,0));
  AOutStream.TXTAddStringEOL(dxfGroupCode(100));
  AOutStream.TXTAddStringEOL('AcDbDictionary');
  AOutStream.TXTAddStringEOL(dxfGroupCode(281));
  AOutStream.TXTAddStringEOL('1');
  for i:=0 to High(TSNODStyles) do begin
    AOutStream.TXTAddStringEOL(dxfGroupCode(3));
    AOutStream.TXTAddStringEOL(TSNODStyles[i]^.Name);
    AOutStream.TXTAddStringEOL(dxfGroupCode(350));
    AOutStream.TXTAddStringEOL(inttohex(TSNODHandles[i],0));
  end;
  for i:=0 to High(TSNODStyles) do
    WriteTableStyleObjectToStream(AOutStream,TSNODStyles[i],TSNODHandles[i],
      ADictHandle,AIODXFContext.TextStyleNameHandleMap,
      TSNODXDictHandles[i],TSNODMapHandles[i]);
  { Указатели на стили чертежа после записи не нужны }
  SetLength(TSNODStyles,0);
end;

{ Класс TABLESTYLE — если стили есть, а в CLASSES шаблона его нет
  (шаблон DXF 2000). Группа 91 (число экземпляров) — с DXF 2004. }
procedure TableStyleNODClasses(var AOutStream:TZctnrVectorBytes;
  var ADrawing:TSimpleDrawing;var AIODXFContext:TIODXFSaveContext);
begin
  if (ADrawing.DXFTableStyleTable.Count=0)
     or (AIODXFContext.TemplateClassNames.IndexOf('TABLESTYLE')>=0) then
    Exit;
  AOutStream.TXTAddStringEOL(dxfGroupCode(0));
  AOutStream.TXTAddStringEOL('CLASS');
  AOutStream.TXTAddStringEOL(dxfGroupCode(1));
  AOutStream.TXTAddStringEOL('TABLESTYLE');
  AOutStream.TXTAddStringEOL(dxfGroupCode(2));
  AOutStream.TXTAddStringEOL('AcDbTableStyle');
  AOutStream.TXTAddStringEOL(dxfGroupCode(3));
  AOutStream.TXTAddStringEOL('ObjectDBX Classes');
  AOutStream.TXTAddStringEOL(dxfGroupCode(90));
  AOutStream.TXTAddStringEOL('4095');
  if AIODXFContext.Header.Version>=AC1018 then begin
    AOutStream.TXTAddStringEOL(dxfGroupCode(91));
    AOutStream.TXTAddStringEOL(IntToStr(ADrawing.DXFTableStyleTable.Count));
  end;
  AOutStream.TXTAddStringEOL(dxfGroupCode(280));
  AOutStream.TXTAddStringEOL('0');
  AOutStream.TXTAddStringEOL(dxfGroupCode(281));
  AOutStream.TXTAddStringEOL('0');
end;

{ Новый хэндл стиля таблицы по имени — для ссылок других объектов шаблона
  на пропущенные объекты ветки ACAD_TABLESTYLE шаблона }
function TableStyleNODFindObjectHandle(const AName:string;
  var AIODXFContext:TIODXFSaveContext):TDWGHandle;
var
  hs:string;
begin
  Result:=0;
  if AIODXFContext.TableStyleNameHandleMap.MyGetValue(AName,hs) then
    Result:=StrToQWord('$'+hs);
end;

procedure RegisterTableStyleNODAdapter;
var
  h:TZNODHandler;
begin
  h:=Default(TZNODHandler);
  h.Key:='ACAD_TABLESTYLE';
  h.ObjectType:='TABLESTYLE';
  { R4: стили таблиц пишутся и в DXF 2000 }
  h.MinVersion:=AC1015;
  h.DefaultName:='Standard';
  h.LoadProc:=nil;
  h.ReserveHandlesProc:=TableStyleNODReserveHandles;
  h.SaveProc:=TableStyleNODSave;
  h.ClassesProc:=TableStyleNODClasses;
  h.FindObjectHandleProc:=TableStyleNODFindObjectHandle;
  RegisterNODHandler(h);
end;

initialization
  RegisterTableStyleNODAdapter;
end.
