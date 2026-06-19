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
  uzestylestablesdxf,uzclog;
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

{ Регистрирует pre-save callback. Все зарегистрированные callback'и
  вызываются в порядке регистрации в начале savedxf20XX. }
procedure RegisterBeforeSaveDxfProc(proc:TBeforeSaveDxfProc);
procedure RegisterObjectsSaveDxfProc(proc:TObjectsSaveDxfProc);

function savedxf20XX(const SavedFileName:string;const TemplateFileName:string;var drawing:TSimpleDrawing;AVer:TZCDxfVersion):boolean;

implementation

var
  BeforeSaveDxfProcs:array of TBeforeSaveDxfProc;
  ObjectsSaveDxfProcs:array of TObjectsSaveDxfProc;

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

{ Записывает один объект TABLESTYLE в выходной поток DXF.
  StyleHandle — заранее назначенный хэндл объекта.
  OwnerHandle — хэндл словаря ACAD_TABLESTYLE (владелец). }
procedure WriteTableStyleObjectToStream(
  var outstream: TZctnrVectorBytes;
  Style: PTGDBDXFTableStyle;
  const StyleHandle: TDWGHandle;
  const OwnerHandle: TDWGHandle);
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

  { Блок ACAD_XDICTIONARY намеренно НЕ пишем (issue #1339): сам объект
    расширенного словаря в выходной файл не сохраняется, поэтому ссылка 360
    на Style^.XDictHandle (старый хэндл из загруженного файла) указывала бы
    на несуществующий объект — висячая ссылка, из-за которой AutoCAD
    открывает пустой чертёж. }

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
  VarsDict.Add('$DWGCODEPAGE',ZCCP2Str(drawing.DXFCodePage));

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

  { Переменные для сохранения стилей таблиц в секции OBJECTS }
  inobjectssec: boolean;
  tablestyledicthandle: TDWGHandle;
  tablestyleiter: itrec;
  ptablestyle: PTGDBDXFTableStyle;
  writtenstylecount: integer;
  intablestyledict: boolean;
  { Массивы предварительно выделенных хэндлов для стилей таблиц }
  tsHandles: array of TDWGHandle;
  tsCount: integer;
  tsHandlesAllocated: boolean;
  beforeProcIdx: integer;

  procedure RunObjectsSaveDxfProcs;
  var
    objectsProcIdx: Integer;
  begin
    for objectsProcIdx:=0 to High(ObjectsSaveDxfProcs) do
      if Assigned(ObjectsSaveDxfProcs[objectsProcIdx]) then
        ObjectsSaveDxfProcs[objectsProcIdx](
          outstream,drawing,IODXFContext);
  end;

  { Предварительно выделяет хэндлы для стилей таблиц и строит карту
    «имя стиля -> новый хэндл», а также вычисляет новый хэндл владельца
    (*Model_Space) для сырых сущностей ACAD_TABLE. Должно вызываться ДО
    записи секции ENTITIES, т.к. сырая сущность ACAD_TABLE ссылается на
    стиль таблицы (342) и владельца (330), которые иначе пишутся позже
    или перенумеровываются (issue #1339). }
  procedure PreallocateTableStyleHandles;
  var
    psIdx: integer;
    psStyle: PTGDBDXFTableStyle;
    psIter: itrec;
    msHandle: TDWGHandle;
  begin
    if tsHandlesAllocated then
      Exit;
    tsHandlesAllocated:=True;

    { Новый хэндл владельца сущностей пространства модели. В шаблоне и в
      исходных файлах AutoCAD блок *Model_Space всегда имеет хэндл 1F. }
    msHandle:=OldHandele2NewHandle.MyGetValue($1F);
    if msHandle>0 then
      IODXFContext.AcadTableOwnerHandle:=msHandle;

    tsCount:=drawing.DXFTableStyleTable.count;
    if tsCount>0 then begin
      SetLength(tsHandles, tsCount);
      psIdx:=0;
      psStyle:=drawing.DXFTableStyleTable.beginiterate(psIter);
      while (psStyle<>nil) and (psIdx<tsCount) do begin
        tsHandles[psIdx]:=IODXFContext.handle;
        Inc(IODXFContext.handle);
        if not IODXFContext.TableStyleNameHandleMap.MyContans(psStyle^.Name) then
          IODXFContext.TableStyleNameHandleMap.Add(
            psStyle^.Name, inttohex(tsHandles[psIdx],0));
        Inc(psIdx);
        psStyle:=drawing.DXFTableStyleTable.iterate(psIter);
      end;
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

  //if AVer<ZCDxf2007 then begin
    IODXFContext.Header.DWGCodePage:=ZCCodePage2ACDWGCodePage(drawing.DXFCodePage){SysCP2ACCP(ACodePage)};
    IODXFContext.Header.iDWGCodePage:=ZCCodePage2SysCP(drawing.DXFCodePage);//ACodePage;
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
    intablestyledict:=False;
    tablestyledicthandle:=0;
    writtenstylecount:=0;
    tsCount:=0;
    tsHandlesAllocated:=False;
    SetLength(tsHandles, 0);
    MakeVariablesDict(IODXFContext.VarsDict,drawing);
    processedvarscount:=IODXFContext.VarsDict.Count;
    while templatefile.notEOF do begin
      groups:=templatefile.readString;
      values:=templatefile.readString;
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
            { Обнаружение словаря ACAD_TABLESTYLE по хэндлу }
            if inobjectssec and (groupi=5)
               and (tablestyledicthandle>0)
               and (lasthandle=tablestyledicthandle) then
              intablestyledict:=True;
          end;
        end else if (groupi=2) and (values='ENTITIES') then begin
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
          { Перед записью сущностей выделяем хэндлы стилей таблиц и
            вычисляем хэндл владельца — сырые ACAD_TABLE ссылаются на них
            (issue #1339). }
          PreallocateTableStyleHandles;
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
          outstream.TXTAddStringEOL(floattostr(-drawing.pcamera^.prop.look.x));
          outstream.TXTAddStringEOL(dxfGroupCode(26));
          outstream.TXTAddStringEOL(floattostr(-drawing.pcamera^.prop.look.y));
          outstream.TXTAddStringEOL(dxfGroupCode(36));
          outstream.TXTAddStringEOL(floattostr(-drawing.pcamera^.prop.look.z));
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
                  if (pcurrtextstyle^.Name<>'') and
                     (not IODXFContext.TextStyleNameHandleMap.MyContans(pcurrtextstyle^.Name)) then
                    IODXFContext.TextStyleNameHandleMap.Add(
                      pcurrtextstyle^.Name,inttohex(temphandle,0));
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
                  if (pcurrtextstyle^.Name<>'') and
                     (not IODXFContext.TextStyleNameHandleMap.MyContans(pcurrtextstyle^.Name)) then
                    IODXFContext.TextStyleNameHandleMap.Add(
                      pcurrtextstyle^.Name,inttohex(temphandle,0));
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

        { === Обработка секции OBJECTS: сохранение стилей таблиц === }
        end else if (groupi=2) and (values='OBJECTS') then begin
          { Вход в секцию OBJECTS.
            Предварительно выделяем хэндлы для всех стилей таблиц из чертежа,
            чтобы потом использовать их и в словаре, и в объектах TABLESTYLE. }
          inobjectssec:=True;
          { Хэндлы стилей таблиц уже могли быть выделены перед секцией
            ENTITIES (issue #1339). Вызов идемпотентен. }
          PreallocateTableStyleHandles;
          if tsCount>0 then
            programlog.LogOutFormatStr(
              'uzeffdxfout: выделены хэндлы для %d стилей таблиц',
              [tsCount], LM_Info);
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
        end else if inobjectssec and (groupi=3) and (values='ACAD_TABLESTYLE') then begin
          { Нашли запись ACAD_TABLESTYLE в корневом словаре.
            Следующая пара (350, handle) — хэндл словаря стилей таблиц.
            Сохраняем оба хэндла (старый и перемапленный) для дальнейшего
            отслеживания объекта словаря. }
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);
          groups:=templatefile.readString;
          values:=templatefile.readString;
          groupi:=StrToInt(groups);
          if groupi=350 then begin
            valuei:=StrToInt('$'+values);
            intable:=OldHandele2NewHandle.MyGetValue(valuei);
            if intable>0 then begin
              tablestyledicthandle:=intable;
              lasthandle:=intable;
            end else begin
              OldHandele2NewHandle.Add(valuei,IODXFContext.handle);
              tablestyledicthandle:=IODXFContext.handle;
              lasthandle:=IODXFContext.handle;
              Inc(IODXFContext.handle);
            end;
            outstream.TXTAddStringEOL(groups);
            outstream.TXTAddStringEOL(inttohex(tablestyledicthandle,0));
          end else begin
            outstream.TXTAddStringEOL(groups);
            outstream.TXTAddStringEOL(values);
          end;
        end else if inobjectssec and intablestyledict and (tsCount>0) and (groupi=3) then begin
          { Внутри словаря ACAD_TABLESTYLE: пропускаем шаблонные записи (3/350 пары).
            Они будут заменены нашими записями. }
          groups:=templatefile.readString;
          values:=templatefile.readString;
          { Пропускаем пару 350/handle шаблонной записи }
        end else if inobjectssec and intablestyledict and (groupi=0) then begin
          { Конец словаря ACAD_TABLESTYLE. Записываем записи для стилей из чертежа. }
          intablestyledict:=False;
          if tsCount>0 then begin
            i:=0;
            ptablestyle:=drawing.DXFTableStyleTable.beginiterate(tablestyleiter);
            while (ptablestyle<>nil) and (i<tsCount) do begin
              outstream.TXTAddStringEOL(dxfGroupCode(3));
              outstream.TXTAddStringEOL(ptablestyle^.Name);
              outstream.TXTAddStringEOL(dxfGroupCode(350));
              outstream.TXTAddStringEOL(inttohex(tsHandles[i],0));
              Inc(i);
              ptablestyle:=drawing.DXFTableStyleTable.iterate(tablestyleiter);
            end;
          end;
          { Если следующий объект — TABLESTYLE, обрабатываем его отдельно }
          if values='TABLESTYLE' then begin
            { Не записываем 0/TABLESTYLE — он будет заменён нашими стилями.
              Пропускаем содержимое до следующего объекта. }
            while True do begin
              repeat
                groups:=templatefile.readString;
                values:=templatefile.readString;
                groupi:=StrToInt(groups);
                if (groupi=5) or (groupi=320) or (groupi=330) or (groupi=340)
                   or (groupi=350) or (groupi=1005) or (groupi=390)
                   or (groupi=360) or (groupi=105) then begin
                  valuei:=StrToInt('$'+values);
                  if valuei<>0 then begin
                    intable:=OldHandele2NewHandle.MyGetValue(valuei);
                    if intable<=0 then begin
                      OldHandele2NewHandle.Add(valuei,IODXFContext.handle);
                      Inc(IODXFContext.handle);
                    end;
                  end;
                end;
              until (groupi=0) or (not templatefile.notEOF);
              if values<>'TABLESTYLE' then
                Break;
            end;
            { Записываем стили из чертежа }
            if tsCount>0 then begin
              i:=0;
              ptablestyle:=drawing.DXFTableStyleTable.beginiterate(tablestyleiter);
              while (ptablestyle<>nil) and (i<tsCount) do begin
                WriteTableStyleObjectToStream(
                  outstream,ptablestyle,tsHandles[i],tablestyledicthandle);
                Inc(writtenstylecount);
                Inc(i);
                ptablestyle:=drawing.DXFTableStyleTable.iterate(tablestyleiter);
              end;
            end;
            if values=dxfName_ENDSEC then begin
              RunObjectsSaveDxfProcs;
              inobjectssec:=False;
            end;
            outstream.TXTAddStringEOL(dxfGroupCode(0));
            outstream.TXTAddStringEOL(values);
          end else begin
            { Записываем текущую пару (начало следующего объекта) }
            outstream.TXTAddStringEOL(groups);
            outstream.TXTAddStringEOL(values);
          end;
        end else if inobjectssec and (tsCount>0) and (groupi=0) and (values='TABLESTYLE') then begin
          { Начало объекта TABLESTYLE из шаблона.
            Пропускаем все шаблонные TABLESTYLE и заменяем стилями из чертежа. }
          while True do begin
            repeat
              groups:=templatefile.readString;
              values:=templatefile.readString;
              groupi:=StrToInt(groups);
              { Хэндлы внутри пропускаемого объекта регистрируем в маппинге }
              if (groupi=5) or (groupi=320) or (groupi=330) or (groupi=340)
                 or (groupi=350) or (groupi=1005) or (groupi=390)
                 or (groupi=360) or (groupi=105) then begin
                valuei:=StrToInt('$'+values);
                if valuei<>0 then begin
                  intable:=OldHandele2NewHandle.MyGetValue(valuei);
                  if intable<=0 then begin
                    OldHandele2NewHandle.Add(valuei,IODXFContext.handle);
                    Inc(IODXFContext.handle);
                  end;
                end;
              end;
            until (groupi=0) or (not templatefile.notEOF);
            if values<>'TABLESTYLE' then
              Break;
          end;

          { Записываем все стили таблиц из чертежа с предварительно
            выделенными хэндлами }
          if tsCount>0 then begin
            i:=0;
            ptablestyle:=drawing.DXFTableStyleTable.beginiterate(tablestyleiter);
            while (ptablestyle<>nil) and (i<tsCount) do begin
              WriteTableStyleObjectToStream(
                outstream,ptablestyle,tsHandles[i],tablestyledicthandle);
              Inc(writtenstylecount);
              Inc(i);
              ptablestyle:=drawing.DXFTableStyleTable.iterate(tablestyleiter);
            end;
          end;
          { Записываем текущую пару (начало следующего объекта) }
          if values=dxfName_ENDSEC then begin
            RunObjectsSaveDxfProcs;
            inobjectssec:=False;
          end;
          outstream.TXTAddStringEOL(dxfGroupCode(0));
          outstream.TXTAddStringEOL(values);
        end else if inobjectssec and (groupi=0) and (values=dxfName_ENDSEC) then begin
          { Конец секции OBJECTS. Если шаблон не содержал TABLESTYLE,
            записываем стили из чертежа перед ENDSEC. }
          if (writtenstylecount=0) and (tsCount>0)
             and (tablestyledicthandle>0) then begin
            i:=0;
            ptablestyle:=drawing.DXFTableStyleTable.beginiterate(tablestyleiter);
            while (ptablestyle<>nil) and (i<tsCount) do begin
              WriteTableStyleObjectToStream(
                outstream,ptablestyle,tsHandles[i],tablestyledicthandle);
              Inc(writtenstylecount);
              Inc(i);
              ptablestyle:=drawing.DXFTableStyleTable.iterate(tablestyleiter);
            end;
          end;
          RunObjectsSaveDxfProcs;
          inobjectssec:=False;
          outstream.TXTAddStringEOL(groups);
          outstream.TXTAddStringEOL(values);

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

begin
end.
