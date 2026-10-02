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
@author(Vladimir Bobrov)
}

{
  Модуль: uzeacadtable_dxf_nod
  Назначение: NOD-обработчик ключа ACDB_RECOMPOSE_DATA — запись XRECORD
  перекомпоновки таблиц AutoCAD 2008+ через реестр uzeffdxfnodregistry
  (issue #1465, этап 10 ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md).

  AutoCAD хранит в NOD прямую ссылку на XRECORD (не словарь):
    блок реакторов 102 (ACAD_REACTORS) со ссылкой 330 на NOD, 330 <NOD>,
    100 AcDbXrecord,
    280 1, 90 1, затем 330 на каждый TABLESTYLE чертежа и 330 на каждую
    главную сущность ACAD_TABLE (продолжения разорванных таблиц не входят),
  см. cad_source/test/tablerazdel2.dxf, tablebugheader.dxf.

  Запись: ReserveHandlesProc выделяет хэндл XRECORD, если в чертеже есть
  таблицы (иначе 0 — запись не пишется); SaveProc пишет XRECORD. Хэндлы
  стилей берутся из TableStyleNameHandleMap (заполняет обработчик
  ACAD_TABLESTYLE), хэндлы таблиц — из общей карты p2h, по которой их
  пишет uzeacadtable_dxf_write. Чтение: запись «забирает» реестр
  (ClaimNODObjectEntry), её данные читает индекс NOD таблиц
  (uzeffdxfnodacadtable).
  Зависимости: uzeffdxfnodregistry, uzeffdxfnodacadtable, uzeffdxfnodlog
}

unit uzeacadtable_dxf_nod;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

implementation

uses
  SysUtils,
  uzeTypes,
  uzeconsts,
  gzctnrVectorTypes,
  uzctnrVectorBytesStream,
  uzedrawingsimple,
  uzeentity,
  uzeffdxfsupport,
  uzestylestablesdxf,
  uzeffdxfnodlog,
  uzeffdxfnodregistry,
  uzeffdxfnodacadtable;

const
  { Тип объекта записи ACDB_RECOMPOSE_DATA }
  CRecomposeObjType = 'XRECORD';
  { Флаг клонирования записи (280) и версия данных (90), как у AutoCAD }
  CRecomposeCloneFlag = '1';
  CRecomposeDataVersion = '1';

{ Пара «код группы — значение» без перекодировки }
procedure WritePair(var AOutStream: TZctnrVectorBytes; ACode: Integer;
  const AValue: string);
begin
  AOutStream.TXTAddStringEOL(dxfGroupCode(ACode));
  AOutStream.TXTAddStringEOL(AValue);
end;

{ True, если в пространстве модели есть таблицы ACAD_TABLE }
function DrawingHasAcadTables(var ADrawing: TSimpleDrawing): Boolean;
var
  Entity: PGDBObjEntity;
  Iter: itrec;
begin
  Result := False;
  if ADrawing.pObjRoot = nil then
    Exit;
  Entity := ADrawing.pObjRoot^.ObjArray.beginiterate(Iter);
  while Entity <> nil do
  begin
    if Entity^.GetObjType = GDBAcadTableID then
      Exit(True);
    Entity := ADrawing.pObjRoot^.ObjArray.iterate(Iter);
  end;
end;

function RecomposeNODReserveHandles(var ADrawing: TSimpleDrawing;
  var AIODXFContext: TIODXFSaveContext): TDWGHandle;
begin
  Result := 0;
  if not DrawingHasAcadTables(ADrawing) then
    Exit;
  Result := AIODXFContext.handle;
  Inc(AIODXFContext.handle);
  NODLogTraceFormatStr(
    'uzeacadtable_dxf_nod: выделен хэндл %s для %s',
    [IntToHex(Result, 0), CAcadTableRecomposeKey]);
end;

{ Ссылки 330 на стили таблиц — в порядке таблицы стилей чертежа }
procedure WriteTableStyleRefs(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext);
var
  Style: PTGDBDXFTableStyle;
  Iter: itrec;
  HandleStr: string;
begin
  Style := ADrawing.DXFTableStyleTable.beginiterate(Iter);
  while Style <> nil do
  begin
    if AIODXFContext.TableStyleNameHandleMap.MyGetValue(Style^.Name,
         HandleStr) then
      WritePair(AOutStream, 330, HandleStr);
    Style := ADrawing.DXFTableStyleTable.iterate(Iter);
  end;
end;

{ Ссылки 330 на главные сущности таблиц — по общей карте p2h, по которой
  хэндлы получают сами сущности при записи ENTITIES }
procedure WriteAcadTableRefs(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext);
var
  Entity: PGDBObjEntity;
  Iter: itrec;
  Handle: TDWGHandle;
begin
  Entity := ADrawing.pObjRoot^.ObjArray.beginiterate(Iter);
  while Entity <> nil do
  begin
    if Entity^.GetObjType = GDBAcadTableID then
    begin
      AIODXFContext.p2h.MyGetOrCreateValue(Entity, AIODXFContext.handle,
        Handle);
      WritePair(AOutStream, 330, IntToHex(Handle, 0));
    end;
    Entity := ADrawing.pObjRoot^.ObjArray.iterate(Iter);
  end;
end;

procedure RecomposeNODSave(var AOutStream: TZctnrVectorBytes;
  var ADrawing: TSimpleDrawing; var AIODXFContext: TIODXFSaveContext;
  ADictHandle, ANODHandle: TDWGHandle);
begin
  if ADictHandle = 0 then
    Exit;
  WritePair(AOutStream, 0, CRecomposeObjType);
  WritePair(AOutStream, 5, IntToHex(ADictHandle, 0));
  WritePair(AOutStream, 102, '{ACAD_REACTORS');
  WritePair(AOutStream, 330, IntToHex(ANODHandle, 0));
  WritePair(AOutStream, 102, '}');
  WritePair(AOutStream, 330, IntToHex(ANODHandle, 0));
  WritePair(AOutStream, 100, 'AcDbXrecord');
  WritePair(AOutStream, 280, CRecomposeCloneFlag);
  WritePair(AOutStream, 90, CRecomposeDataVersion);
  WriteTableStyleRefs(AOutStream, ADrawing, AIODXFContext);
  WriteAcadTableRefs(AOutStream, ADrawing, AIODXFContext);
  NODLogTraceFormatStr('uzeacadtable_dxf_nod: записан %s (%s)',
    [CAcadTableRecomposeKey, IntToHex(ADictHandle, 0)]);
end;

procedure RegisterRecomposeNODHandler;
var
  H: TZNODHandler;
begin
  H := Default(TZNODHandler);
  H.Key := CAcadTableRecomposeKey;
  H.ObjectType := CRecomposeObjType;
  { Запись перекомпоновки таблиц появилась в AutoCAD 2008 (DXF 2007) }
  H.MinVersion := AC1021;
  H.ReserveHandlesProc := RecomposeNODReserveHandles;
  H.SaveProc := RecomposeNODSave;
  RegisterNODHandler(H);
end;

initialization
  RegisterRecomposeNODHandler;
end.
