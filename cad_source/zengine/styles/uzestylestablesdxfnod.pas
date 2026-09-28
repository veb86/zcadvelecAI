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
  Модуль: uzestylestablesdxfnod
  Назначение: работа со словарём ACAD_TABLESTYLE через модель NOD
  (uzeffdxfnod). ТЗ cad_source/zengine/TZ_NOD_NamedObjectDictionary.md.

  Этап 1: ExtractTableStyleDictionaryFromNOD — замена
  ExtractTableStyleDictionary из uzestylestablesdxf. Старая функция ищет
  пару 3/ACAD_TABLESTYLE глобально по всему тексту OBJECTS и может найти
  её не в NOD (например, в стороннем словаре или XRECORD); новая берёт
  запись ACAD_TABLESTYLE только из корневого словаря.
  Пока используется только в тестах и к загрузке не подключена; на
  этапе 5 модуль станет NOD-обработчиком ACAD_TABLESTYLE.
}
unit uzestylestablesdxfnod;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}
interface

uses
  Classes,
  uzeffdxfobjects,
  uzeffdxfnod;

const
  { Ключ NOD словаря стилей таблиц }
  CNODTableStyleKey = 'ACAD_TABLESTYLE';
  { Тип объекта стиля таблицы }
  CDXFTableStyleObjType = 'TABLESTYLE';

{ Заполняет StyleNameByHandle по словарю NOD/ACAD_TABLESTYLE:
  имя — хэндл объекта записи (DXFHandleToStr: верхний регистр, без
  ведущих нулей), значение — имя стиля (ключ словаря).
  Формат тот же, что у ExtractTableStyleDictionary.
  OutDictHandle — хэндл словаря ACAD_TABLESTYLE ('' если не найден).
  Возвращает False, если NOD нет, в нём нет ключа ACAD_TABLESTYLE или
  ключ ссылается не на словарь. Записи с битыми ссылками (объекта нет)
  пропускаются. }
function ExtractTableStyleDictionaryFromNOD(
  AModel: TZNODModel;
  StyleNameByHandle: TStringList;
  out OutDictHandle: string): Boolean;

implementation

uses
  SysUtils,
  uzeffdxfnodlog;

function ExtractTableStyleDictionaryFromNOD(
  AModel: TZNODModel;
  StyleNameByHandle: TStringList;
  out OutDictHandle: string): Boolean;
var
  Dict: TZDXFDictionary;
  I: Integer;
  Entry: TZDXFDictEntry;
  Obj: TZDXFRawObject;
begin
  StyleNameByHandle.Clear;
  OutDictHandle := '';
  Dict := AModel.ResolveDictionary(CNODTableStyleKey);
  if Dict = nil then
    Exit(False);
  OutDictHandle := DXFHandleToStr(Dict.Handle);
  for I := 0 to Dict.Count - 1 do begin
    Entry := Dict[I];
    Obj := AModel.FindObject(Entry.TargetHandle);
    if Obj = nil then begin
      NODLogTraceFormatStr(
        'uzestylestablesdxfnod: %s/%s: object %s not found, skipped',
        [CNODTableStyleKey, Entry.Key, DXFHandleToStr(Entry.TargetHandle)]);
      Continue;
    end;
    if not SameText(Obj.ObjType, CDXFTableStyleObjType) then
      NODLogTraceFormatStr(
        'uzestylestablesdxfnod: %s/%s: object %s is %s, not %s',
        [CNODTableStyleKey, Entry.Key, Obj.HandleStr, Obj.ObjType,
         CDXFTableStyleObjType]);
    StyleNameByHandle.Values[DXFHandleToStr(Entry.TargetHandle)] := Entry.Key;
  end;
  Result := True;
end;

end.
