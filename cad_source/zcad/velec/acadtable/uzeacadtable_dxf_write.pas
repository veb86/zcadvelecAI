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
  Модуль: uzeacadtable_dxf_write
  Назначение: Экспорт данных таблицы ACAD_TABLE в DXF-поток.
  На текущем этапе экспорт не реализован (этап 3).
  Зависимости: uzeacadtable_types, uzctnrVectorBytesStream,
               uzedrawingdef, uzeffdxfsupport, uzclog
}

unit uzeacadtable_dxf_write;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeacadtable_types,
  uzctnrVectorBytesStream, uzedrawingdef, uzeffdxfsupport,
  uzclog, uzbLogIntf;

// Сохраняет таблицу в DXF-поток.
// TODO (этап 3): добавить полноценный экспорт в формат ACAD_TABLE.
procedure WriteAcadTableToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext);

implementation

procedure WriteAcadTableToDXF(
  var AOutStream: TZctnrVectorBytes;
  var ADrawing: TDrawingDef;
  var AIODXFContext: TIODXFSaveContext);
begin
  // Экспорт не реализован (этап 3)
  programlog.LogOutStr(
    'AcadTable: dxf_write: WriteAcadTableToDXF — ' +
    'экспорт не реализован (этап 3)', LM_Info);
end;

end.
