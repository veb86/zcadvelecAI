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

unit uzestylestables;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}
interface
uses
  sysutils,UGDBNamedObjectsArray,gzctnrVector,uzeNamedObject;
type

  TTableCellJustify=(jcl(*'TopLeft'*),
    jcc(*'TopCenter'*),
    jcr(*'TopRight'*));
  PTGDBTableCellStyle=^TGDBTableCellStyle;

  { Стиль ячейки таблицы с полями для хранения DXF-параметров.
    Внимание: добавление полей типа string в этот record небезопасно,
    так как GZVector использует raw memory (GetMem/Move) для хранения. }
  TGDBTableCellStyle=record
    Width,TextWidth:double;
    CF:TTableCellJustify;
    { Дополнительные поля для импорта/экспорта DXF (только value types) }
    TextHeight: double;             { Высота текста (группа 140) }
    Alignment: Integer;             { Выравнивание (группа 170) }
    TextColor: Integer;             { Цвет текста (группа 62) }
    BackgroundColor: Integer;       { Цвет фона (группа 63) }
    BackgroundColorEnabled: Boolean;{ Включён ли цвет фона (группа 283) }
  end;

  GDBCellFormatArray=GZVector<TGDBTableCellStyle>;

  { Стиль таблицы — именованный объект с параметрами отображения таблицы }
  TGDBTableStyle=object(GDBNamedObject)
    rowheight:integer;
    textheight:double;
    tblformat:GDBCellFormatArray;
    HeadBlockName:string;
    { Дополнительные поля для импорта/экспорта DXF }
    TitleSuppressed: Boolean;         { Подавление строки заголовка (группа 280) }
    ColumnHeadingSuppressed: Boolean; { Подавление строки колонок (группа 281) }
    { Имена текстовых стилей для трёх типов строк: title, header, data (группа 7) }
    CellTextStyleName: array[0..2] of string;
    constructor Init(const n:string);
    destructor Done;virtual;
  end;
  PTGDBTableStyle=^TGDBTableStyle;




PGDBTableStyleArray=^GDBTableStyleArray;
GDBTableStyleArray= object(GDBNamedObjectsArray<PTGDBTableStyle,TGDBTableStyle>)
                    constructor init(m:Integer);
                    constructor initnul;
                    function AddStyle(const name:String):PTGDBTableStyle;
              end;
var
  PTempTableStyle:PTGDBTableStyle;
implementation

constructor GDBTableStyleArray.init;
begin
  inherited init(m);
  //addlayer(LNSysLayerName,CGDBWhile,lwgdbdefault,true,false,true);
end;
constructor GDBTableStyleArray.initnul;
begin
  inherited initnul;
  //objsizeof:=sizeof(TGDBTableStyle);
  //size:=sizeof(TGDBTableStyle);
end;
constructor TGDBTableStyle.Init;
var
  I: Integer;
begin
    inherited;
    tblformat.init(10);
    TitleSuppressed := False;
    ColumnHeadingSuppressed := False;
    { Инициализируем имена текстовых стилей для трёх типов строк }
    for I := 0 to 2 do
      pointer(CellTextStyleName[I]) := nil;
end;
destructor TGDBTableStyle.Done;
var
  I: Integer;
begin
    inherited;
    tblformat.Done;
    HeadBlockName:='';
    { Освобождаем строки массива имён текстовых стилей }
    for I := 0 to 2 do
      CellTextStyleName[I] := '';
end;
function GDBTableStyleArray.AddStyle;
var
  p:PTGDBTableStyle;
begin
     case AddItem(name,pointer(p)) of
             IsFounded:
                       begin
                       end;
             IsCreated:
                       begin
                            p^.init(Name);
                       end;
             IsError:
                       begin
                       end;
     end;
     result:=p;
end;
begin
end.
