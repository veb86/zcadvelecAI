# issue #1446: добавляет поле NODModel в TIODXFLoadContext (uzeffdxfsupport.pas,
# файл с CRLF — переводы строк сохраняются).
p='cad_source/zengine/fileformats/uzeffdxfsupport.pas'
s=open(p,encoding='utf-8',newline='').read()
assert '\r\n' in s
s=s.replace('\r\n','\n')
def rep(old,new):
    global s
    assert old in s, old
    s=s.replace(old,new,1)
rep("""  uzMVReader,UGDBPoint3DArray,uzeTypes,Classes;
""","""  uzMVReader,UGDBPoint3DArray,uzeTypes,Classes,uzeffdxfnod;
""")
rep("""    TableRowStyleTypes:TDXFRowStyleTypeArray;
    TableRowStyleTypesValid:boolean;

    procedure InitRec;""","""    TableRowStyleTypes:TDXFRowStyleTypeArray;
    TableRowStyleTypesValid:boolean;

    { Модель секции OBJECTS и Named Object Dictionary (этап 2 ТЗ
      cad_source/zengine/TZ_NOD_NamedObjectDictionary.md). Строится в
      AddFromDXF до разбора TABLES/BLOCKS/ENTITIES и живёт до Done.
      nil — контекст создан не AddFromDXF (например, в AddFromDXF12);
      пустая модель (NOD=nil) — R12, нет секции OBJECTS или ошибка её
      разбора. Владеет контекст: освобождается в Done. }
    NODModel:TZNODModel;

    procedure InitRec;""")
rep("""  SetLength(TableRowStyleTypes,0);
  TableRowStyleTypesValid:=False;
end;
""","""  SetLength(TableRowStyleTypes,0);
  TableRowStyleTypesValid:=False;

  NODModel:=nil;
end;
""")
rep("""  SetLength(TableRowStyleTypes,0);
end;
""","""  SetLength(TableRowStyleTypes,0);
  FreeAndNil(NODModel);
end;
""")
open(p,'w',encoding='utf-8',newline='').write(s.replace('\n','\r\n'))
