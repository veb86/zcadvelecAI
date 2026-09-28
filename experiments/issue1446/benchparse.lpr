program benchparse;
// issue #1446: профиль NOD pre-pass (разбор OBJECTS и построение модели)
// на больших DXF. Использование: benchparse <файл.dxf>...
{$mode objfpc}{$H+}
uses SysUtils, Classes, Interfaces, uzeffdxfobjects, uzeffdxfnod, uzeffdxf;
var
  I, J, R: Integer;
  B: TStringBuilder;
  L: TStringList;
  Text, Err: string;
  List: TZDXFRawObjectList;
  Model: TZNODModel;
  T: QWord;
  TParse, TModel: QWord;
begin
  for I := 1 to ParamCount do begin
    { Секция OBJECTS так же, как её сохраняет загрузчик в RawObjectsSection }
    L := TStringList.Create;
    L.LoadFromFile(ParamStr(I));
    J := 0;
    while (J + 3 < L.Count) and not ((Trim(L[J]) = '0') and (Trim(L[J + 1]) = 'SECTION')
      and (Trim(L[J + 3]) = 'OBJECTS')) do
      Inc(J);
    B := TStringBuilder.Create;
    for R := J to L.Count - 1 do
      B.Append(L[R]).Append(LineEnding);
    Text := B.ToString;
    B.Free;
    L.Free;
    TParse := High(QWord); TModel := High(QWord);
    for R := 1 to 5 do begin
      List := TZDXFRawObjectList.Create(True);
      T := GetTickCount64;
      ParseDxfObjectsSection(Text, List, Err);
      T := GetTickCount64 - T; if T < TParse then TParse := T;
      if R = 1 then writeln('objects: ', List.Count);
      List.Free;
      Model := TZNODModel.Create;
      T := GetTickCount64;
      BuildDXFNODModel(Text, Model, Err);
      T := GetTickCount64 - T; if T < TModel then TModel := T;
      Model.Free;
    end;
    writeln(ExtractFileName(ParamStr(I)), ': ', Length(Text), ' bytes, parse ', TParse,
      ' ms, BuildDXFNODModel ', TModel, ' ms');
  end;
end.
