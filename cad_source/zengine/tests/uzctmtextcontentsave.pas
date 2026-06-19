unit uzctmtextcontentsave;
{$Codepage UTF8}
{$Mode delphi}{$H+}

{ Issue #1344 regression test.

  Текст внутри таблиц ACAD_TABLE не отображался в AutoCAD: сетка таблицы
  рисовалась, а текст ячеек был пустой до тех пор, пока пользователь не
  выполнял трансформацию (перенос/поворот/масштаб).

  Корень проблемы — в кэш-графике таблицы. AutoCAD хранит отрисованную
  таблицу в анонимном блоке (*T1, *T2, ...), где текст ячеек лежит в виде
  MTEXT. ZCAD загружает эти блоки штатным DXF-загрузчиком: GDBObjMText.
  LoadFromDXF кладёт текст в поле Content, но оставляет поле template
  пустым (template заполняется только при форматировании в FormatContent).
  При пересохранении GDBObjMText.SaveToDXF писал группу 1 из template и,
  поскольку template был пустым, в DXF попадала пустая строка — текст
  ячеек терялся в кэш-графике, и AutoCAD показывал пустые ячейки, пока
  трансформация не помечала таблицу "грязной" и не перестраивала её.

  Исправление: SaveToDXF берёт Content, если template пустой (та же
  логика, что и в FormatContent: «if template='' then template:=content»).

  Тест воспроизводит ровно состояние загруженного из блока MTEXT
  (Content задан, template пуст) и проверяет, что SaveToDXF пишет
  непустую группу 1 с текстом ячейки. }

interface

uses
  SysUtils,
  Classes,
  fpcunit,
  testregistry,
  Interfaces,
  uzeTypes,
  uzeenttext,
  uzeentmtext,
  uzedrawingsimple,
  uzestylestexts,
  uzctnrVectorBytesStream,
  uzeffdxfsupport;

type
  TMTextContentSaveTest = class(TTestCase)
  private
    function SaveMTextToDXFText(AMText: PGDBObjMText;
      var ADrawing: TSimpleDrawing): String;
  published
    // Loaded-from-block state: Content set, template empty -> group 1 must
    // carry the text (the actual #1344 bug).
    procedure SavesContentWhenTemplateEmpty;
    // Control: when template is set it must still be written (no regression
    // for the normal path).
    procedure SavesTemplateWhenPresent;
  end;

implementation

function EnsureStandardStyle(var ADrawing: TSimpleDrawing): PGDBTextStyle;
var
  Prop: GDBTextStyleProp;
begin
  Result := ADrawing.GetTextStyleTable^.FindStyle('Standard', False);
  if Result = nil then
  begin
    Prop.size := 2.5;
    Prop.oblique := 0;
    Prop.wfactor := 1;
    Result := ADrawing.GetTextStyleTable^.addstyle(
      'Standard', '', '', Prop, False);
  end;
end;

function TMTextContentSaveTest.SaveMTextToDXFText(AMText: PGDBObjMText;
  var ADrawing: TSimpleDrawing): String;
var
  OutStream: TZctnrVectorBytes;
  SaveContext: TIODXFSaveContext;
  FileName: String;
  Lines: TStringList;
begin
  OutStream.init(8 * 1024);
  SaveContext.InitRec;
  SaveContext.Header.Version := AC1021;
  SaveContext.Header.iVersion := 1021;
  try
    AMText^.SaveToDXF(OutStream, ADrawing, SaveContext);
    FileName := IncludeTrailingPathDelimiter(GetTempDir(False)) +
      'zcad_mtext_content_save_test.dxf';
    if OutStream.SaveToFile(FileName) < 0 then
      raise Exception.Create('Не удалось сохранить временный DXF-поток');
    Lines := TStringList.Create;
    try
      Lines.LoadFromFile(FileName);
      Result := Lines.Text;
    finally
      Lines.Free;
      DeleteFile(FileName);
    end;
  finally
    SaveContext.Done;
    OutStream.done;
  end;
end;

procedure TMTextContentSaveTest.SavesContentWhenTemplateEmpty;
var
  Drawing: TSimpleDrawing;
  MText: PGDBObjMText;
  DXFText: String;
begin
  Drawing.init(nil);
  try
    MText := GDBObjMText.CreateInstance;
    MText^.TXTStyle := EnsureStandardStyle(Drawing);
    // Состояние MTEXT после GDBObjMText.LoadFromDXF: текст в Content,
    // template пуст (как у MTEXT из анонимного блока *T таблицы).
    MText^.Content := 'Zagolovok';
    MText^.Template := '';

    DXFText := SaveMTextToDXFText(MText, Drawing);

    // До исправления группа 1 писалась из пустого template, и текст
    // терялся. Теперь SaveToDXF подставляет Content.
    CheckTrue(Pos('Zagolovok', DXFText) > 0,
      'SaveToDXF должен писать текст ячейки даже когда template пуст ' +
      '(issue #1344). DXF: ' + DXFText);
  finally
    Drawing.done;
  end;
end;

procedure TMTextContentSaveTest.SavesTemplateWhenPresent;
var
  Drawing: TSimpleDrawing;
  MText: PGDBObjMText;
  DXFText: String;
begin
  Drawing.init(nil);
  try
    MText := GDBObjMText.CreateInstance;
    MText^.TXTStyle := EnsureStandardStyle(Drawing);
    MText^.Content := '';
    MText^.Template := 'Podpis';

    DXFText := SaveMTextToDXFText(MText, Drawing);

    CheckTrue(Pos('Podpis', DXFText) > 0,
      'SaveToDXF должен писать текст из template, когда он задан. DXF: ' +
      DXFText);
  finally
    Drawing.done;
  end;
end;

initialization
  RegisterTest(TMTextContentSaveTest);

end.
