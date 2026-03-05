// uexpsvgcommands.pas
unit uexpsvgcommands;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uzccommandsabstract, uzccommandsimpl, uzccommandsmanager,
  uzeentity, uzcdrawings, uzcinterface,
  uzeentblockinsert, uexpsvgblock, uzeconsts, gzctnrVectorTypes,
  UGDBSelectedObjArray, Dialogs, FileUtil, uzeTypes;

implementation

// Проверка выделения: строго один блок (как в uzvaddconnection)
function CheckSingleBlockSelected: PGDBObjBlockInsert;
var
  psd: PSelectedObjDesc;
  ir: itrec;
  selectedEntity: PGDBObjEntity;
begin
  Result := nil;

  // Получаем массив выделенных объектов (аналогично uzvaddconnection)
  if drawings.GetCurrentDWG^.SelObjArray.Count = 0 then
  begin
    zcUI.TextMessage('Ошибка: не выделено ни одного объекта', TMWOShowError);
    Exit;
  end;

  if drawings.GetCurrentDWG^.SelObjArray.Count > 1 then
  begin
    zcUI.TextMessage('Ошибка: выделено более одного объекта. Выберите только один блок.', TMWOShowError);
    Exit;
  end;

  // Получаем первый (и единственный) выделенный объект
  psd := drawings.GetCurrentDWG^.SelObjArray.getDataMutable(0);
  if not Assigned(psd) then Exit;

  selectedEntity := psd^.objaddr;
  if not Assigned(selectedEntity) then Exit;

  // Проверяем, что это блок
  if selectedEntity^.GetObjType <> GDBBlockInsertID then
  begin
    zcUI.TextMessage('Ошибка: выделенный объект не является блоком', TMWOShowError);
    Exit;
  end;

  Result := PGDBObjBlockInsert(selectedEntity);
end;

function ExportSVGBlock_Command(const Context: TZCADCommandContext;
  Operands: TCommandOperands): TCommandResult;
var
  BlockInsert: PGDBObjBlockInsert;
  Exporter: TBlockSVGExporter;
  SaveDialog: TSaveDialog;
  FileName: string;
begin
  Result := 0;
  
  // 1. Проверка выделения
  BlockInsert := CheckSingleBlockSelected;
  if not Assigned(BlockInsert) then Exit;

  // 2. Диалог сохранения
  SaveDialog := TSaveDialog.Create(nil);
  try
    SaveDialog.Filter := 'SVG файлы (*.svg)|*.svg|Все файлы (*.*)|*.*';
    SaveDialog.DefaultExt := 'svg';
    SaveDialog.FileName := 'export.svg';

    if not SaveDialog.Execute then Exit;
    FileName := SaveDialog.FileName;
  finally
    SaveDialog.Free;
  end;

  // 3. Экспорт
  Exporter := TBlockSVGExporter.Create;
  try
    if Exporter.ExportBlock(BlockInsert, FileName) then
      zcUI.TextMessage('Экспорт завершен успешно: ' + FileName, TMWOShowError)
    else
      zcUI.TextMessage('Ошибка при сохранении файла', TMWOShowError);
  finally
    Exporter.Free;
  end;
end;

initialization
  // Регистрация команды
  CreateZCADCommand(@ExportSVGBlock_Command, 'ExportSVGBlock', 0, 0);

end.