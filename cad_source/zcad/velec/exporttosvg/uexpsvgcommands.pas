// uexpsvgcommands.pas
unit uexpsvgcommands;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uzccommandsabstract, uzccommandsimpl, uzccommandsmanager,
  uzeentity, uzcdrawings, uzcinterface,
  uzeentblockinsert, uexpsvgblock;

implementation

// Проверка выделения: строго один блок (как в uzvaddconnection)
function CheckSingleBlockSelected: PGDBObjBlockInsert;
var
  psd: PTSelectedObjDescriptor;
  count: Integer;
  selectedEntity: PGDBObjEntity;
begin
  Result := nil;
  
  // Получаем массив выделенных объектов (аналогично uzvaddconnection)
  count := uzcdrawings.GetCurrentDWG.SelObjArray.Count;
  
  if count = 0 then
  begin
    ZCMsgCallBackInterface.TextMessage('Ошибка: не выделено ни одного объекта', TMsgType.SMWarning);
    Exit;
  end;
  
  if count > 1 then
  begin
    ZCMsgCallBackInterface.TextMessage('Ошибка: выделено более одного объекта. Выберите только один блок.', TMsgType.SMWarning);
    Exit;
  end;
  
  // Получаем первый (и единственный) выделенный объект
  psd := uzcdrawings.GetCurrentDWG.SelObjArray.GetPDataAsPointer(0);
  if not Assigned(psd) then Exit;
  
  selectedEntity := PGDBObjEntity(psd^.objaddr);
  if not Assigned(selectedEntity) then Exit;
  
  // Проверяем, что это блок
  if selectedEntity.GetType <> GDBBlockInsertID then
  begin
    ZCMsgCallBackInterface.TextMessage('Ошибка: выделенный объект не является блоком', TMsgType.SMWarning);
    Exit;
  end;
  
  Result := PGDBObjBlockInsert(selectedEntity);
end;

procedure ExportSVGBlock_Command;
var
  BlockInsert: PGDBObjBlockInsert;
  Exporter: TBlockSVGExporter;
  SaveDialog: TSaveDialog;
  FileName: string;
begin
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
      ZCMsgCallBackInterface.TextMessage('Экспорт завершен успешно: ' + FileName, TMsgType.SMResult)
    else
      ZCMsgCallBackInterface.TextMessage('Ошибка при сохранении файла', TMsgType.SMError);
  finally
    Exporter.Free;
  end;
end;

initialization
  // Регистрация команды
  CreateCommandFastObjectPlugin(@ExportSVGBlock_Command, 'ExportSVGBlock', 0, 0);

end.