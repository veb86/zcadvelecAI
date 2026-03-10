// uexpsvgcommands.pas
unit uexpsvgcommands;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  uzccommandsabstract, uzccommandsimpl, uzccommandsmanager,
  uzeentity, uzcdrawings, uzcinterface,
  uzeentblockinsert, uzeentdevice, uexpsvgblock, uzeconsts, gzctnrVectorTypes,
  UGDBSelectedObjArray, Dialogs, FileUtil, uzeTypes;

implementation

// Проверка выделения: строго один блок или device
function CheckSingleBlockOrDeviceSelected(out IsDevice: Boolean): PGDBObjEntity;
var
  psd: PSelectedObjDesc;
  ir: itrec;
  selectedEntity: PGDBObjEntity;
begin
  Result := nil;
  IsDevice := False;

  // Получаем массив выделенных объектов (аналогично uzvaddconnection)
  if drawings.GetCurrentDWG^.SelObjArray.Count = 0 then
  begin
    zcUI.TextMessage('Ошибка: не выделено ни одного объекта', TMWOShowError);
    Exit;
  end;

  if drawings.GetCurrentDWG^.SelObjArray.Count > 1 then
  begin
    zcUI.TextMessage('Ошибка: выделено более одного объекта. Выберите только один блок или устройство.', TMWOShowError);
    Exit;
  end;

  // Получаем первый (и единственный) выделенный объект
  psd := drawings.GetCurrentDWG^.SelObjArray.getDataMutable(0);
  if not Assigned(psd) then Exit;

  selectedEntity := psd^.objaddr;
  if not Assigned(selectedEntity) then Exit;

  // Проверяем, что это блок или device
  case selectedEntity^.GetObjType of
    GDBBlockInsertID:
      begin
        Result := selectedEntity;
        IsDevice := False;
      end;
    GDBDeviceID:
      begin
        Result := selectedEntity;
        IsDevice := True;
      end;
  else
    begin
      zcUI.TextMessage('Ошибка: выделенный объект не является блоком или устройством', TMWOShowError);
      Exit;
    end;
  end;
end;

function ExportSVGBlock_Command(const Context: TZCADCommandContext;
  Operands: TCommandOperands): TCommandResult;
var
  SelectedEntity: PGDBObjEntity;
  IsDevice: Boolean;
  Exporter: TBlockSVGExporter;
  SaveDialog: TSaveDialog;
  FileName: string;
begin
  Result := 0;

  // 1. Проверка выделения
  SelectedEntity := CheckSingleBlockOrDeviceSelected(IsDevice);
  if not Assigned(SelectedEntity) then Exit;

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
    if IsDevice then
    begin
      // Экспорт устройства
      if Exporter.ExportDevice(PGDBObjDevice(SelectedEntity), FileName) then
        zcUI.TextMessage('Экспорт завершен успешно: ' + FileName, TMWOShowError)
      else
        zcUI.TextMessage('Ошибка при сохранении файла', TMWOShowError);
    end
    else
    begin
      // Экспорт блока
      if Exporter.ExportBlock(PGDBObjBlockInsert(SelectedEntity), FileName) then
        zcUI.TextMessage('Экспорт завершен успешно: ' + FileName, TMWOShowError)
      else
        zcUI.TextMessage('Ошибка при сохранении файла', TMWOShowError);
    end;
  finally
    Exporter.Free;
  end;
end;

initialization
  // Регистрация команды
  CreateZCADCommand(@ExportSVGBlock_Command, 'ExportSVGBlock', 0, 0);

end.