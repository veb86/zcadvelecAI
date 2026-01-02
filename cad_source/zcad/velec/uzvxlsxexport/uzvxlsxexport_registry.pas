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
{$mode objfpc}{$H+}

unit uzvxlsxexport_registry;

{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils, Classes,
  uzvxlsxexport_types, uzclog;

type
  // Реестр команд экспорта и команд ячеек
  TXlsxCommandRegistry = class
  private
    FExportCommands: TXlsxExportCommandList;
    FCellCommands: TXlsxCellCommandList;
  public
    constructor Create;
    destructor Destroy; override;

    // Регистрация команды экспорта
    procedure RegisterExportCommand(
      const AName: String;
      AHandler: TXlsxExportCommandHandler;
      const ADescription: String
    );

    // Регистрация команды ячейки
    procedure RegisterCellCommand(
      const AName: String;
      AHandler: TXlsxCellCommandHandler;
      const ADescription: String
    );

    // Найти обработчик команды экспорта
    function FindExportCommand(
      const AName: String
    ): TXlsxExportCommandHandler;

    // Найти обработчик команды ячейки
    function FindCellCommand(
      const AName: String
    ): TXlsxCellCommandHandler;

    // Проверить, зарегистрирована ли команда экспорта
    function IsExportCommandRegistered(const AName: String): Boolean;

    // Проверить, зарегистрирована ли команда ячейки
    function IsCellCommandRegistered(const AName: String): Boolean;

    // Получить список всех команд экспорта
    function GetExportCommandNames: TStringList;

    // Получить список всех команд ячеек
    function GetCellCommandNames: TStringList;

    property ExportCommands: TXlsxExportCommandList read FExportCommands;
    property CellCommands: TXlsxCellCommandList read FCellCommands;
  end;

implementation

{ TXlsxCommandRegistry }

constructor TXlsxCommandRegistry.Create;
begin
  FExportCommands := TXlsxExportCommandList.Create;
  FCellCommands := TXlsxCellCommandList.Create;

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Реестр команд создан',
    [],
    LM_Info
  );
end;

destructor TXlsxCommandRegistry.Destroy;
var
  i: Integer;
begin
  // Освобождаем команды экспорта
  for i := 0 to FExportCommands.Size - 1 do
    FExportCommands[i].Free;
  FExportCommands.Free;

  // Освобождаем команды ячеек
  for i := 0 to FCellCommands.Size - 1 do
    FCellCommands[i].Free;
  FCellCommands.Free;

  inherited Destroy;
end;

procedure TXlsxCommandRegistry.RegisterExportCommand(
  const AName: String;
  AHandler: TXlsxExportCommandHandler;
  const ADescription: String
);
var
  cmdInfo: TXlsxExportCommandInfo;
begin
  cmdInfo := TXlsxExportCommandInfo.Create(AName, AHandler, ADescription);
  FExportCommands.PushBack(cmdInfo);

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Зарегистрирована команда экспорта "%s"',
    [AName],
    LM_Info
  );
end;

procedure TXlsxCommandRegistry.RegisterCellCommand(
  const AName: String;
  AHandler: TXlsxCellCommandHandler;
  const ADescription: String
);
var
  cmdInfo: TXlsxCellCommandInfo;
begin
  cmdInfo := TXlsxCellCommandInfo.Create(AName, AHandler, ADescription);
  FCellCommands.PushBack(cmdInfo);

  programlog.LogOutFormatStr(
    'uzvxlsxexport: Зарегистрирована команда ячейки "%s"',
    [AName],
    LM_Info
  );
end;

function TXlsxCommandRegistry.FindExportCommand(
  const AName: String
): TXlsxExportCommandHandler;
var
  i: Integer;
  lowerName: String;
begin
  Result := nil;
  lowerName := LowerCase(Trim(AName));

  for i := 0 to FExportCommands.Size - 1 do
  begin
    if LowerCase(FExportCommands[i].CommandName) = lowerName then
    begin
      Result := FExportCommands[i].Handler;
      Exit;
    end;
  end;
end;

function TXlsxCommandRegistry.FindCellCommand(
  const AName: String
): TXlsxCellCommandHandler;
var
  i: Integer;
  lowerName: String;
begin
  Result := nil;
  lowerName := LowerCase(Trim(AName));

  for i := 0 to FCellCommands.Size - 1 do
  begin
    if LowerCase(FCellCommands[i].CommandName) = lowerName then
    begin
      Result := FCellCommands[i].Handler;
      Exit;
    end;
  end;
end;

function TXlsxCommandRegistry.IsExportCommandRegistered(
  const AName: String
): Boolean;
begin
  Result := Assigned(FindExportCommand(AName));
end;

function TXlsxCommandRegistry.IsCellCommandRegistered(
  const AName: String
): Boolean;
begin
  Result := Assigned(FindCellCommand(AName));
end;

function TXlsxCommandRegistry.GetExportCommandNames: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to FExportCommands.Size - 1 do
    Result.Add(FExportCommands[i].CommandName);
end;

function TXlsxCommandRegistry.GetCellCommandNames: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to FCellCommands.Size - 1 do
    Result.Add(FCellCommands[i].CommandName);
end;

end.
