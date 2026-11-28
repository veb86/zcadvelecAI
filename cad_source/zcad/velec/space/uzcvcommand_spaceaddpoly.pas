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
{$MODE OBJFPC}{$H+}
unit uzCVCommand_SpaceAddPoly;
{$INCLUDE zengineconfig.inc}

interface

uses
  SysUtils,
  uzccommandsabstract,
  uzccommandsimpl,
  uzcLog,
  uzeentity,
  uzegeometrytypes,
  uzvcommand_spaceadd,
  uzvcommand_spaceutils,
  uzccommand_3dpoly;

type
  {** Значение переменной пространства, которое нужно записать в зону
      после добавления расширений (например Space_RoomPos = '101')}
  TSpaceAddPolyValue = record
    VarName: string;   // Имя переменной / Variable name
    Value: string;     // Значение в строковом виде / Value as string
  end;
  TSpaceAddPolyValues = array of TSpaceAddPolyValue;

  {** Вершины контура зоны в МСК}
  TSpaceAddPolyPoints = array of TzePoint3d;

{** Неинтерактивный вариант команды SpaceAddPoly для внешнего управления
    (HTTP-команды SPACE_ADD_ROOM / SPACE_ADD_POLY).

    Создаёт замкнутую 3D-полилинию по вершинам APoints и применяет к ней
    ровно те же расширения, что и интерактивная команда SpaceAddPoly
    (ParseOperandsToStruct + ApplySpaceExtensions), после чего записывает
    значения переменных AValues.

    Использует ЛОКАЛЬНУЮ структуру операндов, поэтому не затрагивает
    gOperandsStruct запущенной в этот момент интерактивной команды.

    @param(AOperands - операнды в формате панели инструментов, например
           256,'',Space_RoomPos,'Позиция помещения',String)
    @param(APoints - вершины контура, минимум 3)
    @param(AValues - значения переменных; переменная должна быть объявлена
           в AOperands, иначе вызывается исключение)
    @param(AToConstructRoot - True: добавить в ConstructRoot (режим
           BEGIN_BATCH/END_BATCH), False: в чертёж с Undo и перерисовкой)
    @param(ASelect - выделить созданную зону, как это делает SpaceAddPoly;
           игнорируется при AToConstructRoot)
    @return(указатель на созданный примитив)}
function SpaceAddPolyFromPoints(const AOperands: TCommandOperands;
  const APoints: TSpaceAddPolyPoints; const AValues: TSpaceAddPolyValues;
  AToConstructRoot, ASelect: Boolean): PGDBObjEntity;

implementation

uses
  uzeentpolyline,
  uzcdrawings,
  uzcutils,
  uzcinterface,
  uzgldrawcontext,
  uzcEnitiesVariablesExtender,
  uzsbVarmanDef;

const
  // Минимальное число вершин замкнутого контура зоны
  SPACE_POLY_MIN_VERTICES = 3;
  // Цвет «поСлою» - значение по умолчанию у ParseOperandsToStruct
  SPACE_POLY_COLOR_BYLAYER = 256;

{** Создание замкнутой 3D-полилинии с вершинами APoints и свойствами
    текущего чертежа (как у SpaceAddPoly до применения расширений)}
function CreateSpacePolyline(
  const APoints: TSpaceAddPolyPoints): PGDBObjPolyline;
var
  i: Integer;
begin
  Result := GDBObjPolyline.CreateInstance;
  zcSetEntPropFromCurrentDrawingProp(Result);
  for i := 0 to High(APoints) do
    Result^.VertexArrayInOCS.PushBackData(APoints[i]);
  Result^.Closed := True;
  // GoodAddObjectToObjArray не меняет владельца, поэтому задаём его сразу
  Result^.bp.ListPos.Owner := drawings.GetCurrentROOT;
end;

{** Запись значений переменных в уже добавленный extdrVariables.
    Переменная обязана быть объявлена операндами - иначе исключение,
    чтобы внешний клиент не получил «успех» с потерянным значением}
procedure WriteSpaceValues(const APEnt: PGDBObjEntity;
  const AValues: TSpaceAddPolyValues);
var
  LVarExt: TVariablesExtender;
  LVar: pvardesk;
  i: Integer;
begin
  if Length(AValues) = 0 then
    Exit;
  LVarExt := APEnt^.specialize GetExtension<TVariablesExtender>;
  if LVarExt = nil then
    raise Exception.Create('Variables extension was not added');
  for i := 0 to High(AValues) do begin
    LVar := LVarExt.entityunit.FindVariable(AValues[i].VarName);
    if (LVar = nil) or (LVar^.data.PTD = nil) then
      raise Exception.CreateFmt('Variable "%s" is not declared',
        [AValues[i].VarName]);
    LVar^.SetValueFromString(AValues[i].Value);
  end;
end;

{** Построение готовой (ещё не добавленной в чертёж) зоны: полилиния,
    расширения SpaceAddPoly, значения переменных и FormatEntity.
    При ошибке примитив освобождается, а исключение пробрасывается}
function BuildSpaceEntity(const AStruct: TOperandsStruct;
  const APoints: TSpaceAddPolyPoints;
  const AValues: TSpaceAddPolyValues): PGDBObjPolyline;
var
  LDC: TDrawContext;
begin
  Result := CreateSpacePolyline(APoints);
  try
    // Те же расширения, что и на стадии ESSSetEntity у SpaceAddPoly
    ApplySpaceExtensions(Result, AStruct);
    WriteSpaceValues(Result, AValues);
    LDC := drawings.GetCurrentDWG^.CreateDrawingRC;
    Result^.FormatEntity(drawings.GetCurrentDWG^, LDC);
  except
    Result^.done;
    Freemem(Pointer(Result));
    raise;
  end;
end;

{** Выделение созданной зоны - так же, как на стадии ESSCommandEnd,
    чтобы её параметры сразу были видны в инспекторе}
procedure SelectSpaceEntity(const APEnt: PGDBObjEntity);
begin
  zcUI.Do_GUIaction(drawings.GetCurrentDWG^.wa, zcMsgUIReturnToDefaultObject);
  APEnt^.Select(drawings.GetCurrentDWG^.wa.param.SelDesc.Selectedobjcount,
    @drawings.GetCurrentDWG^.Selector);
  zcUI.Do_GUIaction(drawings.GetCurrentDWG^.wa, zcMsgUIActionSelectionChanged);
end;

{** Добавление зоны: в ConstructRoot (BEGIN_BATCH) или в чертёж с Undo}
procedure PlaceSpaceEntity(APEnt: PGDBObjEntity;
  AToConstructRoot, ASelect: Boolean);
begin
  if AToConstructRoot then begin
    zcAddEntToCurrentDrawingConstructRoot(APEnt);
    Exit;
  end;
  zcAddEntToCurrentDrawingWithUndo(APEnt);
  if ASelect then
    SelectSpaceEntity(APEnt);
  zcRedrawCurrentDrawing;
end;

function SpaceAddPolyFromPoints(const AOperands: TCommandOperands;
  const APoints: TSpaceAddPolyPoints; const AValues: TSpaceAddPolyValues;
  AToConstructRoot, ASelect: Boolean): PGDBObjEntity;
var
  LStruct: TOperandsStruct;
begin
  if drawings.GetCurrentDWG = nil then
    raise Exception.Create('No current drawing');
  if Length(APoints) < SPACE_POLY_MIN_VERTICES then
    raise Exception.CreateFmt('At least %d points are required',
      [SPACE_POLY_MIN_VERTICES]);

  // Локальная структура: gOperandsStruct интерактивной команды не трогаем.
  // listParam не управляемое поле, поэтому обнуляем его до разбора
  LStruct.indexColor := SPACE_POLY_COLOR_BYLAYER;
  LStruct.namelayer := '';
  LStruct.listParam := nil;
  try
    // Тот же разбор операндов, что и у интерактивной команды SpaceAddPoly
    ParseOperandsToStruct(AOperands, LStruct);
    Result := BuildSpaceEntity(LStruct, APoints, AValues);
  finally
    FreeAndNil(LStruct.listParam);
  end;

  PlaceSpaceEntity(Result, AToConstructRoot, ASelect);
  programlog.LogOutFormatStr('SpaceAddPolyFromPoints: %d vertices, operands "%s"',
    [Length(APoints), AOperands], LM_Info);
end;

function _SpaceAddPoly_com_CommandStart(const Context:TZCADCommandContext;
  operands:TCommandOperands):TCommandResult;
begin
        // Initialize operands structure
  gOperandsStruct.listParam := TParamInfoList.Create;  // Создаем экземпляр TVector / Create TVector instance
  gOperandsStruct.indexColor := 256;  // ByLayer
  gOperandsStruct.namelayer := '';

    // Разбираем операнды и заполняем структуру
  // Parse operands and fill structure
  ParseOperandsToStruct(operands, gOperandsStruct);

  result:=_3DPoly_com_CommandStart(Context,operands);
  p3dplESP:=@AddExtdrToRectangle;
  if assigned(p3dplESP) then
    p3dplESP(ESSSuppressCommandParams,nil,nil);
end;


procedure startup;
begin

  CreateCommandRTEdObjectPlugin(@_SpaceAddPoly_com_CommandStart,@_3DPoly_com_CommandEnd,
    @_3DPoly_com_CommandEnd,nil,@_3DPoly_com_BeforeClick,@_3DPoly_com_AfterClick,
  nil,nil,'SpaceAddPoly',0,0);
end;

procedure Finalize;
begin
end;

initialization
  programlog.LogOutFormatStr(clUInit,[{$INCLUDE %FILE%}],LM_Info,UnitsInitializeLMId);

  startup;

finalization
  ProgramLog.LogOutFormatStr(clUFin,[{$INCLUDE %FILE%}],LM_Info,UnitsFinalizeLMId);
  finalize;
end.
