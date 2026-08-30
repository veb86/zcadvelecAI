{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*****************************************************************************
}

{
@author(HTTP Insert Device Command for ZCAD)
@author(Vladimir Bobrov)

HTTP command:

  INSERT_DEV

Формат:

  [
    "DeviceName",
    x,
    y,
    scaleX,
    scaleY,
    rotate,
    [
      ["PARAMETER1", "value1"],
      ["PARAMETER2", "BOOLEAN_1"],
      ["PARAMETER3", "INTEGER_10"],
      ["PARAMETER4", "FLOAT_3.14"]
    ]
  ]

Пример:

  [
    "QF1",
    100,
    200,
    1,
    1,
    0,
    [
      ["param1", "value1"],
      ["param2", "BOOLEAN_1"],
      ["param3", "INTEGER_10"],
      ["param4", "FLOAT_3.14"]
    ]
  ]

Типы параметров определяются по префиксу значения:

  - BOOLEAN_0 или BOOLEAN_1 → Boolean
  - INTEGER_<число> → Integer
  - FLOAT_<число> → Float (Double)
  - без префикса → String

Если параметр с указанным именем не найден в устройстве,
он будет создан автоматически с соответствующим типом.

Устройство создаётся в ConstructRoot.

Undo и Redraw здесь НЕ выполняются.

При работе внутри BEGIN_BATCH / END_BATCH
финальный перенос в чертёж выполняется END_BATCH.
}

{$mode objfpc}{$H+}

unit uzvhttpcmdinsertdev;

{$INCLUDE zengineconfig.inc}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  uzclog,
  uzbLogTypes,
  uzccommandsmanager,
  uzcstrconsts,
  uzegeometrytypes,
  uzgldrawcontext, uzsbVarmanDef,uzcvariablesutils,
  uzvhttpipc,
  uzcenitiesvariablesextender;

function HTTPCommandInsertDev(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;

implementation

uses
  uzcdrawings,
  uzeTypes,
  uzeentityfactory,
  uzeentdevice,
  uzeentblockinsert,
  uzcutils,
  uzvconsts,
  uzvmanemgetgem,
  uzegeometry,
  Varman;


{=============================================================================
  ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
=============================================================================}

function GetFloatFromJSON(
  AArray: TJSONArray;
  AIndex: Integer;
  out AValue: Double
): Boolean;
begin
  Result := False;
  AValue := 0;

  if AArray = nil then
    Exit;

  if
    (AIndex < 0) or
    (AIndex >= AArray.Count)
  then
    Exit;

  if AArray.Items[AIndex] = nil then
    Exit;

  try

    AValue :=
      AArray.Items[AIndex].AsFloat;

    Result := True;

  except

    on E: Exception do
      Result := False;

  end;

end;


function GetStringFromJSON(
  AArray: TJSONArray;
  AIndex: Integer;
  out AValue: String
): Boolean;
begin
  Result := False;
  AValue := '';

  if AArray = nil then
    Exit;

  if
    (AIndex < 0) or
    (AIndex >= AArray.Count)
  then
    Exit;

  if AArray.Items[AIndex] = nil then
    Exit;

  try

    AValue :=
      AArray.Items[AIndex].AsString;

    Result := True;

  except

    on E: Exception do
      Result := False;

  end;

end;


function JSONItemIsArray(
  AJSON: TJSONData
): Boolean;
begin
  Result :=
    (AJSON <> nil) and
    (AJSON.JSONType = jtArray);
end;


{=============================================================================
  СОЗДАНИЕ УСТРОЙСТВА
=============================================================================}

function CreateHTTPDevice(
  const ADeviceName: String;
  const APoint: TzePoint3d;
  const AScaleX: Double;
  const AScaleY: Double;
  const ARotate: Double
): PGDBObjDevice;
var
  BlockName: String;
  PDevice: PGDBObjDevice;
  RC: TDrawContext;
begin

  Result := nil;


  if ADeviceName = '' then
    Exit;


  {-------------------------------------------------------------------------
    Имя устройства в проекте обычно имеет специальный префикс.

    Например:

      <device>QF1

    Для устройства реальный blockName:

      QF1
  -------------------------------------------------------------------------}

  if AnsiPos(
       velec_beforeNameGlobalSchemaBlock,
       ADeviceName
     ) = 1
  then
  begin

    BlockName :=
      Copy(
        ADeviceName,
        Length(velec_beforeNameGlobalSchemaBlock) + 1,
        Length(ADeviceName)
      );

  end
  else
  begin

    BlockName :=
      ADeviceName;

  end;


  {-------------------------------------------------------------------------
    Загружаем блок устройства.
  -------------------------------------------------------------------------}

  drawings.AddBlockFromDBIfNeed(
    drawings.GetCurrentDWG,
    ADeviceName
  );


  drawings.AddBlockFromDBIfNeed(
    drawings.GetCurrentDWG,
    BlockName
  );


  {-------------------------------------------------------------------------
    Создаём устройство.
  -------------------------------------------------------------------------}

  PDevice :=
    GDBObjDevice.CreateInstance;


  if PDevice = nil then
    Exit;


  PDevice^.Name :=
    BlockName;


  PDevice^.Local.P_insert :=
    APoint;


  PDevice^.scale :=
    uzegeometry.CreateVector(
      AScaleX,
      AScaleY,
      1
    );


  PDevice^.rotate :=
    ARotate;


  {-------------------------------------------------------------------------
    Строим геометрию.
  -------------------------------------------------------------------------}

  PDevice^.BuildVarGeometry(
    drawings.GetCurrentDWG^
  );


  PDevice^.BuildGeometry(
    drawings.GetCurrentDWG^
  );


  RC :=
    drawings.GetCurrentDWG^.CreateDrawingRC;


  PDevice^.FormatEntity(
    drawings.GetCurrentDWG^,
    RC
  );


  Result :=
    PDevice;

end;


{=============================================================================
  НАСТРОЙКА ПАРАМЕТРА
=============================================================================}

procedure SetHTTPDeviceParameter(
  ADevice: PGDBObjDevice;
  const AParameterName: String;
  const AValue: String
);
var
  PVD: pvardesk;

  Value: String;
  IsSpecialValue: Boolean;
  
  Varext: TVariablesExtender;
  VD: vardesk;
  VarType: String;

begin

  if ADevice = nil then
    Exit;


  if AParameterName = '' then
    Exit;


  {-------------------------------------------------------------------------
    Ищем переменную устройства.
  -------------------------------------------------------------------------}

  PVD :=
    FindVariableInEnt(
      ADevice,
      AParameterName
    );


  if PVD = nil then
  begin

    {-----------------------------------------------------------------------
      Переменная не найдена — создаём новую.
      Определяем тип по значению.
    -----------------------------------------------------------------------}

    Value :=
      Trim(AValue);

    VarType := 'String';

    {---------------------------------------------------------------------
      Проверяем BOOLEAN_
    ---------------------------------------------------------------------}

    if AnsiPos(
         'BOOLEAN_',
         Value
       ) = 1
    then
    begin

      VarType := 'Boolean';

    end

    {---------------------------------------------------------------------
      Проверяем INTEGER_
    ---------------------------------------------------------------------}

    else
    if AnsiPos(
         'INTEGER_',
         Value
       ) = 1
    then
    begin

      VarType := 'Integer';

    end

    {---------------------------------------------------------------------
      Проверяем FLOAT_
    ---------------------------------------------------------------------}

    else
    if AnsiPos(
         'FLOAT_',
         Value
       ) = 1
    then
    begin

      VarType := 'Float';

    end;


    {-----------------------------------------------------------------------
      Получаем расширение переменных устройства.
    -----------------------------------------------------------------------}

    Varext :=
      ADevice^.specialize GetExtension<TVariablesExtender>;


    if Varext = nil then
    begin

      {-------------------------------------------------------------------
        Расширения нет — создаём его.
      -------------------------------------------------------------------}

      Varext :=
        TVariablesExtender.Create(ADevice);

      ADevice^.AddExtension(Varext);

    end;


    {-----------------------------------------------------------------------
      Создаём новую переменную.
    -----------------------------------------------------------------------}

    VD :=
      Varext.entityunit.CreateVariable(
        AParameterName,
        VarType
      );


    {-----------------------------------------------------------------------
      Находим созданную переменную.
    -----------------------------------------------------------------------}

    PVD :=
      Varext.entityunit.FindVariable(AParameterName);


    if PVD = nil then
      raise Exception.CreateFmt(
        'Failed to create parameter "%s" in device "%s"',
        [
          AParameterName,
          ADevice^.Name
        ]
      );


    {-----------------------------------------------------------------------
      Устанавливаем пользовательское имя (опционально).
    -----------------------------------------------------------------------}

    PVD^.username := AParameterName;


  end;


  Value :=
    Trim(AValue);


  IsSpecialValue :=
    True;


  {-------------------------------------------------------------------------
    BOOLEAN_
  -------------------------------------------------------------------------}

  if AnsiPos(
       'BOOLEAN_',
       Value
     ) = 1
  then
  begin

    Value :=
      StringReplace(
        Value,
        'BOOLEAN_',
        '',
        [rfReplaceAll, rfIgnoreCase]
      );

    Value :=
      Trim(Value);


    if Value = '1' then
      PBoolean(PVD^.data.Addr.Instance)^ := True

    else
    if Value = '0' then
      PBoolean(PVD^.data.Addr.Instance)^ := False

    else
      raise Exception.CreateFmt(
        'Invalid BOOLEAN value "%s" for parameter "%s"',
        [
          AValue,
          AParameterName
        ]
      );


    IsSpecialValue :=
      False;

  end;


  {-------------------------------------------------------------------------
    INTEGER_
  -------------------------------------------------------------------------}

  if AnsiPos(
       'INTEGER_',
       Value
     ) = 1
  then
  begin

    Value :=
      StringReplace(
        Value,
        'INTEGER_',
        '',
        [rfReplaceAll, rfIgnoreCase]
      );

    Value :=
      Trim(Value);


    PInteger(
      PVD^.data.Addr.Instance
    )^ :=
      StrToInt(Value);


    IsSpecialValue :=
      False;

  end;


  {-------------------------------------------------------------------------
    FLOAT_
  -------------------------------------------------------------------------}

  if AnsiPos(
       'FLOAT_',
       Value
     ) = 1
  then
  begin

    Value :=
      StringReplace(
        Value,
        'FLOAT_',
        '',
        [rfReplaceAll, rfIgnoreCase]
      );

    Value :=
      Trim(Value);


    PDouble(
      PVD^.data.Addr.Instance
    )^ :=
      StrToFloat(Value);


    IsSpecialValue :=
      False;

  end;


  {-------------------------------------------------------------------------
    Если это не специальный тип — записываем строку.
  -------------------------------------------------------------------------}

  if IsSpecialValue then
  begin

    PString(
      PVD^.data.Addr.Instance
    )^ :=
      AValue;

  end;

end;


{=============================================================================
  INSERT_DEV
=============================================================================}

function HTTPCommandInsertDev(
  AArgs: TJSONArray;
  out AResult: string;
  out AError: string
): Boolean;

var
  DeviceName: String;

  X: Double;
  Y: Double;

  ScaleX: Double;
  ScaleY: Double;

  Rotate: Double;

  Point: TzePoint3d;

  Device: PGDBObjDevice;

  Parameters: TJSONArray;
  ParameterData: TJSONArray;

  ParameterName: String;
  ParameterValue: String;

  I: Integer;

begin

  AResult := '';
  AError := '';

  Result := False;


  {===========================================================================
    Проверка аргументов
  ===========================================================================}

  if AArgs = nil then
  begin

    AError :=
      'INSERT_DEV requires arguments';

    Exit;

  end;


  if AArgs.Count < 6 then
  begin

    AError :=
      'INSERT_DEV requires at least 6 arguments: ' +
      '[name,x,y,scaleX,scaleY,rotate]';

    Exit;

  end;


  {===========================================================================
    Имя устройства
  ===========================================================================}

  if not GetStringFromJSON(
    AArgs,
    0,
    DeviceName
  ) then
  begin

    AError :=
      'INSERT_DEV: invalid device name';

    Exit;

  end;


  if DeviceName = '' then
  begin

    AError :=
      'INSERT_DEV: device name is empty';

    Exit;

  end;


  {===========================================================================
    Координаты
  ===========================================================================}

  if not GetFloatFromJSON(
    AArgs,
    1,
    X
  ) then
  begin

    AError :=
      'INSERT_DEV: invalid X coordinate';

    Exit;

  end;


  if not GetFloatFromJSON(
    AArgs,
    2,
    Y
  ) then
  begin

    AError :=
      'INSERT_DEV: invalid Y coordinate';

    Exit;

  end;


  {===========================================================================
    Масштаб
  ===========================================================================}

  if not GetFloatFromJSON(
    AArgs,
    3,
    ScaleX
  ) then
  begin

    AError :=
      'INSERT_DEV: invalid scaleX';

    Exit;

  end;


  if not GetFloatFromJSON(
    AArgs,
    4,
    ScaleY
  ) then
  begin

    AError :=
      'INSERT_DEV: invalid scaleY';

    Exit;

  end;


  {===========================================================================
    Поворот
  ===========================================================================}

  if not GetFloatFromJSON(
    AArgs,
    5,
    Rotate
  ) then
  begin

    AError :=
      'INSERT_DEV: invalid rotation';

    Exit;

  end;


  {===========================================================================
    Формируем точку
  ===========================================================================}

  Point.x := X;
  Point.y := Y;
  Point.z := 0;


  {===========================================================================
    Создаём устройство
  ===========================================================================}

  try

    Device :=
      CreateHTTPDevice(
        DeviceName,
        Point,
        ScaleX,
        ScaleY,
        Rotate
      );


    if Device = nil then
    begin

      raise Exception.Create(
        'Failed to create device'
      );

    end;


    {=======================================================================
      Настройка параметров

      Args[6] — массив:

        [
          ["PARAM1","value1"],
          ["PARAM2","BOOLEAN_1"],
          ["PARAM3","INTEGER_10"]
        ]
    =======================================================================}

    if AArgs.Count >= 7 then
    begin

      if not JSONItemIsArray(
        AArgs.Items[6]
      ) then
      begin

        raise Exception.Create(
          'INSERT_DEV: args[6] must be an array of parameters'
        );

      end;


      Parameters :=
        TJSONArray(
          AArgs.Items[6]
        );


      for I := 0 to Parameters.Count - 1 do
      begin

        if not JSONItemIsArray(
          Parameters.Items[I]
        ) then
        begin

          raise Exception.CreateFmt(
            'INSERT_DEV: parameter %d must be an array [name,value]',
            [I]
          );

        end;


        ParameterData :=
          TJSONArray(
            Parameters.Items[I]
          );


        if ParameterData.Count < 2 then
        begin

          raise Exception.CreateFmt(
            'INSERT_DEV: parameter %d requires [name,value]',
            [I]
          );

        end;


        if not GetStringFromJSON(
          ParameterData,
          0,
          ParameterName
        ) then
        begin

          raise Exception.CreateFmt(
            'INSERT_DEV: invalid parameter name at index %d',
            [I]
          );

        end;


        if not GetStringFromJSON(
          ParameterData,
          1,
          ParameterValue
        ) then
        begin

          raise Exception.CreateFmt(
            'INSERT_DEV: invalid parameter value at index %d',
            [I]
          );

        end;


        SetHTTPDeviceParameter(
          Device,
          ParameterName,
          ParameterValue
        );

      end;

    end;


    {=======================================================================
      Добавляем устройство в ConstructRoot.

      Здесь НЕ делаем:

        Undo
        Redraw
        MoveConstructRootTo

      Это будет выполнено END_BATCH.
    =======================================================================}

    zcAddEntToCurrentDrawingConstructRoot(
      Device
    );

    zcMoveEntsFromConstructRootToCurrentDrawingWithUndo(
      'HTTP_INSERT_DEV'
    );

       //  if commandmanager.MoveConstructRootTo(rscmSpecifyFirstPoint)=IRNormal then //двигаем их
       //zcMoveEntsFromConstructRootToCurrentDrawingWithUndo('ExampleConstructToModalSpace'); //если все ок, копируем в чертеж

    {=======================================================================
      Результат
    =======================================================================}

    AResult :=
      Format(
        'Device "%s" inserted',
        [DeviceName]
      );


    ProgramLog.LogOutFormatStr(
      'HTTP INSERT_DEV: device "%s" inserted at %.3f, %.3f',
      [
        DeviceName,
        X,
        Y
      ],
      LM_Info,
      0
    );


    Result := True;


  except

    on E: Exception do
    begin

      AError :=
        Format(
          'INSERT_DEV error: %s',
          [E.Message]
        );


      ProgramLog.LogOutFormatStr(
        'HTTP INSERT_DEV error: %s',
        [E.Message],
        LM_Error,
        0
      );


      Result := False;

    end;

  end;

end;


{=============================================================================
  INITIALIZATION
=============================================================================}

initialization

  ProgramLog.LogOutFormatStr(
    'Unit "%s" initialization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsInitializeLMId
  );


  RegisterHTTPIPCCommand(
    'INSERT_DEV',
    @HTTPCommandInsertDev
  );


{=============================================================================
  FINALIZATION
=============================================================================}

finalization

  ProgramLog.LogOutFormatStr(
    'Unit "%s" finalization',
    [{$INCLUDE %FILE%}],
    LM_Info,
    UnitsFinalizeLMId
  );

end.
