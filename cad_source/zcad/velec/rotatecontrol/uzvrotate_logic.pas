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

{**Модуль логики вращения объектов}
unit uzvrotate_logic;

{$INCLUDE zengineconfig.inc}

interface
uses
  SysUtils,
  uzegeometrytypes,
  uzegeometry,
  uzeentity,
  uzeentwithmatrix,
  uzcdrawings,
  uzgldrawcontext,
  UGDBSelectedObjArray,
  gzctnrVectorTypes,
  uzvrotate_struct,
  uzclog;

{**Инициализировать данные вращения из выделенных объектов
   @return True если инициализация успешна}
function InitRotationData: Boolean;

{**Освободить данные вращения}
procedure FreeRotationData;

{**Вычислить центр вращения (геометрический центр bounding box)
   @return точка центра вращения}
function CalculateRotationCenter: TzePoint3d;

{**Применить вращение к выделенным объектам
   @param AAngleX - угол вращения вокруг оси X в радианах
   @param AAngleY - угол вращения вокруг оси Y в радианах
   @param AAngleZ - угол вращения вокруг оси Z в радианах}
procedure ApplyRotation(AAngleX, AAngleY, AAngleZ: Double);

{**Сбросить вращение к исходному состоянию}
procedure ResetRotation;

{**Подтвердить вращение (применить окончательно)}
procedure CommitRotation;

{**Получить количество выделенных объектов
   @return количество объектов}
function GetSelectedObjectsCount: Integer;

implementation

{**Проверить наличие выделенных объектов}
function HasSelectedObjects: Boolean;
begin
  Result := drawings.GetCurrentDWG^.SelObjArray.Count > 0;
end;

{**Получить количество выделенных объектов}
function GetSelectedObjectsCount: Integer;
begin
  Result := drawings.GetCurrentDWG^.SelObjArray.Count;
end;

{**Сохранить исходное состояние объекта
   @param AEntity - указатель на объект
   @param AIndex - индекс в массиве состояний}
procedure SaveEntityState(AEntity: PGDBObjEntity; AIndex: Integer);
var
  pWithMatrix: PGDBObjWithMatrix;
begin
  if AEntity = nil then
    Exit;

  RotationData.States[AIndex].EntityPtr := AEntity;

  // Если объект поддерживает матрицу, сохраняем её
  if AEntity^.GetMatrix <> nil then
  begin
    pWithMatrix := PGDBObjWithMatrix(AEntity);
    RotationData.States[AIndex].OriginalMatrix := pWithMatrix^.ObjMatrix;
  end
  else
  begin
    RotationData.States[AIndex].OriginalMatrix := OneMatrix;
  end;
end;

{**Инициализировать данные вращения из выделенных объектов}
function InitRotationData: Boolean;
var
  psd: PSelectedObjDesc;
  ir: itrec;
  count, i: Integer;
begin
  Result := False;

  if not HasSelectedObjects then
  begin
    programlog.LogOutFormatStr(
      'uzvrotate_logic: нет выделенных объектов',
      [],
      LM_Info
    );
    Exit;
  end;

  // Считаем количество объектов
  count := drawings.GetCurrentDWG^.SelObjArray.Count;

  // Выделяем память для массива состояний
  SetLength(RotationData.States, count);

  // Сохраняем исходное состояние каждого объекта
  i := 0;
  psd := drawings.GetCurrentDWG^.SelObjArray.beginiterate(ir);

  if psd <> nil then
  begin
    repeat
      if psd^.objaddr <> nil then
      begin
        SaveEntityState(psd^.objaddr, i);
        Inc(i);
      end;
      psd := drawings.GetCurrentDWG^.SelObjArray.iterate(ir);
    until psd = nil;
  end;

  // Вычисляем центр вращения
  RotationData.Center := CalculateRotationCenter;

  // Обнуляем углы
  RotationData.AngleX := 0;
  RotationData.AngleY := 0;
  RotationData.AngleZ := 0;

  programlog.LogOutFormatStr(
    'uzvrotate_logic: инициализировано объектов = %d, центр = (%.2f, %.2f, %.2f)',
    [count, RotationData.Center.x, RotationData.Center.y, RotationData.Center.z],
    LM_Info
  );

  Result := True;
end;

{**Освободить данные вращения}
procedure FreeRotationData;
begin
  SetLength(RotationData.States, 0);
  RotationData.AngleX := 0;
  RotationData.AngleY := 0;
  RotationData.AngleZ := 0;
  RotationData.Center := NulVertex;
end;

{**Вычислить центр вращения (геометрический центр bounding box)}
function CalculateRotationCenter: TzePoint3d;
var
  psd: PSelectedObjDesc;
  ir: itrec;
  bb: TBoundingBox;
  firstObject: Boolean;
  dc: TDrawContext;
begin
  Result := NulVertex;
  firstObject := True;

  dc := drawings.GetCurrentDWG^.CreateDrawingRC;
  psd := drawings.GetCurrentDWG^.SelObjArray.beginiterate(ir);

  if psd = nil then
    Exit;

  repeat
    if psd^.objaddr <> nil then
    begin
      // Получаем bounding box объекта
      psd^.objaddr^.getonlyoutbound(dc);

      if firstObject then
      begin
        bb := psd^.objaddr^.vp.BoundingBox;
        firstObject := False;
      end
      else
      begin
        // Объединяем bounding box
        ConcatBB(bb, psd^.objaddr^.vp.BoundingBox);
      end;
    end;

    psd := drawings.GetCurrentDWG^.SelObjArray.iterate(ir);
  until psd = nil;

  // Вычисляем центр bounding box
  if not firstObject then
  begin
    Result.x := (bb.LBN.x + bb.RTF.x) / 2;
    Result.y := (bb.LBN.y + bb.RTF.y) / 2;
    Result.z := (bb.LBN.z + bb.RTF.z) / 2;
  end;
end;

{**Создать комбинированную матрицу вращения
   @param AAngleX - угол вокруг X в радианах
   @param AAngleY - угол вокруг Y в радианах
   @param AAngleZ - угол вокруг Z в радианах
   @param ACenter - центр вращения
   @return комбинированная матрица преобразования}
function CreateCombinedRotationMatrix(
  AAngleX, AAngleY, AAngleZ: Double;
  const ACenter: TzePoint3d
): TzeTypedMatrix4d;
var
  translateToOrigin: TzeTypedMatrix4d;
  translateBack: TzeTypedMatrix4d;
  rotX, rotY, rotZ: TzeTypedMatrix4d;
  combined: TzeTypedMatrix4d;
begin
  // Матрица сдвига в начало координат
  translateToOrigin := CreateTranslationMatrix(
    CreateVertex(-ACenter.x, -ACenter.y, -ACenter.z)
  );

  // Матрица сдвига обратно
  translateBack := CreateTranslationMatrix(ACenter);

  // Матрицы вращения по осям
  rotX := CreateRotationMatrixX(AAngleX);
  rotY := CreateRotationMatrixY(AAngleY);
  rotZ := CreateRotationMatrixZ(AAngleZ);

  // Комбинируем: сдвиг к началу -> вращение Z -> Y -> X -> сдвиг обратно
  combined := translateToOrigin;
  combined := MatrixMultiply(combined, rotZ);
  combined := MatrixMultiply(combined, rotY);
  combined := MatrixMultiply(combined, rotX);
  combined := MatrixMultiply(combined, translateBack);

  Result := combined;
end;

{**Применить вращение к выделенным объектам}
procedure ApplyRotation(AAngleX, AAngleY, AAngleZ: Double);
var
  i: Integer;
  pEntity: PGDBObjEntity;
  pWithMatrix: PGDBObjWithMatrix;
  rotMatrix: TzeTypedMatrix4d;
  newMatrix: TzeTypedMatrix4d;
  dc: TDrawContext;
begin
  if Length(RotationData.States) = 0 then
    Exit;

  // Сохраняем текущие углы
  RotationData.AngleX := AAngleX;
  RotationData.AngleY := AAngleY;
  RotationData.AngleZ := AAngleZ;

  // Создаём комбинированную матрицу вращения
  rotMatrix := CreateCombinedRotationMatrix(
    AAngleX,
    AAngleY,
    AAngleZ,
    RotationData.Center
  );

  dc := drawings.GetCurrentDWG^.CreateDrawingRC;

  // Применяем вращение к каждому объекту
  for i := 0 to High(RotationData.States) do
  begin
    pEntity := PGDBObjEntity(RotationData.States[i].EntityPtr);

    if pEntity = nil then
      Continue;

    // Проверяем, поддерживает ли объект матрицу
    if pEntity^.GetMatrix <> nil then
    begin
      pWithMatrix := PGDBObjWithMatrix(pEntity);

      // Применяем матрицу вращения к исходной матрице
      newMatrix := MatrixMultiply(
        RotationData.States[i].OriginalMatrix,
        rotMatrix
      );
      pWithMatrix^.ObjMatrix := newMatrix;

      // Пересчитываем объект из матрицы
      pWithMatrix^.ReCalcFromObjMatrix;
    end
    else
    begin
      // Для объектов без матрицы используем Transform
      // Сначала сбрасываем к исходному состоянию (если возможно)
      // затем применяем новую трансформацию
      pEntity^.transform(rotMatrix);
    end;

    // Форматируем объект
    pEntity^.FormatEntity(drawings.GetCurrentDWG^, dc);
  end;

  // Обновляем отображение
  drawings.GetCurrentDWG^.ConstructObjRoot.ObjArray.Free;
end;

{**Сбросить вращение к исходному состоянию}
procedure ResetRotation;
var
  i: Integer;
  pEntity: PGDBObjEntity;
  pWithMatrix: PGDBObjWithMatrix;
  dc: TDrawContext;
begin
  if Length(RotationData.States) = 0 then
    Exit;

  dc := drawings.GetCurrentDWG^.CreateDrawingRC;

  // Восстанавливаем исходное состояние каждого объекта
  for i := 0 to High(RotationData.States) do
  begin
    pEntity := PGDBObjEntity(RotationData.States[i].EntityPtr);

    if pEntity = nil then
      Continue;

    // Восстанавливаем матрицу
    if pEntity^.GetMatrix <> nil then
    begin
      pWithMatrix := PGDBObjWithMatrix(pEntity);
      pWithMatrix^.ObjMatrix := RotationData.States[i].OriginalMatrix;
      pWithMatrix^.ReCalcFromObjMatrix;
    end;

    // Форматируем объект
    pEntity^.FormatEntity(drawings.GetCurrentDWG^, dc);
  end;

  // Обнуляем углы
  RotationData.AngleX := 0;
  RotationData.AngleY := 0;
  RotationData.AngleZ := 0;

  programlog.LogOutFormatStr(
    'uzvrotate_logic: вращение сброшено',
    [],
    LM_Info
  );
end;

{**Подтвердить вращение (применить окончательно)}
procedure CommitRotation;
var
  dc: TDrawContext;
begin
  dc := drawings.GetCurrentDWG^.CreateDrawingRC;

  // Форматируем все объекты после редактирования
  drawings.GetCurrentROOT^.FormatAfterEdit(drawings.GetCurrentDWG^, dc);

  programlog.LogOutFormatStr(
    'uzvrotate_logic: вращение применено (X=%.1f, Y=%.1f, Z=%.1f градусов)',
    [
      RotationData.AngleX * 180 / Pi,
      RotationData.AngleY * 180 / Pi,
      RotationData.AngleZ * 180 / Pi
    ],
    LM_Info
  );

  // Освобождаем данные
  FreeRotationData;
end;

end.
