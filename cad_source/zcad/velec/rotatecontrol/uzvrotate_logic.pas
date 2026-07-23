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

{**Модуль логики вращения объектов.
   Использует подход с применением дельта-трансформации для правильной работы
   с любыми типами объектов, включая дуги. Основан на алгоритме из uzccommand_rotate.}
unit uzvrotate_logic;

{$INCLUDE zengineconfig.inc}

interface
uses
  SysUtils,
  Math,
  uzegeometrytypes,
  uzegeometry,
  uzeentity,
  uzcdrawings,
  uzgldrawcontext,
  UGDBSelectedObjArray,
  gzctnrVectorTypes,
  uzvrotate_struct,
  uzclog;

{**Инициализировать данные вращения из выделенных объектов.
   @return True если инициализация успешна}
function InitRotationData: Boolean;

{**Освободить данные вращения}
procedure FreeRotationData;

{**Вычислить центр вращения (геометрический центр bounding box).
   @return точка центра вращения}
function CalculateRotationCenter: TzePoint3d;

{**Применить вращение к выделенным объектам.
   Использует дельта-трансформацию: вычисляет разницу между текущим и новым
   вращением, затем применяет её через метод Transform объектов.
   @param AAngleX - угол вращения вокруг оси X в радианах
   @param AAngleY - угол вращения вокруг оси Y в радианах
   @param AAngleZ - угол вращения вокруг оси Z в радианах}
procedure ApplyRotation(AAngleX, AAngleY, AAngleZ: Double);

{**Сбросить вращение к исходному состоянию}
procedure ResetRotation;

{**Подтвердить вращение (применить окончательно)}
procedure CommitRotation;

{**Получить количество выделенных объектов.
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

{**Сохранить указатель на объект.
   @param AEntity - указатель на объект
   @param AIndex - индекс в массиве состояний}
procedure SaveEntityPointer(AEntity: PGDBObjEntity; AIndex: Integer);
begin
  if AEntity = nil then
    Exit;
  RotationData.States[AIndex].EntityPtr := AEntity;
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

  // Выделяем память для массива указателей на объекты
  SetLength(RotationData.States, count);

  // Сохраняем указатели на объекты
  i := 0;
  psd := drawings.GetCurrentDWG^.SelObjArray.beginiterate(ir);

  if psd <> nil then
  begin
    repeat
      if psd^.objaddr <> nil then
      begin
        SaveEntityPointer(psd^.objaddr, i);
        Inc(i);
      end;
      psd := drawings.GetCurrentDWG^.SelObjArray.iterate(ir);
    until psd = nil;
  end;

  // Вычисляем центр вращения
  RotationData.Center := CalculateRotationCenter;

  // Начинаем с нулевых углов
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
  RotationData.Center := cP3d__0__0__0;
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
  Result := cP3d__0__0__0;
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

{**Создать матрицу вращения вокруг точки.
   Порядок вращения: сначала Z, затем Y, затем X.
   Алгоритм: сдвиг в центр -> вращение -> сдвиг обратно.
   @param AAngleX - угол вокруг X в радианах
   @param AAngleY - угол вокруг Y в радианах
   @param AAngleZ - угол вокруг Z в радианах
   @param ACenter - центр вращения
   @return комбинированная матрица преобразования}
function CreateRotationMatrixAroundPoint(
  AAngleX, AAngleY, AAngleZ: Double;
  const ACenter: TzePoint3d
): TzeTypedMatrix4d;
var
  translateToOrigin: TzeTypedMatrix4d;
  translateBack: TzeTypedMatrix4d;
  rotX, rotY, rotZ: TzeTypedMatrix4d;
  combined: TzeTypedMatrix4d;
begin
  // Порядок операций аналогичен uzccommand_rotate.pas
  translateToOrigin := CreateTranslationMatrix(-ACenter.asVector);
  translateBack := CreateTranslationMatrix(ACenter.asVector);

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

{**Применить вращение к выделенным объектам.
   Вычисляет дельта-трансформацию от текущего состояния к новому и применяет её.}
procedure ApplyRotation(AAngleX, AAngleY, AAngleZ: Double);
var
  i: Integer;
  pEntity: PGDBObjEntity;
  currentRotMatrix: TzeTypedMatrix4d;
  newRotMatrix: TzeTypedMatrix4d;
  deltaMatrix: TzeTypedMatrix4d;
  inverseCurrentRot: TzeTypedMatrix4d;
  dc: TDrawContext;
begin
  if Length(RotationData.States) = 0 then
    Exit;

  // Создаём матрицу текущего вращения (до изменения)
  currentRotMatrix := CreateRotationMatrixAroundPoint(
    RotationData.AngleX,
    RotationData.AngleY,
    RotationData.AngleZ,
    RotationData.Center
  );

  // Создаём матрицу нового вращения
  newRotMatrix := CreateRotationMatrixAroundPoint(
    AAngleX,
    AAngleY,
    AAngleZ,
    RotationData.Center
  );

  // Вычисляем дельта-матрицу: инверсия текущего * новое = переход от текущего к новому
  inverseCurrentRot := currentRotMatrix;
  MatrixInvert(inverseCurrentRot);
  deltaMatrix := MatrixMultiply(inverseCurrentRot, newRotMatrix);

  dc := drawings.GetCurrentDWG^.CreateDrawingRC;

  // Применяем дельта-трансформацию к каждому объекту
  for i := 0 to High(RotationData.States) do
  begin
    pEntity := PGDBObjEntity(RotationData.States[i].EntityPtr);

    if pEntity = nil then
      Continue;

    // Применяем дельта-вращение через Transform
    // Это правильно работает для всех типов объектов, включая дуги
    pEntity^.Transform(deltaMatrix);

    // Обновляем отображение объекта
    pEntity^.FormatEntity(drawings.GetCurrentDWG^, dc);
  end;

  // Сохраняем новые углы как текущие
  RotationData.AngleX := AAngleX;
  RotationData.AngleY := AAngleY;
  RotationData.AngleZ := AAngleZ;
end;

{**Сбросить вращение к исходному состоянию.
   Применяет обратную трансформацию для возврата к начальной позиции.}
procedure ResetRotation;
var
  i: Integer;
  pEntity: PGDBObjEntity;
  currentRotMatrix: TzeTypedMatrix4d;
  inverseMatrix: TzeTypedMatrix4d;
  dc: TDrawContext;
begin
  if Length(RotationData.States) = 0 then
    Exit;

  // Если углы уже нулевые, ничего делать не нужно
  if (Abs(RotationData.AngleX) < 1e-10) and
     (Abs(RotationData.AngleY) < 1e-10) and
     (Abs(RotationData.AngleZ) < 1e-10) then
    Exit;

  dc := drawings.GetCurrentDWG^.CreateDrawingRC;

  // Создаём матрицу текущего вращения
  currentRotMatrix := CreateRotationMatrixAroundPoint(
    RotationData.AngleX,
    RotationData.AngleY,
    RotationData.AngleZ,
    RotationData.Center
  );

  // Вычисляем обратную матрицу
  inverseMatrix := currentRotMatrix;
  MatrixInvert(inverseMatrix);

  // Применяем обратную трансформацию к каждому объекту
  for i := 0 to High(RotationData.States) do
  begin
    pEntity := PGDBObjEntity(RotationData.States[i].EntityPtr);

    if pEntity = nil then
      Continue;

    // Применяем обратное вращение
    pEntity^.Transform(inverseMatrix);

    // Обновляем отображение объекта
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

{**Подтвердить вращение (применить окончательно).
   Обновляет отображение и освобождает ресурсы.}
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
