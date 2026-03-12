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

{**Модуль структур данных для управления вращением объектов}
unit uzvrotate_struct;

{$INCLUDE zengineconfig.inc}

interface
uses
  uzegeometrytypes;

const
  {**Минимальное значение позиции ползунка}
  TRACKBAR_MIN = -100;

  {**Максимальное значение позиции ползунка}
  TRACKBAR_MAX = 100;

  {**Начальная позиция ползунка (центр)}
  TRACKBAR_CENTER = 0;

  {**Максимальный угол поворота в градусах}
  MAX_ROTATION_ANGLE = 360.0;

type
  {**Состояние вращения для хранения исходных данных объектов}
  TRotationState = record
    OriginalMatrix: TzeTypedMatrix4d;
    EntityPtr: Pointer;
  end;

  {**Массив состояний вращения}
  TRotationStateArray = array of TRotationState;

  {**Данные о текущем вращении}
  TRotationData = record
    AngleX: Double;
    AngleY: Double;
    AngleZ: Double;
    Center: TzePoint3d;
    States: TRotationStateArray;
  end;

var
  {**Глобальные данные о текущем вращении}
  RotationData: TRotationData;

{**Преобразовать позицию ползунка в угол в радианах
   @param APosition - позиция ползунка (-100..100)
   @return угол в радианах}
function TrackBarPositionToAngle(APosition: Integer): Double;

{**Преобразовать угол в радианах в позицию ползунка
   @param AAngle - угол в радианах
   @return позиция ползунка (-100..100)}
function AngleToTrackBarPosition(AAngle: Double): Integer;

implementation
uses
  Math;

{**Преобразовать позицию ползунка в угол в радианах}
function TrackBarPositionToAngle(APosition: Integer): Double;
begin
  // Position / Max * 360 градусов, затем преобразуем в радианы
  Result := (APosition / TRACKBAR_MAX) * MAX_ROTATION_ANGLE * Pi / 180.0;
end;

{**Преобразовать угол в радианах в позицию ползунка}
function AngleToTrackBarPosition(AAngle: Double): Integer;
var
  degrees: Double;
begin
  // Преобразуем радианы в градусы, затем в позицию ползунка
  degrees := AAngle * 180.0 / Pi;
  Result := Round((degrees / MAX_ROTATION_ANGLE) * TRACKBAR_MAX);

  // Ограничиваем диапазон
  if Result < TRACKBAR_MIN then
    Result := TRACKBAR_MIN
  else if Result > TRACKBAR_MAX then
    Result := TRACKBAR_MAX;
end;

end.
