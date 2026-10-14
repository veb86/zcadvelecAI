program test_issue1468_block_matrix;
// Проверка перевода координат ошибки из системы координат блока в мировую
// (issue #1468): TryGetSpellBlockMatrix и TransformSpellVolume.
// Сборка: experiments/compile_spell_units.sh experiments/test_issue1468_block_matrix.lpr
{$mode objfpc}{$H+}
uses
  Interfaces, SysUtils,
  uzegeometrytypes, uzegeometry, uzeentity, uzeentblockinsert,
  uzvfspelldata, uzvfspellblocks;

var
  Failed: integer = 0;

procedure Check(Condition: boolean; const Msg: string);
begin
  if Condition then
    WriteLn('OK   ', Msg)
  else begin
    WriteLn('FAIL ', Msg);
    Inc(Failed);
  end;
end;

function SamePoint(const A, B: TzePoint3d): boolean;
begin
  Result := (Abs(A.x - B.x) < 1e-9) and (Abs(A.y - B.y) < 1e-9) and
    (Abs(A.z - B.z) < 1e-9);
end;

function PointStr(const P: TzePoint3d): string;
begin
  Result := Format('(%.6f, %.6f, %.6f)', [P.x, P.y, P.z]);
end;

function MakePoint(x, y, z: double): TzePoint3d;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

var
  outerInsert, innerInsert: GDBObjBlockInsert;
  path: TSpellBlockPath;
  matrix: TzeTypedMatrix4d;
  localPoint, expected, actual: TzePoint3d;
  volume, worldVolume: TBoundingBox;
begin
  outerInsert.initnul;
  innerInsert.initnul;

  // Внешняя вставка в модели: масштаб 2, поворот 90°, перенос (100, 50)
  outerInsert.objmatrix := MatrixMultiply(
    MatrixMultiply(CreateScaleMatrix(2), CreateRotationMatrixZ(Pi / 2)),
    CreateTranslationMatrix(100, 50, 0));
  // Вложенная вставка внутри определения внешнего блока: перенос (10, 0)
  innerInsert.objmatrix := CreateTranslationMatrix(10, 0, 0);

  localPoint := MakePoint(1, 2, 0);

  // Пустая цепочка — примитив модели, матрицы нет
  Check(not TryGetSpellBlockMatrix(nil, matrix), 'empty block path');

  // Один уровень: точка определения блока -> мир
  SetLength(path, 1);
  path[0] := @outerInsert;
  Check(TryGetSpellBlockMatrix(path, matrix), 'single level matrix');
  expected := VectorTransform3D(localPoint, outerInsert.objmatrix);
  actual := VectorTransform3D(localPoint, matrix);
  Check(SamePoint(actual, expected),
    'single level point ' + PointStr(actual) + ' = ' + PointStr(expected));
  // (1,2) *2 -> (2,4), поворот 90° -> (-4,2), перенос -> (96,52)
  Check(SamePoint(actual, MakePoint(96, 52, 0)),
    'single level point is (96, 52, 0)');

  // Два уровня: определение вложенного блока -> внешний блок -> мир
  SetLength(path, 2);
  path[0] := @outerInsert;
  path[1] := @innerInsert;
  Check(TryGetSpellBlockMatrix(path, matrix), 'nested matrix');
  expected := VectorTransform3D(
    VectorTransform3D(localPoint, innerInsert.objmatrix),
    outerInsert.objmatrix);
  actual := VectorTransform3D(localPoint, matrix);
  Check(SamePoint(actual, expected),
    'nested point ' + PointStr(actual) + ' = ' + PointStr(expected));
  // (1,2)+(10,0) -> (11,2), *2 -> (22,4), поворот -> (-4,22), перенос -> (96,72)
  Check(SamePoint(actual, MakePoint(96, 72, 0)),
    'nested point is (96, 72, 0)');

  // Габарит слова в блоке переводится в мировой габарит
  volume.pMin := MakePoint(0, 0, 0);
  volume.pMax := MakePoint(4, 1, 0);
  worldVolume := TransformSpellVolume(volume, matrix);
  // Углы (0,0)->(100,70), (4,1)->(98,78): min (98,70), max (100,78)
  Check(SamePoint(worldVolume.pMin, MakePoint(98, 70, 0)),
    'world volume min ' + PointStr(worldVolume.pMin));
  Check(SamePoint(worldVolume.pMax, MakePoint(100, 78, 0)),
    'world volume max ' + PointStr(worldVolume.pMax));

  // Элемент цепочки, не являющийся вставкой блока, отклоняется
  path[1] := nil;
  Check(not TryGetSpellBlockMatrix(path, matrix), 'invalid block path');

  if Failed > 0 then begin
    WriteLn(Failed, ' check(s) failed');
    Halt(1);
  end;
  WriteLn('All checks passed');
end.
