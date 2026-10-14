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

unit uzvfspellblocks;

{$mode objfpc}{$H+}

// Поддержка проверки орфографии внутри блоков (issue #1468).
// Текстовые примитивы блока хранятся в определении блока в локальной
// системе координат блока. Ошибка запоминает цепочку вставок блоков
// (от вставки в модели до вложенной вставки), через которую было найдено
// определение блока; по этой цепочке строится матрица перехода из
// локальной системы координат блока в мировую.

interface

uses
  zebaseundocommands,
  uzegeometrytypes, uzeentity, uzeblockdef,
  uzvfspelldata;

const
  // Максимальная глубина вложенности блоков при сканировании
  MAX_SPELL_BLOCK_NESTING = 32;

type
  // Изменение текста примитива в определении блока с перестроением
  // всех вставок этого блока при отмене и повторе
  TUndoCmdSpellBlockText = class(TUCmdBase)
  private
    FEntity: PGDBObjEntity;
    FBlockDef: PGDBObjBlockdef;
    FOldTemplate: string;
    FNewTemplate: string;
    procedure ApplyTemplate(const ATemplate: string);
  public
    constructor Create(AEntity: PGDBObjEntity; ABlockDef: PGDBObjBlockdef;
      const ANewTemplate: string);
    procedure UnDo; override;
    procedure Comit; override;
  end;

// Является ли примитив вставкой блока (устройства не сканируются)
function IsSpellBlockInsert(EntityPtr: PGDBObjEntity): boolean;

// Получить определение блока для вставки в текущем чертеже
function GetSpellBlockDef(InsertPtr: PGDBObjEntity): PGDBObjBlockdef;

// Получить определение блока, в котором находится примитив ошибки
function GetSpellErrorBlockDef(const BlockPath: TSpellBlockPath): PGDBObjBlockdef;

// Матрица перехода из системы координат определения блока в мировую
function TryGetSpellBlockMatrix(const BlockPath: TSpellBlockPath;
  out Matrix: TzeTypedMatrix4d): boolean;

// Перевести габарит из системы координат блока в мировую
function TransformSpellVolume(const Volume: TBoundingBox;
  const Matrix: TzeTypedMatrix4d): TBoundingBox;

// Используется ли определение блока Target внутри BlockDef (с вложенностью)
function SpellBlockDefUses(BlockDef, Target: PGDBObjBlockdef;
  Depth: integer = 0): boolean;

// Перестроить вставки блоков текущего пространства, использующие BlockDef
function RebuildSpellBlockInserts(BlockDef: PGDBObjBlockdef): integer;

// Заменить текст примитива в определении блока с поддержкой отмены
function ApplySpellBlockTextCorrection(EntityPtr: PGDBObjEntity;
  const BlockPath: TSpellBlockPath; const NewTemplate,
  UndoCommandName: string): boolean;

implementation

uses
  gzctnrVectorTypes,
  uzclog, uzcdrawing, uzcdrawings, uzcutils,
  uzeconsts, uzeentblockinsert, uzeenttext, uzetypes, uzegeometry,
  uzgldrawcontext;

function IsSpellBlockInsert(EntityPtr: PGDBObjEntity): boolean;
begin
  Result := Assigned(EntityPtr) and
    (EntityPtr^.GetObjType = GDBBlockInsertID);
end;

function GetSpellBlockDef(InsertPtr: PGDBObjEntity): PGDBObjBlockdef;
begin
  Result := nil;

  if not IsSpellBlockInsert(InsertPtr) or
     (drawings.GetCurrentDWG = nil) then
    Exit;

  Result := drawings.GetCurrentDWG^.BlockDefArray.getblockdef(
    PGDBObjBlockInsert(InsertPtr)^.Name);
end;

function GetSpellErrorBlockDef(const BlockPath: TSpellBlockPath): PGDBObjBlockdef;
begin
  Result := nil;

  if Length(BlockPath) = 0 then
    Exit;

  Result := GetSpellBlockDef(PGDBObjEntity(BlockPath[High(BlockPath)]));
end;

function TryGetSpellBlockMatrix(const BlockPath: TSpellBlockPath;
  out Matrix: TzeTypedMatrix4d): boolean;
var
  i: integer;
begin
  Result := False;
  Matrix := cOneMatrix;

  if Length(BlockPath) = 0 then
    Exit;

  for i := 0 to High(BlockPath) do
    if not IsSpellBlockInsert(PGDBObjEntity(BlockPath[i])) then
      Exit;

  // Точка блока переводится сначала матрицей вложенной вставки,
  // затем матрицами внешних вставок вплоть до вставки в модели
  Matrix := PGDBObjBlockInsert(BlockPath[High(BlockPath)])^.objmatrix;
  for i := High(BlockPath) - 1 downto 0 do
    Matrix := MatrixMultiply(Matrix,
      PGDBObjBlockInsert(BlockPath[i])^.objmatrix);

  Result := True;
end;

function TransformSpellVolume(const Volume: TBoundingBox;
  const Matrix: TzeTypedMatrix4d): TBoundingBox;
var
  i: integer;
  corner: TzePoint3d;
begin
  for i := 0 to 7 do begin
    if (i and 1) = 0 then
      corner.x := Volume.pMin.x
    else
      corner.x := Volume.pMax.x;
    if (i and 2) = 0 then
      corner.y := Volume.pMin.y
    else
      corner.y := Volume.pMax.y;
    if (i and 4) = 0 then
      corner.z := Volume.pMin.z
    else
      corner.z := Volume.pMax.z;

    corner := VectorTransform3D(corner, Matrix);
    if i = 0 then begin
      Result.pMin := corner;
      Result.pMax := corner;
    end else begin
      if corner.x < Result.pMin.x then
        Result.pMin.x := corner.x;
      if corner.y < Result.pMin.y then
        Result.pMin.y := corner.y;
      if corner.z < Result.pMin.z then
        Result.pMin.z := corner.z;
      if corner.x > Result.pMax.x then
        Result.pMax.x := corner.x;
      if corner.y > Result.pMax.y then
        Result.pMax.y := corner.y;
      if corner.z > Result.pMax.z then
        Result.pMax.z := corner.z;
    end;
  end;
end;

function SpellBlockDefUses(BlockDef, Target: PGDBObjBlockdef;
  Depth: integer): boolean;
var
  entityPtr: PGDBObjEntity;
  iterator: itrec;
begin
  Result := False;

  if not Assigned(BlockDef) or not Assigned(Target) then
    Exit;

  if BlockDef = Target then begin
    Result := True;
    Exit;
  end;

  if Depth >= MAX_SPELL_BLOCK_NESTING then
    Exit;

  entityPtr := BlockDef^.ObjArray.beginiterate(iterator);
  if entityPtr <> nil then
    repeat
      if IsSpellBlockInsert(entityPtr) and
         SpellBlockDefUses(GetSpellBlockDef(entityPtr), Target,
           Depth + 1) then begin
        Result := True;
        Exit;
      end;
      entityPtr := BlockDef^.ObjArray.iterate(iterator);
    until entityPtr = nil;
end;

function RebuildSpellBlockInserts(BlockDef: PGDBObjBlockdef): integer;
var
  drawing: PTZCADDrawing;
  drawContext: TDrawContext;
  entityPtr: PGDBObjEntity;
  iterator: itrec;
begin
  Result := 0;

  if not Assigned(BlockDef) or (drawings.GetCurrentDWG = nil) or
     (drawings.GetCurrentDWG^.GetCurrentROOT = nil) then
    Exit;

  drawing := PTZCADDrawing(drawings.GetCurrentDWG);
  drawContext := drawing^.CreateDrawingRC;

  entityPtr := drawing^.GetCurrentROOT^.ObjArray.beginiterate(iterator);
  if entityPtr <> nil then
    repeat
      if IsSpellBlockInsert(entityPtr) and
         SpellBlockDefUses(GetSpellBlockDef(entityPtr), BlockDef) then begin
        entityPtr^.BuildGeometry(drawing^);
        entityPtr^.FormatEntity(drawing^, drawContext);
        entityPtr^.YouChanged(drawing^);
        Inc(Result);
      end;
      entityPtr := drawing^.GetCurrentROOT^.ObjArray.iterate(iterator);
    until entityPtr = nil;

  programlog.LogOutFormatStr(
    'RebuildSpellBlockInserts: block="%s", rebuilt inserts=%d',
    [BlockDef^.Name, Result], LM_Info);
end;

{ TUndoCmdSpellBlockText }

constructor TUndoCmdSpellBlockText.Create(AEntity: PGDBObjEntity;
  ABlockDef: PGDBObjBlockdef; const ANewTemplate: string);
begin
  inherited Create;
  FEntity := AEntity;
  FBlockDef := ABlockDef;
  FOldTemplate := string(PGDBObjText(AEntity)^.Template);
  FNewTemplate := ANewTemplate;
end;

procedure TUndoCmdSpellBlockText.ApplyTemplate(const ATemplate: string);
var
  drawing: PTZCADDrawing;
  drawContext: TDrawContext;
begin
  if not Assigned(FEntity) or (drawings.GetCurrentDWG = nil) then
    Exit;

  drawing := PTZCADDrawing(drawings.GetCurrentDWG);
  PGDBObjText(FEntity)^.Template := TDXFEntsInternalStringType(ATemplate);
  drawContext := drawing^.CreateDrawingRC;
  FEntity^.FormatEntity(drawing^, drawContext);
  RebuildSpellBlockInserts(FBlockDef);
end;

procedure TUndoCmdSpellBlockText.UnDo;
begin
  ApplyTemplate(FOldTemplate);
end;

procedure TUndoCmdSpellBlockText.Comit;
begin
  ApplyTemplate(FNewTemplate);
end;

function ApplySpellBlockTextCorrection(EntityPtr: PGDBObjEntity;
  const BlockPath: TSpellBlockPath; const NewTemplate,
  UndoCommandName: string): boolean;
var
  drawing: PTZCADDrawing;
  blockDef: PGDBObjBlockdef;
  undoCommand: TUndoCmdSpellBlockText;
begin
  Result := False;

  if not Assigned(EntityPtr) or (drawings.GetCurrentDWG = nil) then
    Exit;

  blockDef := GetSpellErrorBlockDef(BlockPath);
  if not Assigned(blockDef) then begin
    programlog.LogOutFormatStr(
      'ApplySpellBlockTextCorrection: block definition not found', [],
      LM_Info);
    Exit;
  end;

  drawing := PTZCADDrawing(drawings.GetCurrentDWG);
  zcStartUndoCommand(drawing^, UndoCommandName, False);
  try
    undoCommand := TUndoCmdSpellBlockText.Create(EntityPtr, blockDef,
      NewTemplate);
    drawing^.UndoStack.PushBackData(undoCommand);
    Inc(drawing^.UndoStack.CurrentCommand);
    undoCommand.Comit;
    zcEndUndoCommand(drawing^);
  except
    zcEndUndoCommand(drawing^);
    raise;
  end;

  programlog.LogOutFormatStr(
    'ApplySpellBlockTextCorrection: block="%s", depth=%d',
    [blockDef^.Name, Length(BlockPath)], LM_Info);
  Result := True;
end;

end.
