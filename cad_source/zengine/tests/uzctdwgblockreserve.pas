unit uzctdwgblockreserve;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TDWGBlockReserveTest = class(TTestCase)
  published
    procedure CountsRawBlockHeadersOnly;
    procedure ReserveKeepsBlockDefPointersStableUntilOwnerResolve;
  end;

implementation

uses
  SysUtils,
  dwg,
  UGDBObjBlockdefArray,
  uzeblockdef,
  uzedwgblockreserve;

procedure InitRawObject(var Obj: Dwg_Object; RawType: DWG_OBJECT_TYPE);
begin
  FillChar(Obj, SizeOf(Obj), 0);
  Obj.fixedtype := RawType;
  Obj.supertype := DWG_SUPERTYPE_OBJECT;
end;

procedure TDWGBlockReserveTest.CountsRawBlockHeadersOnly;
var
  Raw: Dwg_Data;
  Objects: array[0..3] of Dwg_Object;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  InitRawObject(Objects[0], DWG_TYPE_BLOCK_HEADER);
  InitRawObject(Objects[1], DWG_TYPE_LINE);
  InitRawObject(Objects[2], DWG_TYPE_BLOCK_HEADER);
  InitRawObject(Objects[3], DWG_TYPE_LAYER);
  Raw.num_objects := Length(Objects);
  Raw.&object := @Objects[0];

  AssertEquals(2, DWGRawBlockHeaderCount(Raw));
end;

procedure TDWGBlockReserveTest.ReserveKeepsBlockDefPointersStableUntilOwnerResolve;
var
  Raw: Dwg_Data;
  Objects: array[0..4] of Dwg_Object;
  BlockDefs: GDBObjBlockdefArray;
  First, Found: PGDBObjBlockdef;
  I: Integer;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  for I := Low(Objects) to High(Objects) do
    InitRawObject(Objects[I], DWG_TYPE_BLOCK_HEADER);
  Raw.num_objects := Length(Objects);
  Raw.&object := @Objects[0];

  BlockDefs.init(1);
  try
    DWGReserveBlockDefCapacity(Raw, BlockDefs);

    First := BlockDefs.create('B0');
    AssertNotNull(First);
    for I := 1 to High(Objects) do
      BlockDefs.create('B' + IntToStr(I));

    { Missing block fallback is created during ResolveRefs, before owner
      resolution. It must not trigger a grow after block handles were already
      registered with pointers into this array. }
    BlockDefs.create('*DWG_MISSING_BLOCK');

    Found := BlockDefs.getblockdef('B0');
    AssertNotNull(Found);
    AssertEquals('registered block-def pointer remains valid',
      PtrInt(First), PtrInt(Found));
  finally
    BlockDefs.done;
  end;
end;

initialization
  RegisterTests([TDWGBlockReserveTest]);
end.
