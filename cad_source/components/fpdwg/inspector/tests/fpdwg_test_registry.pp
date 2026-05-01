unit fpdwg_test_registry;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGRegistryTest = class(TTestCase)
  published
    procedure AddIndexesObjectsByHandle;
    procedure GetMissingHandleRaises;
    procedure RejectsZeroHandle;
    procedure StrictDuplicateRaisesAndKeepsOriginal;
    procedure TolerantDuplicateCreatesDiagnosticObjectAndWarning;
    procedure IterateVisitsUniqueObjectsAndDuplicates;
  end;

  TFPDWGDocumentTest = class(TTestCase)
  published
    procedure RegisterSyntheticTablesAddsStableTableObjects;
    procedure RegisterSyntheticTablesIsIdempotent;
  end;

implementation

uses
  SysUtils,
  dwg,
  fpdwg_types,
  fpdwg_logger,
  fpdwg_model_base,
  fpdwg_model_unknown,
  fpdwg_registry,
  fpdwg_document;

type
  TIterateState = record
    Count: Integer;
    SawLayer: Boolean;
    SawDuplicate: Boolean;
  end;
  PIterateState = ^TIterateState;

function NewObject(AHandle: TDWGHandle; ADomainType: TDWGDomainObjectType
  ): TDWGObject;
begin
  Result := TDWGObject.Create;
  Result.Handle := AHandle;
  Result.DomainType := ADomainType;
  Result.RawObjectType := DWG_TYPE_LAYER;
end;

procedure CountObject(Obj: TDWGObject; Data: Pointer);
var
  State: PIterateState;
begin
  State := PIterateState(Data);
  Inc(State^.Count);
  if Obj.DomainType = dotLayer then
    State^.SawLayer := True;
  if Obj is TDWGDuplicateHandleObject then
    State^.SawDuplicate := True;
end;

procedure TFPDWGRegistryTest.AddIndexesObjectsByHandle;
var
  Registry: TObjectRegistry;
  Obj10, Obj20, Found: TDWGObject;
begin
  Registry := TObjectRegistry.Create(lmStrict, nil);
  try
    Obj20 := NewObject($20, dotUnknown);
    Obj10 := NewObject($10, dotLayer);

    AssertTrue(Registry.Add(Obj20) = Obj20);
    AssertTrue(Registry.Add(Obj10) = Obj10);

    AssertEquals(2, Registry.Count);
    AssertEquals(2, Registry.UniqueCount);
    AssertEquals(0, Registry.DuplicateCount);
    AssertEquals(Int64($10), Int64(Registry.ObjectAt(0).Handle));
    AssertEquals(Int64($20), Int64(Registry.ObjectAt(1).Handle));
    AssertTrue(Registry.TryGet($10, Found));
    AssertTrue(Found = Obj10);
    AssertTrue(Registry.TryGet($20, Found));
    AssertTrue(Found = Obj20);
    AssertFalse(Registry.TryGet($30, Found));
  finally
    Registry.Free;
  end;
end;

procedure TFPDWGRegistryTest.GetMissingHandleRaises;
var
  Registry: TObjectRegistry;
  Raised: Boolean;
begin
  Registry := TObjectRegistry.Create(lmStrict, nil);
  try
    Raised := False;
    try
      Registry.Get($99);
    except
      on EDWGRegistryError do
        Raised := True;
    end;
    AssertTrue(Raised);
  finally
    Registry.Free;
  end;
end;

procedure TFPDWGRegistryTest.RejectsZeroHandle;
var
  Registry: TObjectRegistry;
  Obj: TDWGObject;
  Raised: Boolean;
begin
  Registry := TObjectRegistry.Create(lmStrict, nil);
  Obj := NewObject(0, dotUnknown);
  try
    Raised := False;
    try
      Registry.Add(Obj);
    except
      on EDWGRegistryError do
        Raised := True;
    end;
    AssertTrue(Raised);
  finally
    Obj.Free;
    Registry.Free;
  end;
end;

procedure TFPDWGRegistryTest.StrictDuplicateRaisesAndKeepsOriginal;
var
  Registry: TObjectRegistry;
  Original, Duplicate, Found: TDWGObject;
  Raised: Boolean;
begin
  Registry := TObjectRegistry.Create(lmStrict, nil);
  Duplicate := nil;
  try
    Original := NewObject($42, dotLayer);
    Registry.Add(Original);
    Duplicate := NewObject($42, dotUnknown);

    Raised := False;
    try
      Registry.Add(Duplicate);
    except
      on EDWGDuplicateHandleError do
        Raised := True;
    end;

    AssertTrue(Raised);
    AssertEquals(1, Registry.Count);
    AssertTrue(Registry.TryGet($42, Found));
    AssertTrue(Found = Original);
  finally
    Duplicate.Free;
    Registry.Free;
  end;
end;

procedure TFPDWGRegistryTest.TolerantDuplicateCreatesDiagnosticObjectAndWarning;
var
  Registry: TObjectRegistry;
  Logger: IDWGLogger;
  MemoryLogger: TDWGMemoryLogger;
  Original, Registered: TDWGObject;
  Duplicate: TDWGDuplicateHandleObject;
begin
  MemoryLogger := TDWGMemoryLogger.Create;
  Logger := MemoryLogger;
  Registry := TObjectRegistry.Create(lmTolerant, Logger);
  try
    Original := NewObject($55, dotLayer);
    Registry.Add(Original);

    Registered := Registry.Add(NewObject($55, dotLine));

    AssertTrue(Registered is TDWGDuplicateHandleObject);
    Duplicate := TDWGDuplicateHandleObject(Registered);
    AssertEquals(Int64($55), Int64(Duplicate.Handle));
    AssertEquals(Int64($55), Int64(Duplicate.OriginalHandle));
    AssertTrue(Duplicate.ConflictWith = Original);
    AssertEquals(2, Registry.Count);
    AssertEquals(1, Registry.UniqueCount);
    AssertEquals(1, Registry.DuplicateCount);
    AssertEquals(1, MemoryLogger.ErrorCount);
    AssertEquals(1201, MemoryLogger.GetError(0).Code);
  finally
    Registry.Free;
    Logger := nil;
  end;
end;

procedure TFPDWGRegistryTest.IterateVisitsUniqueObjectsAndDuplicates;
var
  Registry: TObjectRegistry;
  State: TIterateState;
begin
  Registry := TObjectRegistry.Create(lmTolerant, nil);
  try
    Registry.Add(NewObject($10, dotLayer));
    Registry.Add(NewObject($20, dotUnknown));
    Registry.Add(NewObject($20, dotLine));

    FillChar(State, SizeOf(State), 0);
    Registry.Iterate(@CountObject, @State);

    AssertEquals(3, State.Count);
    AssertTrue(State.SawLayer);
    AssertTrue(State.SawDuplicate);
  finally
    Registry.Free;
  end;
end;

procedure TFPDWGDocumentTest.RegisterSyntheticTablesAddsStableTableObjects;
var
  Doc: TDWGDocument;
  Obj: TDWGObject;
  Table: TDWGSyntheticTable;
begin
  Doc := TDWGDocument.Create(lmStrict, nil);
  try
    Doc.RegisterSyntheticTables;

    AssertEquals(6, Doc.Registry.Count);
    AssertTrue(Doc.Registry.TryGet(DWG_SYNTHETIC_LAYER_TABLE_HANDLE, Obj));
    AssertTrue(Obj is TDWGSyntheticTable);
    Table := TDWGSyntheticTable(Obj);
    AssertEquals('LAYER', Table.TableKind);
    AssertEquals(Ord(dotSyntheticTable), Ord(Table.DomainType));
    AssertEquals(Ord(osResolved), Ord(Table.Status));
  finally
    Doc.Free;
  end;
end;

procedure TFPDWGDocumentTest.RegisterSyntheticTablesIsIdempotent;
var
  Doc: TDWGDocument;
begin
  Doc := TDWGDocument.Create(lmStrict, nil);
  try
    Doc.RegisterSyntheticTables;
    Doc.RegisterSyntheticTables;

    AssertEquals(6, Doc.Registry.Count);
  finally
    Doc.Free;
  end;
end;

begin
  RegisterTests([TFPDWGRegistryTest, TFPDWGDocumentTest]);
end.
