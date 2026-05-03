unit fpdwg_test_resolver;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGResolverTest = class(TTestCase)
  published
    procedure ResolvesOwnerLayerAndLinetypeReferences;
    procedure BrokenRequiredReferencesMarkObjectBrokenAndWarn;
    procedure ResolvesEntityPrevNextReferences;
    procedure ResolvesTextStyleReference;
  end;

implementation

uses
  SysUtils,
  fpdwg_types,
  fpdwg_logger,
  fpdwg_resolver,
  fpdwg_document,
  fpdwg_model_base,
  fpdwg_model_blocks,
  fpdwg_model_entities,
  fpdwg_model_tables;

function Ref(AHandle: TDWGHandle): TDWGHandleRef;
begin
  Result.Value := AHandle;
  Result.Source := hsAbsoluteRef;
end;

function NewLinetype(AHandle: TDWGHandle; const AName: string): TDWGLinetype;
begin
  Result := TDWGLinetype.Create;
  Result.Handle := AHandle;
  Result.LinetypeName := AName;
  Result.OwnerHandle := Ref(DWG_SYNTHETIC_LTYPE_TABLE_HANDLE);
end;

function NewLayer(AHandle, ALinetypeHandle: TDWGHandle;
  const AName: string): TDWGLayer;
begin
  Result := TDWGLayer.Create;
  Result.Handle := AHandle;
  Result.LayerName := AName;
  Result.OwnerHandle := Ref(DWG_SYNTHETIC_LAYER_TABLE_HANDLE);
  Result.LinetypeHandle := Ref(ALinetypeHandle);
end;

function NewBlockHeader(AHandle: TDWGHandle;
  const AName: string): TDWGBlockHeader;
begin
  Result := TDWGBlockHeader.Create;
  Result.Handle := AHandle;
  Result.BlockName := AName;
  Result.OwnerHandle := Ref(DWG_SYNTHETIC_BLOCK_RECORD_TABLE_HANDLE);
end;

function NewLine(AHandle, AOwnerHandle, ALayerHandle,
  ALinetypeHandle: TDWGHandle): TDWGLine;
begin
  Result := TDWGLine.Create;
  Result.Handle := AHandle;
  Result.OwnerHandle := Ref(AOwnerHandle);
  Result.LayerHandle := Ref(ALayerHandle);
  Result.LinetypeHandle := Ref(ALinetypeHandle);
end;

function NewText(AHandle, AOwnerHandle, ALayerHandle, ALinetypeHandle,
  AStyleHandle: TDWGHandle): TDWGText;
begin
  Result := TDWGText.Create;
  Result.Handle := AHandle;
  Result.OwnerHandle := Ref(AOwnerHandle);
  Result.LayerHandle := Ref(ALayerHandle);
  Result.LinetypeHandle := Ref(ALinetypeHandle);
  Result.StyleHandle := Ref(AStyleHandle);
end;

procedure TFPDWGResolverTest.ResolvesOwnerLayerAndLinetypeReferences;
var
  Doc: TDWGDocument;
  Resolver: TDWGResolver;
  Linetype: TDWGLinetype;
  Layer: TDWGLayer;
  BlockHeader: TDWGBlockHeader;
  Line: TDWGLine;
  Obj: TDWGObject;
begin
  Doc := TDWGDocument.Create(lmTolerant, nil);
  try
    Doc.RegisterSyntheticTables;
    Linetype := NewLinetype($20, 'Continuous');
    Layer := NewLayer($10, $20, 'Walls');
    BlockHeader := NewBlockHeader($30, '*Model_Space');
    Line := NewLine($40, $30, $10, $20);

    Doc.AddObject(Linetype);
    Doc.AddObject(Layer);
    Doc.AddObject(BlockHeader);
    Doc.AddObject(Line);

    Resolver := TDWGResolver.Create(Doc.Registry, nil);
    try
      Resolver.ResolveAll;
    finally
      Resolver.Free;
    end;

    AssertTrue(Doc.Registry.TryGet(DWG_SYNTHETIC_LAYER_TABLE_HANDLE, Obj));
    AssertTrue(Layer.Owner = Obj);
    AssertTrue(Layer.Linetype = Linetype);
    AssertEquals(Ord(osResolved), Ord(Layer.Status));

    AssertTrue(Doc.Registry.TryGet(DWG_SYNTHETIC_LTYPE_TABLE_HANDLE, Obj));
    AssertTrue(Linetype.Owner = Obj);
    AssertEquals(Ord(osResolved), Ord(Linetype.Status));

    AssertTrue(Doc.Registry.TryGet(DWG_SYNTHETIC_BLOCK_RECORD_TABLE_HANDLE,
      Obj));
    AssertTrue(BlockHeader.Owner = Obj);
    AssertEquals(Ord(osResolved), Ord(BlockHeader.Status));

    AssertTrue(Line.Owner = BlockHeader);
    AssertTrue(Line.Layer = Layer);
    AssertTrue(Line.Linetype = Linetype);
    AssertEquals(Ord(osResolved), Ord(Line.Status));
  finally
    Doc.Free;
  end;
end;

procedure TFPDWGResolverTest.ResolvesTextStyleReference;
var
  Doc: TDWGDocument;
  Resolver: TDWGResolver;
  Linetype: TDWGLinetype;
  Layer: TDWGLayer;
  BlockHeader: TDWGBlockHeader;
  Style: TDWGObject;
  Text: TDWGText;
begin
  Doc := TDWGDocument.Create(lmTolerant, nil);
  try
    Doc.RegisterSyntheticTables;
    Linetype := NewLinetype($20, 'Continuous');
    Layer := NewLayer($10, $20, 'Walls');
    BlockHeader := NewBlockHeader($30, '*Model_Space');
    Style := TDWGObject.Create;
    Style.Handle := $50;
    Style.DomainType := dotStyle;
    Style.Status := osResolved;
    Text := NewText($49, $30, $10, $20, $50);

    Doc.AddObject(Linetype);
    Doc.AddObject(Layer);
    Doc.AddObject(BlockHeader);
    Doc.AddObject(Style);
    Doc.AddObject(Text);

    Resolver := TDWGResolver.Create(Doc.Registry, nil);
    try
      Resolver.ResolveAll;
    finally
      Resolver.Free;
    end;

    AssertTrue(Text.Owner = BlockHeader);
    AssertTrue(Text.Layer = Layer);
    AssertTrue(Text.Linetype = Linetype);
    AssertTrue(Text.Style = Style);
    AssertEquals(Ord(osResolved), Ord(Text.Status));
  finally
    Doc.Free;
  end;
end;

procedure TFPDWGResolverTest.BrokenRequiredReferencesMarkObjectBrokenAndWarn;
var
  Doc: TDWGDocument;
  Logger: IDWGLogger;
  MemoryLogger: TDWGMemoryLogger;
  Resolver: TDWGResolver;
  Line: TDWGLine;
begin
  MemoryLogger := TDWGMemoryLogger.Create;
  Logger := MemoryLogger;
  Doc := TDWGDocument.Create(lmTolerant, Logger);
  try
    Line := NewLine($40, $30, $10, $20);
    Doc.AddObject(Line);

    Resolver := TDWGResolver.Create(Doc.Registry, Logger);
    try
      Resolver.ResolveAll;
    finally
      Resolver.Free;
    end;

    AssertEquals(Ord(osBroken), Ord(Line.Status));
    AssertTrue(Line.Owner = nil);
    AssertTrue(Line.Layer = nil);
    AssertTrue(Line.Linetype = nil);
    AssertEquals(3, MemoryLogger.ErrorCount);
    AssertEquals(1301, MemoryLogger.GetError(0).Code);
  finally
    Doc.Free;
    Logger := nil;
  end;
end;

procedure TFPDWGResolverTest.ResolvesEntityPrevNextReferences;
var
  Doc: TDWGDocument;
  Resolver: TDWGResolver;
  Linetype: TDWGLinetype;
  Layer: TDWGLayer;
  BlockHeader: TDWGBlockHeader;
  FirstLine, SecondLine: TDWGLine;
begin
  Doc := TDWGDocument.Create(lmTolerant, nil);
  try
    Doc.RegisterSyntheticTables;
    Linetype := NewLinetype($20, 'Continuous');
    Layer := NewLayer($10, $20, 'Walls');
    BlockHeader := NewBlockHeader($30, '*Model_Space');
    FirstLine := NewLine($40, $30, $10, $20);
    SecondLine := NewLine($41, $30, $10, $20);
    BlockHeader.FirstEntityHandle := Ref($40);
    BlockHeader.LastEntityHandle := Ref($41);
    FirstLine.NextEntityHandle := Ref($41);
    SecondLine.PrevEntityHandle := Ref($40);

    Doc.AddObject(Linetype);
    Doc.AddObject(Layer);
    Doc.AddObject(BlockHeader);
    Doc.AddObject(FirstLine);
    Doc.AddObject(SecondLine);

    Resolver := TDWGResolver.Create(Doc.Registry, nil);
    try
      Resolver.ResolveAll;
    finally
      Resolver.Free;
    end;

    AssertTrue(BlockHeader.FirstEntity = FirstLine);
    AssertTrue(BlockHeader.LastEntity = SecondLine);
    AssertTrue(FirstLine.NextEntity = SecondLine);
    AssertTrue(SecondLine.PrevEntity = FirstLine);
    AssertEquals(Ord(osResolved), Ord(FirstLine.Status));
    AssertEquals(Ord(osResolved), Ord(SecondLine.Status));
  finally
    Doc.Free;
  end;
end;

begin
  RegisterTests([TFPDWGResolverTest]);
end.
