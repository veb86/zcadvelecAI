unit fpdwg_test_factory;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGFactoryTest = class(TTestCase)
  published
    procedure DefaultFactoryRegistersStage4Mappers;
    procedure LayerMapperCopiesScalarFieldsAndKeepsReferencesUnresolved;
    procedure LinetypeMapperCopiesDescriptionAndPattern;
    procedure BlockHeaderMapperCopiesNamesAndEntityReferences;
    procedure LineMapperCopiesGeometryAndCommonEntityHandles;
    procedure CircleMapperCopiesGeometryAndCommonEntityHandles;
    procedure TextMapperCopiesTextGeometryAndCommonEntityHandles;
    procedure FilterByDomainTypeMaterializesAllowedTypesAndStubsOthers;
    procedure UnknownFallbackCopiesDiagnosticsRawBytesAndWarning;
  end;

implementation

uses
  SysUtils,
  Math,
  dwg,
  fpdwg_types,
  fpdwg_logger,
  fpdwg_libredwg_utils,
  fpdwg_filter,
  fpdwg_factory,
  fpdwg_model_base,
  fpdwg_model_blocks,
  fpdwg_model_entities,
  fpdwg_model_tables,
  fpdwg_model_unknown;

function TestContext(ALogger: IDWGLogger = nil): TDWGBuilderContext;
begin
  Result := TDWGBuilderContext.Default;
  Result.Version := dvR2007;
  Result.Codepage := 65001;
  Result.Logger := ALogger;
end;

procedure InitRawObject(var Raw: Dwg_Object; RawType: DWG_OBJECT_TYPE;
  Supertype: DWG_OBJECT_SUPERTYPE; Handle: TDWGHandle);
begin
  FillChar(Raw, SizeOf(Raw), 0);
  Raw.fixedtype := RawType;
  Raw.supertype := Supertype;
  Raw.handle.value := Handle;
end;

procedure InitAbsoluteRef(var RawRef: Dwg_Object_Ref; Handle: TDWGHandle);
begin
  FillChar(RawRef, SizeOf(RawRef), 0);
  RawRef.absolute_ref := Handle;
end;

procedure InitFallbackRef(var RawRef: Dwg_Object_Ref; Handle: TDWGHandle);
begin
  FillChar(RawRef, SizeOf(RawRef), 0);
  RawRef.handleref.value := Handle;
end;

procedure TFPDWGFactoryTest.DefaultFactoryRegistersStage4Mappers;
var
  Factory: TDWGObjectFactory;
  Mapper: IDWGObjectMapper;
begin
  Factory := TDWGObjectFactory.CreateDefault;
  try
    AssertTrue(Factory.TryGetMapper(DWG_TYPE_LAYER, Mapper));
    AssertTrue(Factory.TryGetMapper(DWG_TYPE_LTYPE, Mapper));
    AssertTrue(Factory.TryGetMapper(DWG_TYPE_BLOCK_HEADER, Mapper));
    AssertTrue(Factory.TryGetMapper(DWG_TYPE_LINE, Mapper));
    AssertTrue(Factory.TryGetMapper(DWG_TYPE_CIRCLE, Mapper));
    AssertTrue(Factory.TryGetMapper(DWG_TYPE_TEXT, Mapper));
    AssertEquals(6, Factory.MapperCount);
  finally
    Factory.Free;
  end;
end;

procedure TFPDWGFactoryTest.LayerMapperCopiesScalarFieldsAndKeepsReferencesUnresolved;
var
  Factory: TDWGObjectFactory;
  Raw: Dwg_Object;
  RawObject: Dwg_Object_Object;
  RawLayer: Dwg_Object_LAYER;
  LTypeRef: Dwg_Object_Ref;
  LayerName: AnsiString;
  Obj: TDWGObject;
  Layer: TDWGLayer;
begin
  Factory := TDWGObjectFactory.CreateDefault;
  try
    InitRawObject(Raw, DWG_TYPE_LAYER, DWG_SUPERTYPE_OBJECT, $10);
    FillChar(RawObject, SizeOf(RawObject), 0);
    FillChar(RawLayer, SizeOf(RawLayer), 0);
    InitAbsoluteRef(LTypeRef, $20);
    LayerName := 'Walls';

    Raw.tio.&object := @RawObject;
    RawObject.tio.LAYER := @RawLayer;
    RawLayer.name := PAnsiChar(LayerName);
    RawLayer.color.index := 7;
    RawLayer.linewt := 25;
    RawLayer.off := 0;
    RawLayer.locked := 1;
    RawLayer.plotflag := 1;
    RawLayer.ltype := @LTypeRef;

    Obj := Factory.CreateObject(Raw, TestContext);
    try
      AssertTrue(Obj is TDWGLayer);
      Layer := TDWGLayer(Obj);
      AssertEquals(Int64($10), Int64(Layer.Handle));
      AssertEquals(Ord(dotLayer), Ord(Layer.DomainType));
      AssertEquals('Walls', Layer.LayerName);
      AssertEquals(7, Layer.ColorIndex);
      AssertEquals(25, Layer.LineWeight);
      AssertFalse(Layer.Off);
      AssertTrue(Layer.Locked);
      AssertTrue(Layer.Plot);
      AssertEquals(Int64($20), Int64(Layer.LinetypeHandle.Value));
      AssertEquals(Ord(hsAbsoluteRef), Ord(Layer.LinetypeHandle.Source));
      AssertEquals(Int64(DWG_SYNTHETIC_LAYER_TABLE_HANDLE),
        Int64(Layer.OwnerHandle.Value));
      AssertTrue(Layer.Owner = nil);
      AssertTrue(Layer.Linetype = nil);
      AssertEquals(Ord(osRaw), Ord(Layer.Status));
    finally
      Obj.Free;
    end;
  finally
    Factory.Free;
  end;
end;

procedure TFPDWGFactoryTest.LinetypeMapperCopiesDescriptionAndPattern;
var
  Factory: TDWGObjectFactory;
  Raw: Dwg_Object;
  RawObject: Dwg_Object_Object;
  RawLType: Dwg_Object_LTYPE;
  NameText, DescriptionText: AnsiString;
  Obj: TDWGObject;
  Linetype: TDWGLinetype;
begin
  Factory := TDWGObjectFactory.CreateDefault;
  try
    InitRawObject(Raw, DWG_TYPE_LTYPE, DWG_SUPERTYPE_OBJECT, $21);
    FillChar(RawObject, SizeOf(RawObject), 0);
    FillChar(RawLType, SizeOf(RawLType), 0);
    NameText := 'Dashed';
    DescriptionText := 'Dashed line';

    Raw.tio.&object := @RawObject;
    RawObject.tio.LTYPE := @RawLType;
    RawLType.name := PAnsiChar(NameText);
    RawLType.description := PAnsiChar(DescriptionText);
    RawLType.pattern_len := 12.5;

    Obj := Factory.CreateObject(Raw, TestContext);
    try
      AssertTrue(Obj is TDWGLinetype);
      Linetype := TDWGLinetype(Obj);
      AssertEquals('Dashed', Linetype.LinetypeName);
      AssertEquals('Dashed line', Linetype.Description);
      AssertEquals(12.5, Linetype.PatternLength, 0.000001);
      AssertEquals(Int64(DWG_SYNTHETIC_LTYPE_TABLE_HANDLE),
        Int64(Linetype.OwnerHandle.Value));
      AssertEquals(Ord(osRaw), Ord(Linetype.Status));
    finally
      Obj.Free;
    end;
  finally
    Factory.Free;
  end;
end;

procedure TFPDWGFactoryTest.BlockHeaderMapperCopiesNamesAndEntityReferences;
var
  Factory: TDWGObjectFactory;
  Raw: Dwg_Object;
  RawObject: Dwg_Object_Object;
  RawBlock: Dwg_Object_BLOCK_HEADER;
  NameText: AnsiString;
  BlockRef, FirstRef, LastRef, EndRef, LayoutRef: Dwg_Object_Ref;
  Obj: TDWGObject;
  BlockHeader: TDWGBlockHeader;
begin
  Factory := TDWGObjectFactory.CreateDefault;
  try
    InitRawObject(Raw, DWG_TYPE_BLOCK_HEADER, DWG_SUPERTYPE_OBJECT, $30);
    FillChar(RawObject, SizeOf(RawObject), 0);
    FillChar(RawBlock, SizeOf(RawBlock), 0);
    InitAbsoluteRef(BlockRef, $31);
    InitAbsoluteRef(FirstRef, $32);
    InitAbsoluteRef(LastRef, $33);
    InitAbsoluteRef(EndRef, $34);
    InitAbsoluteRef(LayoutRef, $35);
    NameText := '*Model_Space';

    Raw.tio.&object := @RawObject;
    RawObject.tio.BLOCK_HEADER := @RawBlock;
    RawBlock.name := PAnsiChar(NameText);
    RawBlock.base_pt.x := 1.5;
    RawBlock.base_pt.y := 2.5;
    RawBlock.base_pt.z := 3.5;
    RawBlock.block_entity := @BlockRef;
    RawBlock.first_entity := @FirstRef;
    RawBlock.last_entity := @LastRef;
    RawBlock.endblk_entity := @EndRef;
    RawBlock.layout := @LayoutRef;

    Obj := Factory.CreateObject(Raw, TestContext);
    try
      AssertTrue(Obj is TDWGBlockHeader);
      BlockHeader := TDWGBlockHeader(Obj);
      AssertEquals('*Model_Space', BlockHeader.BlockName);
      AssertEquals(1.5, BlockHeader.BasePoint.X, 0.000001);
      AssertEquals(2.5, BlockHeader.BasePoint.Y, 0.000001);
      AssertEquals(3.5, BlockHeader.BasePoint.Z, 0.000001);
      AssertEquals(Int64($31), Int64(BlockHeader.BlockEntityHandle.Value));
      AssertEquals(Int64($32), Int64(BlockHeader.FirstEntityHandle.Value));
      AssertEquals(Int64($33), Int64(BlockHeader.LastEntityHandle.Value));
      AssertEquals(Int64($34), Int64(BlockHeader.EndBlockEntityHandle.Value));
      AssertEquals(Int64($35), Int64(BlockHeader.LayoutHandle.Value));
      AssertEquals(Int64(DWG_SYNTHETIC_BLOCK_RECORD_TABLE_HANDLE),
        Int64(BlockHeader.OwnerHandle.Value));
    finally
      Obj.Free;
    end;
  finally
    Factory.Free;
  end;
end;

procedure TFPDWGFactoryTest.LineMapperCopiesGeometryAndCommonEntityHandles;
var
  Factory: TDWGObjectFactory;
  Raw: Dwg_Object;
  RawEntity: Dwg_Object_Entity;
  RawLine: Dwg_Entity_LINE;
  OwnerRef, LayerRef, LTypeRef: Dwg_Object_Ref;
  Obj: TDWGObject;
  Line: TDWGLine;
begin
  Factory := TDWGObjectFactory.CreateDefault;
  try
    InitRawObject(Raw, DWG_TYPE_LINE, DWG_SUPERTYPE_ENTITY, $40);
    FillChar(RawEntity, SizeOf(RawEntity), 0);
    FillChar(RawLine, SizeOf(RawLine), 0);
    InitAbsoluteRef(OwnerRef, $41);
    InitAbsoluteRef(LayerRef, $42);
    InitFallbackRef(LTypeRef, $43);

    Raw.tio.entity := @RawEntity;
    RawEntity.tio.LINE := @RawLine;
    RawEntity.ownerhandle := @OwnerRef;
    RawEntity.layer := @LayerRef;
    RawEntity.ltype := @LTypeRef;
    RawEntity.color.index := 256;
    RawEntity.linewt := 18;
    RawEntity.invisible := 0;
    RawLine.start.x := 0.0;
    RawLine.start.y := 0.0;
    RawLine.start.z := 1.0;
    RawLine.end_.x := 3.0;
    RawLine.end_.y := 4.0;
    RawLine.end_.z := 13.0;

    Obj := Factory.CreateObject(Raw, TestContext);
    try
      AssertTrue(Obj is TDWGLine);
      Line := TDWGLine(Obj);
      AssertEquals(Int64($41), Int64(Line.OwnerHandle.Value));
      AssertEquals(Int64($42), Int64(Line.LayerHandle.Value));
      AssertEquals(Int64($43), Int64(Line.LinetypeHandle.Value));
      AssertEquals(Ord(hsHandleref), Ord(Line.LinetypeHandle.Source));
      AssertEquals(256, Line.ColorIndex);
      AssertEquals(18, Line.LineWeight);
      AssertTrue(Line.Visible);
      AssertEquals(13.0, Line.Length3D, 0.000001);
      AssertEquals(5.0, Line.LengthXY, 0.000001);
      AssertTrue(Line.Layer = nil);
      AssertTrue(Line.Linetype = nil);
      AssertEquals(Ord(osRaw), Ord(Line.Status));
    finally
      Obj.Free;
    end;
  finally
    Factory.Free;
  end;
end;

procedure TFPDWGFactoryTest.CircleMapperCopiesGeometryAndCommonEntityHandles;
var
  Factory: TDWGObjectFactory;
  Raw: Dwg_Object;
  RawEntity: Dwg_Object_Entity;
  RawCircle: Dwg_Entity_CIRCLE;
  OwnerRef, LayerRef, LTypeRef: Dwg_Object_Ref;
  Obj: TDWGObject;
  Circle: TDWGCircle;
begin
  Factory := TDWGObjectFactory.CreateDefault;
  try
    InitRawObject(Raw, DWG_TYPE_CIRCLE, DWG_SUPERTYPE_ENTITY, $45);
    FillChar(RawEntity, SizeOf(RawEntity), 0);
    FillChar(RawCircle, SizeOf(RawCircle), 0);
    InitAbsoluteRef(OwnerRef, $46);
    InitAbsoluteRef(LayerRef, $47);
    InitFallbackRef(LTypeRef, $48);

    Raw.tio.entity := @RawEntity;
    RawEntity.tio.CIRCLE := @RawCircle;
    RawEntity.ownerhandle := @OwnerRef;
    RawEntity.layer := @LayerRef;
    RawEntity.ltype := @LTypeRef;
    RawEntity.color.index := 3;
    RawEntity.linewt := 20;
    RawEntity.invisible := 1;
    RawCircle.center.x := 10.0;
    RawCircle.center.y := 20.0;
    RawCircle.center.z := 30.0;
    RawCircle.radius := 7.5;
    RawCircle.thickness := 2.25;
    RawCircle.extrusion.x := 0.0;
    RawCircle.extrusion.y := 0.0;
    RawCircle.extrusion.z := -1.0;

    Obj := Factory.CreateObject(Raw, TestContext);
    try
      AssertTrue(Obj is TDWGCircle);
      Circle := TDWGCircle(Obj);
      AssertEquals(Int64($46), Int64(Circle.OwnerHandle.Value));
      AssertEquals(Int64($47), Int64(Circle.LayerHandle.Value));
      AssertEquals(Int64($48), Int64(Circle.LinetypeHandle.Value));
      AssertEquals(Ord(hsHandleref), Ord(Circle.LinetypeHandle.Source));
      AssertEquals(3, Circle.ColorIndex);
      AssertEquals(20, Circle.LineWeight);
      AssertFalse(Circle.Visible);
      AssertEquals(10.0, Circle.Center.X, 0.000001);
      AssertEquals(20.0, Circle.Center.Y, 0.000001);
      AssertEquals(30.0, Circle.Center.Z, 0.000001);
      AssertEquals(7.5, Circle.Radius, 0.000001);
      AssertEquals(15.0, Circle.Diameter, 0.000001);
      AssertEquals(2.25, Circle.Thickness, 0.000001);
      AssertEquals(-1.0, Circle.Extrusion.Z, 0.000001);
      AssertTrue(Circle.Layer = nil);
      AssertTrue(Circle.Linetype = nil);
      AssertEquals(Ord(osRaw), Ord(Circle.Status));
    finally
      Obj.Free;
    end;
  finally
    Factory.Free;
  end;
end;

procedure TFPDWGFactoryTest.TextMapperCopiesTextGeometryAndCommonEntityHandles;
var
  Factory: TDWGObjectFactory;
  Raw: Dwg_Object;
  RawEntity: Dwg_Object_Entity;
  RawText: Dwg_Entity_TEXT;
  OwnerRef, LayerRef, LTypeRef, StyleRef: Dwg_Object_Ref;
  TextValue: AnsiString;
  Obj: TDWGObject;
  Text: TDWGText;
begin
  Factory := TDWGObjectFactory.CreateDefault;
  try
    InitRawObject(Raw, DWG_TYPE_TEXT, DWG_SUPERTYPE_ENTITY, $49);
    FillChar(RawEntity, SizeOf(RawEntity), 0);
    FillChar(RawText, SizeOf(RawText), 0);
    InitAbsoluteRef(OwnerRef, $4A);
    InitAbsoluteRef(LayerRef, $4B);
    InitFallbackRef(LTypeRef, $4C);
    InitAbsoluteRef(StyleRef, $4D);
    TextValue := 'Panel A1';

    Raw.tio.entity := @RawEntity;
    RawEntity.tio.TEXT := @RawText;
    RawEntity.ownerhandle := @OwnerRef;
    RawEntity.layer := @LayerRef;
    RawEntity.ltype := @LTypeRef;
    RawEntity.color.index := 2;
    RawEntity.linewt := 15;
    RawEntity.invisible := 0;
    RawText.elevation := 9.0;
    RawText.ins_pt.x := 1.0;
    RawText.ins_pt.y := 2.0;
    RawText.alignment_pt.x := 3.0;
    RawText.alignment_pt.y := 4.0;
    RawText.extrusion.x := 0.0;
    RawText.extrusion.y := 0.0;
    RawText.extrusion.z := 1.0;
    RawText.thickness := 0.25;
    RawText.oblique_angle := 0.5;
    RawText.rotation := 1.25;
    RawText.height := 2.5;
    RawText.width_factor := 0.85;
    RawText.text_value := PAnsiChar(TextValue);
    RawText.generation := 2;
    RawText.horiz_alignment := 1;
    RawText.vert_alignment := 3;
    RawText.style := @StyleRef;

    Obj := Factory.CreateObject(Raw, TestContext);
    try
      AssertTrue(Obj is TDWGText);
      Text := TDWGText(Obj);
      AssertEquals(Int64($4A), Int64(Text.OwnerHandle.Value));
      AssertEquals(Int64($4B), Int64(Text.LayerHandle.Value));
      AssertEquals(Int64($4C), Int64(Text.LinetypeHandle.Value));
      AssertEquals(Ord(hsHandleref), Ord(Text.LinetypeHandle.Source));
      AssertEquals(Int64($4D), Int64(Text.StyleHandle.Value));
      AssertEquals('Panel A1', Text.TextValue);
      AssertEquals(2, Text.ColorIndex);
      AssertEquals(15, Text.LineWeight);
      AssertTrue(Text.Visible);
      AssertEquals(1.0, Text.InsertPoint.X, 0.000001);
      AssertEquals(2.0, Text.InsertPoint.Y, 0.000001);
      AssertEquals(9.0, Text.InsertPoint.Z, 0.000001);
      AssertEquals(3.0, Text.AlignmentPoint.X, 0.000001);
      AssertEquals(4.0, Text.AlignmentPoint.Y, 0.000001);
      AssertEquals(9.0, Text.AlignmentPoint.Z, 0.000001);
      AssertEquals(0.25, Text.Thickness, 0.000001);
      AssertEquals(0.5, Text.ObliqueAngle, 0.000001);
      AssertEquals(1.25, Text.Rotation, 0.000001);
      AssertEquals(2.5, Text.Height, 0.000001);
      AssertEquals(0.85, Text.WidthFactor, 0.000001);
      AssertEquals(2, Text.Generation);
      AssertEquals(1, Text.HorizontalAlignment);
      AssertEquals(3, Text.VerticalAlignment);
      AssertTrue(Text.Layer = nil);
      AssertTrue(Text.Linetype = nil);
      AssertTrue(Text.Style = nil);
      AssertEquals(Ord(osRaw), Ord(Text.Status));
    finally
      Obj.Free;
    end;
  finally
    Factory.Free;
  end;
end;

procedure TFPDWGFactoryTest.FilterByDomainTypeMaterializesAllowedTypesAndStubsOthers;
var
  Factory: TDWGObjectFactory;
  Filter: TFilterStrategy;
  RawLayer, RawLine: Dwg_Object;
  RawObject: Dwg_Object_Object;
  RawLayerData: Dwg_Object_LAYER;
  RawEntity: Dwg_Object_Entity;
  RawLineData: Dwg_Entity_LINE;
  Obj: TDWGObject;
begin
  Factory := TDWGObjectFactory.CreateDefault;
  Filter := TFilterByDomainType.Create([dotLayer]);
  try
    InitRawObject(RawLayer, DWG_TYPE_LAYER, DWG_SUPERTYPE_OBJECT, $51);
    FillChar(RawObject, SizeOf(RawObject), 0);
    FillChar(RawLayerData, SizeOf(RawLayerData), 0);
    RawLayer.tio.&object := @RawObject;
    RawObject.tio.LAYER := @RawLayerData;

    Obj := Factory.CreateObject(RawLayer, TestContext, Filter);
    try
      AssertTrue(Obj is TDWGLayer);
    finally
      Obj.Free;
    end;

    InitRawObject(RawLine, DWG_TYPE_LINE, DWG_SUPERTYPE_ENTITY, $52);
    FillChar(RawEntity, SizeOf(RawEntity), 0);
    FillChar(RawLineData, SizeOf(RawLineData), 0);
    RawLine.tio.entity := @RawEntity;
    RawEntity.tio.LINE := @RawLineData;

    Obj := Factory.CreateObject(RawLine, TestContext, Filter);
    try
      AssertTrue(Obj is TDWGStubObject);
      AssertFalse(Obj is TDWGLine);
      AssertEquals(Int64($52), Int64(Obj.Handle));
      AssertEquals(Ord(dotLine), Ord(Obj.DomainType));
      AssertEquals(Ord(DWG_TYPE_LINE), Ord(Obj.RawObjectType));
      AssertEquals(Ord(osRaw), Ord(Obj.Status));
    finally
      Obj.Free;
    end;
  finally
    Filter.Free;
    Factory.Free;
  end;
end;

procedure TFPDWGFactoryTest.UnknownFallbackCopiesDiagnosticsRawBytesAndWarning;
var
  Factory: TDWGObjectFactory;
  Logger: IDWGLogger;
  MemoryLogger: TDWGMemoryLogger;
  Ctx: TDWGBuilderContext;
  Raw: Dwg_Object;
  UnknownBits: array[0..1] of Byte;
  UnknownRest: array[0..2] of Byte;
  Obj: TDWGObject;
  Unknown: TDWGUnknownObject;
begin
  MemoryLogger := TDWGMemoryLogger.Create;
  Logger := MemoryLogger;
  Factory := TDWGObjectFactory.CreateDefault;
  try
    InitRawObject(Raw, DWG_TYPE_ARC, DWG_SUPERTYPE_ENTITY, $60);
    Raw.size := 42;
    Raw.bitsize := 104;
    Raw.num_unknown_bits := 16;
    Raw.num_unknown_rest := 3;
    UnknownBits[0] := $AA;
    UnknownBits[1] := $BB;
    UnknownRest[0] := $CC;
    UnknownRest[1] := $DD;
    UnknownRest[2] := $EE;
    Raw.unknown_bits := PAnsiChar(@UnknownBits[0]);
    Raw.unknown_rest := PAnsiChar(@UnknownRest[0]);

    Ctx := TestContext(Logger);
    Ctx.DumpUnknown := True;

    Obj := Factory.CreateObject(Raw, Ctx);
    try
      AssertTrue(Obj is TDWGUnknownObject);
      Unknown := TDWGUnknownObject(Obj);
      AssertEquals(Ord(dotUnknown), Ord(Unknown.DomainType));
      AssertEquals(Ord(DWG_SUPERTYPE_ENTITY), Ord(Unknown.Supertype));
      AssertEquals(42, Integer(Unknown.Size));
      AssertEquals(104, Integer(Unknown.BitSize));
      AssertEquals(16, Integer(Unknown.UnknownBitsSize));
      AssertEquals(3, Integer(Unknown.UnknownRestSize));
      AssertTrue(Pos('mapper not registered', Unknown.Reason) > 0);
      AssertEquals(5, Length(Unknown.RawBytes));
      AssertEquals($AA, Integer(Unknown.RawBytes[0]));
      AssertEquals($BB, Integer(Unknown.RawBytes[1]));
      AssertEquals($CC, Integer(Unknown.RawBytes[2]));
      AssertEquals($DD, Integer(Unknown.RawBytes[3]));
      AssertEquals($EE, Integer(Unknown.RawBytes[4]));
      AssertEquals(1, MemoryLogger.ErrorCount);
      AssertEquals(1101, MemoryLogger.GetError(0).Code);
    finally
      Obj.Free;
    end;
  finally
    Factory.Free;
    Logger := nil;
  end;
end;

begin
  RegisterTests([TFPDWGFactoryTest]);
end.
