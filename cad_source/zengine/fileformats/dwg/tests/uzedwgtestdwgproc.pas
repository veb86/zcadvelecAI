unit uzedwgtestdwgproc;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TFPDWGProcHandleTest = class(TTestCase)
  published
    procedure NilRefReturnsFalse;
    procedure EmptyRefReturnsFalse;
    procedure AbsoluteRefWinsOverHandleref;
    procedure HandlerefIsFallbackWhenAbsoluteRefIsZero;
    procedure ObjectHandleValueReadsRawHandle;
    procedure ObjectOwnerHandleEntityReadsOwnerRef;
    procedure ObjectOwnerHandleObjectReadsOwnerRef;
    procedure ObjectOwnerHandleNilTioReturnsFalse;
    // Issue #1118: entmode-aware owner resolution. When LibreDWG sets
    // entmode=1 or 2 the ownerhandle is null and the implicit owner is
    // paper/model space. The helper must follow Dwg_Data^.pspace_block /
    // mspace_block to recover the BLOCK_HEADER handle.
    procedure ObjectOwnerHandleEntmodeMSpaceReturnsMSpaceBlockHandle;
    procedure ObjectOwnerHandleEntmodePSpaceReturnsPSpaceBlockHandle;
    procedure ObjectOwnerHandleEntmodeFallsBackWhenBlockMissing;
    procedure ObjectOwnerHandleEntmodeExplicitReadsOwnerHandle;
    // Issue #1120: testdwg2007.dwg has entmode=2 LINE entities whose
    // mspace_block pointer is nil after decode, but header_vars.BLOCK_RECORD_
    // MSPACE and/or block_control.model_space carry a usable handle ref.
    // The helper must consult those fields before falling through to
    // ownerhandle (which is null on entmode=1/2 entities).
    procedure ObjectOwnerHandleEntmodeMSpaceUsesHeaderVarsWhenBlockMissing;
    procedure ObjectOwnerHandleEntmodePSpaceUsesHeaderVarsWhenBlockMissing;
    procedure ObjectOwnerHandleEntmodeMSpaceUsesBlockControlWhenHeaderVarsEmpty;
    procedure ObjectOwnerHandleEntmodePSpaceUsesBlockControlWhenHeaderVarsEmpty;
    procedure ObjectOwnerHandleEntmodeMSpaceFallsBackWhenAllPathsEmpty;
    procedure EntityLineTypeFlagsMapInlineKinds;
    procedure EntityLineTypeFlag0IgnoresHandleInR2000;
    procedure EntityLineTypePreR2000ReadsHandleWhenNotByLayer;
    procedure EntityLineTypeFlag3ReadsExplicitHandle;
    procedure EntityCommonPropsCopiesVisualFields;
    procedure EntityCommonPropsNormalizeByLayerColorAndLineWeight;
    procedure LayerVisualPropsPositiveColorKeepsLayerOn;
    procedure LayerVisualPropsByLayerMethodKeepsRawACI;
    procedure LayerVisualPropsRawACIBeatsDecodedWhiteFallback;
    procedure LayerVisualPropsTruecolorPackedACIBeatsByLayerFallback;
    procedure LayerVisualPropsNegativeColorTurnsLayerOff;
    procedure HeaderCurrentLayerHandleReadsCLAYER;
    procedure HeaderCurrentTextStyleHandleReadsTEXTSTYLE;
  end;

  TFPDWGProcTextStyleTest = class(TTestCase)
  published
    procedure StylePropsCopiesNameFontAndMetrics;
    procedure StylePropsDefaultsMissingWidthFactor;
  end;

  TFPDWGProcLineTest = class(TTestCase)
  published
    procedure CopyLineEndpointsCopiesAllAxes;
    procedure CopyLineEndpointsKeepsStartZSeparateFromStartX;
  end;

  { Stage 5 (TZ §12.5) scalar-copy regression tests. They use FillChar fakes for
    the LibreDWG record so they run without libredwg.so and without dragging the
    ZCAD entity graph in. Each test asserts that the dwgproc helper drops
    the right axis / radius / angle / string into the corresponding TDWG*Props
    field — the same payload the production mapper later writes onto the ZCAD
    entity. }
  TFPDWGProcCircleTest = class(TTestCase)
  published
    procedure CopyCircleCopiesCenterAndRadius;
    procedure CopyCirclePreservesThickness;
  end;

  TFPDWGProcArcTest = class(TTestCase)
  published
    procedure CopyArcCopiesCenterRadiusAngles;
    procedure CopyArcKeepsAnglesIndependent;
  end;

  TFPDWGProcPointTest = class(TTestCase)
  published
    procedure CopyPointCopiesAllAxes;
    procedure CopyPointPreservesXAngle;
  end;

  TFPDWGProcTextTest = class(TTestCase)
  published
    procedure CopyTextCopiesGeometry;
    procedure CopyTextPreservesAlignmentFlags;
  end;

  TFPDWGProcMTextTest = class(TTestCase)
  published
    procedure CopyMTextCopiesGeometry;
    procedure CopyMTextPreservesLineSpacing;
  end;

  TFPDWGProcLWPolylineTest = class(TTestCase)
  published
    procedure CopyLWPolylineClosedFlagFromBit512;
    procedure CopyLWPolylineCopiesPoints;
    procedure CopyLWPolylineMismatchedBulgesAreIgnored;
    procedure CopyLWPolylineExplicitWidthsOverrideConstWidth;
    procedure CopyLWPolylineWidthRecordCountMatchesVertices;
    procedure CopyLWPolylineEmptyPolylineProducesEmptyArrays;
  end;

implementation

uses
  dwg,
  dwgproc;

procedure TFPDWGProcHandleTest.NilRefReturnsFalse;
var
  Value: QWord;
begin
  AssertFalse(DWGRefHandleValue(nil, Value));
  AssertEquals(Int64(0), Int64(Value));
end;

procedure TFPDWGProcHandleTest.EmptyRefReturnsFalse;
var
  RawRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(RawRef, SizeOf(RawRef), 0);

  AssertFalse(DWGRefHandleValue(@RawRef, Value));
  AssertEquals(Int64(0), Int64(Value));
end;

procedure TFPDWGProcHandleTest.AbsoluteRefWinsOverHandleref;
var
  RawRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(RawRef, SizeOf(RawRef), 0);
  RawRef.absolute_ref := $2A;
  RawRef.handleref.value := $33;

  AssertTrue(DWGRefHandleValue(@RawRef, Value));
  AssertEquals(Int64($2A), Int64(Value));
end;

procedure TFPDWGProcHandleTest.HandlerefIsFallbackWhenAbsoluteRefIsZero;
var
  RawRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(RawRef, SizeOf(RawRef), 0);
  RawRef.absolute_ref := 0;
  RawRef.handleref.value := $33;

  AssertTrue(DWGRefHandleValue(@RawRef, Value));
  AssertEquals(Int64($33), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectHandleValueReadsRawHandle;
var
  Obj: Dwg_Object;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  Obj.handle.value := $ABCDEF;

  AssertEquals(Int64($ABCDEF), Int64(DWGObjectHandleValue(Obj)));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntityReadsOwnerRef;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  OwnerRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(OwnerRef, SizeOf(OwnerRef), 0);
  OwnerRef.absolute_ref := $40;
  Ent.ownerhandle := @OwnerRef;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($40), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleObjectReadsOwnerRef;
var
  Obj: Dwg_Object;
  Inner: Dwg_Object_Object;
  OwnerRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Inner, SizeOf(Inner), 0);
  FillChar(OwnerRef, SizeOf(OwnerRef), 0);
  OwnerRef.absolute_ref := 0;
  OwnerRef.handleref.value := $51;
  Inner.ownerhandle := @OwnerRef;
  Obj.supertype := DWG_SUPERTYPE_OBJECT;
  Obj.tio.&object := @Inner;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($51), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleNilTioReturnsFalse;
var
  Obj: Dwg_Object;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := nil;

  AssertFalse(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64(0), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntmodeMSpaceReturnsMSpaceBlockHandle;
var
  Obj, MSpace: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Dwg: Dwg_Data;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(MSpace, SizeOf(MSpace), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Dwg, SizeOf(Dwg), 0);
  MSpace.handle.value := $1F;
  Dwg.mspace_block := @MSpace;
  Ent.entmode := 2; // MSPACE implicit owner
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.parent := @Dwg;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($1F), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntmodePSpaceReturnsPSpaceBlockHandle;
var
  Obj, PSpace: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Dwg: Dwg_Data;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(PSpace, SizeOf(PSpace), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Dwg, SizeOf(Dwg), 0);
  PSpace.handle.value := $2E;
  Dwg.pspace_block := @PSpace;
  Ent.entmode := 1; // PSPACE implicit owner
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.parent := @Dwg;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($2E), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntmodeFallsBackWhenBlockMissing;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Dwg: Dwg_Data;
  OwnerRef: Dwg_Object_Ref;
  Value: QWord;
begin
  // entmode signals MSPACE but mspace_block is nil (e.g., parsed without
  // layouts). The helper must fall back to ownerhandle so a present
  // legacy ref still wins instead of returning False.
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Dwg, SizeOf(Dwg), 0);
  FillChar(OwnerRef, SizeOf(OwnerRef), 0);
  Dwg.mspace_block := nil;
  OwnerRef.absolute_ref := $77;
  Ent.ownerhandle := @OwnerRef;
  Ent.entmode := 2;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.parent := @Dwg;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($77), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntmodeExplicitReadsOwnerHandle;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  OwnerRef: Dwg_Object_Ref;
  Value: QWord;
begin
  // entmode=3 means an explicit ownerhandle is present; the implicit-owner
  // branch must not interfere even if parent is nil.
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(OwnerRef, SizeOf(OwnerRef), 0);
  OwnerRef.absolute_ref := $88;
  Ent.ownerhandle := @OwnerRef;
  Ent.entmode := 3;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($88), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntmodeMSpaceUsesHeaderVarsWhenBlockMissing;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Dwg: Dwg_Data;
  HeaderRef: Dwg_Object_Ref;
  Value: QWord;
begin
  // Issue #1120: mspace_block stays nil on some decode paths but
  // header_vars.BLOCK_RECORD_MSPACE carries the handle. The helper must
  // resolve the owner from there instead of falling through to the null
  // ownerhandle.
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Dwg, SizeOf(Dwg), 0);
  FillChar(HeaderRef, SizeOf(HeaderRef), 0);
  Dwg.mspace_block := nil;
  HeaderRef.absolute_ref := $A1;
  Dwg.header_vars.BLOCK_RECORD_MSPACE := @HeaderRef;
  Ent.entmode := 2;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.parent := @Dwg;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($A1), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntmodePSpaceUsesHeaderVarsWhenBlockMissing;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Dwg: Dwg_Data;
  HeaderRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Dwg, SizeOf(Dwg), 0);
  FillChar(HeaderRef, SizeOf(HeaderRef), 0);
  Dwg.pspace_block := nil;
  HeaderRef.absolute_ref := $B2;
  Dwg.header_vars.BLOCK_RECORD_PSPACE := @HeaderRef;
  Ent.entmode := 1;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.parent := @Dwg;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($B2), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntmodeMSpaceUsesBlockControlWhenHeaderVarsEmpty;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Dwg: Dwg_Data;
  ControlRef: Dwg_Object_Ref;
  Value: QWord;
begin
  // Path C: both mspace_block and header_vars.BLOCK_RECORD_MSPACE are empty,
  // but block_control.model_space holds the handle. Mirrors the libredwg
  // dwg_model_space_object() helper's last fallback.
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Dwg, SizeOf(Dwg), 0);
  FillChar(ControlRef, SizeOf(ControlRef), 0);
  Dwg.mspace_block := nil;
  Dwg.header_vars.BLOCK_RECORD_MSPACE := nil;
  ControlRef.absolute_ref := $C3;
  Dwg.block_control.model_space := @ControlRef;
  Ent.entmode := 2;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.parent := @Dwg;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($C3), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntmodePSpaceUsesBlockControlWhenHeaderVarsEmpty;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Dwg: Dwg_Data;
  ControlRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Dwg, SizeOf(Dwg), 0);
  FillChar(ControlRef, SizeOf(ControlRef), 0);
  Dwg.pspace_block := nil;
  Dwg.header_vars.BLOCK_RECORD_PSPACE := nil;
  ControlRef.absolute_ref := $D4;
  Dwg.block_control.paper_space := @ControlRef;
  Ent.entmode := 1;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.parent := @Dwg;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($D4), Int64(Value));
end;

procedure TFPDWGProcHandleTest.ObjectOwnerHandleEntmodeMSpaceFallsBackWhenAllPathsEmpty;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Dwg: Dwg_Data;
  OwnerRef: Dwg_Object_Ref;
  Value: QWord;
begin
  // All three implicit-owner paths empty: helper falls back to ownerhandle.
  // The existing FallsBackWhenBlockMissing test only seeds ownerhandle and
  // mspace_block; this case additionally seeds header_vars / block_control
  // empty so the assertion holds regardless of which paths exist.
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Dwg, SizeOf(Dwg), 0);
  FillChar(OwnerRef, SizeOf(OwnerRef), 0);
  Dwg.mspace_block := nil;
  Dwg.header_vars.BLOCK_RECORD_MSPACE := nil;
  Dwg.block_control.model_space := nil;
  OwnerRef.absolute_ref := $E5;
  Ent.ownerhandle := @OwnerRef;
  Ent.entmode := 2;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.parent := @Dwg;

  AssertTrue(DWGObjectOwnerHandleValue(Obj, Value));
  AssertEquals(Int64($E5), Int64(Value));
end;

procedure TFPDWGProcHandleTest.EntityLineTypeFlagsMapInlineKinds;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Kind: TDWGEntityLineTypeKind;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;

  Ent.ltype_flags := 0;
  AssertTrue('flag=0 must map to inline ByLayer',
    DWGEntityLineTypeRefValue(Obj, Kind, Value));
  AssertEquals('flag=0 kind', Ord(dltByLayer), Ord(Kind));
  AssertEquals('flag=0 handle', Int64(0), Int64(Value));

  Ent.ltype_flags := 1;
  AssertTrue('flag=1 must map to inline ByBlock',
    DWGEntityLineTypeRefValue(Obj, Kind, Value));
  AssertEquals('flag=1 kind', Ord(dltByBlock), Ord(Kind));
  AssertEquals('flag=1 handle', Int64(0), Int64(Value));

  Ent.ltype_flags := 2;
  AssertTrue('flag=2 must map to inline Continuous',
    DWGEntityLineTypeRefValue(Obj, Kind, Value));
  AssertEquals('flag=2 kind', Ord(dltContinuous), Ord(Kind));
  AssertEquals('flag=2 handle', Int64(0), Int64(Value));
end;

procedure TFPDWGProcHandleTest.EntityLineTypeFlag0IgnoresHandleInR2000;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Dwg: Dwg_Data;
  LTypeRef: Dwg_Object_Ref;
  Kind: TDWGEntityLineTypeKind;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Dwg, SizeOf(Dwg), 0);
  FillChar(LTypeRef, SizeOf(LTypeRef), 0);
  Dwg.header.version := R_2007;
  LTypeRef.absolute_ref := $64;
  Ent.ltype := @LTypeRef;
  Ent.ltype_flags := 0;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.parent := @Dwg;

  AssertTrue(DWGEntityLineTypeRefValue(Obj, Kind, Value));
  AssertEquals('R2000+ flag=0 is always ByLayer',
    Ord(dltByLayer), Ord(Kind));
  AssertEquals('inline ByLayer has no handle', Int64(0), Int64(Value));
  AssertFalse('legacy handle helper must ignore inline ByLayer',
    DWGEntityLineTypeHandleValue(Obj, Value));
  AssertEquals('legacy helper clears inline handle', Int64(0), Int64(Value));
end;

procedure TFPDWGProcHandleTest.EntityLineTypePreR2000ReadsHandleWhenNotByLayer;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Dwg: Dwg_Data;
  LTypeRef: Dwg_Object_Ref;
  Kind: TDWGEntityLineTypeKind;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Dwg, SizeOf(Dwg), 0);
  FillChar(LTypeRef, SizeOf(LTypeRef), 0);
  Dwg.header.version := R_14;
  LTypeRef.absolute_ref := $64;
  Ent.isbylayerlt := 0;
  Ent.ltype := @LTypeRef;
  Ent.ltype_flags := 0;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.parent := @Dwg;

  AssertTrue(DWGEntityLineTypeRefValue(Obj, Kind, Value));
  AssertEquals('R13/R14 explicit ltype handle',
    Ord(dltHandle), Ord(Kind));
  AssertEquals('R13/R14 explicit handle', Int64($64), Int64(Value));
end;

procedure TFPDWGProcHandleTest.EntityLineTypeFlag3ReadsExplicitHandle;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  LTypeRef: Dwg_Object_Ref;
  Kind: TDWGEntityLineTypeKind;
  Value: QWord;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(LTypeRef, SizeOf(LTypeRef), 0);
  LTypeRef.absolute_ref := $64;
  Ent.ltype := @LTypeRef;
  Ent.ltype_flags := 3;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;

  AssertTrue(DWGEntityLineTypeRefValue(Obj, Kind, Value));
  AssertEquals('explicit kind', Ord(dltHandle), Ord(Kind));
  AssertEquals('explicit handle', Int64($64), Int64(Value));
  AssertTrue('legacy handle helper reads explicit handle',
    DWGEntityLineTypeHandleValue(Obj, Value));
  AssertEquals('legacy helper explicit handle', Int64($64), Int64(Value));
end;

procedure TFPDWGProcHandleTest.EntityCommonPropsCopiesVisualFields;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Props: TDWGEntityCommonProps;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  Ent.color.index := 5;
  Ent.color.method := DWG_COLOR_METHOD_ACI;
  Ent.linewt := 5;
  Ent.ltype_scale := 2.5;
  Ent.ltype_flags := 2;
  Ent.invisible := 1;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;

  AssertTrue(DWGEntityCommonPropsValue(Obj, Props));
  AssertEquals('ACI color copied', 5, Props.ColorIndex);
  AssertEquals('lineweight enum converted to DXF 370 value', 18, Props.LineWeight);
  AssertEquals('linetype scale copied', 2.5, Props.LineTypeScale, 0.0);
  AssertEquals('linetype flags copied', 2, Props.LineTypeFlags);
  AssertTrue('invisible flag copied', Props.Invisible);
end;

procedure TFPDWGProcHandleTest.EntityCommonPropsNormalizeByLayerColorAndLineWeight;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Props: TDWGEntityCommonProps;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  Ent.color.index := 9;
  Ent.color.method := DWG_COLOR_METHOD_BYLAYER;
  Ent.linewt := 29;
  Ent.ltype_scale := 0;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;

  AssertTrue(DWGEntityCommonPropsValue(Obj, Props));
  AssertEquals('ByLayer color normalized to DXF/ZCAD index', 256,
    Props.ColorIndex);
  AssertEquals('lineweight 29 is ByLayer', -1, Props.LineWeight);
  AssertEquals('empty linetype scale falls back to one', 1.0,
    Props.LineTypeScale, 0.0);
  AssertFalse('visible by default', Props.Invisible);
end;

procedure TFPDWGProcHandleTest.LayerVisualPropsPositiveColorKeepsLayerOn;
var
  Layer: Dwg_Object_LAYER;
  Props: TDWGLayerVisualProps;
begin
  FillChar(Layer, SizeOf(Layer), 0);
  Layer.color.index := 7;
  Layer.color.method := DWG_COLOR_METHOD_ACI;
  Layer.linewt := 31;
  Layer.off := 1;
  Layer.locked := 1;
  Layer.plotflag := 1;

  AssertTrue(DWGLayerVisualPropsValue(@Layer, Props));
  AssertEquals('ACI color copied', 7, Props.ColorIndex);
  AssertEquals('lineweight 31 is ByLwDefault', -3, Props.LineWeight);
  AssertTrue('positive color keeps layer visible despite raw off flag',
    Props.On);
  AssertTrue('locked copied', Props.Locked);
  AssertTrue('plot flag copied', Props.Plot);
end;

procedure TFPDWGProcHandleTest.LayerVisualPropsByLayerMethodKeepsRawACI;
var
  Layer: Dwg_Object_LAYER;
  Props: TDWGLayerVisualProps;
begin
  FillChar(Layer, SizeOf(Layer), 0);
  Layer.color.index := 4;
  Layer.color.raw := 95;
  Layer.color.method := DWG_COLOR_METHOD_BYLAYER;
  Layer.linewt := 7;

  AssertTrue(DWGLayerVisualPropsValue(@Layer, Props));
  AssertEquals('layer table BYLAYER method keeps decoded ACI index', 4,
    Props.ColorIndex);
  AssertEquals('lineweight 7 converts to DXF 370 value', 25,
    Props.LineWeight);
  AssertTrue('positive decoded ACI keeps layer visible', Props.On);
end;

procedure TFPDWGProcHandleTest.LayerVisualPropsRawACIBeatsDecodedWhiteFallback;
var
  Layer: Dwg_Object_LAYER;
  Props: TDWGLayerVisualProps;
begin
  FillChar(Layer, SizeOf(Layer), 0);
  Layer.color.index := 7;
  Layer.color.raw := 95;
  Layer.color.rgb := $C2FFFFFF;
  Layer.color.method := DWG_COLOR_METHOD_ACI;
  Layer.linewt := 29;

  AssertTrue(DWGLayerVisualPropsValue(@Layer, Props));
  AssertEquals('preserved raw CMC ACI beats decoded palette white', 95,
    Props.ColorIndex);
  AssertEquals('lineweight 29 is ByLayer', -1, Props.LineWeight);
  AssertTrue('raw ACI keeps layer visible', Props.On);
  AssertFalse('raw ACI is enough to avoid lost-color diagnostic',
    DWGColorLooksLikeLostACI(Layer.color));

  Layer.color.raw := 0;
  AssertTrue('ACI white without raw index is diagnostic-only suspicious',
    DWGColorLooksLikeLostACI(Layer.color));
end;

procedure TFPDWGProcHandleTest.LayerVisualPropsTruecolorPackedACIBeatsByLayerFallback;
var
  Layer: Dwg_Object_LAYER;
  Props: TDWGLayerVisualProps;
begin
  FillChar(Layer, SizeOf(Layer), 0);
  Layer.color.index := 256;
  Layer.color.raw := 0;
  Layer.color.rgb := $C300005F;
  Layer.color.method := DWG_COLOR_METHOD_TRUECOLOR;
  Layer.linewt := 29;

  AssertTrue(DWGLayerVisualPropsValue(@Layer, Props));
  AssertEquals('packed TRUECOLOR CMC ACI beats decoded ByLayer fallback', 95,
    Props.ColorIndex);
  AssertEquals('lineweight 29 is ByLayer', -1, Props.LineWeight);
  AssertTrue('positive packed ACI keeps layer visible', Props.On);

  Layer.color.rgb := $C3000005;
  AssertTrue(DWGLayerVisualPropsValue(@Layer, Props));
  AssertEquals('packed TRUECOLOR CMC keeps blue ACI', 5, Props.ColorIndex);
end;

procedure TFPDWGProcHandleTest.LayerVisualPropsNegativeColorTurnsLayerOff;
var
  Layer: Dwg_Object_LAYER;
  Props: TDWGLayerVisualProps;
begin
  FillChar(Layer, SizeOf(Layer), 0);
  Layer.color.index := -3;
  Layer.color.method := DWG_COLOR_METHOD_ACI;
  Layer.linewt := 29;
  Layer.off := 0;

  AssertTrue(DWGLayerVisualPropsValue(@Layer, Props));
  AssertEquals('negative ACI color normalized for display', 3,
    Props.ColorIndex);
  AssertEquals('lineweight 29 is ByLayer', -1, Props.LineWeight);
  AssertFalse('negative color means layer is off', Props.On);
end;

procedure TFPDWGProcHandleTest.HeaderCurrentLayerHandleReadsCLAYER;
var
  Raw: Dwg_Data;
  LayerRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  FillChar(LayerRef, SizeOf(LayerRef), 0);
  LayerRef.absolute_ref := $10;
  Raw.header_vars.CLAYER := @LayerRef;

  AssertTrue(DWGHeaderCurrentLayerHandleValue(Raw, Value));
  AssertEquals(Int64($10), Int64(Value));

  Raw.header_vars.CLAYER := nil;
  AssertFalse(DWGHeaderCurrentLayerHandleValue(Raw, Value));
  AssertEquals(Int64(0), Int64(Value));
end;

procedure TFPDWGProcHandleTest.HeaderCurrentTextStyleHandleReadsTEXTSTYLE;
var
  Raw: Dwg_Data;
  StyleRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  FillChar(StyleRef, SizeOf(StyleRef), 0);
  StyleRef.absolute_ref := $35;
  Raw.header_vars.TEXTSTYLE := @StyleRef;

  AssertTrue(DWGHeaderCurrentTextStyleHandleValue(Raw, Value));
  AssertEquals(Int64($35), Int64(Value));

  Raw.header_vars.TEXTSTYLE := nil;
  AssertFalse(DWGHeaderCurrentTextStyleHandleValue(Raw, Value));
  AssertEquals(Int64(0), Int64(Value));
end;

procedure TFPDWGProcTextStyleTest.StylePropsCopiesNameFontAndMetrics;
var
  Style: Dwg_Object_STYLE;
  Props: TDWGTextStyleProps;
  NameText, FontText, BigFontText: AnsiString;
begin
  FillChar(Style, SizeOf(Style), 0);
  NameText := 'style2Text';
  FontText := 'arial.ttf';
  BigFontText := 'bigfont.shx';
  Style.name := PChar(NameText);
  Style.font_file := PChar(FontText);
  Style.bigfont_file := PChar(BigFontText);
  Style.text_size := 2.5;
  Style.width_factor := 0.75;
  Style.oblique_angle := 0.125;
  Style.is_shape := 0;
  Style.is_vertical := 1;

  AssertTrue(DWGTextStylePropsValue(@Style, R_2004, Props));
  AssertEquals(NameText, Props.Name);
  AssertEquals(FontText, Props.FontFile);
  AssertEquals(BigFontText, Props.BigFontFile);
  AssertEquals(2.5, Props.TextSize, 0.0);
  AssertEquals(0.75, Props.WidthFactor, 0.0);
  AssertEquals(0.125, Props.ObliqueAngle, 0.0);
  AssertFalse(Props.IsShape);
  AssertTrue(Props.IsVertical);
end;

procedure TFPDWGProcTextStyleTest.StylePropsDefaultsMissingWidthFactor;
var
  Style: Dwg_Object_STYLE;
  Props: TDWGTextStyleProps;
begin
  FillChar(Style, SizeOf(Style), 0);
  Style.width_factor := 0;

  AssertTrue(DWGTextStylePropsValue(@Style, R_2004, Props));
  AssertEquals(1.0, Props.WidthFactor, 0.0);
end;

procedure TFPDWGProcLineTest.CopyLineEndpointsCopiesAllAxes;
var
  Line: Dwg_Entity_LINE;
  Endpoints: TDWGLineEndpoints;
begin
  FillChar(Line, SizeOf(Line), 0);
  Line.start.x := 1.0;
  Line.start.y := 2.0;
  Line.start.z := 3.0;
  Line.end_.x := 4.0;
  Line.end_.y := 5.0;
  Line.end_.z := 6.0;

  DWGCopyLineEndpoints(Line, Endpoints);

  AssertEquals('start.x', 1.0, Endpoints.StartX, 0.0);
  AssertEquals('start.y', 2.0, Endpoints.StartY, 0.0);
  AssertEquals('start.z', 3.0, Endpoints.StartZ, 0.0);
  AssertEquals('end.x', 4.0, Endpoints.EndX, 0.0);
  AssertEquals('end.y', 5.0, Endpoints.EndY, 0.0);
  AssertEquals('end.z', 6.0, Endpoints.EndZ, 0.0);
end;

procedure TFPDWGProcLineTest.CopyLineEndpointsKeepsStartZSeparateFromStartX;
var
  Line: Dwg_Entity_LINE;
  Endpoints: TDWGLineEndpoints;
begin
  // Regression: previously start.z and end_.z were copied from start.x/end_.x,
  // collapsing the line onto an angled plane. Verify Z is independent of X.
  FillChar(Line, SizeOf(Line), 0);
  Line.start.x := 10.0;
  Line.start.y := 0.0;
  Line.start.z := 99.0;
  Line.end_.x := 20.0;
  Line.end_.y := 0.0;
  Line.end_.z := 77.0;

  DWGCopyLineEndpoints(Line, Endpoints);

  AssertEquals('start.x preserved', 10.0, Endpoints.StartX, 0.0);
  AssertEquals('start.z must come from raw .z, not .x', 99.0, Endpoints.StartZ, 0.0);
  AssertEquals('end.x preserved', 20.0, Endpoints.EndX, 0.0);
  AssertEquals('end.z must come from raw .z, not .x', 77.0, Endpoints.EndZ, 0.0);
end;

{ ---------- TFPDWGProcCircleTest ---------- }

procedure TFPDWGProcCircleTest.CopyCircleCopiesCenterAndRadius;
var
  Circle: Dwg_Entity_CIRCLE;
  Props: TDWGCircleProps;
begin
  FillChar(Circle, SizeOf(Circle), 0);
  Circle.center.x := 1.5;
  Circle.center.y := 2.5;
  Circle.center.z := 3.5;
  Circle.radius := 7.25;
  DWGCopyCircleProps(Circle, Props);
  AssertEquals('center.x', 1.5, Props.CenterX, 0.0);
  AssertEquals('center.y', 2.5, Props.CenterY, 0.0);
  AssertEquals('center.z', 3.5, Props.CenterZ, 0.0);
  AssertEquals('radius',   7.25, Props.Radius, 0.0);
end;

procedure TFPDWGProcCircleTest.CopyCirclePreservesThickness;
var
  Circle: Dwg_Entity_CIRCLE;
  Props: TDWGCircleProps;
begin
  FillChar(Circle, SizeOf(Circle), 0);
  Circle.thickness := 0.75;
  DWGCopyCircleProps(Circle, Props);
  AssertEquals('thickness', 0.75, Props.Thickness, 0.0);
end;

{ ---------- TFPDWGProcArcTest ---------- }

procedure TFPDWGProcArcTest.CopyArcCopiesCenterRadiusAngles;
var
  Arc: Dwg_Entity_ARC;
  Props: TDWGArcProps;
begin
  FillChar(Arc, SizeOf(Arc), 0);
  Arc.center.x := 10.0;
  Arc.center.y := 20.0;
  Arc.center.z := 30.0;
  Arc.radius := 4.0;
  Arc.start_angle := 0.5;
  Arc.end_angle := 1.5;
  DWGCopyArcProps(Arc, Props);
  AssertEquals('center.x', 10.0, Props.CenterX, 0.0);
  AssertEquals('center.y', 20.0, Props.CenterY, 0.0);
  AssertEquals('center.z', 30.0, Props.CenterZ, 0.0);
  AssertEquals('radius',    4.0, Props.Radius, 0.0);
  AssertEquals('start',     0.5, Props.StartAngle, 0.0);
  AssertEquals('end',       1.5, Props.EndAngle, 0.0);
end;

procedure TFPDWGProcArcTest.CopyArcKeepsAnglesIndependent;
var
  Arc: Dwg_Entity_ARC;
  Props: TDWGArcProps;
begin
  // Regression guard: start_angle and end_angle must not be cross-wired to the
  // same source field.
  FillChar(Arc, SizeOf(Arc), 0);
  Arc.start_angle := 11.0;
  Arc.end_angle := 22.0;
  DWGCopyArcProps(Arc, Props);
  AssertEquals('start',  11.0, Props.StartAngle, 0.0);
  AssertEquals('end',    22.0, Props.EndAngle, 0.0);
end;

{ ---------- TFPDWGProcPointTest ---------- }

procedure TFPDWGProcPointTest.CopyPointCopiesAllAxes;
var
  Point: Dwg_Entity_POINT;
  Props: TDWGPointProps;
begin
  FillChar(Point, SizeOf(Point), 0);
  Point.x := 1.0;
  Point.y := 2.0;
  Point.z := 3.0;
  DWGCopyPointProps(Point, Props);
  AssertEquals('x', 1.0, Props.X, 0.0);
  AssertEquals('y', 2.0, Props.Y, 0.0);
  AssertEquals('z', 3.0, Props.Z, 0.0);
end;

procedure TFPDWGProcPointTest.CopyPointPreservesXAngle;
var
  Point: Dwg_Entity_POINT;
  Props: TDWGPointProps;
begin
  FillChar(Point, SizeOf(Point), 0);
  Point.thickness := 0.5;
  Point.x_ang := 1.234;
  DWGCopyPointProps(Point, Props);
  AssertEquals('thickness', 0.5, Props.Thickness, 0.0);
  AssertEquals('xangle',    1.234, Props.XAngle, 0.0);
end;

{ ---------- TFPDWGProcTextTest ---------- }

procedure TFPDWGProcTextTest.CopyTextCopiesGeometry;
var
  Text: Dwg_Entity_TEXT;
  Props: TDWGTextProps;
begin
  FillChar(Text, SizeOf(Text), 0);
  Text.ins_pt.x := 1.0;
  Text.ins_pt.y := 2.0;
  Text.elevation := 3.0;
  Text.height := 0.5;
  Text.rotation := 0.25;
  Text.oblique_angle := 0.1;
  Text.width_factor := 1.25;
  Text.text_value := nil;
  DWGCopyTextProps(Text, R_2004, Props);
  AssertEquals('insertX', 1.0, Props.InsertX, 0.0);
  AssertEquals('insertY', 2.0, Props.InsertY, 0.0);
  AssertEquals('insertZ', 3.0, Props.InsertZ, 0.0);
  AssertEquals('height',  0.5, Props.Height,  0.0);
  AssertEquals('rotation',0.25,Props.Rotation,0.0);
  AssertEquals('oblique', 0.1, Props.Oblique, 0.0);
  AssertEquals('wfactor', 1.25,Props.WidthFactor,0.0);
  AssertEquals('value empty for nil text_value', '', Props.Value);
end;

procedure TFPDWGProcTextTest.CopyTextPreservesAlignmentFlags;
var
  Text: Dwg_Entity_TEXT;
  Props: TDWGTextProps;
begin
  FillChar(Text, SizeOf(Text), 0);
  Text.alignment_pt.x := 9.0;
  Text.alignment_pt.y := 8.0;
  Text.generation := 2;
  Text.horiz_alignment := 3;
  Text.vert_alignment := 4;
  DWGCopyTextProps(Text, R_2004, Props);
  AssertEquals('alignX', 9.0, Props.AlignX, 0.0);
  AssertEquals('alignY', 8.0, Props.AlignY, 0.0);
  AssertEquals('generation', 2, Props.Generation);
  AssertEquals('horiz', 3, Props.HorizAlignment);
  AssertEquals('vert',  4, Props.VertAlignment);
end;

{ ---------- TFPDWGProcMTextTest ---------- }

procedure TFPDWGProcMTextTest.CopyMTextCopiesGeometry;
var
  MText: Dwg_Entity_MTEXT;
  Props: TDWGMTextProps;
begin
  FillChar(MText, SizeOf(MText), 0);
  MText.ins_pt.x := 1.0;
  MText.ins_pt.y := 2.0;
  MText.ins_pt.z := 3.0;
  MText.x_axis_dir.x := 0.5;
  MText.x_axis_dir.y := 0.5;
  MText.x_axis_dir.z := 0.0;
  MText.rect_width := 100.0;
  MText.rect_height := 50.0;
  MText.text_height := 2.5;
  MText.attachment := 1;
  MText.text := nil;
  DWGCopyMTextProps(MText, R_2004, Props);
  AssertEquals('insertX', 1.0, Props.InsertX, 0.0);
  AssertEquals('insertY', 2.0, Props.InsertY, 0.0);
  AssertEquals('insertZ', 3.0, Props.InsertZ, 0.0);
  AssertEquals('xaxisX',  0.5, Props.XAxisX, 0.0);
  AssertEquals('rectW',   100.0, Props.RectWidth, 0.0);
  AssertEquals('rectH',   50.0, Props.RectHeight, 0.0);
  AssertEquals('textH',   2.5, Props.TextHeight, 0.0);
  AssertEquals('attach',  1, Props.Attachment);
end;

procedure TFPDWGProcMTextTest.CopyMTextPreservesLineSpacing;
var
  MText: Dwg_Entity_MTEXT;
  Props: TDWGMTextProps;
begin
  FillChar(MText, SizeOf(MText), 0);
  MText.linespace_factor := 1.5;
  DWGCopyMTextProps(MText, R_2004, Props);
  AssertEquals('linespace', 1.5, Props.LineSpaceFactor, 0.0);
end;

{ ---------- TFPDWGProcLWPolylineTest ---------- }

procedure TFPDWGProcLWPolylineTest.CopyLWPolylineClosedFlagFromBit512;
var
  LWP: Dwg_Entity_LWPOLYLINE;
  Props: TDWGLWPolylineProps;
begin
  FillChar(LWP, SizeOf(LWP), 0);
  LWP.flag := 512;
  DWGCopyLWPolylineProps(LWP, Props);
  AssertTrue('flag=512 -> closed', Props.Closed);

  LWP.flag := 1; // some other flag bit set, 512 is not
  DWGCopyLWPolylineProps(LWP, Props);
  AssertFalse('flag=1 -> open', Props.Closed);
end;

procedure TFPDWGProcLWPolylineTest.CopyLWPolylineCopiesPoints;
var
  LWP: Dwg_Entity_LWPOLYLINE;
  Props: TDWGLWPolylineProps;
  Points: array[0..1] of BITCODE_2RD;
begin
  FillChar(LWP, SizeOf(LWP), 0);
  FillChar(Points, SizeOf(Points), 0);
  Points[0].x := 1.0; Points[0].y := 2.0;
  Points[1].x := 3.0; Points[1].y := 4.0;
  LWP.const_width := 0.25;
  LWP.elevation := 5.0;
  LWP.num_points := 2;
  LWP.points := @Points[0];
  DWGCopyLWPolylineProps(LWP, Props);
  AssertEquals('elevation', 5.0, Props.Elevation, 0.0);
  AssertEquals('count', 2, Length(Props.Vertices));
  AssertEquals('p0.x', 1.0, Props.Vertices[0].X, 0.0);
  AssertEquals('p0.y', 2.0, Props.Vertices[0].Y, 0.0);
  AssertEquals('p1.x', 3.0, Props.Vertices[1].X, 0.0);
  AssertEquals('p1.y', 4.0, Props.Vertices[1].Y, 0.0);
  AssertEquals('p0.startw fallback to const_width', 0.25,
    Props.Vertices[0].StartWidth, 0.0);
  AssertEquals('p0.endw fallback to const_width',   0.25,
    Props.Vertices[0].EndWidth, 0.0);
end;

procedure TFPDWGProcLWPolylineTest.CopyLWPolylineMismatchedBulgesAreIgnored;
var
  LWP: Dwg_Entity_LWPOLYLINE;
  Props: TDWGLWPolylineProps;
  Points: array[0..1] of BITCODE_2RD;
  Bulges: array[0..0] of BITCODE_BD; // mismatched count
begin
  FillChar(LWP, SizeOf(LWP), 0);
  FillChar(Points, SizeOf(Points), 0);
  Bulges[0] := 99.0;
  Points[0].x := 1.0; Points[0].y := 2.0;
  Points[1].x := 3.0; Points[1].y := 4.0;
  LWP.num_points := 2;
  LWP.points := @Points[0];
  // num_bulges=1 != num_points=2 -> bulges must be ignored
  LWP.num_bulges := 1;
  LWP.bulges := @Bulges[0];
  DWGCopyLWPolylineProps(LWP, Props);
  AssertEquals('count', 2, Length(Props.Vertices));
  AssertEquals('p0.bulge ignored due to mismatch', 0.0,
    Props.Vertices[0].Bulge, 0.0);
  AssertEquals('p1.bulge ignored due to mismatch', 0.0,
    Props.Vertices[1].Bulge, 0.0);
end;

procedure TFPDWGProcLWPolylineTest.CopyLWPolylineExplicitWidthsOverrideConstWidth;
var
  LWP: Dwg_Entity_LWPOLYLINE;
  Props: TDWGLWPolylineProps;
  Points: array[0..1] of BITCODE_2RD;
  Widths: array[0..1] of Dwg_LWPOLYLINE_width;
begin
  FillChar(LWP, SizeOf(LWP), 0);
  FillChar(Points, SizeOf(Points), 0);
  FillChar(Widths, SizeOf(Widths), 0);
  Points[0].x := 0.0; Points[0].y := 0.0;
  Points[1].x := 1.0; Points[1].y := 1.0;
  Widths[0].start := 0.10; Widths[0].end_ := 0.20;
  Widths[1].start := 0.30; Widths[1].end_ := 0.40;
  LWP.const_width := 5.0;
  LWP.num_points := 2;
  LWP.points := @Points[0];
  LWP.num_widths := 2;
  LWP.widths := @Widths[0];
  DWGCopyLWPolylineProps(LWP, Props);
  AssertEquals('p0.startw', 0.10, Props.Vertices[0].StartWidth, 0.0);
  AssertEquals('p0.endw',   0.20, Props.Vertices[0].EndWidth, 0.0);
  AssertEquals('p1.startw', 0.30, Props.Vertices[1].StartWidth, 0.0);
  AssertEquals('p1.endw',   0.40, Props.Vertices[1].EndWidth, 0.0);
end;

procedure TFPDWGProcLWPolylineTest.CopyLWPolylineWidthRecordCountMatchesVertices;
var
  LWP: Dwg_Entity_LWPOLYLINE;
  Props: TDWGLWPolylineProps;
  Points: array[0..2] of BITCODE_2RD;
begin
  FillChar(LWP, SizeOf(LWP), 0);
  FillChar(Points, SizeOf(Points), 0);
  LWP.num_points := 3;
  LWP.points := @Points[0];

  LWP.flag := 0;
  DWGCopyLWPolylineProps(LWP, Props);
  AssertFalse('open polyline', Props.Closed);
  AssertEquals('open width records', 3, DWGLWPolylineWidthRecordCount(Props));

  LWP.flag := 512;
  DWGCopyLWPolylineProps(LWP, Props);
  AssertTrue('closed polyline', Props.Closed);
  AssertEquals('closed width records', 3, DWGLWPolylineWidthRecordCount(Props));
end;

procedure TFPDWGProcLWPolylineTest.CopyLWPolylineEmptyPolylineProducesEmptyArrays;
var
  LWP: Dwg_Entity_LWPOLYLINE;
  Props: TDWGLWPolylineProps;
begin
  FillChar(LWP, SizeOf(LWP), 0);
  LWP.num_points := 0;
  DWGCopyLWPolylineProps(LWP, Props);
  AssertEquals('vertices empty', 0, Length(Props.Vertices));
  AssertFalse('open', Props.Closed);
end;

begin
  RegisterTests([
    TFPDWGProcHandleTest, TFPDWGProcTextStyleTest, TFPDWGProcLineTest,
    TFPDWGProcCircleTest, TFPDWGProcArcTest, TFPDWGProcPointTest,
    TFPDWGProcTextTest, TFPDWGProcMTextTest,
    TFPDWGProcLWPolylineTest
  ]);
end.
