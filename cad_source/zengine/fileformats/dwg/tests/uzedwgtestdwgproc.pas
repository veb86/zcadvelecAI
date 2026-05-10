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
    procedure HeaderCurrentLineTypeHandleReadsCELTYPE;
    procedure HeaderCurrentTextStyleHandleReadsTEXTSTYLE;
    procedure HeaderCurrentDimStyleHandleReadsDIMSTYLE;
    procedure HeaderCurrentEntityPropsReadsDrawingDefaults;
    procedure HeaderCurrentEntityPropsDefaultsMissingScales;
    procedure HeaderViewPropsReadsModelSpaceView;
    procedure HeaderViewPropsMarksPaperSpaceWhenTilemodeOff;
    procedure HeaderViewPropsRejectsZeroViewSize;
    procedure VPortViewPropsCopiesCenterHeightAndWidth;
    procedure VPortViewPropsDerivesWidthFromAspectRatio;
  end;

  TFPDWGProcReadCodeTest = class(TTestCase)
  published
    procedure Issue1163ReadCode2368IsCritical;
    procedure NonCriticalReadCodeStaysLoadable;
  end;

  TFPDWGProcTextStyleTest = class(TTestCase)
  published
    procedure StylePropsCopiesNameFontAndMetrics;
    procedure StylePropsDecodesCP1251Name;
    procedure StylePropsKeepsR2010UnicodeName;
    procedure StylePropsDefaultsMissingWidthFactor;
  end;

  TFPDWGProcLinetypeTest = class(TTestCase)
  published
    procedure LinetypePropsCopiesDashPattern;
    procedure LinetypePropsKeepsR2010UnicodeText;
    procedure LinetypePropsCopiesR11DashPattern;
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
    procedure BITCODET2TextUsesHeaderCodepage;
    procedure BITCODET2TextKeepsR2010UnicodeTableName;
    procedure BITCODET2TextDecodesR2013SingleByteCP1251;
    procedure CopyTextCopiesGeometry;
    procedure CopyTextDecodesCP1251Value;
    procedure CopyTextPreservesAlignmentFlags;
    procedure TextEffectiveInsertUsesAlignmentPointForAlignedText;
    procedure TextEffectiveInsertKeepsInsertPointForDefaultText;
    procedure TextEffectiveInsertIgnoresDataFlagForDefaultLeftText;
    procedure TextJustifyMapsBaselineAndBottomRows;
    procedure TextJustifyKeepsCenterAndMiddleCenterDistinct;
  end;

  TFPDWGProcMTextTest = class(TTestCase)
  published
    procedure CopyMTextCopiesGeometry;
    procedure CopyMTextDecodesCP1251Value;
    procedure CopyMTextPreservesLineSpacing;
    procedure MTextAttachmentMapsBottomLeft;
    procedure MTextAttachmentUsesDefaultForInvalidValue;
  end;

  TFPDWGProcInsertTest = class(TTestCase)
  published
    procedure CopyInsertPreservesNegativeExtrusion;
    procedure CopyMInsertPreservesNegativeExtrusion;
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

  { Stage 7 (TZ §12.7): proxy graphics must be copied before dwg_free()
    releases LibreDWG memory. These tests stay in dwgproc so they exercise
    the raw payload copy without pulling ZCAD proxy entity units into the
    fpdwg unit test binary. }
  TFPDWGProcProxyTest = class(TTestCase)
  published
    procedure CopyProxyPayloadCopiesGraphicBytes;
    procedure CopyProxyPayloadNilPointerIsEmpty;
    procedure CopyProxyPayloadWithoutGraphicsIsEmpty;
    procedure CopyProxyPayloadRejectsCorruptMetadataBeforeDereferencingData;
    procedure CopyProxyPayloadFallsBackToPreviewWhenMetadataIsCorrupt;
    procedure CopyEntityPreviewProxyPayloadCopiesGraphicBytes;
    procedure CopyEntityPreviewProxyPayloadAcceptsUnsetProxyFlagWithProxyHeader;
    procedure CopyEntityPreviewProxyPayloadRejectsInvalidPreview;
  end;

  { Stage 8 (TZ §12.8): scalar-copy guards for the additional entity payloads
    before their mapper units allocate ZCAD entities. Keeping these tests in
    dwgproc means they run against fake LibreDWG records and do not need
    libredwg.so or a drawing graph. }
  TFPDWGProcStage8GeometryTest = class(TTestCase)
  published
    procedure Copy3DFaceCopiesCornersAndInvisibleFlags;
    procedure CopySolidPromotes2DCornersToElevationPlane;
    procedure CopyEllipseCopiesAxesAndAngles;
    procedure CopySplineCopiesKnotsControlAndFitPoints;
    procedure CopyHatchCopiesPolylineBoundary;
    procedure CopyHatchCopiesPatternDefLines;
    procedure CopyPolylineRefsCopiesOwnedVertexHandles;
  end;

implementation

uses
  SysUtils,
  dwg,
  dwgproc,
  uzedwgtext;

procedure TFPDWGProcHandleTest.NilRefReturnsFalse;
var
  Value: QWord;
begin
  AssertFalse(DWGRefHandleValue(nil, Value));
  AssertEquals(Int64(0), Int64(Value));
end;

procedure TFPDWGProcReadCodeTest.Issue1163ReadCode2368IsCritical;
var
  Code: Integer;
  Text: string;
begin
  Code := Ord(DWG_ERR_VALUEOUTOFBOUNDS) or
    Ord(DWG_ERR_SECTIONNOTFOUND) or
    Ord(DWG_ERR_INVALIDDWG);

  AssertEquals('issue log read code', 2368, Code);
  AssertTrue('critical bits must abort parsing',
    DWGReadCodeIsCritical(Code));
  Text := DWGReadCodeToText(Code);
  AssertTrue('diagnostic includes VALUEOUTOFBOUNDS',
    Pos('DWG_ERR_VALUEOUTOFBOUNDS', Text) > 0);
  AssertTrue('diagnostic includes SECTIONNOTFOUND',
    Pos('DWG_ERR_SECTIONNOTFOUND', Text) > 0);
  AssertTrue('diagnostic includes INVALIDDWG',
    Pos('DWG_ERR_INVALIDDWG', Text) > 0);
end;

procedure TFPDWGProcReadCodeTest.NonCriticalReadCodeStaysLoadable;
var
  Code: Integer;
begin
  Code := Ord(DWG_ERR_INVALIDHANDLE) or Ord(DWG_ERR_VALUEOUTOFBOUNDS);

  AssertFalse('warnings below DWG_ERR_CLASSESNOTFOUND are recoverable',
    DWGReadCodeIsCritical(Code));
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

procedure TFPDWGProcHandleTest.HeaderCurrentLineTypeHandleReadsCELTYPE;
var
  Raw: Dwg_Data;
  LineTypeRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  FillChar(LineTypeRef, SizeOf(LineTypeRef), 0);
  LineTypeRef.absolute_ref := $22;
  Raw.header_vars.CELTYPE := @LineTypeRef;

  AssertTrue(DWGHeaderCurrentLineTypeHandleValue(Raw, Value));
  AssertEquals(Int64($22), Int64(Value));

  Raw.header_vars.CELTYPE := nil;
  AssertFalse(DWGHeaderCurrentLineTypeHandleValue(Raw, Value));
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

procedure TFPDWGProcHandleTest.HeaderCurrentDimStyleHandleReadsDIMSTYLE;
var
  Raw: Dwg_Data;
  DimStyleRef: Dwg_Object_Ref;
  Value: QWord;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  FillChar(DimStyleRef, SizeOf(DimStyleRef), 0);
  DimStyleRef.absolute_ref := $45;
  Raw.header_vars.DIMSTYLE := @DimStyleRef;

  AssertTrue(DWGHeaderCurrentDimStyleHandleValue(Raw, Value));
  AssertEquals(Int64($45), Int64(Value));

  Raw.header_vars.DIMSTYLE := nil;
  AssertFalse(DWGHeaderCurrentDimStyleHandleValue(Raw, Value));
  AssertEquals(Int64(0), Int64(Value));
end;

procedure TFPDWGProcHandleTest.HeaderCurrentEntityPropsReadsDrawingDefaults;
var
  Raw: Dwg_Data;
  Props: TDWGHeaderCurrentEntityProps;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  Raw.header_vars.CECOLOR.index := 5;
  Raw.header_vars.CECOLOR.method := DWG_COLOR_METHOD_ACI;
  Raw.header_vars.CELWEIGHT := 7;
  Raw.header_vars.CELTSCALE := 2.5;
  Raw.header_vars.LTSCALE := 3.5;
  Raw.header_vars.LWDISPLAY := 1;

  AssertTrue(DWGHeaderCurrentEntityPropsValue(Raw, Props));
  AssertEquals(5, Props.ColorIndex);
  AssertEquals(25, Props.LineWeight);
  AssertEquals(2.5, Props.LineTypeScale, 0.0);
  AssertEquals(3.5, Props.GlobalLineTypeScale, 0.0);
  AssertTrue(Props.LineWeightDisplay);
end;

procedure TFPDWGProcHandleTest.HeaderCurrentEntityPropsDefaultsMissingScales;
var
  Raw: Dwg_Data;
  Props: TDWGHeaderCurrentEntityProps;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  Raw.header_vars.CECOLOR.index := 9;
  Raw.header_vars.CECOLOR.method := DWG_COLOR_METHOD_BYLAYER;
  Raw.header_vars.CELWEIGHT := 29;

  AssertTrue(DWGHeaderCurrentEntityPropsValue(Raw, Props));
  AssertEquals(256, Props.ColorIndex);
  AssertEquals(-1, Props.LineWeight);
  AssertEquals(1.0, Props.LineTypeScale, 0.0);
  AssertEquals(1.0, Props.GlobalLineTypeScale, 0.0);
  AssertFalse(Props.LineWeightDisplay);
end;

procedure TFPDWGProcHandleTest.HeaderViewPropsReadsModelSpaceView;
var
  Raw: Dwg_Data;
  Props: TDWGViewProps;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  Raw.header_vars.TILEMODE := 1;
  Raw.header_vars.VIEWCTR.x := 120.5;
  Raw.header_vars.VIEWCTR.y := -42.25;
  Raw.header_vars.VIEWSIZE := 640;

  AssertTrue(DWGHeaderViewPropsValue(Raw, Props));
  AssertEquals(120.5, Props.CenterX, 0.0);
  AssertEquals(-42.25, Props.CenterY, 0.0);
  AssertEquals(640.0, Props.Height, 0.0);
  AssertFalse(Props.HasWidth);
  AssertEquals(Ord(dvsModelSpace), Ord(Props.Space));
end;

procedure TFPDWGProcHandleTest.HeaderViewPropsMarksPaperSpaceWhenTilemodeOff;
var
  Raw: Dwg_Data;
  Props: TDWGViewProps;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  Raw.header_vars.TILEMODE := 0;
  Raw.header_vars.VIEWCTR.x := 11;
  Raw.header_vars.VIEWCTR.y := 22;
  Raw.header_vars.VIEWSIZE := 33;

  AssertTrue(DWGHeaderViewPropsValue(Raw, Props));
  AssertEquals(Ord(dvsPaperSpace), Ord(Props.Space));
end;

procedure TFPDWGProcHandleTest.HeaderViewPropsRejectsZeroViewSize;
var
  Raw: Dwg_Data;
  Props: TDWGViewProps;
begin
  FillChar(Raw, SizeOf(Raw), 0);
  Raw.header_vars.TILEMODE := 1;
  Raw.header_vars.VIEWSIZE := 0;

  AssertFalse(DWGHeaderViewPropsValue(Raw, Props));
  AssertEquals(0.0, Props.Height, 0.0);
  AssertEquals(Ord(dvsUnknown), Ord(Props.Space));
end;

procedure TFPDWGProcHandleTest.VPortViewPropsCopiesCenterHeightAndWidth;
var
  VPort: Dwg_Object_VPORT;
  Props: TDWGViewProps;
begin
  FillChar(VPort, SizeOf(VPort), 0);
  VPort.VIEWCTR.x := 12;
  VPort.VIEWCTR.y := 34;
  VPort.VIEWSIZE := 200;
  VPort.view_width := 500;
  VPort.aspect_ratio := 2.5;

  AssertTrue(DWGVPortViewPropsValue(@VPort, Props));
  AssertEquals(12.0, Props.CenterX, 0.0);
  AssertEquals(34.0, Props.CenterY, 0.0);
  AssertEquals(200.0, Props.Height, 0.0);
  AssertEquals(500.0, Props.Width, 0.0);
  AssertTrue(Props.HasWidth);
  AssertEquals(Ord(dvsModelSpace), Ord(Props.Space));
end;

procedure TFPDWGProcHandleTest.VPortViewPropsDerivesWidthFromAspectRatio;
var
  VPort: Dwg_Object_VPORT;
  Props: TDWGViewProps;
begin
  FillChar(VPort, SizeOf(VPort), 0);
  VPort.VIEWCTR.x := -5;
  VPort.VIEWCTR.y := 7;
  VPort.VIEWSIZE := 100;
  VPort.view_width := 0;
  VPort.aspect_ratio := 1.6;

  AssertTrue(DWGVPortViewPropsValue(@VPort, Props));
  AssertEquals(-5.0, Props.CenterX, 0.0);
  AssertEquals(7.0, Props.CenterY, 0.0);
  AssertEquals(100.0, Props.Height, 0.0);
  AssertEquals(160.0, Props.Width, 0.0);
  AssertTrue(Props.HasWidth);
  AssertEquals(Ord(dvsModelSpace), Ord(Props.Space));
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

procedure TFPDWGProcTextStyleTest.StylePropsDecodesCP1251Name;
var
  Style: Dwg_Object_STYLE;
  Props: TDWGTextStyleProps;
  NameText, Expected: AnsiString;
begin
  FillChar(Style, SizeOf(Style), 0);
  NameText := #$D1#$F2#$E8#$EB#$FC;
  Expected := #$D0#$A1#$D1#$82#$D0#$B8#$D0#$BB#$D1#$8C;
  Style.name := PChar(NameText);

  AssertTrue(DWGTextStylePropsValue(@Style, R_2004, 29, Props));
  AssertEquals(Expected, Props.Name);
end;

procedure TFPDWGProcTextStyleTest.StylePropsKeepsR2010UnicodeName;
var
  Style: Dwg_Object_STYLE;
  Props: TDWGTextStyleProps;
  NameText, FontText: UnicodeString;
  ExpectedName, ExpectedFont, StoredName: AnsiString;
begin
  FillChar(Style, SizeOf(Style), 0);
  ExpectedName := #$D1#$81#$D1#$82#$D0#$B8#$D0#$BB#$D1#$8C'1';
  ExpectedFont := #$D1#$88#$D1#$80#$D0#$B8#$D1#$84#$D1#$82'.shx';
  NameText := UTF8Decode(ExpectedName);
  FontText := UTF8Decode(ExpectedFont);
  Style.name := PAnsiChar(PUnicodeChar(NameText));
  Style.font_file := PAnsiChar(PUnicodeChar(FontText));

  AssertTrue(DWGTextStylePropsValue(@Style, R_2010, 29, Props));
  StoredName := DWGDecodedTextForZCAD(Props.Name);

  AssertEquals(ExpectedName, StoredName);
  AssertEquals(ExpectedFont, DWGDecodedTextForZCAD(Props.FontFile));
  AssertEquals('style name must not be folded to question marks',
    0, Pos('?', StoredName));
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

procedure TFPDWGProcLinetypeTest.LinetypePropsCopiesDashPattern;
var
  LType: Dwg_Object_LTYPE;
  Dashes: array[0..2] of Dwg_LTYPE_dash;
  NameText, DescriptionText: AnsiString;
  Props: TDWGLinetypeProps;
begin
  FillChar(LType, SizeOf(LType), 0);
  FillChar(Dashes, SizeOf(Dashes), 0);
  NameText := 'Dashed';
  DescriptionText := 'Dashed line';
  LType.name := PAnsiChar(NameText);
  LType.description := PAnsiChar(DescriptionText);
  LType.pattern_len := 0.75;
  LType.numdashes := 3;
  LType.dashes := @Dashes[0];
  Dashes[0].length := 0.5;
  Dashes[1].length := -0.25;
  Dashes[2].length := 0.0;

  AssertTrue(DWGLinetypePropsValue(@LType, R_2004, Props));
  AssertEquals('name', 'Dashed', Props.Name);
  AssertEquals('description', 'Dashed line', Props.Description);
  AssertEquals('pattern length', 0.75, Props.PatternLength, 0.0);
  AssertEquals('dash count', 3, Length(Props.Dashes));
  AssertEquals('first dash length', 0.5, Props.Dashes[0].Length, 0.0);
  AssertEquals('second dash length', -0.25, Props.Dashes[1].Length, 0.0);
  AssertEquals('point dash length', 0.0, Props.Dashes[2].Length, 0.0);
  AssertEquals('simple dash kind', Ord(dldDash), Ord(Props.Dashes[0].Kind));
end;

procedure TFPDWGProcLinetypeTest.LinetypePropsKeepsR2010UnicodeText;
var
  LType: Dwg_Object_LTYPE;
  Dashes: array[0..0] of Dwg_LTYPE_dash;
  NameText, DescriptionText, DashText: UnicodeString;
  ExpectedName, ExpectedDescription, ExpectedDashText: AnsiString;
  Props: TDWGLinetypeProps;
begin
  FillChar(LType, SizeOf(LType), 0);
  FillChar(Dashes, SizeOf(Dashes), 0);
  ExpectedName := #$D0#$9B#$D0#$B8#$D0#$BD#$D0#$B8#$D1#$8F'1';
  ExpectedDescription :=
    #$D0#$9E#$D0#$BF#$D0#$B8#$D1#$81#$D0#$B0#$D0#$BD#$D0#$B8#$D0#$B5;
  ExpectedDashText := #$D1#$82#$D0#$B5#$D0#$BA#$D1#$81#$D1#$82;
  NameText := UTF8Decode(ExpectedName);
  DescriptionText := UTF8Decode(ExpectedDescription);
  DashText := UTF8Decode(ExpectedDashText);
  LType.name := PAnsiChar(PUnicodeChar(NameText));
  LType.description := PAnsiChar(PUnicodeChar(DescriptionText));
  LType.numdashes := 1;
  LType.dashes := @Dashes[0];
  Dashes[0].shape_flag := 2;
  Dashes[0].text := PAnsiChar(PUnicodeChar(DashText));

  AssertTrue(DWGLinetypePropsValue(@LType, R_2010, 29, Props));

  AssertEquals(ExpectedName, DWGDecodedTextForZCAD(Props.Name));
  AssertEquals(ExpectedDescription, DWGDecodedTextForZCAD(Props.Description));
  AssertEquals('dash count', 1, Length(Props.Dashes));
  AssertEquals(ExpectedDashText, DWGDecodedTextForZCAD(Props.Dashes[0].Text));
  AssertEquals('linetype text must not be folded to question marks',
    0, Pos('?', DWGDecodedTextForZCAD(Props.Dashes[0].Text)));
end;

procedure TFPDWGProcLinetypeTest.LinetypePropsCopiesR11DashPattern;
var
  LType: Dwg_Object_LTYPE;
  Props: TDWGLinetypeProps;
begin
  FillChar(LType, SizeOf(LType), 0);
  LType.pattern_len := 1.25;
  LType.numdashes := 2;
  LType.dashes := nil;
  LType.dashes_r11[0] := 0.75;
  LType.dashes_r11[1] := -0.5;

  AssertTrue(DWGLinetypePropsValue(@LType, R_11, Props));
  AssertEquals('pattern length', 1.25, Props.PatternLength, 0.0);
  AssertEquals('r11 dash count', 2, Length(Props.Dashes));
  AssertEquals('first r11 dash length', 0.75, Props.Dashes[0].Length, 0.0);
  AssertEquals('second r11 dash length', -0.5, Props.Dashes[1].Length, 0.0);
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

procedure TFPDWGProcTextTest.BITCODET2TextUsesHeaderCodepage;
var
  RawDWG: Dwg_Data;
  DWGContext: TDWGCtx;
  RawText, Expected, Decoded: AnsiString;
begin
  FillChar(RawDWG, SizeOf(RawDWG), 0);
  RawDWG.header.version := R_2000;
  RawDWG.header.codepage := 29;
  DWGContext.CreateRec(RawDWG);
  RawText := #$CF#$F0#$E8#$E2#$E5#$F2;
  Expected := #$D0#$9F#$D1#$80#$D0#$B8#$D0#$B2#$D0#$B5#$D1#$82;

  BITCODE_T2Text(PChar(RawText), DWGContext, Decoded);

  AssertEquals(Expected, Decoded);
end;

procedure TFPDWGProcTextTest.BITCODET2TextKeepsR2010UnicodeTableName;
var
  RawDWG: Dwg_Data;
  DWGContext: TDWGCtx;
  RawText: UnicodeString;
  Expected, Decoded, Stored: AnsiString;
begin
  FillChar(RawDWG, SizeOf(RawDWG), 0);
  RawDWG.header.version := R_2010;
  RawDWG.header.codepage := 29;
  DWGContext.CreateRec(RawDWG);
  Expected := #$D0#$A1#$D0#$BB#$D0#$BE#$D0#$B9'1';
  RawText := UTF8Decode(Expected);

  // Issue #1164: R2010+ table mappers must store decoded UTF-16 text as-is.
  BITCODE_T2Text(PAnsiChar(PUnicodeChar(RawText)), DWGContext, Decoded);
  Stored := DWGDecodedTextForZCAD(Decoded);

  AssertEquals(Expected, Stored);
  AssertEquals('table name must not be folded to question marks',
    0, Pos('?', Stored));
end;

procedure TFPDWGProcTextTest.BITCODET2TextDecodesR2013SingleByteCP1251;
var
  RawDWG: Dwg_Data;
  DWGContext: TDWGCtx;
  RawText, Expected, Decoded: AnsiString;
begin
  FillChar(RawDWG, SizeOf(RawDWG), 0);
  RawDWG.header.version := R_2013;
  RawDWG.header.codepage := 29;
  DWGContext.CreateRec(RawDWG);
  RawText := #$CF#$F0#$EE#$EC#$EF#$F2;
  Expected := #$D0#$9F#$D1#$80#$D0#$BE#$D0#$BC#$D0#$BF#$D1#$82;

  BITCODE_T2Text(PChar(RawText), DWGContext, Decoded);

  AssertEquals(Expected, Decoded);
end;

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

procedure TFPDWGProcTextTest.CopyTextDecodesCP1251Value;
var
  Text: Dwg_Entity_TEXT;
  Props: TDWGTextProps;
  RawText, Expected: AnsiString;
begin
  FillChar(Text, SizeOf(Text), 0);
  RawText := #$D2#$E5#$EA#$F1#$F2;
  Expected := #$D0#$A2#$D0#$B5#$D0#$BA#$D1#$81#$D1#$82;
  Text.text_value := PChar(RawText);

  DWGCopyTextProps(Text, R_2004, 29, Props);

  AssertEquals(Expected, Props.Value);
end;

procedure TFPDWGProcTextTest.CopyTextPreservesAlignmentFlags;
var
  Text: Dwg_Entity_TEXT;
  Props: TDWGTextProps;
begin
  FillChar(Text, SizeOf(Text), 0);
  Text.dataflags := 2;
  Text.alignment_pt.x := 9.0;
  Text.alignment_pt.y := 8.0;
  Text.generation := 2;
  Text.horiz_alignment := 3;
  Text.vert_alignment := 4;
  DWGCopyTextProps(Text, R_2004, Props);
  AssertEquals('dataflags', 2, Props.DataFlags);
  AssertEquals('alignX', 9.0, Props.AlignX, 0.0);
  AssertEquals('alignY', 8.0, Props.AlignY, 0.0);
  AssertEquals('generation', 2, Props.Generation);
  AssertEquals('horiz', 3, Props.HorizAlignment);
  AssertEquals('vert',  4, Props.VertAlignment);
end;

procedure TFPDWGProcTextTest.TextEffectiveInsertUsesAlignmentPointForAlignedText;
var
  Props: TDWGTextProps;
  X, Y, Z: Double;
begin
  FillChar(Props, SizeOf(Props), 0);
  Props.InsertX := 1.0;
  Props.InsertY := 2.0;
  Props.InsertZ := 3.0;
  Props.AlignX := 9.0;
  Props.AlignY := 8.0;
  Props.HorizAlignment := 1;

  DWGTextEffectiveInsertPoint(Props, X, Y, Z);

  AssertEquals('effective insert X', 9.0, X, 0.0);
  AssertEquals('effective insert Y', 8.0, Y, 0.0);
  AssertEquals('effective insert Z', 3.0, Z, 0.0);
end;

procedure TFPDWGProcTextTest.TextEffectiveInsertKeepsInsertPointForDefaultText;
var
  Props: TDWGTextProps;
  X, Y, Z: Double;
begin
  FillChar(Props, SizeOf(Props), 0);
  Props.InsertX := 1.0;
  Props.InsertY := 2.0;
  Props.InsertZ := 3.0;
  Props.AlignX := 9.0;
  Props.AlignY := 8.0;

  DWGTextEffectiveInsertPoint(Props, X, Y, Z);

  AssertEquals('effective insert X', 1.0, X, 0.0);
  AssertEquals('effective insert Y', 2.0, Y, 0.0);
  AssertEquals('effective insert Z', 3.0, Z, 0.0);
end;

procedure TFPDWGProcTextTest.TextEffectiveInsertIgnoresDataFlagForDefaultLeftText;
var
  Props: TDWGTextProps;
  X, Y, Z: Double;
begin
  FillChar(Props, SizeOf(Props), 0);
  Props.DataFlags := 2;
  Props.InsertX := 11.0;
  Props.InsertY := 12.0;
  Props.InsertZ := 13.0;
  Props.AlignX := 0.0;
  Props.AlignY := 0.0;

  DWGTextEffectiveInsertPoint(Props, X, Y, Z);

  AssertEquals('left text must use geometry X', 11.0, X, 0.0);
  AssertEquals('left text must use geometry Y', 12.0, Y, 0.0);
  AssertEquals('left text keeps elevation', 13.0, Z, 0.0);
end;

procedure TFPDWGProcTextTest.TextJustifyMapsBaselineAndBottomRows;
begin
  AssertEquals('baseline left', Ord(dwtjLeft),
    Ord(DWGTextAlignmentToJustifyKind(0, 0)));
  AssertEquals('baseline right', Ord(dwtjRight),
    Ord(DWGTextAlignmentToJustifyKind(2, 0)));
  AssertEquals('bottom left', Ord(dwtjBottomLeft),
    Ord(DWGTextAlignmentToJustifyKind(0, 1)));
  AssertEquals('bottom right', Ord(dwtjBottomRight),
    Ord(DWGTextAlignmentToJustifyKind(2, 1)));
end;

procedure TFPDWGProcTextTest.TextJustifyKeepsCenterAndMiddleCenterDistinct;
begin
  AssertEquals('baseline center', Ord(dwtjCenter),
    Ord(DWGTextAlignmentToJustifyKind(1, 0)));
  AssertEquals('vertical middle center', Ord(dwtjMiddleCenter),
    Ord(DWGTextAlignmentToJustifyKind(1, 2)));
  AssertEquals('horizontal middle mode', Ord(dwtjMiddleCenter),
    Ord(DWGTextAlignmentToJustifyKind(4, 0)));
  AssertTrue('Center and MiddleCenter must be distinct',
    DWGTextAlignmentToJustifyKind(1, 0) <>
    DWGTextAlignmentToJustifyKind(1, 2));
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

procedure TFPDWGProcMTextTest.CopyMTextDecodesCP1251Value;
var
  MText: Dwg_Entity_MTEXT;
  Props: TDWGMTextProps;
  RawText, Expected: AnsiString;
begin
  FillChar(MText, SizeOf(MText), 0);
  RawText := #$CC#$F2#$E5#$EA#$F1#$F2;
  Expected := #$D0#$9C#$D1#$82#$D0#$B5#$D0#$BA#$D1#$81#$D1#$82;
  MText.text := PChar(RawText);

  DWGCopyMTextProps(MText, R_2004, 29, Props);

  AssertEquals(Expected, Props.Value);
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

procedure TFPDWGProcMTextTest.MTextAttachmentMapsBottomLeft;
begin
  AssertEquals('attachment 7', Ord(dwgmtjBottomLeft),
    Ord(DWGMTextAttachmentToJustify(7)));
end;

procedure TFPDWGProcMTextTest.MTextAttachmentUsesDefaultForInvalidValue;
begin
  AssertEquals('attachment 0 fallback', Ord(dwgmtjMiddleCenter),
    Ord(DWGMTextAttachmentToJustify(0, dwgmtjMiddleCenter)));
end;

{ ---------- TFPDWGProcInsertTest ---------- }

procedure TFPDWGProcInsertTest.CopyInsertPreservesNegativeExtrusion;
var
  Insert: Dwg_Entity_INSERT;
  Props: TDWGInsertProps;
begin
  FillChar(Insert, SizeOf(Insert), 0);
  Insert.ins_pt.x := 1.0;
  Insert.ins_pt.y := 2.0;
  Insert.ins_pt.z := 3.0;
  Insert.scale.x := 4.0;
  Insert.scale.y := 5.0;
  Insert.scale.z := 6.0;
  Insert.rotation := 0.25;
  Insert.extrusion.x := 0.0;
  Insert.extrusion.y := 0.0;
  Insert.extrusion.z := -1.0;

  DWGCopyInsertProps(Insert, Props);

  AssertEquals('insert x', 1.0, Props.InsertPoint.X, 0.0);
  AssertEquals('insert y', 2.0, Props.InsertPoint.Y, 0.0);
  AssertEquals('insert z', 3.0, Props.InsertPoint.Z, 0.0);
  AssertEquals('scale x', 4.0, Props.Scale.X, 0.0);
  AssertEquals('scale y', 5.0, Props.Scale.Y, 0.0);
  AssertEquals('scale z', 6.0, Props.Scale.Z, 0.0);
  AssertEquals('rotation', 0.25, Props.Rotation, 0.0);
  AssertEquals('normal x', 0.0, Props.Extrusion.X, 0.0);
  AssertEquals('normal y', 0.0, Props.Extrusion.Y, 0.0);
  AssertEquals('normal z', -1.0, Props.Extrusion.Z, 0.0);
end;

procedure TFPDWGProcInsertTest.CopyMInsertPreservesNegativeExtrusion;
var
  Insert: Dwg_Entity_MINSERT;
  Props: TDWGInsertProps;
begin
  FillChar(Insert, SizeOf(Insert), 0);
  Insert.ins_pt.x := -7.0;
  Insert.ins_pt.y := 8.0;
  Insert.ins_pt.z := 9.0;
  Insert.scale.x := 0.5;
  Insert.scale.y := 1.5;
  Insert.scale.z := 2.5;
  Insert.rotation := 1.25;
  Insert.extrusion.x := 0.0;
  Insert.extrusion.y := 0.0;
  Insert.extrusion.z := -1.0;

  DWGCopyInsertProps(Insert, Props);

  AssertEquals('insert x', -7.0, Props.InsertPoint.X, 0.0);
  AssertEquals('insert y', 8.0, Props.InsertPoint.Y, 0.0);
  AssertEquals('insert z', 9.0, Props.InsertPoint.Z, 0.0);
  AssertEquals('scale x', 0.5, Props.Scale.X, 0.0);
  AssertEquals('scale y', 1.5, Props.Scale.Y, 0.0);
  AssertEquals('scale z', 2.5, Props.Scale.Z, 0.0);
  AssertEquals('rotation', 1.25, Props.Rotation, 0.0);
  AssertEquals('normal x', 0.0, Props.Extrusion.X, 0.0);
  AssertEquals('normal y', 0.0, Props.Extrusion.Y, 0.0);
  AssertEquals('normal z', -1.0, Props.Extrusion.Z, 0.0);
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

procedure TFPDWGProcProxyTest.CopyProxyPayloadCopiesGraphicBytes;
var
  Proxy: Dwg_Entity_PROXY_ENTITY;
  Bytes: array[0..3] of BITCODE_RC;
  Payload: TDWGProxyEntityPayload;
begin
  FillChar(Proxy, SizeOf(Proxy), 0);
  Bytes[0] := $CA;
  Bytes[1] := $FE;
  Bytes[2] := $10;
  Bytes[3] := $92;
  Proxy.proxy_id := 498;
  Proxy.class_id := 77;
  Proxy.dwg_versions := 1021;
  Proxy.from_dxf := 0;
  Proxy.proxy_data_size := Length(Bytes);
  Proxy.proxy_data := @Bytes[0];
  Proxy.data_size := 12;

  DWGCopyProxyEntityPayload(@Proxy, Payload);

  AssertTrue(Payload.HasGraphic);
  AssertEquals(498, Payload.ProxyID);
  AssertEquals(77, Payload.ClassID);
  AssertEquals(1021, Payload.DWGVersions);
  AssertEquals(12, Payload.EntityDataSize);
  AssertEquals(4, Length(Payload.Graphic));
  AssertEquals(Integer($CA), Integer(Payload.Graphic[0]));
  AssertEquals(Integer($FE), Integer(Payload.Graphic[1]));
  AssertEquals(Integer($10), Integer(Payload.Graphic[2]));
  AssertEquals(Integer($92), Integer(Payload.Graphic[3]));

  Bytes[0] := 0;
  AssertEquals('payload owns a copy independent from LibreDWG memory',
    Integer($CA), Integer(Payload.Graphic[0]));
end;

procedure TFPDWGProcProxyTest.CopyProxyPayloadNilPointerIsEmpty;
var
  Payload: TDWGProxyEntityPayload;
begin
  DWGCopyProxyEntityPayload(nil, Payload);

  AssertFalse(Payload.HasGraphic);
  AssertEquals(0, Length(Payload.Graphic));
end;

procedure TFPDWGProcProxyTest.CopyProxyPayloadWithoutGraphicsIsEmpty;
var
  Proxy: Dwg_Entity_PROXY_ENTITY;
  Payload: TDWGProxyEntityPayload;
begin
  FillChar(Proxy, SizeOf(Proxy), 0);
  Proxy.proxy_id := 498;
  Proxy.class_id := 9;
  Proxy.proxy_data_size := 3;
  Proxy.proxy_data := nil;

  DWGCopyProxyEntityPayload(@Proxy, Payload);

  AssertFalse(Payload.HasGraphic);
  AssertEquals(498, Payload.ProxyID);
  AssertEquals(9, Payload.ClassID);
  AssertEquals(0, Length(Payload.Graphic));
end;

procedure TFPDWGProcProxyTest.CopyProxyPayloadRejectsCorruptMetadataBeforeDereferencingData;
type
  PBITCODE_RC = ^BITCODE_RC;
var
  Proxy: Dwg_Entity_PROXY_ENTITY;
  Payload: TDWGProxyEntityPayload;
begin
  FillChar(Proxy, SizeOf(Proxy), 0);
  Proxy.proxy_id := 515;
  Proxy.class_id := 11468833;
  Proxy.dwg_version := 6199;
  Proxy.from_dxf := 8;
  Proxy.proxy_data_size := 16;
  Proxy.proxy_data := PBITCODE_RC(PtrUInt(1));

  DWGCopyProxyEntityPayload(@Proxy, Payload);

  AssertFalse('corrupt metadata is not copyable',
    DWGProxyEntityPayloadLooksSane(@Proxy));
  AssertFalse('invalid source pointer must not be dereferenced',
    Payload.HasGraphic);
  AssertEquals(515, Payload.ProxyID);
  AssertEquals(11468833, Payload.ClassID);
  AssertEquals(8, Payload.FromDXF);
  AssertEquals(0, Length(Payload.Graphic));
end;

procedure TFPDWGProcProxyTest.CopyProxyPayloadFallsBackToPreviewWhenMetadataIsCorrupt;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Proxy: Dwg_Entity_PROXY_ENTITY;
  Bytes: array[0..11] of BITCODE_RC;
  Payload: TDWGProxyEntityPayload;
  UsedPreview: Boolean;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Proxy, SizeOf(Proxy), 0);
  FillChar(Bytes, SizeOf(Bytes), 0);

  Bytes[0] := Length(Bytes);
  Bytes[4] := 1;
  Ent.preview_exists := 1;
  Ent.preview_is_proxy := 0;
  Ent.preview_size := Length(Bytes);
  Ent.preview := BITCODE_TF(@Bytes[0]);
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;

  Proxy.proxy_id := 515;
  Proxy.class_id := 11468833;
  Proxy.dwg_version := 6199;
  Proxy.from_dxf := 8;
  Proxy.proxy_data_size := 655021840;
  Proxy.proxy_data := nil;

  AssertFalse('fixture models the corrupt LibreDWG metadata from issue #1170',
    DWGProxyEntityPayloadLooksSane(@Proxy));
  AssertTrue('valid entity preview is used as fallback',
    DWGCopyProxyEntityPayloadOrPreview(@Proxy, Obj, Payload, UsedPreview));

  AssertTrue('fallback source is reported', UsedPreview);
  AssertTrue(Payload.HasGraphic);
  AssertEquals(498, Payload.ProxyID);
  AssertEquals(12, Length(Payload.Graphic));
  AssertEquals(Integer(12), Integer(Payload.Graphic[0]));
  AssertEquals(Integer(1), Integer(Payload.Graphic[4]));
end;

procedure TFPDWGProcProxyTest.CopyEntityPreviewProxyPayloadCopiesGraphicBytes;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Klass: Dwg_Class;
  Bytes: array[0..3] of BITCODE_RC;
  Payload: TDWGProxyEntityPayload;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Klass, SizeOf(Klass), 0);
  Bytes[0] := $31;
  Bytes[1] := $AC;
  Bytes[2] := $10;
  Bytes[3] := $FE;
  Ent.preview_exists := 1;
  Ent.preview_is_proxy := 1;
  Ent.preview_size := Length(Bytes);
  Ent.preview := BITCODE_TF(@Bytes[0]);
  Klass.number := 601;
  Klass.dwg_version := 33;
  Klass.maint_version := 4;
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;
  Obj.klass := @Klass;

  AssertTrue(DWGCopyEntityPreviewProxyPayload(Obj, Payload));

  AssertTrue(Payload.HasGraphic);
  AssertEquals(498, Payload.ProxyID);
  AssertEquals(601, Payload.ClassID);
  AssertEquals(15, Payload.DWGVersions);
  AssertEquals(33, Payload.DWGVersion);
  AssertEquals(4, Payload.MaintVersion);
  AssertEquals(0, Payload.EntityDataSize);
  AssertEquals(4, Length(Payload.Graphic));
  AssertEquals(Integer($31), Integer(Payload.Graphic[0]));
  AssertEquals(Integer($AC), Integer(Payload.Graphic[1]));
  AssertEquals(Integer($10), Integer(Payload.Graphic[2]));
  AssertEquals(Integer($FE), Integer(Payload.Graphic[3]));

  Bytes[0] := 0;
  AssertEquals('payload owns a copy independent from LibreDWG memory',
    Integer($31), Integer(Payload.Graphic[0]));
end;

procedure TFPDWGProcProxyTest.CopyEntityPreviewProxyPayloadAcceptsUnsetProxyFlagWithProxyHeader;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Bytes: array[0..11] of BITCODE_RC;
  Payload: TDWGProxyEntityPayload;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  FillChar(Bytes, SizeOf(Bytes), 0);
  Bytes[0] := Length(Bytes);
  Bytes[4] := 1;
  Ent.preview_exists := 1;
  Ent.preview_is_proxy := 0;
  Ent.preview_size := Length(Bytes);
  Ent.preview := BITCODE_TF(@Bytes[0]);
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;

  AssertTrue('valid proxy graphic header is accepted even when LibreDWG leaves preview_is_proxy unset',
    DWGCopyEntityPreviewProxyPayload(Obj, Payload));

  AssertTrue(Payload.HasGraphic);
  AssertEquals(12, Length(Payload.Graphic));
  AssertEquals(Integer(12), Integer(Payload.Graphic[0]));
  AssertEquals(Integer(1), Integer(Payload.Graphic[4]));
end;

procedure TFPDWGProcProxyTest.CopyEntityPreviewProxyPayloadRejectsInvalidPreview;
var
  Obj: Dwg_Object;
  Ent: Dwg_Object_Entity;
  Bytes: array[0..1] of BITCODE_RC;
  Payload: TDWGProxyEntityPayload;
begin
  FillChar(Obj, SizeOf(Obj), 0);
  FillChar(Ent, SizeOf(Ent), 0);
  Bytes[0] := $01;
  Bytes[1] := $02;
  Ent.preview_exists := 1;
  Ent.preview_is_proxy := 0;
  Ent.preview_size := Length(Bytes);
  Ent.preview := BITCODE_TF(@Bytes[0]);
  Obj.supertype := DWG_SUPERTYPE_ENTITY;
  Obj.tio.entity := @Ent;

  AssertFalse('invalid non-proxy previews are ignored',
    DWGCopyEntityPreviewProxyPayload(Obj, Payload));
  AssertFalse(Payload.HasGraphic);
  AssertEquals(0, Length(Payload.Graphic));

  Ent.preview_is_proxy := 1;
  Ent.preview_exists := 0;
  AssertFalse('missing preview flag is ignored',
    DWGCopyEntityPreviewProxyPayload(Obj, Payload));
  AssertFalse(Payload.HasGraphic);
  AssertEquals(0, Length(Payload.Graphic));
end;

procedure TFPDWGProcStage8GeometryTest.Copy3DFaceCopiesCornersAndInvisibleFlags;
var
  Face: Dwg_Entity__3DFACE;
  Props: TDWG3DFaceProps;
begin
  FillChar(Face, SizeOf(Face), 0);
  Face.corner1.x := 1.0;  Face.corner1.y := 2.0;  Face.corner1.z := 3.0;
  Face.corner2.x := 4.0;  Face.corner2.y := 5.0;  Face.corner2.z := 6.0;
  Face.corner3.x := 7.0;  Face.corner3.y := 8.0;  Face.corner3.z := 9.0;
  Face.corner4.x := 10.0; Face.corner4.y := 11.0; Face.corner4.z := 12.0;
  Face.invis_flags := 3;

  DWGCopy3DFaceProps(Face, Props);

  AssertEquals('corner1.x', 1.0, Props.Corners[0].X, 0.0);
  AssertEquals('corner2.y', 5.0, Props.Corners[1].Y, 0.0);
  AssertEquals('corner3.z', 9.0, Props.Corners[2].Z, 0.0);
  AssertEquals('corner4.x', 10.0, Props.Corners[3].X, 0.0);
  AssertEquals('invisible flags', 3, Props.InvisibleFlags);
end;

procedure TFPDWGProcStage8GeometryTest.CopySolidPromotes2DCornersToElevationPlane;
var
  Solid: Dwg_Entity_SOLID;
  Props: TDWGSolidProps;
begin
  FillChar(Solid, SizeOf(Solid), 0);
  Solid.elevation := 7.5;
  Solid.corner1.x := 1.0; Solid.corner1.y := 2.0;
  Solid.corner2.x := 3.0; Solid.corner2.y := 4.0;
  Solid.corner3.x := 5.0; Solid.corner3.y := 6.0;
  Solid.corner4.x := 7.0; Solid.corner4.y := 8.0;
  Solid.extrusion.x := 0.0;
  Solid.extrusion.y := 0.0;
  Solid.extrusion.z := 1.0;

  DWGCopySolidProps(Solid, Props);

  AssertEquals('corner1.x', 1.0, Props.Corners[0].X, 0.0);
  AssertEquals('corner1.z from elevation', 7.5, Props.Corners[0].Z, 0.0);
  AssertEquals('corner4.y', 8.0, Props.Corners[3].Y, 0.0);
  AssertEquals('corner4.z from elevation', 7.5, Props.Corners[3].Z, 0.0);
  AssertEquals('extrusion.z', 1.0, Props.Extrusion.Z, 0.0);
end;

procedure TFPDWGProcStage8GeometryTest.CopyEllipseCopiesAxesAndAngles;
var
  Ellipse: Dwg_Entity_ELLIPSE;
  Props: TDWGEllipseProps;
begin
  FillChar(Ellipse, SizeOf(Ellipse), 0);
  Ellipse.center.x := 1.0;
  Ellipse.center.y := 2.0;
  Ellipse.center.z := 3.0;
  Ellipse.sm_axis.x := 4.0;
  Ellipse.sm_axis.y := 5.0;
  Ellipse.sm_axis.z := 6.0;
  Ellipse.extrusion.z := 1.0;
  Ellipse.axis_ratio := 0.5;
  Ellipse.start_angle := 0.25;
  Ellipse.end_angle := 2.75;

  DWGCopyEllipseProps(Ellipse, Props);

  AssertEquals('center.z', 3.0, Props.Center.Z, 0.0);
  AssertEquals('major axis y', 5.0, Props.MajorAxis.Y, 0.0);
  AssertEquals('normal z', 1.0, Props.Extrusion.Z, 0.0);
  AssertEquals('ratio', 0.5, Props.AxisRatio, 0.0);
  AssertEquals('start angle', 0.25, Props.StartAngle, 0.0);
  AssertEquals('end angle', 2.75, Props.EndAngle, 0.0);
end;

procedure TFPDWGProcStage8GeometryTest.CopySplineCopiesKnotsControlAndFitPoints;
var
  Spline: Dwg_Entity_SPLINE;
  Props: TDWGSplineProps;
  Knots: array[0..3] of BITCODE_BD;
  Control: array[0..1] of Dwg_SPLINE_control_point;
  Fits: array[0..1] of BITCODE_3DPOINT;
begin
  FillChar(Spline, SizeOf(Spline), 0);
  FillChar(Control, SizeOf(Control), 0);
  FillChar(Fits, SizeOf(Fits), 0);
  Knots[0] := 0.0;
  Knots[1] := 0.0;
  Knots[2] := 1.0;
  Knots[3] := 1.0;
  Control[0].x := 1.0; Control[0].y := 2.0; Control[0].z := 3.0; Control[0].w := 1.0;
  Control[1].x := 4.0; Control[1].y := 5.0; Control[1].z := 6.0; Control[1].w := 0.5;
  Fits[0].x := 7.0; Fits[0].y := 8.0; Fits[0].z := 9.0;
  Fits[1].x := 10.0; Fits[1].y := 11.0; Fits[1].z := 12.0;
  Spline.flag := 1 or 4;
  Spline.degree := 3;
  Spline.closed_b := 1;
  Spline.rational := 1;
  Spline.num_knots := Length(Knots);
  Spline.knots := @Knots[0];
  Spline.num_ctrl_pts := Length(Control);
  Spline.ctrl_pts := @Control[0];
  Spline.num_fit_pts := Length(Fits);
  Spline.fit_pts := @Fits[0];

  DWGCopySplineProps(Spline, Props);

  AssertTrue('closed', Props.Closed);
  AssertTrue('rational', Props.Rational);
  AssertEquals('degree', 3, Props.Degree);
  AssertEquals('knots count', 4, Length(Props.Knots));
  AssertEquals('last knot', 1.0, Props.Knots[3], 0.0);
  AssertEquals('control count', 2, Length(Props.ControlPoints));
  AssertEquals('control[1].w', 0.5, Props.ControlPoints[1].W, 0.0);
  AssertEquals('fit count', 2, Length(Props.FitPoints));
  AssertEquals('fit[1].z', 12.0, Props.FitPoints[1].Z, 0.0);
end;

procedure TFPDWGProcStage8GeometryTest.CopyHatchCopiesPolylineBoundary;
var
  Hatch: Dwg_Entity_HATCH;
  Props: TDWGHatchProps;
  Path: Dwg_HATCH_Path;
  Points: array[0..2] of Dwg_HATCH_PolylinePath;
  Name: AnsiString;
begin
  FillChar(Hatch, SizeOf(Hatch), 0);
  FillChar(Path, SizeOf(Path), 0);
  FillChar(Points, SizeOf(Points), 0);
  Name := 'ANSI31';
  Points[0].point.x := 1.0; Points[0].point.y := 2.0;
  Points[1].point.x := 3.0; Points[1].point.y := 4.0; Points[1].bulge := 0.25;
  Points[2].point.x := 5.0; Points[2].point.y := 6.0;
  Path.flag := 2;
  Path.closed := 1;
  Path.num_segs_or_paths := Length(Points);
  Path.polyline_paths := @Points[0];
  Hatch.name := PChar(Name);
  Hatch.elevation := 9.0;
  Hatch.extrusion.z := 1.0;
  Hatch.is_solid_fill := 0;
  Hatch.num_paths := 1;
  Hatch.paths := @Path;
  Hatch.style := 2;
  Hatch.angle := 0.125;
  Hatch.scale_spacing := 2.5;

  DWGCopyHatchProps(Hatch, R_2004, Props);

  AssertEquals('pattern name', Name, Props.PatternName);
  AssertEquals('elevation', 9.0, Props.Elevation, 0.0);
  AssertEquals('normal z', 1.0, Props.Extrusion.Z, 0.0);
  AssertEquals('path count', 1, Length(Props.Paths));
  AssertTrue('polyline boundary', Props.Paths[0].IsPolyline);
  AssertTrue('closed boundary', Props.Paths[0].Closed);
  AssertEquals('point count', 3, Length(Props.Paths[0].PolylinePoints));
  AssertEquals('point[1].bulge', 0.25,
    Props.Paths[0].PolylinePoints[1].Bulge, 0.0);
end;

procedure TFPDWGProcStage8GeometryTest.CopyHatchCopiesPatternDefLines;
var
  Hatch: Dwg_Entity_HATCH;
  Props: TDWGHatchProps;
  DefLines: array[0..1] of Dwg_HATCH_DefLine;
  Dashes: array[0..2] of BITCODE_BD;
begin
  FillChar(Hatch, SizeOf(Hatch), 0);
  FillChar(DefLines, SizeOf(DefLines), 0);
  Dashes[0] := 0.5;
  Dashes[1] := -0.25;
  Dashes[2] := 0.125;
  DefLines[0].angle := 0.7853981633974483;
  DefLines[0].pt0.x := 1.0; DefLines[0].pt0.y := 2.0;
  DefLines[0].offset.x := 3.0; DefLines[0].offset.y := 4.0;
  DefLines[0].num_dashes := Length(Dashes);
  DefLines[0].dashes := @Dashes[0];
  DefLines[1].angle := 1.5707963267948966;
  DefLines[1].pt0.x := 5.0; DefLines[1].pt0.y := 6.0;
  DefLines[1].offset.x := 0.0; DefLines[1].offset.y := 7.0;
  Hatch.is_solid_fill := 0;
  Hatch.num_deflines := Length(DefLines);
  Hatch.deflines := @DefLines[0];

  DWGCopyHatchProps(Hatch, R_2004, Props);

  AssertEquals('pattern line count', 2, Length(Props.PatternLines));
  AssertEquals('line[0] angle', 0.7853981633974483,
    Props.PatternLines[0].Angle, 0.0);
  AssertEquals('line[0] base x', 1.0, Props.PatternLines[0].Base.X, 0.0);
  AssertEquals('line[0] base y', 2.0, Props.PatternLines[0].Base.Y, 0.0);
  AssertEquals('line[0] offset x', 3.0, Props.PatternLines[0].Offset.X, 0.0);
  AssertEquals('line[0] offset y', 4.0, Props.PatternLines[0].Offset.Y, 0.0);
  AssertEquals('line[0] dash count', 3, Length(Props.PatternLines[0].Dashes));
  AssertEquals('line[0] dash[1]', -0.25,
    Props.PatternLines[0].Dashes[1], 0.0);
  AssertEquals('line[1] angle', 1.5707963267948966,
    Props.PatternLines[1].Angle, 0.0);
  AssertEquals('line[1] base x', 5.0, Props.PatternLines[1].Base.X, 0.0);
  AssertEquals('line[1] offset y', 7.0, Props.PatternLines[1].Offset.Y, 0.0);
  AssertEquals('line[1] dash count', 0, Length(Props.PatternLines[1].Dashes));
end;

procedure TFPDWGProcStage8GeometryTest.CopyPolylineRefsCopiesOwnedVertexHandles;
var
  Poly: Dwg_Entity_POLYLINE_2D;
  Props: TDWGPolylineRefProps;
  Refs: array[0..1] of Dwg_Object_Ref;
  Handles: array[0..1] of BITCODE_H;
begin
  FillChar(Poly, SizeOf(Poly), 0);
  FillChar(Refs, SizeOf(Refs), 0);
  Refs[0].absolute_ref := $101;
  Refs[1].absolute_ref := $102;
  Handles[0] := @Refs[0];
  Handles[1] := @Refs[1];
  Poly.flag := 1;
  Poly.elevation := 4.5;
  Poly.num_owned := Length(Handles);
  Poly.vertex := @Handles[0];

  DWGCopyPolyline2DRefProps(Poly, Props);

  AssertTrue('closed flag bit copied', Props.Closed);
  AssertEquals('elevation', 4.5, Props.Elevation, 0.0);
  AssertEquals('handle count', 2, Length(Props.VertexHandles));
  AssertEquals('first handle', Int64($101), Int64(Props.VertexHandles[0]));
  AssertEquals('second handle', Int64($102), Int64(Props.VertexHandles[1]));
end;

begin
  RegisterTests([
    TFPDWGProcHandleTest, TFPDWGProcReadCodeTest,
    TFPDWGProcTextStyleTest, TFPDWGProcLinetypeTest,
    TFPDWGProcLineTest, TFPDWGProcCircleTest, TFPDWGProcArcTest,
    TFPDWGProcPointTest,
    TFPDWGProcTextTest, TFPDWGProcMTextTest,
    TFPDWGProcInsertTest,
    TFPDWGProcLWPolylineTest, TFPDWGProcProxyTest,
    TFPDWGProcStage8GeometryTest
  ]);
end.
