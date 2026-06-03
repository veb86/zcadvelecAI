unit uzctdwgdimstylelinetype;

{ Issue #1280 regression: DWG-loaded dimension styles are initialized with nil
  line-type pointers. The DXF loader normalizes those fields to ByBlock after
  reading tables; DWG import must do the same before DimStyleEditForm can open. }

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  fpcunit,
  testregistry;

type
  TDWGDimStyleLineTypeTest = class(TTestCase)
  published
    procedure EndImportAssignsFallbackLineTypes;
  end;

implementation

uses
  uzeTypes,
  uzedrawingsimple,
  uzeffmanager,
  uzgldrawcontext,
  uzestylesdim,
  uzestyleslinetypes,
  uzedwgimport;

procedure TDWGDimStyleLineTypeTest.EndImportAssignsFallbackLineTypes;
var
  Drawing: TSimpleDrawing;
  DC: TDrawContext;
  ZDC: TZDrawingContext;
  DimStyle: PGDBDimStyle;
  ByBlockLT: PGDBLtypeProp;
begin
  Drawing.init(nil);
  try
    DC := Drawing.CreateDrawingRC;
    ZDC.CreateRec(Drawing, Drawing.pObjRoot^, TLOLoad, DC);

    BeginDWGImport(ZDC, 'issue-1280-test.dwg');
    try
      DimStyle := PGDBDimStyle(Drawing.DimStyleTable.MergeItem('DWG_STYLE',
        TLOLoad));
      AssertNotNull('test dimstyle must be created', DimStyle);
      DimStyle^.init('DWG_STYLE');
      DimStyle^.SetDefaultValues;

      AssertNull('DWG mapper starts with nil DIMLTYPE',
        DimStyle^.Lines.DIMLTYPE);
      AssertNull('DWG mapper starts with nil DIMLTEX1',
        DimStyle^.Lines.DIMLTEX1);
      AssertNull('DWG mapper starts with nil DIMLTEX2',
        DimStyle^.Lines.DIMLTEX2);
    finally
      EndDWGImport(ZDC);
    end;

    ByBlockLT := Drawing.LTypeStyleTable.GetSystemLT(TLTByBlock);
    AssertNotNull('ByBlock line type must exist', ByBlockLT);
    AssertTrue('DIMLTYPE must fall back to ByBlock',
      ByBlockLT = DimStyle^.Lines.DIMLTYPE);
    AssertTrue('DIMLTEX1 must fall back to ByBlock',
      ByBlockLT = DimStyle^.Lines.DIMLTEX1);
    AssertTrue('DIMLTEX2 must fall back to ByBlock',
      ByBlockLT = DimStyle^.Lines.DIMLTEX2);
  finally
    Drawing.done;
  end;
end;

initialization
  RegisterTest(TDWGDimStyleLineTypeTest);

end.
