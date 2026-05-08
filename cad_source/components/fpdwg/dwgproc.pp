{*************************************************************************** }
{  gfdwg - free implementation of the DWG file format based on LibreDWG      }
{                                                                            }
{        Copyright (C) 2022 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{  You should have received a copy of the GNU General Public License         }
{  along with this program.  If not, see <http://www.gnu.org/licenses/>.     }
{*************************************************************************** }

unit dwgproc;

{$IFDEF FPC}
  {$PACKRECORDS C}
  {$MACRO ON}
  {$IFDEF Windows}
    {$DEFINE extdecl := stdcall}
  {$ELSE}
    {$DEFINE extdecl := cdecl}
  {$ENDIF}
  {$Mode objfpc}{$H+}
  {$ModeSwitch advancedrecords}
{$ENDIF}

interface
  uses
    SysUtils, {ctypes,} dynlibs, dwg, ghashmap, TypInfo,
    // R3: handle / text helpers moved into their own units. Re-exported so
    // existing callers (uzefflibredwg2ents.pas, uzedwgtestdwgproc.pas) keep
    // seeing the same names through `uses dwgproc`.
    uzedwghandle, uzedwgtext;

  resourcestring
    rsHandlerAlreadyReg='Handler already registered for %d';
    rsCouldNotLoadLib='Could not load library: %s';

  const
  {$if defined(Windows)}
    LibreDWG_Lib = 'libredwg-0.dll';
  {$elseif defined(OS2)}
    //LibreDWG_Lib = '';
  {$elseif defined(darwin)}
    //LibreDWG_LIB =  '';
  {$elseif defined(haiku) or defined(OpenBSD)}
    //LibreDWG_LIB = '';
  {$elseif defined(MorphOS)}
    //LibreDWG_LIB = '';
  {$else}
    LibreDWG_LIB = 'libredwg.so';
  {$endif}
  type

    TDWGCtx=record
      DWG:Dwg_Data;
      DWGVer:DWG_VERSION_TYPE;
      procedure CreateRec(var ADWG:Dwg_Data);
    end;

    // Plain record used by ZCAD LINE mapper and its unit tests. It exposes
    // the bare (x,y,z) pairs that GDBObjLine needs without dragging the
    // ZCAD entity unit into dwgproc.
    TDWGLineEndpoints=record
      StartX,StartY,StartZ:double;
      EndX,EndY,EndZ:double;
    end;

    // Stage 5 (TZ §12.5): plain mirror records that LINE/CIRCLE/ARC/POINT/
    // LWPOLYLINE/TEXT/MTEXT mappers fill before pushing into the ZCAD entity.
    // Keeping them in dwgproc means the scalar copy path is unit-testable
    // against fake LibreDWG records without pulling in the zengine entity
    // graph or libredwg.so.
    TDWGCircleProps=record
      CenterX,CenterY,CenterZ:double;
      Radius:double;
      Thickness:double;
    end;

    TDWGArcProps=record
      CenterX,CenterY,CenterZ:double;
      Radius:double;
      Thickness:double;
      StartAngle:double;
      EndAngle:double;
    end;

    TDWGPointProps=record
      X,Y,Z:double;
      Thickness:double;
      XAngle:double;
    end;

    TDWGTextProps=record
      InsertX,InsertY,InsertZ:double;
      AlignX,AlignY:double;
      Height:double;
      Rotation:double;
      Oblique:double;
      WidthFactor:double;
      Generation:integer;
      HorizAlignment:integer;
      VertAlignment:integer;
      Value:string;
    end;

    TDWGMTextProps=record
      InsertX,InsertY,InsertZ:double;
      XAxisX,XAxisY,XAxisZ:double;
      RectWidth,RectHeight:double;
      TextHeight:double;
      Attachment:integer;
      LineSpaceFactor:double;
      Value:string;
    end;

    TDWGMTextJustify=(dwgmtjTopLeft,dwgmtjTopCenter,dwgmtjTopRight,
      dwgmtjMiddleLeft,dwgmtjMiddleCenter,dwgmtjMiddleRight,
      dwgmtjBottomLeft,dwgmtjBottomCenter,dwgmtjBottomRight);

    TDWGLWPolylineVertex=record
      X,Y:double;
      StartWidth,EndWidth,Bulge:double;
    end;

    TDWGLWPolylineProps=record
      Closed:Boolean;
      ConstWidth:double;
      Elevation:double;
      Thickness:double;
      Vertices:array of TDWGLWPolylineVertex;
    end;

    TDWGProxyEntityPayload=record
      ProxyID:Integer;
      ClassID:Integer;
      DWGVersions:Integer;
      MaintVersion:Integer;
      DWGVersion:Integer;
      FromDXF:Integer;
      EntityDataSize:Integer;
      HasGraphic:Boolean;
      Graphic:TBytes;
    end;

    // Stage 8 (TZ §12.8): raw-geometry mirror records for HATCH/SPLINE/
    // ELLIPSE/SOLID/3DFACE/POLYLINE variants. They intentionally stay as
    // plain records so unit tests can exercise the LibreDWG field mapping
    // without allocating ZCAD entities.
    TDWGPoint2D=record
      X,Y:double;
    end;

    TDWGPoint3D=record
      X,Y,Z:double;
    end;

    TDWG3DFaceProps=record
      Corners:array[0..3] of TDWGPoint3D;
      InvisibleFlags:Integer;
    end;

    TDWGSolidProps=record
      Thickness:double;
      Elevation:double;
      Corners:array[0..3] of TDWGPoint3D;
      Extrusion:TDWGPoint3D;
    end;

    TDWGEllipseProps=record
      Center:TDWGPoint3D;
      MajorAxis:TDWGPoint3D;
      Extrusion:TDWGPoint3D;
      AxisRatio:double;
      StartAngle:double;
      EndAngle:double;
    end;

    TDWGSplineControlPoint=record
      X,Y,Z,W:double;
    end;

    TDWGSplineProps=record
      Flag:Integer;
      Scenario:Integer;
      Degree:Integer;
      Closed:Boolean;
      Periodic:Boolean;
      Rational:Boolean;
      Weighted:Boolean;
      Knots:array of double;
      ControlPoints:array of TDWGSplineControlPoint;
      FitPoints:array of TDWGPoint3D;
    end;

    TDWGHatchPolylinePoint=record
      X,Y:double;
      Bulge:double;
    end;

    TDWGHatchPathProps=record
      IsPolyline:Boolean;
      Closed:Boolean;
      PolylinePoints:array of TDWGHatchPolylinePoint;
    end;

    TDWGHatchProps=record
      PatternName:string;
      Elevation:double;
      Extrusion:TDWGPoint3D;
      IsSolidFill:Boolean;
      Style:Integer;
      PatternType:Integer;
      Angle:double;
      Scale:double;
      Paths:array of TDWGHatchPathProps;
    end;

    TDWGPolylineRefProps=record
      Closed:Boolean;
      Elevation:double;
      VertexHandles:array of QWord;
    end;

    // Stage 5 (TZ §12.5): the LibreDWG bindings only expose
    // PDwg_Entity_LINE; declare the pointer aliases the Stage 5 helpers
    // require so callers can pass &dwg.tio.entity^.tio.TEXT directly.
    // R3: PDwg_Entity_TEXT / PDwg_Entity_MTEXT are also declared in
    // uzedwghandle (which is re-exported above) and reach callers from
    // there; the rest stay here next to their TDWGCopy* mappers.
    PDwg_Entity_CIRCLE=^Dwg_Entity_CIRCLE;
    PDwg_Entity_ARC=^Dwg_Entity_ARC;
    PDwg_Entity_POINT=^Dwg_Entity_POINT;
    PDwg_Entity_LWPOLYLINE=^Dwg_Entity_LWPOLYLINE;
    PDwg_Entity_PROXY_ENTITY=^Dwg_Entity_PROXY_ENTITY;
    PDwg_Entity__3DFACE=^Dwg_Entity__3DFACE;
    PDwg_Entity_SOLID=^Dwg_Entity_SOLID;
    PDwg_Entity_ELLIPSE=^Dwg_Entity_ELLIPSE;
    PDwg_Entity_SPLINE=^Dwg_Entity_SPLINE;
    PDwg_Entity_HATCH=^Dwg_Entity_HATCH;
    PDwg_Entity_POLYLINE_2D=^Dwg_Entity_POLYLINE_2D;
    PDwg_Entity_POLYLINE_3D=^Dwg_Entity_POLYLINE_3D;
    PDwg_Entity_POLYLINE_MESH=^Dwg_Entity_POLYLINE_MESH;
    PDwg_Entity_POLYLINE_PFACE=^Dwg_Entity_POLYLINE_PFACE;
    PDwg_Entity_VERTEX_2D=^Dwg_Entity_VERTEX_2D;
    PDwg_Entity_VERTEX_3D=^Dwg_Entity_VERTEX_3D;
    PDwg_Entity_VERTEX_MESH=^Dwg_Entity_VERTEX_MESH;
    PDwg_Entity_VERTEX_PFACE=^Dwg_Entity_VERTEX_PFACE;
    PDwg_Entity_VERTEX_PFACE_FACE=^Dwg_Entity_VERTEX_PFACE_FACE;
    PBITCODE_H=^BITCODE_H;

    TData=PtrInt;
    TCounter=Integer;
    TProcessLongProcess=procedure(const Data:TData;const Counter:TCounter);

    HashDWG_OBJECT_TYPE=class
      class function hash(dot:DWG_OBJECT_TYPE; n:longint):SizeUInt;
    end;

    generic GDWGParser<GUserCtx>=class
      type
        TDWGObjectLoadProc=procedure(var ZContext:GUserCtx;var DWGContext:TDWGCtx;var DWGObject:Dwg_Object;P:Pointer);
        PTDWGObjectData=^TDWGObjectData;
        TDWGObjectData=record
          LoadEntityProc:TDWGObjectLoadProc;
          LoadObjectProc:TDWGObjectLoadProc;
          procedure Create;
        end;
        //work in fpc3.2.2
        //TDWGObjectsDataDict=class (specialize TDictionary<DWG_OBJECT_TYPE,TDWGObjectData>)
        //  function GetMutableValue(key:DWG_OBJECT_TYPE; out PAValue:PTDWGObjectData):boolean;
        //end;
        TDWGObjectsDataDict=specialize THashmap<DWG_OBJECT_TYPE,TDWGObjectData,HashDWG_OBJECT_TYPE>;
      var
        DWGObj2LPDict:TDWGObjectsDataDict;
      constructor create;
      destructor destroy;override;
      procedure RegisterDWGEntityLoadProc(const DOT:DWG_OBJECT_TYPE;const LP:TDWGObjectLoadProc);
      procedure RegisterDWGObjectLoadProc(const DOT:DWG_OBJECT_TYPE;const LP:TDWGObjectLoadProc);
      procedure parseDwg_Data(var ZContext:GUserCtx;var dwg:Dwg_Data;const lpp:TProcessLongProcess;const data:TData);
    end;


  var
    dwg_read_file : function(const filename:pchar;
                             dwg:PDwg_Data):integer;extdecl;
    dxf_read_file : function(const filename:pchar;
                             dwg:PDwg_Data):integer;extdecl;
    dwg_free : procedure(dwg:PDwg_Data);extdecl;

  procedure FreeLibreDWG;
  procedure LoadLibreDWG(lib : pchar = LibreDWG_Lib; reloadlib : Boolean = False);
  // R3: BITCODE_T2Text remains here as a TDWGCtx-aware shim that delegates
  // to uzedwgtext.DWGSafeDecodeText. The real handle/text helpers live in
  // uzedwghandle and uzedwgtext (both re-exported by this unit).
  procedure BITCODE_T2Text(const p:BITCODE_T;constref DWGContext:TDWGCtx;out text:string);
  function DWG_V2Str(v:DWG_VERSION_TYPE):string;

  // Pure copy of a LIBREDWG line geometry into a ZCAD-shaped record.
  // Lives in dwgproc so tests can verify the Z-coord fix without ZCAD deps.
  procedure DWGCopyLineEndpoints(const Line:Dwg_Entity_LINE;out Endpoints:TDWGLineEndpoints);

  // Stage 5 (TZ §12.5): pure scalar copies into the mirror records above.
  // Each routine is pointer-aware (nil source produces a zeroed record) so
  // callers can drive them straight from LibreDWG output without first
  // checking for missing payloads. The text decode goes through
  // DWGSafeDecodeText so the loader does not crash on a stripped-down
  // fixture that omits the payload field.
  procedure DWGCopyCircleProps(const Circle:Dwg_Entity_CIRCLE;
    out Props:TDWGCircleProps);
  procedure DWGCopyArcProps(const Arc:Dwg_Entity_ARC;out Props:TDWGArcProps);
  procedure DWGCopyPointProps(const Point:Dwg_Entity_POINT;
    out Props:TDWGPointProps);
  procedure DWGCopyTextProps(const Text:Dwg_Entity_TEXT;
    Version:DWG_VERSION_TYPE;out Props:TDWGTextProps);
  procedure DWGCopyMTextProps(const MText:Dwg_Entity_MTEXT;
    Version:DWG_VERSION_TYPE;out Props:TDWGMTextProps);
  function DWGMTextAttachmentToJustify(Attachment:Integer;
    DefaultJustify:TDWGMTextJustify=dwgmtjTopLeft):TDWGMTextJustify;
  function DWGLWPolylineWidthRecordCount(const Props:TDWGLWPolylineProps):Integer;
  procedure DWGCopyLWPolylineProps(const LWP:Dwg_Entity_LWPOLYLINE;
    out Props:TDWGLWPolylineProps);
  procedure DWGCopyProxyEntityPayload(PProxy:PDwg_Entity_PROXY_ENTITY;
    out Payload:TDWGProxyEntityPayload);
  // LibreDWG exposes custom/zombie entity proxy graphics on the common
  // entity preview fields even when fixedtype is DWG_TYPE_UNKNOWN_ENT.
  function DWGCopyEntityPreviewProxyPayload(const DWGObject:Dwg_Object;
    out Payload:TDWGProxyEntityPayload):Boolean;
  procedure DWGCopy3DFaceProps(const Face:Dwg_Entity__3DFACE;
    out Props:TDWG3DFaceProps);
  procedure DWGCopySolidProps(const Solid:Dwg_Entity_SOLID;
    out Props:TDWGSolidProps);
  procedure DWGCopyEllipseProps(const Ellipse:Dwg_Entity_ELLIPSE;
    out Props:TDWGEllipseProps);
  procedure DWGCopySplineProps(const Spline:Dwg_Entity_SPLINE;
    out Props:TDWGSplineProps);
  procedure DWGCopyHatchProps(const Hatch:Dwg_Entity_HATCH;
    Version:DWG_VERSION_TYPE;out Props:TDWGHatchProps);
  procedure DWGCopyPolyline2DRefProps(const Polyline:Dwg_Entity_POLYLINE_2D;
    out Props:TDWGPolylineRefProps);
  procedure DWGCopyPolyline3DRefProps(const Polyline:Dwg_Entity_POLYLINE_3D;
    out Props:TDWGPolylineRefProps);
  procedure DWGCopyPolylineMeshRefProps(
    const Polyline:Dwg_Entity_POLYLINE_MESH;
    out Props:TDWGPolylineRefProps);
  procedure DWGCopyPolylinePFaceRefProps(
    const Polyline:Dwg_Entity_POLYLINE_PFACE;
    out Props:TDWGPolylineRefProps);

implementation

  var
    hlib : tlibhandle;

   class function HashDWG_OBJECT_TYPE.hash(dot:DWG_OBJECT_TYPE; n:longint):SizeUInt;
   begin
     result:=ord(dot) mod SizeUInt(n);
   end;

  procedure TDWGCtx.CreateRec(var ADWG:Dwg_Data);
  begin
    DWG:=ADWG;
    DWGVer:=ADWG.HEADER.version;
    if DWGVer=R_INVALID then
      DWGVer:=ADWG.HEADER.from_version;
  end;

  procedure GDWGParser.TDWGObjectData.Create;
  begin
    LoadEntityProc:=nil;
    LoadObjectProc:=nil;
  end;

  procedure GDWGParser.RegisterDWGEntityLoadProc(const DOT:DWG_OBJECT_TYPE;const LP:TDWGObjectLoadProc);
  var
    dod:TDWGObjectData;
  begin
    // Stage 1: catch double registration so a later mapper does not silently
    // overwrite an earlier one. The hashmap container does not provide a public
    // contains() variant on the FPC version we target, so we look up via
    // GetValue and fall back to insert when the key is absent.
    if DWGObj2LPDict.GetValue(DOT,dod) then
      raise Exception.Create(format(rsHandlerAlreadyReg,[Ord(DOT)]));
    dod.Create;
    dod.LoadEntityProc:=LP;
    dod.LoadObjectProc:=nil;
    DWGObj2LPDict.insert(DOT,dod);
  end;

  procedure GDWGParser.RegisterDWGObjectLoadProc(const DOT:DWG_OBJECT_TYPE;const LP:TDWGObjectLoadProc);
  var
    dod:TDWGObjectData;
  begin
    if DWGObj2LPDict.GetValue(DOT,dod) then
      raise Exception.Create(format(rsHandlerAlreadyReg,[Ord(DOT)]));
    dod.Create;
    dod.LoadEntityProc:=nil;
    dod.LoadObjectProc:=LP;
    DWGObj2LPDict.insert(DOT,dod);
  end;

  procedure GDWGParser.parseDwg_Data(var ZContext:GUserCtx;var dwg:Dwg_Data;const lpp:TProcessLongProcess;const data:TData);
  //work in fpc3.2.2
  //var
  //  i:BITCODE_BL;
  //  pdod:PTDWGObjectData;
  //  DWGContext:TDWGCtx;
  //begin
  //  DWGContext.CreateRec(dwg);
  //  if DWGObj2LPDict<>nil then begin
  //    i:=0;
  //    while (i<dwg.num_objects) do begin
  //      if DWGObj2LPDict.GetMutableValue(dwg.&object[i].fixedtype,pdod) then begin
  //        if pdod^.LoadEntityProc<>nil then
  //          pdod^.LoadEntityProc(ZContext,DWGContext,dwg.&object[i],dwg.&object[i].tio.entity^.tio.UNUSED)
  //        else if pdod^.LoadObjectProc<>nil then
  //          pdod^.LoadObjectProc(ZContext,DWGContext,dwg.&object[i],dwg.&object[i].tio.&object^.tio.DUMMY);
  //      end;
  //      if @lpp<>nil then
  //        lpp(data,i);
  //      inc(i);
  //    end;
  //  end;
  //end;
  var
    i:BITCODE_BL;
    dod:TDWGObjectData;
    DWGContext:TDWGCtx;
  begin
    DWGContext.CreateRec(dwg);
    if DWGObj2LPDict<>nil then begin
      i:=0;
      while (i<dwg.num_objects) do begin
        if DWGObj2LPDict.GetValue(dwg.&object[i].fixedtype,dod) then begin
          if dod.LoadEntityProc<>nil then begin
            if dwg.&object[i].tio.entity<>nil then
              dod.LoadEntityProc(ZContext,DWGContext,dwg.&object[i],
                dwg.&object[i].tio.entity^.tio.UNUSED)
            else
              dod.LoadEntityProc(ZContext,DWGContext,dwg.&object[i],nil);
          end else if dod.LoadObjectProc<>nil then begin
            if dwg.&object[i].tio.&object<>nil then
              dod.LoadObjectProc(ZContext,DWGContext,dwg.&object[i],
                dwg.&object[i].tio.&object^.tio.DUMMY)
            else
              dod.LoadObjectProc(ZContext,DWGContext,dwg.&object[i],nil);
          end;
        end;
        if @lpp<>nil then
          lpp(data,i);
        inc(i);
      end;
    end;
  end;

  //work in fpc3.2.2
  //function GDWGParser.TDWGObjectsDataDict.GetMutableValue(key:DWG_OBJECT_TYPE; out PAValue:PTDWGObjectData):Boolean;
  //var
  //  LIndex: SizeInt;
  //  LHash: UInt32;
  //begin
  //  LIndex := FindBucketIndex(FItems, key, LHash);
  //
  //  if LIndex < 0 then begin
  //    result:=false;
  //    PAValue:=nil;
  //  end else begin
  //    result:=true;
  //    PAValue:=@FItems[LIndex].Pair.Value;
  //  end;
  //end;

  constructor GDWGParser.create;
  begin
    DWGObj2LPDict:=TDWGObjectsDataDict.create;
  end;
  destructor GDWGParser.destroy;
  begin
    DWGObj2LPDict.Free;
  end;

  function DWG_V2Str(v:DWG_VERSION_TYPE):string;
  begin
    if Ord(v)>Ord(R_AFTER)then
      v:=R_AFTER;
    result:=GetEnumName(typeinfo(v),Ord(v));
  end;

  procedure BITCODE_T2Text(const p:BITCODE_T;constref DWGContext:TDWGCtx;out text:string);
  begin
    // R3: defer to the version-based helper in uzedwgtext so the actual
    // decoding rules live in one place. We pass the parser's resolved
    // header version (the same one TDWGCtx.CreateRec already populates).
    DWGSafeDecodeText(p,DWGContext.DWGVer,text);
  end;

  procedure DWGCopyLineEndpoints(const Line:Dwg_Entity_LINE;out Endpoints:TDWGLineEndpoints);
  begin
    Endpoints.StartX:=Line.start.x;
    Endpoints.StartY:=Line.start.y;
    Endpoints.StartZ:=Line.start.z;
    Endpoints.EndX:=Line.end_.x;
    Endpoints.EndY:=Line.end_.y;
    Endpoints.EndZ:=Line.end_.z;
  end;

  procedure DWGCopyCircleProps(const Circle:Dwg_Entity_CIRCLE;
    out Props:TDWGCircleProps);
  begin
    Props.CenterX:=Circle.center.x;
    Props.CenterY:=Circle.center.y;
    Props.CenterZ:=Circle.center.z;
    Props.Radius:=Circle.radius;
    Props.Thickness:=Circle.thickness;
  end;

  procedure DWGCopyArcProps(const Arc:Dwg_Entity_ARC;out Props:TDWGArcProps);
  begin
    Props.CenterX:=Arc.center.x;
    Props.CenterY:=Arc.center.y;
    Props.CenterZ:=Arc.center.z;
    Props.Radius:=Arc.radius;
    Props.Thickness:=Arc.thickness;
    Props.StartAngle:=Arc.start_angle;
    Props.EndAngle:=Arc.end_angle;
  end;

  procedure DWGCopyPointProps(const Point:Dwg_Entity_POINT;
    out Props:TDWGPointProps);
  begin
    Props.X:=Point.x;
    Props.Y:=Point.y;
    Props.Z:=Point.z;
    Props.Thickness:=Point.thickness;
    Props.XAngle:=Point.x_ang;
  end;

  procedure DWGCopyTextProps(const Text:Dwg_Entity_TEXT;
    Version:DWG_VERSION_TYPE;out Props:TDWGTextProps);
  begin
    Props.InsertX:=Text.ins_pt.x;
    Props.InsertY:=Text.ins_pt.y;
    Props.InsertZ:=Text.elevation;
    Props.AlignX:=Text.alignment_pt.x;
    Props.AlignY:=Text.alignment_pt.y;
    Props.Height:=Text.height;
    Props.Rotation:=Text.rotation;
    Props.Oblique:=Text.oblique_angle;
    Props.WidthFactor:=Text.width_factor;
    Props.Generation:=Text.generation;
    Props.HorizAlignment:=Text.horiz_alignment;
    Props.VertAlignment:=Text.vert_alignment;
    DWGSafeDecodeText(Text.text_value,Version,Props.Value);
  end;

  procedure DWGCopyMTextProps(const MText:Dwg_Entity_MTEXT;
    Version:DWG_VERSION_TYPE;out Props:TDWGMTextProps);
  begin
    Props.InsertX:=MText.ins_pt.x;
    Props.InsertY:=MText.ins_pt.y;
    Props.InsertZ:=MText.ins_pt.z;
    Props.XAxisX:=MText.x_axis_dir.x;
    Props.XAxisY:=MText.x_axis_dir.y;
    Props.XAxisZ:=MText.x_axis_dir.z;
    Props.RectWidth:=MText.rect_width;
    Props.RectHeight:=MText.rect_height;
    Props.TextHeight:=MText.text_height;
    Props.Attachment:=MText.attachment;
    Props.LineSpaceFactor:=MText.linespace_factor;
    DWGSafeDecodeText(MText.text,Version,Props.Value);
  end;

  function DWGMTextAttachmentToJustify(Attachment:Integer;
    DefaultJustify:TDWGMTextJustify=dwgmtjTopLeft):TDWGMTextJustify;
  begin
    case Attachment of
      1: Result:=dwgmtjTopLeft;
      2: Result:=dwgmtjTopCenter;
      3: Result:=dwgmtjTopRight;
      4: Result:=dwgmtjMiddleLeft;
      5: Result:=dwgmtjMiddleCenter;
      6: Result:=dwgmtjMiddleRight;
      7: Result:=dwgmtjBottomLeft;
      8: Result:=dwgmtjBottomCenter;
      9: Result:=dwgmtjBottomRight;
    else
      Result:=DefaultJustify;
    end;
  end;

  function DWGLWPolylineWidthRecordCount(const Props:TDWGLWPolylineProps):Integer;
  begin
    // ZCAD GDBObjLWPolyline stores one width record per vertex. Open polylines
    // ignore the trailing generated segment later, but CalcWidthSegment still
    // reads the final width slot while building its cached geometry.
    Result:=Length(Props.Vertices);
  end;

  procedure DWGCopyLWPolylineProps(const LWP:Dwg_Entity_LWPOLYLINE;
    out Props:TDWGLWPolylineProps);
  type
    PBitcode2RD=^BITCODE_2RD;
    PBitcodeBD=^BITCODE_BD;
    PLWPWidth=^Dwg_LWPOLYLINE_width;
  var
    i,n:Integer;
    pPoint:PBitcode2RD;
    pBulge:PBitcodeBD;
    pWidth:PLWPWidth;
  begin
    // Stage 5: bit 512 of `flag` marks a closed polyline (per LibreDWG header
    // comments). Bulge / width arrays are only consulted when their counters
    // match num_points; mismatched arrays are treated as missing so a
    // stripped fixture cannot dereference into garbage.
    Props.Closed:=(LWP.flag and 512)<>0;
    Props.ConstWidth:=LWP.const_width;
    Props.Elevation:=LWP.elevation;
    Props.Thickness:=LWP.thickness;
    n:=LWP.num_points;
    if n<0 then
      n:=0;
    SetLength(Props.Vertices,n);
    if (n>0) and (LWP.points<>nil) then begin
      pPoint:=PBitcode2RD(LWP.points);
      for i:=0 to n-1 do begin
        Props.Vertices[i].X:=pPoint^.x;
        Props.Vertices[i].Y:=pPoint^.y;
        Props.Vertices[i].StartWidth:=LWP.const_width;
        Props.Vertices[i].EndWidth:=LWP.const_width;
        Props.Vertices[i].Bulge:=0;
        Inc(pPoint);
      end;
    end;
    if (n>0) and (LWP.num_bulges=BITCODE_BL(n)) and (LWP.bulges<>nil) then begin
      pBulge:=PBitcodeBD(LWP.bulges);
      for i:=0 to n-1 do begin
        Props.Vertices[i].Bulge:=pBulge^;
        Inc(pBulge);
      end;
    end;
    if (n>0) and (LWP.num_widths=BITCODE_BL(n)) and (LWP.widths<>nil) then begin
      pWidth:=PLWPWidth(LWP.widths);
      for i:=0 to n-1 do begin
        Props.Vertices[i].StartWidth:=pWidth^.start;
        Props.Vertices[i].EndWidth:=pWidth^.end_;
        Inc(pWidth);
      end;
    end;
  end;

  procedure DWGCopyProxyEntityPayload(PProxy:PDwg_Entity_PROXY_ENTITY;
    out Payload:TDWGProxyEntityPayload);
    function BLToInt(Value:BITCODE_BL):Integer;
    begin
      if Value>BITCODE_BL(High(Integer)) then
        Result:=High(Integer)
      else
        Result:=Integer(Value);
    end;
  var
    ByteCount:Integer;
  begin
    Payload.ProxyID:=0;
    Payload.ClassID:=0;
    Payload.DWGVersions:=0;
    Payload.MaintVersion:=0;
    Payload.DWGVersion:=0;
    Payload.FromDXF:=0;
    Payload.EntityDataSize:=0;
    Payload.HasGraphic:=False;
    SetLength(Payload.Graphic,0);

    if PProxy=nil then
      Exit;

    Payload.ProxyID:=BLToInt(PProxy^.proxy_id);
    Payload.ClassID:=BLToInt(PProxy^.class_id);
    Payload.DWGVersions:=BLToInt(PProxy^.dwg_versions);
    Payload.MaintVersion:=BLToInt(PProxy^.maint_version);
    Payload.DWGVersion:=BLToInt(PProxy^.dwg_version);
    Payload.FromDXF:=PProxy^.from_dxf;
    Payload.EntityDataSize:=BLToInt(PProxy^.data_size);

    if (PProxy^.proxy_data=nil) or (PProxy^.proxy_data_size=0) then
      Exit;
    if PProxy^.proxy_data_size>BITCODE_BL(High(Integer)) then
      Exit;

    ByteCount:=Integer(PProxy^.proxy_data_size);
    SetLength(Payload.Graphic,ByteCount);
    Move(PProxy^.proxy_data^,Payload.Graphic[0],ByteCount);
    Payload.HasGraphic:=ByteCount>0;
  end;

  function DWGCopyEntityPreviewProxyPayload(const DWGObject:Dwg_Object;
    out Payload:TDWGProxyEntityPayload):Boolean;
    function BLToInt(Value:BITCODE_BL):Integer;
    begin
      if Value>BITCODE_BL(High(Integer)) then
        Result:=High(Integer)
      else
        Result:=Integer(Value);
    end;
    function BLLToInt(Value:BITCODE_BLL):Integer;
    begin
      if Value>BITCODE_BLL(High(Integer)) then
        Result:=High(Integer)
      else
        Result:=Integer(Value);
    end;
  var
    Ent:^Dwg_Object_Entity;
    ByteCount:Integer;
  begin
    Payload.ProxyID:=498;
    Payload.ClassID:=499;
    Payload.DWGVersions:=15;
    Payload.MaintVersion:=0;
    Payload.DWGVersion:=0;
    Payload.FromDXF:=0;
    Payload.EntityDataSize:=0;
    Payload.HasGraphic:=False;
    SetLength(Payload.Graphic,0);
    Result:=False;

    if (DWGObject.supertype<>DWG_SUPERTYPE_ENTITY)
      or (DWGObject.tio.entity=nil) then
      Exit;

    Ent:=DWGObject.tio.entity;
    if (Ent^.preview_exists=0) or (Ent^.preview_is_proxy=0)
      or (Ent^.preview=nil) or (Ent^.preview_size=0) then
      Exit;
    if Ent^.preview_size>BITCODE_BLL(High(Integer)) then
      Exit;

    if DWGObject.klass<>nil then begin
      if DWGObject.klass^.number<>0 then
        Payload.ClassID:=Integer(DWGObject.klass^.number);
      Payload.DWGVersion:=BLToInt(DWGObject.klass^.dwg_version);
      Payload.MaintVersion:=BLToInt(DWGObject.klass^.maint_version);
    end;

    ByteCount:=BLLToInt(Ent^.preview_size);
    Payload.EntityDataSize:=0;
    SetLength(Payload.Graphic,ByteCount);
    Move(Ent^.preview^,Payload.Graphic[0],ByteCount);
    Payload.HasGraphic:=ByteCount>0;
    Result:=Payload.HasGraphic;
  end;

  function DWGBLToInt(Value:BITCODE_BL):Integer;
  begin
    if Value>BITCODE_BL(High(Integer)) then
      Result:=High(Integer)
    else
      Result:=Integer(Value);
  end;

  procedure DWGPoint3DFrom3BD(const Src:BITCODE_3BD;out Dest:TDWGPoint3D);
  begin
    Dest.X:=Src.x;
    Dest.Y:=Src.y;
    Dest.Z:=Src.z;
  end;

  function DWGPoint3DFrom2RDAtElevation(const Src:BITCODE_2RD;
    Elevation:double):TDWGPoint3D;
  begin
    Result.X:=Src.x;
    Result.Y:=Src.y;
    Result.Z:=Elevation;
  end;

  procedure DWGCopy3DFaceProps(const Face:Dwg_Entity__3DFACE;
    out Props:TDWG3DFaceProps);
  begin
    DWGPoint3DFrom3BD(Face.corner1,Props.Corners[0]);
    DWGPoint3DFrom3BD(Face.corner2,Props.Corners[1]);
    DWGPoint3DFrom3BD(Face.corner3,Props.Corners[2]);
    DWGPoint3DFrom3BD(Face.corner4,Props.Corners[3]);
    Props.InvisibleFlags:=Face.invis_flags;
  end;

  procedure DWGCopySolidProps(const Solid:Dwg_Entity_SOLID;
    out Props:TDWGSolidProps);
  begin
    Props.Thickness:=Solid.thickness;
    Props.Elevation:=Solid.elevation;
    Props.Corners[0]:=DWGPoint3DFrom2RDAtElevation(Solid.corner1,Solid.elevation);
    Props.Corners[1]:=DWGPoint3DFrom2RDAtElevation(Solid.corner2,Solid.elevation);
    Props.Corners[2]:=DWGPoint3DFrom2RDAtElevation(Solid.corner3,Solid.elevation);
    Props.Corners[3]:=DWGPoint3DFrom2RDAtElevation(Solid.corner4,Solid.elevation);
    DWGPoint3DFrom3BD(Solid.extrusion,Props.Extrusion);
  end;

  procedure DWGCopyEllipseProps(const Ellipse:Dwg_Entity_ELLIPSE;
    out Props:TDWGEllipseProps);
  begin
    DWGPoint3DFrom3BD(Ellipse.center,Props.Center);
    DWGPoint3DFrom3BD(Ellipse.sm_axis,Props.MajorAxis);
    DWGPoint3DFrom3BD(Ellipse.extrusion,Props.Extrusion);
    Props.AxisRatio:=Ellipse.axis_ratio;
    Props.StartAngle:=Ellipse.start_angle;
    Props.EndAngle:=Ellipse.end_angle;
  end;

  procedure DWGCopySplineProps(const Spline:Dwg_Entity_SPLINE;
    out Props:TDWGSplineProps);
  type
    PBitcodeBD=^BITCODE_BD;
    PBitcode3DPoint=^BITCODE_3DPOINT;
    PSplineControlPoint=^Dwg_SPLINE_control_point;
  var
    i,n:Integer;
    pKnot:PBitcodeBD;
    pControl:PSplineControlPoint;
    pFit:PBitcode3DPoint;
  begin
    Props.Flag:=Spline.flag;
    Props.Scenario:=Spline.scenario;
    Props.Degree:=Spline.degree;
    Props.Closed:=((Spline.flag and 1)<>0) or (Spline.closed_b<>0)
      or ((Spline.splineflags and 4)<>0);
    Props.Periodic:=((Spline.flag and 2)<>0) or (Spline.periodic<>0);
    Props.Rational:=((Spline.flag and 4)<>0) or (Spline.rational<>0);
    Props.Weighted:=Spline.weighted<>0;
    SetLength(Props.Knots,0);
    SetLength(Props.ControlPoints,0);
    SetLength(Props.FitPoints,0);

    n:=DWGBLToInt(Spline.num_knots);
    if (n>0) and (Spline.knots<>nil) then begin
      SetLength(Props.Knots,n);
      pKnot:=PBitcodeBD(Spline.knots);
      for i:=0 to n-1 do begin
        Props.Knots[i]:=pKnot^;
        Inc(pKnot);
      end;
    end;

    n:=DWGBLToInt(Spline.num_ctrl_pts);
    if (n>0) and (Spline.ctrl_pts<>nil) then begin
      SetLength(Props.ControlPoints,n);
      pControl:=PSplineControlPoint(Spline.ctrl_pts);
      for i:=0 to n-1 do begin
        Props.ControlPoints[i].X:=pControl^.x;
        Props.ControlPoints[i].Y:=pControl^.y;
        Props.ControlPoints[i].Z:=pControl^.z;
        Props.ControlPoints[i].W:=pControl^.w;
        Inc(pControl);
      end;
    end;

    n:=DWGBLToInt(Spline.num_fit_pts);
    if (n>0) and (Spline.fit_pts<>nil) then begin
      SetLength(Props.FitPoints,n);
      pFit:=PBitcode3DPoint(Spline.fit_pts);
      for i:=0 to n-1 do begin
        DWGPoint3DFrom3BD(pFit^,Props.FitPoints[i]);
        Inc(pFit);
      end;
    end;
  end;

  procedure DWGCopyHatchProps(const Hatch:Dwg_Entity_HATCH;
    Version:DWG_VERSION_TYPE;out Props:TDWGHatchProps);
  type
    PHatchPath=^Dwg_HATCH_Path;
    PHatchPolylinePath=^Dwg_HATCH_PolylinePath;
  var
    i,j,n,PointCount:Integer;
    pPath:PHatchPath;
    pPoint:PHatchPolylinePath;
  begin
    Props.PatternName:='';
    Props.Elevation:=Hatch.elevation;
    DWGPoint3DFrom3BD(Hatch.extrusion,Props.Extrusion);
    Props.IsSolidFill:=Hatch.is_solid_fill<>0;
    Props.Style:=Hatch.style;
    Props.PatternType:=Hatch.pattern_type;
    Props.Angle:=Hatch.angle;
    Props.Scale:=Hatch.scale_spacing;
    SetLength(Props.Paths,0);
    DWGSafeDecodeText(Hatch.name,Version,Props.PatternName);

    n:=DWGBLToInt(Hatch.num_paths);
    if (n<=0) or (Hatch.paths=nil) then
      Exit;
    SetLength(Props.Paths,n);
    pPath:=PHatchPath(Hatch.paths);
    for i:=0 to n-1 do begin
      Props.Paths[i].IsPolyline:=(pPath^.flag and 2)<>0;
      Props.Paths[i].Closed:=pPath^.closed<>0;
      SetLength(Props.Paths[i].PolylinePoints,0);
      if Props.Paths[i].IsPolyline and (pPath^.polyline_paths<>nil) then begin
        PointCount:=DWGBLToInt(pPath^.num_segs_or_paths);
        SetLength(Props.Paths[i].PolylinePoints,PointCount);
        pPoint:=PHatchPolylinePath(pPath^.polyline_paths);
        for j:=0 to PointCount-1 do begin
          Props.Paths[i].PolylinePoints[j].X:=pPoint^.point.x;
          Props.Paths[i].PolylinePoints[j].Y:=pPoint^.point.y;
          Props.Paths[i].PolylinePoints[j].Bulge:=pPoint^.bulge;
          Inc(pPoint);
        end;
      end;
      Inc(pPath);
    end;
  end;

  procedure DWGCopyPolylineRefPropsCommon(Closed:Boolean;Elevation:double;
    NumOwned:BITCODE_BL;VertexRefs:PBITCODE_H;out Props:TDWGPolylineRefProps);
  var
    i,n:Integer;
    pRef:PBITCODE_H;
    Handle:QWord;
  begin
    Props.Closed:=Closed;
    Props.Elevation:=Elevation;
    SetLength(Props.VertexHandles,0);
    n:=DWGBLToInt(NumOwned);
    if (n<=0) or (VertexRefs=nil) then
      Exit;
    SetLength(Props.VertexHandles,n);
    pRef:=VertexRefs;
    for i:=0 to n-1 do begin
      if DWGRefHandleValue(pRef^,Handle) then
        Props.VertexHandles[i]:=Handle
      else
        Props.VertexHandles[i]:=0;
      Inc(pRef);
    end;
  end;

  procedure DWGCopyPolyline2DRefProps(const Polyline:Dwg_Entity_POLYLINE_2D;
    out Props:TDWGPolylineRefProps);
  begin
    DWGCopyPolylineRefPropsCommon((Polyline.flag and 1)<>0,
      Polyline.elevation,Polyline.num_owned,Polyline.vertex,Props);
  end;

  procedure DWGCopyPolyline3DRefProps(const Polyline:Dwg_Entity_POLYLINE_3D;
    out Props:TDWGPolylineRefProps);
  begin
    DWGCopyPolylineRefPropsCommon((Polyline.flag and 1)<>0,0,
      Polyline.num_owned,Polyline.vertex,Props);
  end;

  procedure DWGCopyPolylineMeshRefProps(
    const Polyline:Dwg_Entity_POLYLINE_MESH;
    out Props:TDWGPolylineRefProps);
  begin
    DWGCopyPolylineRefPropsCommon((Polyline.flag and 1)<>0,0,
      Polyline.num_owned,Polyline.vertex,Props);
  end;

  procedure DWGCopyPolylinePFaceRefProps(
    const Polyline:Dwg_Entity_POLYLINE_PFACE;
    out Props:TDWGPolylineRefProps);
  begin
    DWGCopyPolylineRefPropsCommon(False,0,Polyline.num_owned,
      Polyline.vertex,Props);
  end;



  procedure FreeLibreDWG;
  begin
    if (hlib <> 0) then
      FreeLibrary(hlib);
    hlib:=0;
    dwg_read_file:=nil;
    dxf_read_file:=nil;
    dwg_free:=nil;
  end;

  procedure LoadLibreDWG(lib : pchar = LibreDWG_Lib; reloadlib : Boolean = False);
  begin
    if reloadlib then
      FreeLibreDWG;
    if hlib = 0 then begin
      hlib:=LoadLibrary(lib);
      pointer(dwg_read_file):=GetProcAddress(hlib,'dwg_read_file');
      pointer(dxf_read_file):=GetProcAddress(hlib,'dxf_read_file');
      pointer(dwg_free):=GetProcAddress(hlib,'dwg_free');
    end;
    if hlib=0 then
      raise Exception.Create(format(rsCouldNotLoadLib,[lib]));
  end;

initialization
  hlib:=0;
  FreeLibreDWG;
finalization
  FreeLibreDWG;
end.
