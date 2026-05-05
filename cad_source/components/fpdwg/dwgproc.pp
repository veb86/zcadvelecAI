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
    SysUtils, {ctypes,} dynlibs, dwg, ghashmap, TypInfo;

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
  procedure BITCODE_T2Text(const p:BITCODE_T;constref DWGContext:TDWGCtx;out text:string);
  function DWG_V2Str(v:DWG_VERSION_TYPE):string;

  // Stage 1 helpers: handle extraction shared by ZCAD loader and diagnostics.
  // Each helper accepts already-decoded raw structures so callers can run unit
  // tests against fake records without LibreDWG being available.
  function DWGObjectHandleValue(const Obj:Dwg_Object):QWord;
  function DWGObjectOwnerHandleValue(const Obj:Dwg_Object;out Value:QWord):Boolean;
  function DWGRefHandleValue(Ref:BITCODE_H;out Value:QWord):Boolean;
  // Stage 3 helpers (TZ §12.3): pull layer / linetype refs off an entity-typed
  // Dwg_Object. Returns False when Obj is not an entity, or when the ref is
  // missing/null. Object-typed records (LAYER, LTYPE, BLOCK_HEADER) do not
  // expose these slots, so callers reading from a non-entity object get False.
  function DWGEntityLayerHandleValue(const Obj:Dwg_Object;out Value:QWord):Boolean;
  function DWGEntityLineTypeHandleValue(const Obj:Dwg_Object;out Value:QWord):Boolean;
  function DWGLayerLineTypeHandleValue(const PLayer:PDwg_Object_LAYER;out Value:QWord):Boolean;
  // Safe text decode helper without inspector dependency. Falls back to ANSI for
  // <=R2004, UTF-16LE for newer DWG; nil pointer returns empty string.
  procedure DWGSafeDecodeText(const p:BITCODE_T;Version:DWG_VERSION_TYPE;out text:string);
  // Pure copy of a LIBREDWG line geometry into a ZCAD-shaped record.
  // Lives in dwgproc so tests can verify the Z-coord fix without ZCAD deps.
  procedure DWGCopyLineEndpoints(const Line:Dwg_Entity_LINE;out Endpoints:TDWGLineEndpoints);

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
          if dod.LoadEntityProc<>nil then
            dod.LoadEntityProc(ZContext,DWGContext,dwg.&object[i],dwg.&object[i].tio.entity^.tio.UNUSED)
          else if dod.LoadObjectProc<>nil then
            dod.LoadObjectProc(ZContext,DWGContext,dwg.&object[i],dwg.&object[i].tio.&object^.tio.DUMMY);
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
    if DWGContext.dwg.header.version<=R_2004 then
      text:=pchar(p)
    else
      text:=punicodechar(p)
  end;

  function DWGObjectHandleValue(const Obj:Dwg_Object):QWord;
  begin
    Result:=Obj.handle.value;
  end;

  function DWGRefHandleValue(Ref:BITCODE_H;out Value:QWord):Boolean;
  begin
    Value:=0;
    if Ref=nil then
      Exit(False);
    if Ref^.absolute_ref<>0 then begin
      Value:=Ref^.absolute_ref;
      Exit(True);
    end;
    if Ref^.handleref.value<>0 then begin
      Value:=Ref^.handleref.value;
      Exit(True);
    end;
    Result:=False;
  end;

  function DWGObjectOwnerHandleValue(const Obj:Dwg_Object;out Value:QWord):Boolean;
  begin
    Value:=0;
    case Obj.supertype of
      DWG_SUPERTYPE_ENTITY:
        if Obj.tio.entity<>nil then
          Exit(DWGRefHandleValue(Obj.tio.entity^.ownerhandle,Value));
      DWG_SUPERTYPE_OBJECT:
        if Obj.tio.&object<>nil then
          Exit(DWGRefHandleValue(Obj.tio.&object^.ownerhandle,Value));
    end;
    Result:=False;
  end;

  function DWGEntityLayerHandleValue(const Obj:Dwg_Object;out Value:QWord):Boolean;
  begin
    Value:=0;
    if (Obj.supertype<>DWG_SUPERTYPE_ENTITY) or (Obj.tio.entity=nil) then
      Exit(False);
    Result:=DWGRefHandleValue(Obj.tio.entity^.layer,Value);
  end;

  function DWGEntityLineTypeHandleValue(const Obj:Dwg_Object;out Value:QWord):Boolean;
  begin
    Value:=0;
    if (Obj.supertype<>DWG_SUPERTYPE_ENTITY) or (Obj.tio.entity=nil) then
      Exit(False);
    Result:=DWGRefHandleValue(Obj.tio.entity^.ltype,Value);
  end;

  function DWGLayerLineTypeHandleValue(const PLayer:PDwg_Object_LAYER;out Value:QWord):Boolean;
  begin
    Value:=0;
    if PLayer=nil then
      Exit(False);
    Result:=DWGRefHandleValue(PLayer^.ltype,Value);
  end;

  procedure DWGSafeDecodeText(const p:BITCODE_T;Version:DWG_VERSION_TYPE;out text:string);
  begin
    text:='';
    if p=nil then
      Exit;
    if Version<=R_2004 then
      text:=pchar(p)
    else
      text:=punicodechar(p);
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
