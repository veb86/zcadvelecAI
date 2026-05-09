{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file GPL-3.0.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Andrey Zubarev <zamtmn@yandex.ru>)
}

unit uzeffLibreDWG;
{$Include zengineconfig.inc}
{$Mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
interface
uses
  uzbLogIntf, LazLoggerBase,
  SysUtils,
  dwg,dwgproc,
  uzeffmanager,
  uzelongprocesssupport,{uzgldrawcontext,}forms,
  uzcstrconsts,uzeLogIntf,
  uzedrawingsimple,
  uzedwgentityregistry,
  uzedwgimport,
  LazUTF8;

// Re-exported alias kept for callers that referenced the type previously
// defined in this unit (the actual definition now lives in
// uzedwgentityregistry.pas — see TZ_DWG_LOAD_TO_ZCAD_AUDIT R5).
type
  TZCADDWGParser = uzedwgentityregistry.TZCADDWGParser;

procedure addfromdwg(const filename:String;var ZCDCtx:TZDrawingContext;const LogProc:TZELogProc=nil);
procedure addfromdxf(const filename:String;var ZCDCtx:TZDrawingContext;const LogProc:TZELogProc=nil);

implementation

uses
  uzeffLibreDWG2Ents;

procedure DebugDWG(dwg:PDwg_Data);
begin
  DebugLn(['{WH}header.version: '+DWG_V2Str(dwg^.header.version)]);
  zDebugLn(['{WH}header.from_version: ',DWG_V2Str(dwg^.header.from_version)]);
  zDebugLn(['{WH}header.is_maint: ',dwg^.header.is_maint]);
  zDebugLn(['{WH}header.zero_one_or_three: ',dwg^.header.zero_one_or_three]);
  zDebugLn(['{WH}header.numheader_vars: ',dwg^.header.numheader_vars]);
  zDebugLn(['{WH}header.thumbnail_address: ',dwg^.header.thumbnail_address]);
  zDebugLn(['{WH}header.dwg_version: ',dwg^.header.dwg_version]);
  zDebugLn(['{WH}header.maint_version: ',dwg^.header.maint_version]);
  zDebugLn(['{WH}header.codepage: ',dwg^.header.codepage]);
  zDebugLn(['{WH}dwg.counts: classes=',dwg^.num_classes,
    ', objects=',dwg^.num_objects,
    ', alloced_objects=',dwg^.num_alloced_objects,
    ', entities=',dwg^.num_entities,
    ', object_refs=',dwg^.num_object_refs]);
end;

procedure PLP(const Data:TData;const Counter:TCounter);
begin
 lps.ProgressLongProcess(TLPSHandle(Data),Counter);
end;

procedure addfromdwg(const filename:String;var ZCDCtx:TZDrawingContext;const LogProc:TZELogProc=nil);
var
  dwg:Dwg_Data;
  Success:integer;
  lph:TLPSHandle;
  Loaded:Boolean;
begin
  zDebugLn('{WH}%s',[rsNotYetImplemented]);
  try
    LoadLibreDWG;
  except
    on E : Exception do begin
      zDebugLn(['{EHM}LibreDWG: ',E.Message]);
      exit;
    end;
  end;
  dwg:=default(Dwg_Data);
  dwg.opts:=0;
  Loaded:=False;
  zDebugLn(['{WH}try load file: ',ansistring(filename)]);
  lph:=lps.StartLongProcess('LibreDWG.dwg_read_file',nil);
  try
    {$IFDEF WINDOWS}
    Success:=dwg_read_file(pchar(UTF8ToWinCP(filename)),@dwg);
    {$ELSE WINDOWS}
    Success:=dwg_read_file(pchar(ansistring(filename)),@dwg);
    {$ENDIF}
    Loaded:=True;
    lps.EndLongProcess(lph);
    zDebugLn(['{WH}LibreDWG read code: ',Success,' (',
      DWGReadCodeToText(Success),')']);
    DebugDWG(@dwg);
    if DWGReadCodeIsCritical(Success) then begin
      zDebugLn(['{EHM}LibreDWG: critical read error code ',Success,' (',
        DWGReadCodeToText(Success),'), aborting parse']);
      exit;
    end;
    lph:=lps.StartLongProcess('Parse DWG data',nil,dwg.num_objects);
    try
      // Stage 2 (TZ §12.2): wrap parseDwg_Data with the load context so the
      // LINE handler can register shells + pending owners and the resolver
      // attaches everything in dependency order after parseDwg_Data returns.
      // R4 (TZ §3.4): ScanDWGImport runs the Phase 1 raw scan between Begin
      // and parseDwg_Data so duplicate-handle detection and raw-index capture
      // happen once, before any mapper allocation.
      BeginDWGImport(ZCDCtx);
      try
        ScanDWGImport(dwg);
        GetDWGParser.parseDwg_Data(ZCDCtx,dwg,@PLP,TData(lph));
      finally
        EndDWGImport(ZCDCtx);
      end;
    finally
      lps.EndLongProcess(lph);
    end;
  finally
    if Loaded and Assigned(dwg_free) then
      dwg_free(@dwg);
  end;
end;
procedure addfromdxf(const filename:String;var ZCDCtx:TZDrawingContext;const LogProc:TZELogProc=nil);
var
  dwg:Dwg_Data;
  Success:integer;
  lph:TLPSHandle;
  Loaded:Boolean;
begin
  zDebugLn('{WH}%s',[rsNotYetImplemented]);
  try
    LoadLibreDWG;
  except
    on E : Exception do begin
      debugln('{EHM}LibreDWG: ',E.Message);
      exit;
    end;
  end;
  dwg:=default(Dwg_Data);
  dwg.opts:=0;
  Loaded:=False;
  zDebugLn(['{WH}try load file: ',ansistring(filename)]);
  lph:=lps.StartLongProcess('LibreDWG.dxf_read_file',nil);
  try
    Success:=dxf_read_file(pchar(ansistring(filename)),@dwg);
    Loaded:=True;
    lps.EndLongProcess(lph);
    zDebugLn(['{WH}LibreDWG read code: ',Success,' (',
      DWGReadCodeToText(Success),')']);
    DebugDWG(@dwg);
    if DWGReadCodeIsCritical(Success) then begin
      zDebugLn(['{EHM}LibreDWG: critical dxf read error code ',Success,' (',
        DWGReadCodeToText(Success),'), aborting parse']);
      exit;
    end;
    lph:=lps.StartLongProcess('Parse DWG data',nil,dwg.num_objects);
    try
      BeginDWGImport(ZCDCtx);
      try
        ScanDWGImport(dwg);
        GetDWGParser.parseDwg_Data(ZCDCtx,dwg,@PLP,TData(lph));
      finally
        EndDWGImport(ZCDCtx);
      end;
    finally
      lps.EndLongProcess(lph);
    end;
  finally
    if Loaded and Assigned(dwg_free) then
      dwg_free(@dwg);
  end;
end;

end.
