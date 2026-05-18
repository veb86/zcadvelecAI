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
  uzeffLibreDWG2Ents,
  uzedwglog;

procedure DebugDWG(dwg:PDwg_Data);
begin
  DWGLogInfoFormatStr('header.version: %s',
    [DWG_V2Str(dwg^.header.version)]);
  DWGLogInfoFormatStr('header.from_version: %s',
    [DWG_V2Str(dwg^.header.from_version)]);
  DWGLogInfoFormatStr('header.is_maint: %s',
    [IntToStr(dwg^.header.is_maint)]);
  DWGLogInfoFormatStr('header.zero_one_or_three: %s',
    [IntToStr(dwg^.header.zero_one_or_three)]);
  DWGLogInfoFormatStr('header.numheader_vars: %s',
    [IntToStr(dwg^.header.numheader_vars)]);
  DWGLogInfoFormatStr('header.thumbnail_address: %s',
    [IntToStr(dwg^.header.thumbnail_address)]);
  DWGLogInfoFormatStr('header.dwg_version: %s',
    [IntToStr(dwg^.header.dwg_version)]);
  DWGLogInfoFormatStr('header.maint_version: %s',
    [IntToStr(dwg^.header.maint_version)]);
  DWGLogInfoFormatStr('header.codepage: %s',
    [IntToStr(dwg^.header.codepage)]);
  DWGLogInfoFormatStr(
    'dwg.counts: classes=%d, objects=%d, alloced_objects=%d, entities=%d, object_refs=%d',
    [Integer(dwg^.num_classes), Integer(dwg^.num_objects),
     Integer(dwg^.num_alloced_objects), Integer(dwg^.num_entities),
     Integer(dwg^.num_object_refs)]);
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
  DWGLogInfoFormatStr('%s', [rsNotYetImplemented]);
  try
    LoadLibreDWG;
  except
    on E : Exception do begin
      DWGLogErrorFormatStr('LibreDWG: %s', [E.Message]);
      exit;
    end;
  end;
  dwg:=default(Dwg_Data);
  dwg.opts:=0;
  Loaded:=False;
  DWGLogInfoFormatStr('try load file: %s', [filename]);
  lph:=lps.StartLongProcess('LibreDWG.dwg_read_file',nil);
  try
    {$IFDEF WINDOWS}
    Success:=dwg_read_file(pchar(UTF8ToWinCP(filename)),@dwg);
    {$ELSE WINDOWS}
    Success:=dwg_read_file(pchar(ansistring(filename)),@dwg);
    {$ENDIF}
    Loaded:=True;
    lps.EndLongProcess(lph);
    DWGLogInfoFormatStr('LibreDWG read code: %d (%s)',
      [Success, DWGReadCodeToText(Success)]);
    DebugDWG(@dwg);
    if DWGReadCodeIsCritical(Success) then begin
      DWGLogErrorFormatStr(
        'LibreDWG: critical read error code %d (%s), aborting parse',
        [Success, DWGReadCodeToText(Success)]);
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
      // Issue #1198 P3: forward the source path so EndDWGImport can emit
      // diagnostic side-files next to the DWG when DWG_DIAG_MODE enables them.
      BeginDWGImport(ZCDCtx, filename);
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
  DWGLogInfoFormatStr('%s', [rsNotYetImplemented]);
  try
    LoadLibreDWG;
  except
    on E : Exception do begin
      DWGLogErrorFormatStr('LibreDWG: %s', [E.Message]);
      exit;
    end;
  end;
  dwg:=default(Dwg_Data);
  dwg.opts:=0;
  Loaded:=False;
  DWGLogInfoFormatStr('try load file: %s', [filename]);
  lph:=lps.StartLongProcess('LibreDWG.dxf_read_file',nil);
  try
    Success:=dxf_read_file(pchar(ansistring(filename)),@dwg);
    Loaded:=True;
    lps.EndLongProcess(lph);
    DWGLogInfoFormatStr('LibreDWG read code: %d (%s)',
      [Success, DWGReadCodeToText(Success)]);
    DebugDWG(@dwg);
    if DWGReadCodeIsCritical(Success) then begin
      DWGLogErrorFormatStr(
        'LibreDWG: critical dxf read error code %d (%s), aborting parse',
        [Success, DWGReadCodeToText(Success)]);
      exit;
    end;
    lph:=lps.StartLongProcess('Parse DWG data',nil,dwg.num_objects);
    try
      // Issue #1198 P3: forward the source path so EndDWGImport can emit
      // diagnostic side-files next to the DXF when DWG_DIAG_MODE enables them.
      BeginDWGImport(ZCDCtx, filename);
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
