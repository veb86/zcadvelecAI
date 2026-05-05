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
  LazUTF8;

type

  TZCADDWGParser=specialize GDWGParser<TZDrawingContext>;

var
  ZCDWGParser:TZCADDWGParser=nil;

procedure addfromdwg(const filename:String;var ZCDCtx:TZDrawingContext;const LogProc:TZELogProc=nil);
procedure addfromdxf(const filename:String;var ZCDCtx:TZDrawingContext;const LogProc:TZELogProc=nil);

implementation

uses
  uzeffLibreDWG2Ents;

procedure DebugDWG(dwg:PDwg_Data);
begin
  DebugLn(['{WH}header.version: '+DWG_V2Str(dwg^.header.version)]);
  zDebugLn(['{WH}header.from_version: ',DWG_V2Str(dwg^.header.from_version)]);
  //if (dwg^.header.zero_5[0]=0)and(dwg^.header.zero_5[1]=0)and(dwg^.header.zero_5[2]=0)and(dwg^.header.zero_5[3]=0)and(dwg^.header.zero_5[4]=0)then
  //  zDebugLn(['{WH}header.zero_5: 0,0,0,0,0'])
  //else
  //  zDebugLn(['{WHM}header.zero_5: ',dwg^.header.zero_5[0],',',dwg^.header.zero_5[1],',',dwg^.header.zero_5[2],',',dwg^.header.zero_5[3],',',dwg^.header.zero_5[4]]);
  zDebugLn(['{WH}header.is_maint: ',dwg^.header.is_maint]);
  zDebugLn(['{WH}header.zero_one_or_three: ',dwg^.header.zero_one_or_three]);
  //zDebugLn(['{WH}header.unknown_3: ',dwg^.header.unknown_3]);
  zDebugLn(['{WH}header.numheader_vars: ',dwg^.header.numheader_vars]);
  zDebugLn(['{WH}header.thumbnail_address: ',dwg^.header.thumbnail_address]);
  zDebugLn(['{WH}header.dwg_version: ',dwg^.header.dwg_version]);
  zDebugLn(['{WH}header.maint_version: ',dwg^.header.maint_version]);
  zDebugLn(['{WH}header.codepage: ',dwg^.header.codepage]);
end;

procedure PLP(const Data:TData;const Counter:TCounter);
begin
 lps.ProgressLongProcess(TLPSHandle(Data),Counter);
end;

function IsCriticalDWGReadError(Code:integer):Boolean;
begin
  // LibreDWG dwg_read_file/dxf_read_file return non-zero on critical failure.
  // The high bit (>=$80) signals an unrecoverable error per LibreDWG conventions;
  // lower bits are informational (incomplete, partially loaded), so we only abort
  // on the critical band.
  Result:=(Code and $80)<>0;
end;

procedure addfromdwg(const filename:String;var ZCDCtx:TZDrawingContext;const LogProc:TZELogProc=nil);
var
  dwg:Dwg_Data;
  Success:integer;
  lph:TLPSHandle;
  Loaded:Boolean;
  //DC:TDrawContext;
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
  //fillchar(dwg,sizeof(dwg),0);
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
    zDebugLn(['{WH}Success: ',Success]);
    if IsCriticalDWGReadError(Success) then begin
      zDebugLn(['{EHM}LibreDWG: critical read error code ',Success,', aborting parse']);
      exit;
    end;
    DebugDWG(@dwg);
    lph:=lps.StartLongProcess('Parse DWG data',nil,dwg.num_objects);
    try
      // Stage 2 (TZ §12.2): wrap parseDwg_Data with the load context so the
      // LINE handler can register shells + pending owners and the resolver
      // attaches everything in dependency order after parseDwg_Data returns.
      BeginDWGImport(ZCDCtx);
      try
        ZCDWGParser.parseDwg_Data(ZCDCtx,dwg,@PLP,TData(lph));
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
  //DC:TDrawContext;
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
  //fillchar(dwg,sizeof(dwg),0);
  dwg:=default(Dwg_Data);
  dwg.opts:=0;
  Loaded:=False;
  zDebugLn(['{WH}try load file: ',ansistring(filename)]);
  lph:=lps.StartLongProcess('LibreDWG.dxf_read_file',nil);
  try
    Success:=dxf_read_file(pchar(ansistring(filename)),@dwg);
    Loaded:=True;
    lps.EndLongProcess(lph);
    zDebugLn(['{WH}Success: ',Success]);
    if IsCriticalDWGReadError(Success) then begin
      zDebugLn(['{EHM}LibreDWG: critical dxf read error code ',Success,', aborting parse']);
      exit;
    end;
    DebugDWG(@dwg);
    lph:=lps.StartLongProcess('Parse DWG data',nil,dwg.num_objects);
    try
      BeginDWGImport(ZCDCtx);
      try
        ZCDWGParser.parseDwg_Data(ZCDCtx,dwg,@PLP,TData(lph));
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

initialization
 ZCDWGParser:=TZCADDWGParser.Create;
finalization
 FreeAndNil(ZCDWGParser);
end.
