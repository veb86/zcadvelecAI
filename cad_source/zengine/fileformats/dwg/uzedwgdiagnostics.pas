{*************************************************************************** }
{  fpdwg - DWG to ZCAD import context: diagnostics (Stage 5.x R2)            }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Refactor R2 (per TZ_DWG_LOAD_TO_ZCAD_AUDIT §3.2 / TZ §6.5):
  the warning collector and resolver counters that used to live inline in
  TDWGZCADLoadContext. Pulling them into a dedicated unit keeps the load
  context focused on shell registry + pending queues, and gives the resolver
  a small, tested surface to push warnings to without touching the context's
  private state. }

unit uzedwgdiagnostics;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

uses
  SysUtils, uzedwgtypes;

type
  { Sequential append-only warning log produced during a single import.
    Backed by a dynamic array so consumers can iterate via WarningCount /
    WarningAt without exposing the internal storage. }
  TDWGImportWarningList = class
  private
    FItems: array of TDWGImportWarning;
  public
    procedure Add(Severity: TDWGImportSeverity; Code: Integer;
      Handle: TDWGZCADHandle; const Text: String);
    function Count: Integer;
    function Item(Index: Integer): TDWGImportWarning;
    procedure Clear;
  end;

  { Resolver counters: one slot per attach outcome. Kept as a plain record
    so the load context can expose them as read-only properties without
    re-implementing the increment logic in three places. }
  TDWGImportStats = record
    AttachCount: Integer;
    FallbackCount: Integer;
    CycleCount: Integer;
    RefAttachCount: Integer;
    RefFallbackCount: Integer;
    UnknownEntities: Integer;
    UnknownObjects: Integer;
    ProxiesLoaded: Integer;
    ProxiesFailed: Integer;
    DroppedDueToFreedRaw: Integer;
    procedure Clear;
  end;
  PDWGImportStats = ^TDWGImportStats;

implementation

procedure TDWGImportWarningList.Add(Severity: TDWGImportSeverity;
  Code: Integer; Handle: TDWGZCADHandle; const Text: String);
var
  W: TDWGImportWarning;
begin
  W.Severity := Severity;
  W.Code := Code;
  W.Handle := Handle;
  W.Text := Text;
  SetLength(FItems, Length(FItems) + 1);
  FItems[High(FItems)] := W;
end;

function TDWGImportWarningList.Count: Integer;
begin
  Result := Length(FItems);
end;

function TDWGImportWarningList.Item(Index: Integer): TDWGImportWarning;
begin
  if (Index < 0) or (Index > High(FItems)) then
    raise EDWGLoadContext.CreateFmt('Warning index %d out of range', [Index]);
  Result := FItems[Index];
end;

procedure TDWGImportWarningList.Clear;
begin
  SetLength(FItems, 0);
end;

procedure TDWGImportStats.Clear;
begin
  AttachCount := 0;
  FallbackCount := 0;
  CycleCount := 0;
  RefAttachCount := 0;
  RefFallbackCount := 0;
  UnknownEntities := 0;
  UnknownObjects := 0;
  ProxiesLoaded := 0;
  ProxiesFailed := 0;
  DroppedDueToFreedRaw := 0;
end;

end.
