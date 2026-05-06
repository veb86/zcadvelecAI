{*************************************************************************** }
{  fpdwg - DWG safe text decoder (refactored 5.x R3)                         }
{                                                                            }
{        Copyright (C) 2026 Andrey Zubarev <zamtmn@yandex.ru>                }
{                                                                            }
{  This library is free software, licensed under the terms of the GNU        }
{  General Public License as published by the Free Software Foundation,      }
{  either version 3 of the License, or (at your option) any later version.   }
{*************************************************************************** }

{ Refactor R3 (per TZ_DWG_LOAD_TO_ZCAD_AUDIT §3.3): BITCODE_T2Text and
  DWGSafeDecodeText extracted from dwgproc.pp so the binding unit can shed
  its non-binding helpers. The version-based DWGSafeDecodeText is the
  primary entry point because it is also what TZ_loadDWG.md §support
  expects from the future codepage layer. The TDWGCtx-based BITCODE_T2Text
  variant continues to live in dwgproc.pp as a thin shim that delegates
  here, so existing callers that already hold a TDWGCtx do not have to
  reach into its internals to get a version. }

unit uzedwgtext;

{$IFDEF FPC}
  {$PACKRECORDS C}
  {$Mode objfpc}{$H+}
{$ENDIF}

interface

uses
  dwg;

{ Decode a LibreDWG-allocated BITCODE_T payload into a Pascal string. ANSI
  for DWG <= R2004, UTF-16LE for newer versions. Tolerates a nil pointer:
  the caller gets an empty string instead of an AV. }
procedure DWGSafeDecodeText(const p: BITCODE_T; Version: DWG_VERSION_TYPE;
  out text: string);

implementation

procedure DWGSafeDecodeText(const p: BITCODE_T; Version: DWG_VERSION_TYPE;
  out text: string);
begin
  text := '';
  if p = nil then
    Exit;
  if Version <= R_2004 then
    text := pchar(p)
  else
    text := punicodechar(p);
end;

end.
