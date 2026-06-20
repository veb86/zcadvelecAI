{
*****************************************************************************
*                                                                           *
*  This file is part of the ZCAD                                            *
*                                                                           *
*  See the file COPYING.txt, included in this distribution,                 *
*  for details about the copyright.                                         *
*                                                                           *
*  This program is distributed in the hope that it will be useful,          *
*  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
*                                                                           *
*****************************************************************************
}
{
@author(Vladimir Bobrov)
}

unit uzvfspellzoom;

{$mode objfpc}{$H+}

interface

uses
  uzvfspelldata,gzctnrVectorTypes, uzegeometrytypes;

function TryGetSpellErrorWordVolume(ErrorPtr: PSpellError;
  out Volume: TBoundingBox): boolean;
function ZoomToSpellEntity(ErrorPtr: PSpellError): boolean;
function ZoomToSpellError(ErrorPtr: PSpellError): boolean;

implementation

uses
  SysUtils, StrUtils, Math,
  uzclog,
  uzcdrawing, uzcdrawings,
  uzeconsts, uzeentity, uzeenttext, uzeentmtext, uzegeometry, uzetypes,
  uzefont, uzglgeometry, uzgldrawcontext;

const
  WORD_VOLUME_PADDING_FACTOR = 0.35;
  // Коэффициент увеличения размера слова перед зуммированием.
  // Слово зуммируется так, будто оно в WORD_ZOOM_ENLARGE_FACTOR раз больше,
  // чтобы зум не был слишком близким (см. issue #1296).
  WORD_ZOOM_ENLARGE_FACTOR = 3;

function IsZoomTextEntity(EntityPtr: PGDBObjEntity): boolean;
begin
  Result := False;

  if not Assigned(EntityPtr) then
    Exit;

  case EntityPtr^.GetObjType of
    GDBTextID, GDBMTextID:
      Result := True;
  end;
end;

function GetZoomTextContent(EntityPtr: PGDBObjEntity): string;
begin
  Result := '';

  if not IsZoomTextEntity(EntityPtr) then
    Exit;

  Result := string(PGDBObjText(EntityPtr)^.Content);
  if Result = '' then
    Result := string(PGDBObjText(EntityPtr)^.Template);
end;

function ResolveErrorPosition(const EntityText: string;
  ErrorPtr: PSpellError): integer;
begin
  Result := 0;

  if not Assigned(ErrorPtr) or (ErrorPtr^.ErrorWord = '') then
    Exit;

  Result := ErrorPtr^.EntityPosition;
  if (Result >= 1) and
     (Copy(EntityText, Result, Length(ErrorPtr^.ErrorWord)) =
       ErrorPtr^.ErrorWord) then
    Exit;

  Result := Pos(ErrorPtr^.ErrorWord, EntityText);
end;

function HasUsableFont(TextObj: PGDBObjText): boolean;
begin
  Result := Assigned(TextObj) and Assigned(TextObj^.TXTStyle) and
    Assigned(TextObj^.TXTStyle^.pfont) and
    Assigned(TextObj^.TXTStyle^.pfont^.font);
end;

function MeasureTextAdvance(TextObj: PGDBObjText;
  const AText: TDXFEntsInternalStringType; AStart, ACount: integer): double;
var
  font: pgdbfont;
  i: integer;
  endPosition: integer;
  symbolLength: integer;
  symbol: word;
begin
  Result := 0;

  if not HasUsableFont(TextObj) or (ACount <= 0) then
    Exit;

  if AStart < 1 then
    AStart := 1;

  font := TextObj^.TXTStyle^.pfont;
  i := AStart;
  endPosition := AStart + ACount - 1;
  while (i <= Length(AText)) and (i <= endPosition) do begin
    symbol := getsymbol_fromGDBText(AText, i, symbolLength,
      font^.font.IsUnicode);
    if symbolLength <= 0 then
      symbolLength := 1;

    if symbol <> 1 then
      Result := Result + font^.GetOrReplaceSymbolInfo(symbol)^.NextSymX;

    Inc(i, symbolLength);
  end;
end;

procedure InitVolumeFromPoint(out Volume: TBoundingBox;
  const Point: TzePoint3d);
begin
  Volume.LBN := Point;
  Volume.RTF := Point;
end;

procedure AddPointToVolume(var Volume: TBoundingBox; const Point: TzePoint3d);
begin
  if Point.x < Volume.LBN.x then
    Volume.LBN.x := Point.x;
  if Point.y < Volume.LBN.y then
    Volume.LBN.y := Point.y;
  if Point.z < Volume.LBN.z then
    Volume.LBN.z := Point.z;

  if Point.x > Volume.RTF.x then
    Volume.RTF.x := Point.x;
  if Point.y > Volume.RTF.y then
    Volume.RTF.y := Point.y;
  if Point.z > Volume.RTF.z then
    Volume.RTF.z := Point.z;
end;

// Увеличить объём слова относительно его центра в Factor раз,
// чтобы зум был не таким близким (см. issue #1296).
procedure EnlargeVolume(var Volume: TBoundingBox; Factor: double);
var
  center: TzePoint3d;
begin
  if Factor <= 1 then
    Exit;

  center.x := (Volume.LBN.x + Volume.RTF.x) / 2;
  center.y := (Volume.LBN.y + Volume.RTF.y) / 2;
  center.z := (Volume.LBN.z + Volume.RTF.z) / 2;

  Volume.LBN.x := center.x + (Volume.LBN.x - center.x) * Factor;
  Volume.LBN.y := center.y + (Volume.LBN.y - center.y) * Factor;
  Volume.LBN.z := center.z + (Volume.LBN.z - center.z) * Factor;
  Volume.RTF.x := center.x + (Volume.RTF.x - center.x) * Factor;
  Volume.RTF.y := center.y + (Volume.RTF.y - center.y) * Factor;
  Volume.RTF.z := center.z + (Volume.RTF.z - center.z) * Factor;
end;

procedure PadVolume(var Volume: TBoundingBox; Padding: double);
begin
  if Padding < eps then
    Padding := 1;

  Volume.LBN.x := Volume.LBN.x - Padding;
  Volume.LBN.y := Volume.LBN.y - Padding;
  Volume.LBN.z := Volume.LBN.z - Padding;
  Volume.RTF.x := Volume.RTF.x + Padding;
  Volume.RTF.y := Volume.RTF.y + Padding;
  Volume.RTF.z := Volume.RTF.z + Padding;
end;

function TransformTextPoint(TextObj: PGDBObjText;
  const TextMatrix: TzeTypedMatrix4d; X, Y: double): TzePoint3d;
begin
  Result := CreateVertex(X, Y, 0);
  Result := VectorTransform3D(Result, TextMatrix);
  Result := VectorTransform3D(Result, TextObj^.objmatrix);
end;

function BuildWordVolume(TextObj: PGDBObjText;
  const TextMatrix: TzeTypedMatrix4d; WordLeft, WordRight: double;
  out Volume: TBoundingBox): boolean;
var
  point: TzePoint3d;
begin
  Result := False;

  if not Assigned(TextObj) or (WordRight <= WordLeft) then
    Exit;

  point := TransformTextPoint(TextObj, TextMatrix, WordLeft, 0);
  InitVolumeFromPoint(Volume, point);

  point := TransformTextPoint(TextObj, TextMatrix, WordRight, 0);
  AddPointToVolume(Volume, point);
  point := TransformTextPoint(TextObj, TextMatrix, WordRight, 1);
  AddPointToVolume(Volume, point);
  point := TransformTextPoint(TextObj, TextMatrix, WordLeft, 1);
  AddPointToVolume(Volume, point);

  PadVolume(Volume, TextObj^.textprop.size * WORD_VOLUME_PADDING_FACTOR);
  Result := True;
end;

function TryGetTextWordVolume(TextObj: PGDBObjText; ErrorPtr: PSpellError;
  out Volume: TBoundingBox): boolean;
var
  entityText: string;
  wordStart: integer;
  prefixWidth: double;
  wordWidth: double;
begin
  Result := False;

  if not Assigned(TextObj) or not Assigned(ErrorPtr) then
    Exit;

  entityText := GetZoomTextContent(PGDBObjEntity(TextObj));
  wordStart := ResolveErrorPosition(entityText, ErrorPtr);
  if wordStart < 1 then
    Exit;

  prefixWidth := MeasureTextAdvance(TextObj,
    TDXFEntsInternalStringType(entityText), 1, wordStart - 1);
  wordWidth := MeasureTextAdvance(TextObj,
    TDXFEntsInternalStringType(entityText), wordStart,
    Length(ErrorPtr^.ErrorWord));
  if wordWidth < eps then
    wordWidth := 1;

  Result := BuildWordVolume(TextObj, TextObj^.DrawMatrix, prefixWidth,
    prefixWidth + wordWidth, Volume);
end;

function BuildMTextLineMatrix(MTextObj: PGDBObjMText;
  LinePtr: pGDBStrWithPoint): TzeTypedMatrix4d;
var
  lineTranslation: TzeTypedMatrix4d;
  obliqueOffset: double;
begin
  Result := MTextObj^.DrawMatrix;

  obliqueOffset := 0;
  if Abs(MTextObj^.textprop.wfactor) > eps then
    obliqueOffset := LinePtr^.y * cotan(Pi / 2 - MTextObj^.textprop.oblique) /
      MTextObj^.textprop.wfactor;

  lineTranslation := CreateTranslationMatrix(
    CreateVertex(LinePtr^.x - obliqueOffset, LinePtr^.y, 0));
  Result := MatrixMultiply(lineTranslation, Result);
end;

function TryFindMTextLineWord(MTextObj: PGDBObjMText;
  ErrorPtr: PSpellError; out LinePtr: pGDBStrWithPoint;
  out WordOffset: integer): boolean;
var
  entityText: string;
  lineText: string;
  wordStart: integer;
  scanFrom: integer;
  lineStart: integer;
  lineEnd: integer;
  iterator: itrec;
begin
  Result := False;
  LinePtr := nil;
  WordOffset := 0;

  if not Assigned(MTextObj) or not Assigned(ErrorPtr) then
    Exit;

  entityText := GetZoomTextContent(PGDBObjEntity(MTextObj));
  wordStart := ResolveErrorPosition(entityText, ErrorPtr);
  if wordStart < 1 then
    Exit;

  scanFrom := 1;
  LinePtr := MTextObj^.Text.beginiterate(iterator);
  if LinePtr <> nil then
    repeat
      lineText := string(LinePtr^.str);
      if lineText = '' then
        lineStart := scanFrom
      else
        lineStart := PosEx(lineText, entityText, scanFrom);

      if lineStart < 1 then
        lineStart := scanFrom;

      lineEnd := lineStart + Length(lineText) - 1;
      if (wordStart >= lineStart) and
         (wordStart + Length(ErrorPtr^.ErrorWord) - 1 <= lineEnd) then begin
        WordOffset := wordStart - lineStart + 1;
        Result := True;
        Exit;
      end;

      scanFrom := lineEnd + 1;
      LinePtr := MTextObj^.Text.iterate(iterator);
    until LinePtr = nil;
end;

function TryGetMTextWordVolume(MTextObj: PGDBObjMText; ErrorPtr: PSpellError;
  out Volume: TBoundingBox): boolean;
var
  linePtr: pGDBStrWithPoint;
  wordOffset: integer;
  prefixWidth: double;
  wordWidth: double;
  lineMatrix: TzeTypedMatrix4d;
begin
  Result := False;

  if not TryFindMTextLineWord(MTextObj, ErrorPtr, linePtr, wordOffset) then
    Exit;

  prefixWidth := MeasureTextAdvance(PGDBObjText(MTextObj), linePtr^.str, 1,
    wordOffset - 1);
  wordWidth := MeasureTextAdvance(PGDBObjText(MTextObj), linePtr^.str,
    wordOffset, Length(ErrorPtr^.ErrorWord));
  if wordWidth < eps then
    wordWidth := 1;

  lineMatrix := BuildMTextLineMatrix(MTextObj, linePtr);
  Result := BuildWordVolume(PGDBObjText(MTextObj), lineMatrix, prefixWidth,
    prefixWidth + wordWidth, Volume);
end;

function FormatSpellEntity(EntityPtr: PGDBObjEntity): boolean;
var
  drawing: PTZCADDrawing;
  drawContext: TDrawContext;
begin
  Result := False;

  if not Assigned(EntityPtr) or (drawings.GetCurrentDWG = nil) then
    Exit;

  drawing := PTZCADDrawing(drawings.GetCurrentDWG);
  drawContext := drawing^.CreateDrawingRC;
  EntityPtr^.FormatEntity(drawing^, drawContext);
  Result := True;
end;

function TryGetSpellErrorWordVolume(ErrorPtr: PSpellError;
  out Volume: TBoundingBox): boolean;
var
  entityPtr: PGDBObjEntity;
begin
  Result := False;

  if not Assigned(ErrorPtr) or not Assigned(ErrorPtr^.EntityPtr) then
    Exit;

  entityPtr := PGDBObjEntity(ErrorPtr^.EntityPtr);
  if not IsZoomTextEntity(entityPtr) or not FormatSpellEntity(entityPtr) then
    Exit;

  case entityPtr^.GetObjType of
    GDBMTextID:
      Result := TryGetMTextWordVolume(PGDBObjMText(entityPtr), ErrorPtr,
        Volume);
    GDBTextID:
      Result := TryGetTextWordVolume(PGDBObjText(entityPtr), ErrorPtr,
        Volume);
  end;
end;

function ZoomToSpellEntity(ErrorPtr: PSpellError): boolean;
var
  entityPtr: PGDBObjEntity;
  drawing: PTZCADDrawing;
  drawContext: TDrawContext;
  volume: TBoundingBox;
begin
  Result := False;

  if not Assigned(ErrorPtr) or not Assigned(ErrorPtr^.EntityPtr) or
     (drawings.GetCurrentDWG = nil) then
    Exit;

  entityPtr := PGDBObjEntity(ErrorPtr^.EntityPtr);
  if not FormatSpellEntity(entityPtr) then
    Exit;

  drawing := PTZCADDrawing(drawings.GetCurrentDWG);
  drawContext := drawing^.CreateDrawingRC;
  entityPtr^.getonlyoutbound(drawContext);
  volume := entityPtr^.vp.BoundingBox;
  drawings.GetCurrentDWG^.wa.ZoomToVolume(volume);
  Result := True;
end;

function ZoomToSpellError(ErrorPtr: PSpellError): boolean;
var
  volume: TBoundingBox;
begin
  Result := False;

  if not Assigned(ErrorPtr) then
    Exit;

  if TryGetSpellErrorWordVolume(ErrorPtr, volume) then begin
    EnlargeVolume(volume, WORD_ZOOM_ENLARGE_FACTOR);
    drawings.GetCurrentDWG^.wa.ZoomToVolume(volume);
    programlog.LogOutFormatStr(
      'ZoomToSpellError: zoomed word "%s" at entity position %d',
      [ErrorPtr^.ErrorWord, ErrorPtr^.EntityPosition], LM_Info);
    Result := True;
    Exit;
  end;

  Result := ZoomToSpellEntity(ErrorPtr);
  if Result then
    programlog.LogOutFormatStr(
      'ZoomToSpellError: zoomed entity for word "%s"',
      [ErrorPtr^.ErrorWord], LM_Info)
  else
    programlog.LogOutFormatStr(
      'ZoomToSpellError: unable to zoom word "%s"',
      [ErrorPtr^.ErrorWord], LM_Info);
end;

end.
