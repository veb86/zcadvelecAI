unit uzvhttpwallxdata;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, gzctnrVectorTypes, uzctnrVectorBytesStream, usimplegenerics,
  uzeTypes, uzeBaseExtender, uzeExtdrBaseEntityExtender, uzeentity, uzedrawingdef,
  uzeffdxfsupport, uzgldrawcontext, uzeentgenericsubentry,
  uzvhttpwallgeometry, uzvhttpwallcollector;

type
  TWallField = (wfObjectType, wfWallID, wfRepresentation, wfThickness,
    wfConfidence, wfSource, wfSession, wfHandles, wfInputHandles, wfAxis, wfPairThickness);

  TWallMetadata = class(TBaseEntityExtender)
  private
    FValues: array[TWallField] of string;
    FSource: array[0..1] of TWallSource;
  public
    class function getExtenderName: string; override;
    procedure Assign(Source: TBaseExtender); override;
    procedure CopyExt2Ent(pSourceEntity, pDestEntity: Pointer); override;
    procedure onRemoveFromArray(Entity: Pointer; const Drawing: TDrawingDef); override;
    procedure onBeforeEntityFormat(Entity: Pointer; const Drawing: TDrawingDef;
      var DC: TDrawContext); override;
    procedure onAfterEntityFormat(Entity: Pointer; const Drawing: TDrawingDef;
      var DC: TDrawContext); override;
    procedure onEntityClone(pSourceEntity, pDestEntity: Pointer); override;
    procedure onEntityBuildVarGeometry(Entity: Pointer; const Drawing: TDrawingDef); override;
    procedure onEntitySupportOldVersions(Entity: Pointer; const Drawing: TDrawingDef); override;
    procedure ReorganizeEnts(OldEnts2NewEntsMap: TMapPointerToPointer); override;
    procedure PostLoad(var Context: TIODXFLoadContext); override;
    procedure SaveToDxfObjXData(var Stream: TZctnrVectorBytes; Entity: Pointer;
      var Context: TIODXFSaveContext); override;
  end;

procedure AttachWallMetadata(Entity: PGDBObjEntity; const SessionID: string;
  const Wall: TWallRecognition; const Candidate: TWallCandidate;
  const A, B: TWallSource);
function IsRecognizedWall(Entity: PGDBObjEntity): Boolean;
function AlreadyHasWall(Root: PGDBObjGenericSubEntry; const Candidate: TWallCandidate): Boolean;

implementation

uses
  Math, uzeconsts;

type
  TWallFieldLoader = class
    function LoadField(Name, Value: string; Extension: PExtensionData;
      const Drawing: TDrawingDef; Entity: Pointer): Boolean;
  end;

const
  FieldNames: array[TWallField] of string = ('ZCAD_WALLS_OBJECT_TYPE',
    'ZCAD_WALLS_WALL_ID', 'ZCAD_WALLS_REPRESENTATION', 'ZCAD_WALLS_THICKNESS',
    'ZCAD_WALLS_CONFIDENCE', 'ZCAD_WALLS_SOURCE', 'ZCAD_WALLS_SESSION_ID',
    'ZCAD_WALLS_SOURCE_HANDLES', 'ZCAD_WALLS_INPUT_HANDLES', 'ZCAD_WALLS_CANDIDATE_AXIS',
    'ZCAD_WALLS_CANDIDATE_THICKNESS');

var
  FieldLoader: TWallFieldLoader;

function WallFormat: TFormatSettings;
begin
  Result := DefaultFormatSettings;
  Result.DecimalSeparator := '.';
end;

function WallNumber(Number: Double): string;
begin
  Result := FloatToStrF(Number, ffGeneral, 17, 0, WallFormat);
end;

function EncodePoint(const P: TWallPoint): string;
begin
  Result := WallNumber(P.X) + ',' + WallNumber(P.Y) + ',' + WallNumber(P.Z);
end;

function DecodeAxis(const Value: string; out Axis: TWallAxis): Boolean;
var
  Parts: TStringArray;
begin
  Parts := Value.Split([',']);
  Result := (Length(Parts) = 6) and
    TryStrToFloat(Parts[0], Axis.First.X, WallFormat) and
    TryStrToFloat(Parts[1], Axis.First.Y, WallFormat) and
    TryStrToFloat(Parts[2], Axis.First.Z, WallFormat) and
    TryStrToFloat(Parts[3], Axis.Last.X, WallFormat) and
    TryStrToFloat(Parts[4], Axis.Last.Y, WallFormat) and
    TryStrToFloat(Parts[5], Axis.Last.Z, WallFormat) and
    FiniteWallPoint(Axis.First) and FiniteWallPoint(Axis.Last);
end;

class function TWallMetadata.getExtenderName: string;
begin
  Result := 'ZCAD_WALLS';
end;

procedure TWallMetadata.Assign(Source: TBaseExtender);
begin
  FValues := TWallMetadata(Source).FValues;
  FSource := TWallMetadata(Source).FSource;
end;

procedure TWallMetadata.CopyExt2Ent(pSourceEntity, pDestEntity: Pointer);
var
  Metadata: TWallMetadata;
begin
  Metadata := TWallMetadata(PGDBObjEntity(pDestEntity)^.GetExtension(TWallMetadata));
  if Metadata = nil then begin
    Metadata := TWallMetadata.Create(pDestEntity);
    PGDBObjEntity(pDestEntity)^.AddExtension(Metadata);
  end;
  Metadata.Assign(Self);
end;

procedure TWallMetadata.onRemoveFromArray(Entity: Pointer; const Drawing: TDrawingDef);
begin
end;

procedure TWallMetadata.onBeforeEntityFormat(Entity: Pointer; const Drawing: TDrawingDef;
  var DC: TDrawContext);
begin
end;

procedure TWallMetadata.onAfterEntityFormat(Entity: Pointer; const Drawing: TDrawingDef;
  var DC: TDrawContext);
begin
end;

procedure TWallMetadata.onEntityClone(pSourceEntity, pDestEntity: Pointer);
begin
end;

procedure TWallMetadata.onEntityBuildVarGeometry(Entity: Pointer; const Drawing: TDrawingDef);
begin
end;

procedure TWallMetadata.onEntitySupportOldVersions(Entity: Pointer; const Drawing: TDrawingDef);
begin
end;

procedure TWallMetadata.ReorganizeEnts(OldEnts2NewEntsMap: TMapPointerToPointer);
var
  I: Integer;
  Replacement: Pointer;
begin
  for I := 0 to 1 do
    if (FSource[I].Entity <> nil) and
      OldEnts2NewEntsMap.TryGetValue(FSource[I].Entity, Replacement) then
      FSource[I].Entity := Replacement;
end;

function TWallFieldLoader.LoadField(Name, Value: string; Extension: PExtensionData;
  const Drawing: TDrawingDef; Entity: Pointer): Boolean;
var
  Metadata: TWallMetadata;
  Field: TWallField;
begin
  Metadata := TWallMetadata(PGDBObjEntity(Entity)^.GetExtension(TWallMetadata));
  if Metadata = nil then begin
    Metadata := TWallMetadata.Create(Entity);
    PGDBObjEntity(Entity)^.AddExtension(Metadata);
  end;
  for Field := Low(TWallField) to High(TWallField) do
    if SameText(Name, FieldNames[Field]) then begin
      Metadata.FValues[Field] := Value;
      Exit(True);
    end;
  Result := False;
end;

procedure TWallMetadata.PostLoad(var Context: TIODXFLoadContext);
var
  Handles: TStringArray;
  Entry: TDXFHandle2ZCObject.TPointerWithType;
  H: QWord;
  I: Integer;
begin
  Handles := FValues[wfHandles].Split([',']);
  if Length(Handles) <> 2 then Exit;
  for I := 0 to 1 do begin
    FSource[I].Handle := Handles[I];
    if TryStrToQWord('$' + Handles[I], H) and Context.h2p.TryGetValue(H, Entry) and
      (Entry.&type = OT_Entity) and (Entry.p <> nil) then
      FSource[I].Entity := Entry.p;
  end;
end;

procedure TWallMetadata.SaveToDxfObjXData(var Stream: TZctnrVectorBytes; Entity: Pointer;
  var Context: TIODXFSaveContext);
var
  Field: TWallField;
  Root: PGDBObjGenericSubEntry;
  SourceEntity: PGDBObjEntity;
  Iter: itrec;
  Handles: array[0..1] of string;
  H: TDWGHandle;
  I, Found: Integer;
  Value: string;
begin
  for I := 0 to 1 do Handles[I] := FSource[I].Handle;
  Root := PGDBObjGenericSubEntry(pThisEntity^.bp.ListPos.Owner);
  if (Root <> nil) and ((FSource[0].Entity <> nil) or (FSource[1].Entity <> nil)) then begin
    // В DXF номера назначает экспортер. Используем его таблицу для еще
    // существующих объектов и сохраняем исторические номера удаленных.
    Found := 0;
    SourceEntity := Root^.ObjArray.beginiterate(Iter);
    while (SourceEntity <> nil) and (Found < 2) do begin
      for I := 0 to 1 do
        if (SourceEntity = FSource[I].Entity) and
          (SourceEntity^.GetObjType in [GDBLineID, GDBLWPolylineID]) then begin
          Context.p2h.MyGetOrCreateValue(SourceEntity, Context.handle, H);
          Handles[I] := IntToHex(H, 1);
          Inc(Found);
        end;
      SourceEntity := Root^.ObjArray.iterate(Iter);
    end;
  end;
  for Field := Low(TWallField) to High(TWallField) do begin
    Value := FValues[Field];
    if (Field = wfHandles) and (Handles[0] <> '') and (Handles[1] <> '') then
      Value := Handles[0] + ',' + Handles[1];
    if Value <> '' then
      dxfStringWithoutEncodeOut(Stream, 1000, FieldNames[Field] + '=' + Value);
  end;
end;

procedure AttachWallMetadata(Entity: PGDBObjEntity; const SessionID: string;
  const Wall: TWallRecognition; const Candidate: TWallCandidate;
  const A, B: TWallSource);
var
  Metadata: TWallMetadata;
begin
  Metadata := TWallMetadata.Create(Entity);
  Entity^.AddExtension(Metadata);
  Metadata.FSource[0] := A;
  Metadata.FSource[1] := B;
  Metadata.FValues[wfObjectType] := 'WALL';
  Metadata.FValues[wfWallID] := SessionID + '-C' + IntToStr(Wall.Candidate + 1);
  Metadata.FValues[wfRepresentation] := 'CENTERLINE';
  Metadata.FValues[wfThickness] := WallNumber(Wall.Thickness);
  Metadata.FValues[wfConfidence] := WallNumber(Wall.Confidence);
  Metadata.FValues[wfSource] := 'AI';
  Metadata.FValues[wfSession] := SessionID;
  Metadata.FValues[wfHandles] := A.Handle + ',' + B.Handle;
  Metadata.FValues[wfInputHandles] := Metadata.FValues[wfHandles];
  Metadata.FValues[wfAxis] := EncodePoint(Candidate.Center.First) + ',' +
    EncodePoint(Candidate.Center.Last);
  Metadata.FValues[wfPairThickness] := WallNumber(Candidate.Thickness);
end;

function IsRecognizedWall(Entity: PGDBObjEntity): Boolean;
var
  Metadata: TWallMetadata;
begin
  Metadata := TWallMetadata(Entity^.GetExtension(TWallMetadata));
  Result := (Metadata <> nil) and (Metadata.FValues[wfObjectType] = 'WALL');
end;

function AlreadyHasWall(Root: PGDBObjGenericSubEntry; const Candidate: TWallCandidate): Boolean;
var
  Entity: PGDBObjEntity;
  Metadata: TWallMetadata;
  Axis: TWallAxis;
  Saved: TWallCandidate;
  Thickness: Double;
  Iter: itrec;
  Vertices: TWallVertices;
begin
  SetLength(Vertices, 2);
  Entity := Root^.ObjArray.beginiterate(Iter);
  while Entity <> nil do begin
    if Entity^.GetObjType in [GDBLWPolylineID, GDBPolylineID] then begin
      Metadata := TWallMetadata(Entity^.GetExtension(TWallMetadata));
      if (Metadata <> nil) and (Metadata.FValues[wfObjectType] = 'WALL') and
        TryStrToFloat(Metadata.FValues[wfPairThickness], Thickness, WallFormat) and
        (Abs(Candidate.Thickness - Thickness) <= Max(WallEpsilon, Thickness * 0.001)) and
        DecodeAxis(Metadata.FValues[wfAxis], Axis) then begin
        // Совпадение оси оцениваем с точностью геометрии, а не с допуском ИИ.
        Saved := Candidate;
        Saved.Center := Axis;
        Saved.Thickness := WallEpsilon;
        Vertices[0] := Candidate.Center.First;
        Vertices[1] := Candidate.Center.Last;
        if ValidWallAxis(Vertices, Saved) then Exit(True);
        Vertices[0] := Candidate.Center.Last;
        Vertices[1] := Candidate.Center.First;
        if ValidWallAxis(Vertices, Saved) then Exit(True);
      end;
    end;
    Entity := Root^.ObjArray.iterate(Iter);
  end;
  Result := False;
end;

procedure RegisterFields;
var
  Field: TWallField;
begin
  for Field := Low(TWallField) to High(TWallField) do
    GDBObjEntity.GetDXFIOFeatures.RegisterNamedLoadFeature(FieldNames[Field],
      @FieldLoader.LoadField);
end;

initialization
  FieldLoader := TWallFieldLoader.Create;
  RegisterFields;
finalization
  FieldLoader.Free;
end.
