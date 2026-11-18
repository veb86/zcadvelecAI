unit uzvhttpwallcreator;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  uzvhttpwallgeometry, uzvhttpwallcollector;

function CreateRecognizedWalls(const SessionID: string; const Sources: TWallSources;
  const Segments: TWallSegments; const Candidates: TWallCandidates;
  const Recognized: TWallRecognitions): Integer;

implementation

uses
  SysUtils, uzeTypes, uzeentity, uzeentlwpolyline, uzeentpolyline,
  uzeentgenericsubentry, uzegeometrytypes, uzgldrawcontext, uzcdrawings,
  uzedrawingsimple, uzestyleslayers, uzcutils, uzcdrawing,
  zcmultiobjectcreateundocommand, uzvhttpwallxdata;

procedure BuildCenterline(var Entity: PGDBObjEntity; const Points: TWallVertices);
var
  Flat: Boolean;
  P: TWallPoint;
  Poly2D: PGDBObjLWPolyline;
  Poly3D: PGDBObjPolyline;
  Params: TSegmentParams;
begin
  Flat := True;
  for P in Points do
    Flat := Flat and (P.Z = Points[0].Z);
  if Flat then begin
    Poly2D := GDBObjLWPolyline.CreateInstance;
    Entity := Poly2D;
    Poly2D^.Local.p_insert.z := Points[0].Z;
    Params := Default(TSegmentParams);
    Params.data := CDefaultPolySegmentWidth;
    for P in Points do begin
      Poly2D^.Vertex2D_in_OCS_Array.PushBackData(TzePoint2d.Make(P.X, P.Y));
      Poly2D^.SgmntsParams.PushBackData(Params);
    end;
  end else begin
    Poly3D := GDBObjPolyline.CreateInstance;
    Entity := Poly3D;
    for P in Points do
      Poly3D^.VertexArrayInOCS.PushBackData(ToCADPoint(P));
  end;
end;

function CreateRecognizedWalls(const SessionID: string; const Sources: TWallSources;
  const Segments: TWallSegments; const Candidates: TWallCandidates;
  const Recognized: TWallRecognitions): Integer;
var
  Entities: array of PGDBObjEntity;
  Root: PGDBObjGenericSubEntry;
  Drawing: PTSimpleDrawing;
  Layer: PGDBLayerProp;
  DC: TDrawContext;
  Undo, DoAdd: TMethod;
  Batch: TGDBMultiCreateCommand;
  Candidate: TWallCandidate;
  Wall: TWallRecognition;
  Entity: PGDBObjEntity;
  I, Count: Integer;
begin
  Result := 0;
  Drawing := drawings.GetCurrentDWG;
  Root := drawings.GetCurrentROOT;
  Layer := Drawing^.GetCurrentLayer;
  if (Layer = nil) or SameText(Layer^.Name, WallPreviewLayer) then
    Layer := Drawing^.GetLayerTable^.GetSystemLayer;
  if (Layer = nil) or not Layer^._on or Layer^._lock then
    raise Exception.Create('Для создания стен выберите видимый разблокированный слой.');

  SetLength(Entities, Length(Recognized));
  Count := 0;
  DC := Drawing^.CreateDrawingRC;
  try
    // Подготавливаем все объекты до изменения модели и истории Undo.
    for Wall in Recognized do begin
      Candidate := Candidates[Wall.Candidate];
      if AlreadyHasWall(Root, Candidate) then Continue;
      BuildCenterline(Entities[Count], Wall.Vertices);
      Entity := Entities[Count];
      Inc(Count);
      zcSetEntPropFromCurrentDrawingProp(Entity);
      Entity^.bp.ListPos.Owner := Root;
      Entity^.vp.Layer := Layer;
      AttachWallMetadata(Entity, SessionID, Wall, Candidate,
        Sources[Segments[Candidate.Segments[0]].Source],
        Sources[Segments[Candidate.Segments[1]].Source]);
      Entity^.FormatEntity(Drawing^, DC);
    end;
    if Count = 0 then Exit;
    SetObjCreateManipulator(DoAdd, Undo);
    Batch := PushMultiObjectCreateCommand(PTZCADDrawing(Drawing)^.UndoStack, DoAdd, Undo, Count);
    for I := 0 to Count - 1 do begin
      Batch.AddObject(Entities[I]);
      Entities[I] := nil;
    end;
    Batch.Comit;
    Result := Count;
  finally
    for Entity in Entities do
      if Entity <> nil then begin
        Entity^.done;
        FreeMem(Entity);
      end;
  end;
end;

end.
