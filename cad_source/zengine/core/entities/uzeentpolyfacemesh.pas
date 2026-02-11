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
unit uzeentpolyfacemesh;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}

interface

uses
  uzeentityfactory,uzgldrawcontext,uzedrawingdef,uzecamera,UGDBVectorSnapArray,
  uzestyleslayers,uzeentsubordinated,uzeentcurve,UGDBSelectedObjArray,
  uzeentity,uzctnrVectorBytesStream,uzeTypes,uzeconsts,uzglviewareadata,
  uzegeometrytypes,uzegeometry,uzeffdxfsupport,SysUtils,uzesnap,
  uzMVReader,uzCtnrVectorpBaseEntity,uzbLogIntf,uzclog;

type
  // Структура для хранения индексов вершин грани
  TFaceIndices = record
    Vertex1: Integer;
    Vertex2: Integer;
    Vertex3: Integer;
    Vertex4: Integer;
    VertexCount: Integer; // Количество вершин в грани (3 или 4)
  end;
  
  PFaceIndices = ^TFaceIndices;

  // Массив для хранения граней
  TFaceArray = array of TFaceIndices;
  
  PGDBObjPolyFaceMesh=^GDBObjPolyFaceMesh;

  GDBObjPolyFaceMesh=object(GDBObjCurve)
  private
    FVertexCount: Integer;    // Количество вершин в сети
    FFaceCount: Integer;      // Количество граней в сети
    FFaces: TFaceArray;       // Массив индексов граней
    
  public
    constructor init(own:Pointer;layeraddres:PGDBLayerProp;
      LW:smallint);
    
    // Основные методы сущности
    procedure LoadFromDXF(var rdr:TZMemReader;ptu:PExtensionData;
      var drawing:TDrawingDef;var context:TIODXFLoadContext);virtual;
    
    procedure FormatEntity(var drawing:TDrawingDef;
      var DC:TDrawContext;Stage:TEFStages=EFAllStages);virtual;
    procedure SaveToDXF(var outStream:TZctnrVectorBytes;
      var drawing:TDrawingDef;var IODXFContext:TIODXFSaveContext);virtual;
    procedure DrawGeometry(lw:integer;var DC:TDrawContext;
      const inFrustumState:TInBoundingVolume);virtual;
    function Clone(own:Pointer):PGDBObjEntity;virtual;
    function GetObjTypeName:string;virtual;
    function GetObjType:TObjID;virtual;
    
    // Методы для работы с вершинами и гранями
    function GetVertexCount: Integer;
    function GetFaceCount: Integer;
    function GetFaceVertices(Index: Integer): TFaceIndices;
    procedure AddFace(const Face: TFaceIndices);
    function GetFaceCountReadOnly: Integer; // Только для чтения, для инспектора объектов
    
    // Вспомогательные методы
    class function CreateInstance:PGDBObjPolyFaceMesh;static;
    function CalcTrueInFrustum(
      const frustum:TzeFrustum):TInBoundingVolume;virtual;
  end;

  function AllocAndInitPolyFaceMesh(owner:PGDBObjGenericWithSubordinated):PGDBObjPolyFaceMesh;

implementation

constructor GDBObjPolyFaceMesh.init(own:Pointer;layeraddres:PGDBLayerProp;
  LW:smallint);
begin
  inherited init(own,layeraddres,lw);
  FVertexCount := 0;
  FFaceCount := 0;
  system.SetLength(FFaces, 0);
end;

procedure GDBObjPolyFaceMesh.LoadFromDXF(var rdr:TZMemReader;ptu:PExtensionData;
  var drawing:TDrawingDef;var context:TIODXFLoadContext);
var
  s: string;
  byt: integer;
  polylineFlags: integer;
  vertexFlags: integer;
  currentVertex: TzePoint3d;
  currentFace: TFaceIndices;
  vertexIndex: Integer;
  isProcessingVertex: Boolean;
  isFaceRecord: Boolean;
  isPolyFaceVertex: Boolean;
  faceAdded: Boolean;
begin
  FVertexCount := 0;
  FFaceCount := 0;
  system.SetLength(FFaces, 0);

  // Очищаем кэш вершин от возможных остаточных данных
  context.GDBVertexLoadCache.Clear;

  polylineFlags := 0;
  vertexFlags := 0;
  currentVertex := NulVertex;
  isProcessingVertex := False;
  isFaceRecord := False;
  isPolyFaceVertex := False;
  faceAdded := False;

  byt := rdr.ParseInteger;
  while not rdr.EOF do begin
    s := '';
    if not LoadFromDXFObjShared(rdr,byt,ptu,drawing,context) then
      if dxfLoadGroupCodeString(rdr,0,byt,s) then begin
        // Если мы обрабатывали грань и переходим к следующей VERTEX или SEQEND, добавляем её
        if isFaceRecord and (currentFace.VertexCount >= 3) and not faceAdded then begin
          AddFace(currentFace);
          programlog.LogOutFormatStr('uzeentpolyfacemesh: Добавлена грань с %d вершинами: %d,%d,%d,%d', [currentFace.VertexCount, currentFace.Vertex1, currentFace.Vertex2, currentFace.Vertex3, currentFace.Vertex4], LM_Info);
          faceAdded := True;
        end;

        if s = 'VERTEX' then begin
          isProcessingVertex := True;
          vertexFlags := 0;
          isFaceRecord := False;
          isPolyFaceVertex := False;
          faceAdded := False; // Сбрасываем флаг при начале новой вершины
          currentVertex := NulVertex; // Сбрасываем координаты для новой вершины
          currentFace.VertexCount := 0;
          currentFace.Vertex1 := 0;
          currentFace.Vertex2 := 0;
          currentFace.Vertex3 := 0;
          currentFace.Vertex4 := 0;
        end
        else if s = 'SEQEND' then
          system.Break
        else begin
          // Если встречаем другую сущность, выходим
          s := rdr.ParseString; // Пропускаем значение
          continue; // Переходим к следующей итерации
        end;
      end
      else if isProcessingVertex then begin
        // Обработка VERTEX сущностей
        if dxfLoadGroupCodeString(rdr,100,byt,s) then begin
          // Определяем подтип вершины по DXF классу
          if s = 'AcDbPolyFaceMeshVertex' then
            isPolyFaceVertex := True
          else if s = 'AcDbFaceRecord' then
            isFaceRecord := True;
        end
        else if dxfLoadGroupCodeInteger(rdr,70,byt,vertexFlags) then begin
          // Флаги вершины: 128 = face record, другие значения = vertex record
          isFaceRecord := (vertexFlags and 128) = 128;
          if isFaceRecord then begin
            // Начинаем новую грань
            currentFace.VertexCount := 0;
            currentFace.Vertex1 := 0;
            currentFace.Vertex2 := 0;
            currentFace.Vertex3 := 0;
            currentFace.Vertex4 := 0;
          end;
        end
        else if dxfLoadGroupCodeVertex(rdr,10,byt,currentVertex) then begin
          // Это начало координат вершины (код группы 10)
          // Не добавляем вершину сразу, ждем пока все координаты будут прочитаны
        end
        else if dxfLoadGroupCodeVertex(rdr,30,byt,currentVertex) then begin
          // Z-координата вершины - все координаты прочитаны, можно добавлять вершину
          if isPolyFaceVertex and not isFaceRecord then begin
            // Это координаты вершины PolyFaceMesh (не грань)
            context.GDBVertexLoadCache.PushBackData(currentVertex);
            programlog.LogOutFormatStr('uzeentpolyfacemesh: Добавлена вершина: (%.2f, %.2f, %.2f)', [currentVertex.x, currentVertex.y, currentVertex.z], LM_Info);
          end;
        end
        else begin
          // Обработка других кодов группы для вершины
          if isFaceRecord then begin
            // Обработка записи грани (face record) - обработка индексов
            if dxfLoadGroupCodeInteger(rdr,71,byt,vertexIndex) and (vertexIndex <> 0) then begin
              currentFace.Vertex1 := abs(vertexIndex);
              inc(currentFace.VertexCount);
            end
            else if dxfLoadGroupCodeInteger(rdr,72,byt,vertexIndex) and (vertexIndex <> 0) then begin
              currentFace.Vertex2 := abs(vertexIndex);
              inc(currentFace.VertexCount);
            end
            else if dxfLoadGroupCodeInteger(rdr,73,byt,vertexIndex) and (vertexIndex <> 0) then begin
              currentFace.Vertex3 := abs(vertexIndex);
              inc(currentFace.VertexCount);
            end
            else if dxfLoadGroupCodeInteger(rdr,74,byt,vertexIndex) and (vertexIndex <> 0) then begin
              currentFace.Vertex4 := abs(vertexIndex);
              inc(currentFace.VertexCount);
            end;
          end
          else begin
            // Прочитали что-то другое для обычной вершины, пропускаем
            s := rdr.ParseString;
          end;
        end;
      end
      else begin
        // Обработка заголовка POLYLINE
        if dxfLoadGroupCodeInteger(rdr,70,byt,polylineFlags) then begin
          // Проверяем флаг Polyface Mesh (бит 64)
          if (polylineFlags and 64) = 0 then begin
            programlog.LogOutFormatStr('uzeentpolyfacemesh: Предупреждение - отсутствует флаг Polyface Mesh (64) в POLYLINE', [], LM_Info);
          end;
          programlog.LogOutFormatStr('uzeentpolyfacemesh: Загрузка Polyface Mesh с флагами = %d', [polylineFlags], LM_Info);
        end
        else if dxfLoadGroupCodeInteger(rdr,71,byt,FVertexCount) then begin
          // Количество вершин (может быть неверным в некоторых DXF)
          programlog.LogOutFormatStr('uzeentpolyfacemesh: Объявлено количество вершин = %d', [FVertexCount], LM_Info);
        end
        else if dxfLoadGroupCodeInteger(rdr,72,byt,FFaceCount) then begin
          // Количество граней (может быть неверным в некоторых DXF)
          programlog.LogOutFormatStr('uzeentpolyfacemesh: Объявлено количество граней = %d', [FFaceCount], LM_Info);
        end
        else
          s := rdr.ParseString;
      end;

    // Проверяем EOF перед следующим чтением
    if rdr.EOF then
      break;

    byt := rdr.ParseInteger;
  end;

  // Завершаем обработку последней грани, если она была начата
  if isFaceRecord and (currentFace.VertexCount >= 3) and not faceAdded then begin
    AddFace(currentFace);
    programlog.LogOutFormatStr('uzeentpolyfacemesh: Добавлена грань с %d вершинами: %d,%d,%d,%d', [currentFace.VertexCount, currentFace.Vertex1, currentFace.Vertex2, currentFace.Vertex3, currentFace.Vertex4], LM_Info);
  end;

  // Копируем вершины из кэша в текущий объект
  vertexarrayinocs.SetSize(context.GDBVertexLoadCache.Count);
  context.GDBVertexLoadCache.copyto(vertexarrayinocs);
  context.GDBVertexLoadCache.Clear;

  FVertexCount := vertexarrayinocs.Count;

  programlog.LogOutFormatStr('uzeentpolyfacemesh: Загружено %d вершин и %d граней', [FVertexCount, FFaceCount], LM_Info);
end;

procedure GDBObjPolyFaceMesh.FormatEntity(var drawing:TDrawingDef;
  var DC:TDrawContext;Stage:TEFStages=EFAllStages);
begin
  if assigned(EntExtensions) then
    EntExtensions.RunOnBeforeEntityFormat(@self,drawing,DC);
  FormatWithoutSnapArray;
  calcbb(dc);
  CalcActualVisible(dc.DrawingContext.VActuality);
  
  if (not (ESTemp in State))and(DCODrawable in DC.Options) then begin
    Representation.Clear;
    // Здесь будет код отрисовки граней полигональной сети
    // Пока используем базовую отрисовку для отладки
    if VertexArrayInWCS.Count > 1 then
      Representation.DrawPolyLineWithLT(dc,VertexArrayInWCS,vp,False,False);
  end;
  
  if assigned(EntExtensions) then
    EntExtensions.RunOnAfterEntityFormat(@self,drawing,DC);
end;

procedure GDBObjPolyFaceMesh.SaveToDXF(var outStream:TZctnrVectorBytes;
  var drawing:TDrawingDef;var IODXFContext:TIODXFSaveContext);
begin
  SaveToDXFObjPrefix(outStream,'POLYLINE','AcDbPolyFaceMesh',IODXFContext);
  dxfIntegerout(outStream,66,1);
  dxfvertexout(outStream,10,uzegeometry.NulVertex);
  dxfIntegerout(outStream,70,64); // Флаг Polyface Mesh
  dxfIntegerout(outStream,71,FVertexCount); // Количество вершин
  dxfIntegerout(outStream,72,FFaceCount);   // Количество граней
  
  // TODO: Сохранение вершин и граней
  programlog.LogOutFormatStr('uzeentpolyfacemesh: Сохранение PolyFaceMesh с %d вершинами и %d гранями', [FVertexCount, FFaceCount], LM_Info);
end;

procedure GDBObjPolyFaceMesh.DrawGeometry(lw:integer;var DC:TDrawContext;
  const inFrustumState:TInBoundingVolume);
begin
  self.Representation.DrawGeometry(DC,VP.BoundingBox,inFrustumState);
end;
//
//function GDBObjPolyFaceMesh.Clone(own:Pointer):PGDBObjEntity;
//var
//  tpo: PGDBObjPolyFaceMesh;
//  i: Integer;
//  NewFaces: TFaceArray; // временный массив
//begin
//  GetMem(Pointer(tpo), SizeOf(GDBObjPolyFaceMesh));
//  tpo^.init(own, vp.Layer, vp.LineWeight);
//  CopyVPto(tpo^);
//  CopyExtensionsTo(tpo^);
//
//  // Копируем массив вершин
//  tpo^.vertexarrayinocs.SetSize(vertexarrayinocs.Count);
//  vertexarrayinocs.copyto(tpo^.vertexarrayinocs);
//
//  // Копируем данные о гранях
//  tpo^.FVertexCount := FVertexCount;
//  tpo^.FFaceCount := FFaceCount;
//
//  // 👇 ВАЖНО: сначала работаем с локальным массивом
//  SetLength(NewFaces, Length(FFaces));
//  for i := 0 to High(FFaces) do
//    NewFaces[i] := FFaces[i];
//
//  tpo^.FFaces := NewFaces;  // а потом присваиваем целиком
//
//  tpo^.bp.ListPos.owner := own;
//  Result := tpo;
//end;

function GDBObjPolyFaceMesh.Clone(own:Pointer):PGDBObjEntity;
var
  tpo:PGDBObjPolyFaceMesh;
  i: Integer;
begin
  Getmem(Pointer(tpo),sizeof(GDBObjPolyFaceMesh));
  tpo^.init(own,vp.Layer,vp.LineWeight);
  CopyVPto(tpo^);
  CopyExtensionsTo(tpo^);
  
  // Копируем массив вершин
  tpo^.vertexarrayinocs.SetSize(vertexarrayinocs.Count);
  vertexarrayinocs.copyto(tpo^.vertexarrayinocs);
  
  // Копируем данные о гранях
  tpo^.FVertexCount := FVertexCount;
  tpo^.FFaceCount := FFaceCount;
  system.SetLength(tpo^.FFaces, system.Length(FFaces));
  for i := 0 to High(FFaces) do
    tpo^.FFaces[i] := FFaces[i];
    
  tpo^.bp.ListPos.owner:=own;
  Result:=tpo;
end;

function GDBObjPolyFaceMesh.GetObjTypeName:string;
begin
  Result:=ObjN_GDBObjPolyFaceMesh;
end;

function GDBObjPolyFaceMesh.GetObjType;
begin
  Result:=GDBPolyFaceMeshID;
end;

function GDBObjPolyFaceMesh.GetVertexCount;
begin
  Result := FVertexCount;
end;

function GDBObjPolyFaceMesh.GetFaceCount;
begin
  Result := FFaceCount;
end;

function GDBObjPolyFaceMesh.GetFaceCountReadOnly: Integer;
begin
  Result := FFaceCount;
end;

function GDBObjPolyFaceMesh.GetFaceVertices;
begin
  if (Index >= 0) and (Index < system.Length(FFaces)) then
    Result := FFaces[Index]
  else
    Result := Default(TFaceIndices);
end;

procedure GDBObjPolyFaceMesh.AddFace;
begin
  system.SetLength(FFaces, system.Length(FFaces) + 1);
  FFaces[High(FFaces)] := Face;
  inc(FFaceCount);
end;

function GDBObjPolyFaceMesh.CalcTrueInFrustum(
  const frustum:TzeFrustum):TInBoundingVolume;
begin
  Result := VertexArrayInWCS.CalcTrueInFrustum(frustum,False);
end;

class function GDBObjPolyFaceMesh.CreateInstance;
begin
  Result:=AllocAndInitPolyFaceMesh(nil);
end;

function AllocPolyFaceMesh:Pointer;
begin
  Getmem(pointer(Result),sizeof(GDBObjPolyFaceMesh));
end;

function AllocAndInitPolyFaceMesh(owner:PGDBObjGenericWithSubordinated):PGDBObjPolyFaceMesh;
begin
  Getmem(pointer(Result),sizeof(GDBObjPolyFaceMesh));
  Result.initnul(owner);
  Result.bp.ListPos.Owner:=owner;
end;

begin
  RegisterDXFEntity(GDBPolyFaceMeshID,'POLYLINE','PolyFaceMesh',@AllocPolyFaceMesh,@AllocAndInitPolyFaceMesh);
end.
