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
@author(Andrey Zubarev <zamtmn@yandex.ru>) 
}
unit uzeentarc;
{$Mode delphi}{$H+}
{$INCLUDE zengineconfig.inc}
interface

uses
  SysUtils, Math,
  uzeentityfactory,uzeentsubordinated,uzgldrawcontext,uzedrawingdef,
  uzeentwithlocalcs,uzecamera,uzestyleslayers,UGDBSelectedObjArray,uzeentity,
  UGDBPoint3DArray,uzctnrVectorBytesStream,uzeTypes,uzegeometrytypes,
  uzeconsts,uzglviewareadata,uzegeometry,uzeffdxfsupport,uzeentplain,uzeSnap,
  uzMVReader,uzCtnrVectorpBaseEntity,uzcinterface;

type

  PGDBObjArc=^GDBObjARC;

  GDBObjArc=object(GDBObjPlain)
    R:double;
    StartAngle:double;
    EndAngle:double;
    angle:double;
    Vertex3D_in_WCS_Array:GDBPoint3DArray;
    q0:TzePoint3d;
    q1:TzePoint3d;
    q2:TzePoint3d;
    constructor init(own:Pointer;layeraddres:PGDBLayerProp;
      LW:smallint;p:TzePoint3d;RR,S,E:double);
    constructor initnul;
    procedure LoadFromDXF(var rdr:TZMemReader;ptu:PExtensionData;
      var drawing:TDrawingDef;var context:TIODXFLoadContext);virtual;

    procedure SaveToDXF(var outStream:TZctnrVectorBytes;
      var drawing:TDrawingDef;var IODXFContext:TIODXFSaveContext);virtual;
    procedure DrawGeometry(lw:integer;var DC:TDrawContext;
      const inFrustumState:TInBoundingVolume);virtual;
    procedure addcontrolpoints(tdesc:Pointer);virtual;
    procedure remaponecontrolpoint(pdesc:pcontrolpointdesc;
      ProjectProc:GDBProjectProc);virtual;
    procedure CalcObjMatrix(pdrawing:PTDrawingDef=nil);virtual;
    procedure precalc;
    procedure FormatEntity(var drawing:TDrawingDef;
      var DC:TDrawContext;Stage:TEFStages=EFAllStages);virtual;
    procedure createpoints(var DC:TDrawContext);virtual;
    procedure getoutbound(var DC:TDrawContext);virtual;
    procedure projectpoint;virtual;
    function onmouse(var popa:TZctnrVectorPGDBaseEntity;
      const MF:TzeFrustum;InSubEntry:boolean):boolean;virtual;
    function getsnap(var osp:os_record;var pdata:Pointer;
      const param:OGLWndtype;ProjectProc:GDBProjectProc;SnapMode:TGDBOSMode):boolean;virtual;
    function beforertmodify:Pointer;virtual;
    procedure rtmodifyonepoint(const rtmod:TRTModifyData);virtual;
    function IsRTNeedModify(const Point:PControlPointDesc;
      p:Pointer):boolean;virtual;
    procedure SetFromClone(_clone:PGDBObjEntity);virtual;
    function Clone(own:Pointer):PGDBObjEntity;virtual;
    procedure rtsave(refp:Pointer);virtual;
    destructor done;virtual;
    function GetObjTypeName:string;virtual;
    function calcinfrustum(const frustum:TzeFrustum;
      const Actuality:TVisActuality;var Counters:TCameraCounters;ProjectProc:GDBProjectProc;
      const zoom,currentdegradationfactor:double):boolean;virtual;
    function CalcTrueInFrustum(
      const frustum:TzeFrustum):TInBoundingVolume;virtual;
    procedure ReCalcFromObjMatrix;virtual;
    procedure transform(const t_matrix:TzeTypedMatrix4d);virtual;
    //function GetTangentInPoint(point:TzePoint3d):TzePoint3d;virtual;
    procedure AddOnTrackAxis(var posr:os_record;
      const processaxis:taddotrac);virtual;
    function onpoint(var objects:TZctnrVectorPGDBaseEntity;
      const point:TzePoint3d):boolean;virtual;
    procedure TransformAt(p:PGDBObjEntity;t_matrix:PzeTypedMatrix4d);virtual;
    class function CreateInstance:PGDBObjArc;static;
    function GetObjType:TObjID;virtual;
    function IsStagedFormatEntity:boolean;virtual;
  end;

implementation

function GDBObjARC.IsStagedFormatEntity:boolean;
begin
  Result:=True;
end;

procedure GDBObjARC.TransformAt;
var
  tv:TzeVector4d;
begin
  objmatrix:=uzegeometry.MatrixMultiply(PGDBObjWithLocalCS(p)^.objmatrix,t_matrix^);

  tv:=PzeVector4d(@t_matrix.mtr.v[3])^;
  PzeVector4d(@t_matrix.mtr.v[3])^:=NulVertex4D;
  PzeVector4d(@t_matrix.mtr.v[3])^:=tv;
  ReCalcFromObjMatrix;
end;

function GDBObjARC.onpoint(var objects:TZctnrVectorPGDBaseEntity;
  const point:TzePoint3d):boolean;
begin
  if Vertex3D_in_WCS_Array.onpoint(point,False) then begin
    Result:=
      True;
    objects.
      PushBackData(@self);
  end else
    Result:=
      False;
end;

procedure GDBObjARC.AddOnTrackAxis(var posr:os_record;const processaxis:taddotrac);
var
  m1:TzeTypedMatrix4d;
  dir,tv:TzePoint3d;
begin
  m1:=GetMatrix^;
  MatrixInvert(m1);
  dir:=VectorTransform3D(posr.worldcoord,m1);

  processaxis(posr,dir);
  tv:=uzegeometry.vectordot(dir,zwcs);
  processaxis(posr,tv);
end;

// Процедура трансформации дуги: поворот, масштабирование и зеркализация.
// Углы дуги (StartAngle, EndAngle) измеряются от оси X локальной СК объекта
// (определяется алгоритмом Arbitrary Axis из DXF). После любого поворота, в том
// числе вокруг осей X и Y, углы пересчитываются путём проекции новых направлений
// к точкам начала/конца дуги на оси новой локальной СК.
procedure GDBObjARC.transform;
var
  // Мировые координаты точек дуги до трансформации
  oldStartPoint, oldEndPoint, oldCenter: TzePoint3d;
  // Мировые координаты точек дуги после трансформации
  newStartPoint, newEndPoint, newCenter: TzePoint3d;
  // Оси новой локальной СК (Arbitrary Axis Algorithm)
  newOcsX, newOcsY: TzePoint3d;
  // Нормализованные направления от нового центра к точкам дуги
  dirToStart, dirToEnd: TzePoint3d;
  // Определитель матрицы трансформации (знак указывает на зеркальность)
  det: double;
begin
  // Шаг 1. Вычисляем текущие 3D-позиции ключевых точек дуги в WCS
  precalc;
  oldCenter    := P_insert_in_WCS;
  oldStartPoint := q0;
  oldEndPoint   := q2;

  // Шаг 2. Вычисляем определитель для обнаружения зеркальной трансформации.
  // При det < 0 матрица содержит отражение — порядок начала и конца дуги меняется
  // Для матрицы трансформации с последней строкой [0,0,0,1] определитель равен
  // определителю верхнего левого блока 3×3
  det := t_matrix.mtr.v[0].v[0] * (t_matrix.mtr.v[1].v[1] * t_matrix.mtr.v[2].v[2] - t_matrix.mtr.v[1].v[2] * t_matrix.mtr.v[2].v[1])
       - t_matrix.mtr.v[0].v[1] * (t_matrix.mtr.v[1].v[0] * t_matrix.mtr.v[2].v[2] - t_matrix.mtr.v[1].v[2] * t_matrix.mtr.v[2].v[0])
       + t_matrix.mtr.v[0].v[2] * (t_matrix.mtr.v[1].v[0] * t_matrix.mtr.v[2].v[1] - t_matrix.mtr.v[1].v[1] * t_matrix.mtr.v[2].v[0]);

  // Шаг 3. Переносим точки начала и конца дуги в новые мировые позиции
  newCenter     := VectorTransform3D(oldCenter, t_matrix);
  if det < 0 then begin
    // Зеркальная трансформация: меняем местами начало и конец
    newStartPoint := VectorTransform3D(oldEndPoint, t_matrix);
    newEndPoint   := VectorTransform3D(oldStartPoint, t_matrix);
  end else begin
    newStartPoint := VectorTransform3D(oldStartPoint, t_matrix);
    newEndPoint   := VectorTransform3D(oldEndPoint, t_matrix);
  end;

  // Шаг 4. Обновляем objmatrix и Local.basis через базовый класс.
  // После этого: Local.basis.oz содержит новую нормаль, Local.basis.ox/oy —
  // нормализованные оси из обновлённой objmatrix, Local.P_insert и R обновлены.
  inherited;

  // Шаг 5. Строим канонические оси новой локальной СК (Arbitrary Axis Algorithm).
  // Алгоритм DXF определяет ось X из нормали oz; углы дуги отсчитываются от неё.
  newOcsX := GetXfFromZ(Local.basis.oz);
  newOcsY := NormalizeVertex(VectorDot(Local.basis.oz, newOcsX));

  // Шаг 6. Вычисляем нормализованные направления от нового центра к точкам дуги
  dirToStart := NormalizeVertex(VertexSub(newStartPoint, newCenter));
  dirToEnd   := NormalizeVertex(VertexSub(newEndPoint, newCenter));

  // Шаг 7. Проецируем направления на оси локальной СК и вычисляем новые углы.
  // scalardot — скалярное произведение; оно даёт косинус и синус угла в плоскости дуги
  StartAngle := ArcTan2(scalardot(dirToStart, newOcsY), scalardot(dirToStart, newOcsX));
  if StartAngle < 0 then
    StartAngle := 2 * pi + StartAngle;

  EndAngle := ArcTan2(scalardot(dirToEnd, newOcsY), scalardot(dirToEnd, newOcsX));
  if EndAngle < 0 then
    EndAngle := 2 * pi + EndAngle;

  // Шаг 8. Пересчитываем вспомогательные точки q0, q1, q2 с новыми углами
  precalc;

  // Отладочный вывод для проверки трансформации
  zcUI.TextMessage(Format('[DEBUG transform] det=%.6f (зеркало=%s)', [det, BoolToStr(det < 0, True)]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] oldCenter: (%.6f, %.6f, %.6f)', [oldCenter.x, oldCenter.y, oldCenter.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] newCenter: (%.6f, %.6f, %.6f)', [newCenter.x, newCenter.y, newCenter.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] oldStartPoint: (%.6f, %.6f, %.6f)', [oldStartPoint.x, oldStartPoint.y, oldStartPoint.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] oldEndPoint: (%.6f, %.6f, %.6f)', [oldEndPoint.x, oldEndPoint.y, oldEndPoint.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] newStartPoint: (%.6f, %.6f, %.6f)', [newStartPoint.x, newStartPoint.y, newStartPoint.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] newEndPoint: (%.6f, %.6f, %.6f)', [newEndPoint.x, newEndPoint.y, newEndPoint.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] StartAngle: %.6f (%.2f°)', [StartAngle, StartAngle*180/Pi]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] EndAngle: %.6f (%.2f°)', [EndAngle, EndAngle*180/Pi]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] R: %.6f', [R]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] newOcsX: (%.6f, %.6f, %.6f)', [newOcsX.x, newOcsX.y, newOcsX.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] newOcsY: (%.6f, %.6f, %.6f)', [newOcsY.x, newOcsY.y, newOcsY.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] dirToStart: (%.6f, %.6f, %.6f)', [dirToStart.x, dirToStart.y, dirToStart.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] dirToEnd: (%.6f, %.6f, %.6f)', [dirToEnd.x, dirToEnd.y, dirToEnd.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] Local.basis.OX: (%.6f, %.6f, %.6f)', [Local.basis.ox.x, Local.basis.ox.y, Local.basis.ox.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] Local.basis.OY: (%.6f, %.6f, %.6f)', [Local.basis.oy.x, Local.basis.oy.y, Local.basis.oy.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] Local.basis.OZ: (%.6f, %.6f, %.6f)', [Local.basis.oz.x, Local.basis.oz.y, Local.basis.oz.z]), TMWOHistoryOut);
  zcUI.TextMessage('[DEBUG transform] ObjMatrix после трансформации:', TMWOHistoryOut);
  zcUI.TextMessage(Format('  [%.6f, %.6f, %.6f, %.6f]', [objmatrix.mtr.v[0].v[0], objmatrix.mtr.v[0].v[1], objmatrix.mtr.v[0].v[2], objmatrix.mtr.v[0].v[3]]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  [%.6f, %.6f, %.6f, %.6f]', [objmatrix.mtr.v[1].v[0], objmatrix.mtr.v[1].v[1], objmatrix.mtr.v[1].v[2], objmatrix.mtr.v[1].v[3]]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  [%.6f, %.6f, %.6f, %.6f]', [objmatrix.mtr.v[2].v[0], objmatrix.mtr.v[2].v[1], objmatrix.mtr.v[2].v[2], objmatrix.mtr.v[2].v[3]]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  [%.6f, %.6f, %.6f, %.6f]', [objmatrix.mtr.v[3].v[0], objmatrix.mtr.v[3].v[1], objmatrix.mtr.v[3].v[2], objmatrix.mtr.v[3].v[3]]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] q0: (%.6f, %.6f, %.6f)', [q0.x, q0.y, q0.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] q1: (%.6f, %.6f, %.6f)', [q1.x, q1.y, q1.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG transform] q2: (%.6f, %.6f, %.6f)', [q2.x, q2.y, q2.z]), TMWOHistoryOut);
end;

procedure GDBObjARC.ReCalcFromObjMatrix;
var
  ox,oy:TzePoint3d;
  m:TzeTypedMatrix4d;
  invM:TzeTypedMatrix4d;
  tempPoint: TzePoint3d;
begin
  inherited;

  ox:=GetXfFromZ(Local.basis.oz);
  oy:=NormalizeVertex(VectorDot(Local.basis.oz,Local.basis.ox));
  m:=CreateMatrixFromBasis(ox,oy,Local.basis.oz);

  // Строим обратную матрицу поворота (транспонированную)
  // Для ортогональной матрицы (поворот) inverse = transpose
  invM := m;
  // Транспонируем только поворотную часть 3x3
  invM.mtr.v[0].v[1] := m.mtr.v[1].v[0];
  invM.mtr.v[0].v[2] := m.mtr.v[2].v[0];
  invM.mtr.v[1].v[0] := m.mtr.v[0].v[1];
  invM.mtr.v[1].v[2] := m.mtr.v[2].v[1];
  invM.mtr.v[2].v[0] := m.mtr.v[0].v[2];
  invM.mtr.v[2].v[1] := m.mtr.v[1].v[2];

  // Преобразуем мировые координаты центра в локальные
  // objmatrix = Translation * Scale * Rotation
  // objmatrix.mtr.v[3] = Local.P_insert (не масштабируется!)
  // Для извлечения Local.P_insert нужно применить обратный поворот к переносу
  tempPoint := VectorTransform3D(PzePoint3d(@objmatrix.mtr.v[3])^, invM);
  Local.P_insert := tempPoint;

  // Вычисляем радиус из длины первой оси
  self.R := oneVertexLength(PzePoint3d(@objmatrix.mtr.v[0])^);

  zcUI.TextMessage('[DEBUG ReCalcFromObjMatrix]', TMWOHistoryOut);
  zcUI.TextMessage(Format('  Local.basis.OX: (%.6f, %.6f, %.6f)', [Local.basis.ox.x, Local.basis.ox.y, Local.basis.ox.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  Local.basis.OY: (%.6f, %.6f, %.6f)', [Local.basis.oy.x, Local.basis.oy.y, Local.basis.oy.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  Local.basis.OZ: (%.6f, %.6f, %.6f)', [Local.basis.oz.x, Local.basis.oz.y, Local.basis.oz.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  objmatrix.mtr.v[3]: (%.6f, %.6f, %.6f)', [objmatrix.mtr.v[3].v[0], objmatrix.mtr.v[3].v[1], objmatrix.mtr.v[3].v[2]]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  Local.P_insert: (%.6f, %.6f, %.6f)', [Local.P_insert.x, Local.P_insert.y, Local.P_insert.z]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  objmatrix.mtr.v[0].x: %.6f', [objmatrix.mtr.v[0].v[0]]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  R (вычисленный): %.6f', [self.R]), TMWOHistoryOut);
end;

function GDBObjARC.CalcTrueInFrustum;
var
  i:integer;
  rad:double;
begin
  rad:=abs(ObjMatrix.mtr.v[0].v[0]);
  for i:=0 to 5 do
    if (frustum.v[i].v[0]*P_insert_in_WCS.x+frustum.v[i].v[1]*
      P_insert_in_WCS.y+frustum.v[i].v[2]*P_insert_in_WCS.z+
      frustum.v[i].v[3]+rad{+GetLTCorrectH}<0) then
      exit(IREmpty);
  Result:=Vertex3D_in_WCS_Array.CalcTrueInFrustum(frustum,False);
end;

function GDBObjARC.calcinfrustum;
var
  i:integer;
begin
  Result:=True;
  for i:=0 to 4 do begin
    if (frustum.v[i].v[0]*outbound[0].x+frustum.v[i].v[1]*outbound[0].y+
        frustum.v[i].v[2]*outbound[0].z+frustum.v[i].v[3]<0)  and
       (frustum.v[i].v[0]*outbound[1].x+frustum.v[i].v[1]*outbound[1].y+
        frustum.v[i].v[2]*outbound[1].z+frustum.v[i].v[3]<0)  and
       (frustum.v[i].v[0]*outbound[2].x+frustum.v[i].v[1]*outbound[2].y+
        frustum.v[i].v[2]*outbound[2].z+frustum.v[i].v[3]<0)  and
       (frustum.v[i].v[0]*outbound[3].x+frustum.v[i].v[1]*outbound[3].y+
        frustum.v[i].v[2]*outbound[3].z+frustum.v[i].v[3]<0) then begin
      Result:=False;
      system.break;
    end;
  end;
end;

function GDBObjARC.GetObjTypeName;
begin
  Result:=ObjN_GDBObjArc;
end;

destructor GDBObjARC.done;
begin
  inherited done;
  Vertex3D_in_WCS_Array.Done;
end;

constructor GDBObjARC.initnul;
begin
  inherited initnul(nil);
  r:=1;
  startangle:=0;
  endangle:=pi/2;
  Vertex3D_in_WCS_Array.init(3);
end;

constructor GDBObjARC.init;
begin
  inherited init(own,layeraddres,lw);
  Local.p_insert:=p;
  r:=rr;
  startangle:=s;
  endangle:=e;
  Vertex3D_in_WCS_Array.init(3);
end;

function GDBObjArc.GetObjType;
begin
  Result:=GDBArcID;
end;

procedure GDBObjArc.SaveToDXF;
begin
  SaveToDXFObjPrefix(outStream,'ARC','AcDbCircle',IODXFContext);
  dxfvertexout(outStream,10,Local.p_insert);
  dxfDoubleout(outStream,40,r);
  SaveToDXFObjPostfix(outStream);

  dxfStringWithoutEncodeOut(outStream,100,'AcDbArc');
  dxfDoubleout(outStream,50,startangle*180/pi);
  dxfDoubleout(outStream,51,endangle*180/pi);
end;

procedure GDBObjARC.CalcObjMatrix;
var
  m1:TzeTypedMatrix4d;
  v:TzeVector4d;
begin
  inherited CalcObjMatrix;
  // Масштабируем только оси (радиус), не перенос!
  // objmatrix после inherited = Translation * Rotation
  // Умножаем первые три строки на радиус
  with objmatrix.mtr do begin
    v[0].v[0] := v[0].v[0] * r;
    v[0].v[1] := v[0].v[1] * r;
    v[0].v[2] := v[0].v[2] * r;
    v[0].v[3] := v[0].v[3] * r;

    v[1].v[0] := v[1].v[0] * r;
    v[1].v[1] := v[1].v[1] * r;
    v[1].v[2] := v[1].v[2] * r;
    v[1].v[3] := v[1].v[3] * r;

    v[2].v[0] := v[2].v[0] * r;
    v[2].v[1] := v[2].v[1] * r;
    v[2].v[2] := v[2].v[2] * r;
    v[2].v[3] := v[2].v[3] * r;
  end;

  PzePoint3d(@v)^:=local.p_insert;
  v.z:=0;
  v.w:=1;
  m1:=objMatrix;
  MatrixInvert(m1);
  v:=VectorTransform(v,m1);

  // Отладочный вывод для проверки вычисления матрицы
  zcUI.TextMessage(Format('[DEBUG CalcObjMatrix] R=%.6f', [r]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG CalcObjMatrix] Local.p_insert: (%.6f, %.6f, %.6f)', [Local.p_insert.x, Local.p_insert.y, Local.p_insert.z]), TMWOHistoryOut);
  zcUI.TextMessage('[DEBUG CalcObjMatrix] ObjMatrix после масштабирования:', TMWOHistoryOut);
  zcUI.TextMessage(Format('  [%.6f, %.6f, %.6f, %.6f]', [objmatrix.mtr.v[0].v[0], objmatrix.mtr.v[0].v[1], objmatrix.mtr.v[0].v[2], objmatrix.mtr.v[0].v[3]]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  [%.6f, %.6f, %.6f, %.6f]', [objmatrix.mtr.v[1].v[0], objmatrix.mtr.v[1].v[1], objmatrix.mtr.v[1].v[2], objmatrix.mtr.v[1].v[3]]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  [%.6f, %.6f, %.6f, %.6f]', [objmatrix.mtr.v[2].v[0], objmatrix.mtr.v[2].v[1], objmatrix.mtr.v[2].v[2], objmatrix.mtr.v[2].v[3]]), TMWOHistoryOut);
  zcUI.TextMessage(Format('  [%.6f, %.6f, %.6f, %.6f]', [objmatrix.mtr.v[3].v[0], objmatrix.mtr.v[3].v[1], objmatrix.mtr.v[3].v[2], objmatrix.mtr.v[3].v[3]]), TMWOHistoryOut);
  zcUI.TextMessage(Format('[DEBUG CalcObjMatrix] Вектор v (после инверсии): (%.6f, %.6f, %.6f, %.6f)', [v.x, v.y, v.z, v.w]), TMWOHistoryOut);
end;

procedure GDBObjARC.precalc;
var
  v:TzeVector4d;
begin
  angle:=endangle-startangle;
  if angle<0 then
    angle:=2*pi+angle;
  SinCos(startangle,v.y,v.x);
  v.z:=0;
  v.w:=1;
  v:=VectorTransform(v,objMatrix);
  q0:=PzePoint3d(@v)^;
  SinCos(startangle+angle/2,v.y,v.x);
  v.z:=0;
  v.w:=1;
  v:=VectorTransform(v,objMatrix);
  q1:=PzePoint3d(@v)^;
  SinCos(endangle,v.y,v.x);
  v.z:=0;
  v.w:=1;
  v:=VectorTransform(v,objMatrix);
  q2:=PzePoint3d(@v)^;
end;

procedure GDBObjARC.FormatEntity(var drawing:TDrawingDef;
  var DC:TDrawContext;Stage:TEFStages=EFAllStages);
begin
  if EFCalcEntityCS in stage then begin
    if assigned(EntExtensions) then
      EntExtensions.RunOnBeforeEntityFormat(@self,drawing,DC);

    calcObjMatrix;
    precalc;

    calcbb(dc);
    createpoints(dc);
  end;
  CalcActualVisible(dc.DrawingContext.VActuality);
  if EFDraw in stage then begin
    Representation.Clear;
    if not (ESTemp in State)and(DCODrawable in DC.Options) then
      Representation.DrawPolyLineWithLT(dc,Vertex3D_in_WCS_Array,vp,False,False);
    if assigned(EntExtensions) then
      EntExtensions.RunOnAfterEntityFormat(@self,drawing,DC);
  end;
end;

procedure GDBObjARC.getoutbound;

  function getQuadrant(a:double):integer;
  {
  2|1
  ---
  3|4
  }
  begin
    if a<pi/2 then
      Result:=0
    else if a<pi then
      Result:=1
    else if a<3*pi/2 then
      Result:=2
    else
      Result:=3;
  end;

  function AxisIntersect(q1,q2:integer):integer;
  {
    2
   2|1
  4---1
   3|4
    8
  }
  begin
    Result:=0;
    while q1<>q2 do begin
      Inc(q1);
      q1:=q1 and 3;
      Result:=Result or (1 shl q1);
    end;
  end;

var
  sx,sy,ex,ey,minx,miny,maxx,maxy:double;
  sq,eq,q:integer;
begin
  vp.BoundingBox:=CreateBBFrom2Point(q0,q2);
  sq:=getQuadrant(self.StartAngle);
  eq:=getQuadrant(self.EndAngle);
  q:=AxisIntersect(sq,eq);
  if (self.StartAngle>self.EndAngle)and(q=0) then
    q:=q xor 15;
  SinCos(self.StartAngle,sy,sx);
  SinCos(self.EndAngle,ey,ex);
  if sx>ex then begin
    minx:=ex;
    maxx:=sx;
  end else begin
    minx:=sx;
    maxx:=ex;
  end;
  if sy>ey then begin
    miny:=ey;
    maxy:=sy;
  end else begin
    miny:=sy;
    maxy:=ey;
  end;
  if (q and 1)>0 then begin
    concatBBandPoint(vp.BoundingBox,VectorTransform3d(
      CreateVertex(1,0,0),objMatrix));
    maxx:=1;
  end;
  if (q and 4)>0 then begin
    concatBBandPoint(vp.BoundingBox,VectorTransform3d(
      CreateVertex(-1,0,0),objMatrix));
    minx:=-1;
  end;
  if (q and 2)>0 then begin
    concatBBandPoint(vp.BoundingBox,VectorTransform3d(
      CreateVertex(0,1,0),objMatrix));
    maxy:=1;
  end;
  if (q and 8)>0 then begin
    concatBBandPoint(vp.BoundingBox,VectorTransform3d(
      CreateVertex(0,-1,0),objMatrix));
    miny:=-1;
  end;
   outbound[0]:=VectorTransform3d(CreateVertex(minx,maxy,0),objMatrix);
  outbound[1]:=VectorTransform3d(CreateVertex(maxx,maxy,0),objMatrix);
  outbound[2]:=VectorTransform3d(CreateVertex(maxx,miny,0),objMatrix);
  outbound[3]:=VectorTransform3d(CreateVertex(minx,miny,0),objMatrix);
end;

procedure GDBObjARC.createpoints(var DC:TDrawContext);
var
  i:integer;
  l:double;
  v:TzePoint3d;
  pv:TzePoint3d;
  maxlod:integer;
begin
  angle:=endangle-startangle;
  if angle<0 then
    angle:=2*pi+angle;

  if dc.MaxDetail then
    maxlod:=100
  else
    maxlod:=60;

  l:=r*angle/(dc.DrawingContext.zoom*10);
  if (l>maxlod)or dc.MaxDetail then
    lod:=maxlod
  else begin
    lod:=round(l);
    if lod<5 then
      lod:=5;
  end;
  Vertex3D_in_WCS_Array.SetSize(lod+1);

  Vertex3D_in_WCS_Array.Clear;
  SinCos(startangle,v.y,v.x);
  v.z:=0;
  pv:=VectorTransform3D(v,objmatrix);
  Vertex3D_in_WCS_Array.PushBackData(pv);

  for i:=1 to lod do begin
    SinCos(startangle+i/lod*angle,v.y,v.x);
    v.z:=0;
    pv:=VectorTransform3D(v,objmatrix);
    Vertex3D_in_WCS_Array.PushBackData(pv);
  end;
  Vertex3D_in_WCS_Array.Shrink;
end;

procedure GDBObjARC.DrawGeometry;
begin
  Representation.DrawGeometry(DC,VP.BoundingBox,inFrustumState);
  inherited;
end;

procedure GDBObjARC.projectpoint;
begin

end;

procedure GDBObjARC.LoadFromDXF;
var
  byt:integer;
  dc:TDrawContext;
begin
  byt:=rdr.ParseInteger;
  while byt<>0 do begin
    if not LoadFromDXFObjShared(rdr,byt,ptu,drawing,context) then
      if not dxfLoadGroupCodeVertex(rdr,10,byt,Local.P_insert) then
        if not dxfLoadGroupCodeDouble(rdr,40,byt,r) then
          if not dxfLoadGroupCodeDouble(rdr,50,byt,startangle) then
            if not dxfLoadGroupCodeDouble(rdr,51,byt,endangle) then
              rdr.SkipString;
    byt:=rdr.ParseInteger;
  end;
  startangle:=startangle*pi/180;
  endangle:=endangle*pi/180;
  dc:=drawing.createdrawingrc;
  if vp.Layer=nil then
    vp.Layer:=nil;
  FormatEntity(drawing,dc);
end;

function GDBObjARC.onmouse;
var
  i:integer;
  rad:double;
begin
  rad:=abs(ObjMatrix.mtr.v[0].v[0]);
  for i:=0 to 5 do begin
    if (mf.v[i].v[0]*P_insert_in_WCS.x+mf.v[i].v[1]*P_insert_in_WCS.y+
        mf.v[i].v[2]*P_insert_in_WCS.z+mf.v[i].v[3]+rad<0) then
      exit(False);
  end;
  Result:=Vertex3D_in_WCS_Array.onmouse(mf,False);
  if not Result then
    if CalcPointTrueInFrustum(P_insert_in_WCS,mf)=IRFully then
      Result:=True;
end;

procedure GDBObjARC.remaponecontrolpoint(pdesc:pcontrolpointdesc;
  ProjectProc:GDBProjectProc);
var
  tv:TzePoint3d;
begin
  if pdesc^.pointtype=os_begin then begin
    pdesc.worldcoord:=q0;
    ProjectProc(pdesc.worldcoord,tv);
    pdesc.dispcoord:=ToTzePoint2i(tv);
  end else if pdesc^.pointtype=os_midle then begin
    pdesc.worldcoord:=q1;
    ProjectProc(pdesc.worldcoord,tv);
    pdesc.dispcoord:=ToTzePoint2i(tv);
  end else if pdesc^.pointtype=os_end then begin
    pdesc.worldcoord:=q2;
    ProjectProc(pdesc.worldcoord,tv);
    pdesc.dispcoord:=ToTzePoint2i(tv);
  end;
end;

procedure GDBObjARC.addcontrolpoints(tdesc:Pointer);
var
  pdesc:controlpointdesc;
begin
  PSelectedObjDesc(tdesc)^.pcontrolpoint^.init(3);
  pdesc.selected:=False;
  pdesc.PDrawable:=nil;

  pdesc.pointtype:=os_begin;
  pdesc.attr:=[CPA_Strech];
  pdesc.worldcoord:=q0;
  PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(pdesc);

  pdesc.pointtype:=os_midle;
  pdesc.attr:=[];
  pdesc.worldcoord:=q1;
  PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(pdesc);

  pdesc.pointtype:=os_end;
  pdesc.attr:=[CPA_Strech];
  pdesc.worldcoord:=q1;
  PSelectedObjDesc(tdesc)^.pcontrolpoint^.PushBackData(pdesc);
end;

function GDBObjARC.getsnap;
begin
  if onlygetsnapcount=4 then begin
    Result:=False;
    exit;
  end;
  Result:=True;
  case onlygetsnapcount of
    0:begin
      if (SnapMode and osm_center)<>0 then begin
        osp.worldcoord:=P_insert_in_WCS;
        ProjectProc(osp.worldcoord,osp.dispcoord);
        osp.ostype:=os_center;
      end else
        osp.ostype:=os_none;
    end;
    1:begin
      if (SnapMode and osm_endpoint)<>0 then begin
        osp.worldcoord:=q0;
        ProjectProc(osp.worldcoord,osp.dispcoord);
        osp.ostype:=os_begin;
      end else
        osp.ostype:=os_none;
    end;
    2:begin
      if (SnapMode and osm_midpoint)<>0 then begin
        osp.worldcoord:=q1;
        ProjectProc(osp.worldcoord,osp.dispcoord);
        osp.ostype:=os_midle;
      end else
        osp.ostype:=os_none;
    end;
    3:begin
      if (SnapMode and osm_endpoint)<>0 then begin
        osp.worldcoord:=q2;
        ProjectProc(osp.worldcoord,osp.dispcoord);
        osp.ostype:=os_end;
      end else
        osp.ostype:=os_none;
    end;
  end;
  Inc(onlygetsnapcount);
end;

function GDBObjARC.beforertmodify;
begin
  Getmem(Result,sizeof(tarcrtmodify));
  tarcrtmodify(Result^).p1.x:=q0.x;
  tarcrtmodify(Result^).p1.y:=q0.y;
  tarcrtmodify(Result^).p2.x:=q1.x;
  tarcrtmodify(Result^).p2.y:=q1.y;
  tarcrtmodify(Result^).p3.x:=q2.x;
  tarcrtmodify(Result^).p3.y:=q2.y;
end;

function GDBObjARC.IsRTNeedModify(const Point:PControlPointDesc;p:Pointer):boolean;
begin
  Result:=True;
end;

procedure GDBObjARC.SetFromClone(_clone:PGDBObjEntity);
begin
  q0:=PGDBObjARC(_clone)^.q0;
  q1:=PGDBObjARC(_clone)^.q1;
  q2:=PGDBObjARC(_clone)^.q2;
end;

procedure GDBObjARC.rtmodifyonepoint(const rtmod:TRTModifyData);
var
  tv3d:TzePoint3d;
  tq0,tq1,tq2:TzePoint3d;
  ptdata:tarcrtmodify;
  ad:TArcData;
  m:TzeTypedMatrix4d;
begin
  m:=ObjMatrix;
  MatrixInvert(m);
  m.mtr.v[3]:=NulVector4D;

  tq0:=VectorTransform3D(q0*R,m);
  tq1:=VectorTransform3D(q1*R,m);
  tq2:=VectorTransform3D(q2*R,m);
  tv3d:=VectorTransform3D(rtmod.wc*R,m);

  ptdata.p1.x:=tq0.x;
  ptdata.p1.y:=tq0.y;
  ptdata.p2.x:=tq1.x;
  ptdata.p2.y:=tq1.y;
  ptdata.p3.x:=tq2.x;
  ptdata.p3.y:=tq2.y;

  if rtmod.point.pointtype=os_begin then begin
    ptdata.p1.x:=tv3d.x;
    ptdata.p1.y:=tv3d.y;
  end else if rtmod.point.pointtype=os_midle then begin
    ptdata.p2.x:=tv3d.x;
    ptdata.p2.y:=tv3d.y;
  end else if rtmod.point.pointtype=os_end then begin
    ptdata.p3.x:=tv3d.x;
    ptdata.p3.y:=tv3d.y;
  end;

  if GetArcParamFrom3Point2D(ptdata,ad) then begin
    Local.p_insert.x:=ad.p.x;
    Local.p_insert.y:=ad.p.y;
    Local.p_insert.z:=0;
    startangle:=ad.startangle;
    endangle:=ad.endangle;
    r:=ad.r;
  end;
end;

function GDBObjARC.Clone;
var
  tvo:PGDBObjArc;
begin
  Getmem(Pointer(tvo),sizeof(GDBObjArc));
  tvo^.init(CalcOwner(own),vp.Layer,vp.LineWeight,Local.p_insert,
    r,startangle,endangle);
  tvo^.Local.basis.oz:=Local.basis.oz;
  CopyVPto(tvo^);
  CopyExtensionsTo(tvo^);
  Result:=tvo;
end;

procedure GDBObjARC.rtsave;
begin
  pgdbobjarc(refp)^.Local.p_insert:=Local.p_insert;
  pgdbobjarc(refp)^.startangle:=startangle;
  pgdbobjarc(refp)^.endangle:=endangle;
  pgdbobjarc(refp)^.r:=r;
end;

function AllocArc:PGDBObjArc;
begin
  Getmem(pointer(Result),sizeof(GDBObjArc));
end;

function AllocAndInitArc(owner:PGDBObjGenericWithSubordinated):PGDBObjArc;
begin
  Result:=AllocArc;
  Result.initnul;
  Result.bp.ListPos.Owner:=owner;
end;

procedure SetArcGeomProps(AArc:PGDBObjArc;const args:array of const);
var
  counter:integer;
begin
  counter:=low(args);
  AArc^.Local.P_insert:=CreateVertexFromArray(counter,args);
  AArc^.R:=CreateDoubleFromArray(counter,args);
  AArc^.StartAngle:=CreateDoubleFromArray(counter,args);
  AArc^.EndAngle:=CreateDoubleFromArray(counter,args);
end;

function AllocAndCreateArc(owner:PGDBObjGenericWithSubordinated;
  const args:array of const):PGDBObjArc;
begin
  Result:=AllocAndInitArc(owner);
  SetArcGeomProps(Result,args);
end;

class function GDBObjARC.CreateInstance:PGDBObjArc;
begin
  Result:=AllocAndInitArc(nil);
end;

begin
  RegisterDXFEntity(GDBArcID,'ARC','Arc',@AllocArc,@AllocAndInitArc,@SetArcGeomProps,@AllocAndCreateArc);
end.
