# Issue #32 Analysis: Polyline Grip Shape and Dragging Issues

## Problem Summary
1. **Grip Shape**: Segment center grips should be rectangular (oriented along segment direction) instead of square
2. **Dragging Distortion**: Polyline distorts during segment center grip dragging

## Root Cause Analysis

### 1. Grip Drawing
Currently all grips are drawn as square points in:
- `UGDBControlPointArray.pas:63` - calls `dc.drawer.DrawPoint3DInModelSpace(point^.worldcoord,dc.DrawingContext.matrixs)`
- This renders as GL_Points (square) regardless of grip type

Segment center grips are marked with `pointtype:=os_midle` in `uzeentpolyline.pas:282`

### 2. Dragging Distortion
In `uzeentpolyline.pas:288-308`, `rtmodifyonepoint` method:
```pascal
procedure GDBObjPolyline.rtmodifyonepoint(const rtmod:TRTModifyData);
var
  segmentIndex:integer;
  v1,v2:PGDBVertex;
  offset:GDBVertex;
begin
  if rtmod.point.vertexnum>=0 then begin
    inherited rtmodifyonepoint(rtmod);
  end else begin
    segmentIndex:=-(rtmod.point.vertexnum+1);
    v1:=vertexarrayinocs.getDataMutable(segmentIndex);  // BUG: Using OCS
    if segmentIndex<VertexArrayInWCS.Count-1 then
      v2:=vertexarrayinocs.getDataMutable(segmentIndex+1)
    else
      v2:=vertexarrayinocs.getDataMutable(0);

    offset:=rtmod.dist;
    v1^:=VertexAdd(v1^,offset);  // BUG: Adding offset directly
    v2^:=VertexAdd(v2^,offset);
  end;
end;
```

**Problem**: Uses `vertexarrayinocs` and adds offset directly, which doesn't properly account for coordinate transformations.

**Correct approach** (from `uzeentline.pas:643-648`):
```pascal
tv:=uzegeometry.VertexSub(CoordInOCS.lend,CoordInOCS.lbegin);
tv:=uzegeometry.VertexMulOnSc(tv,0.5);
tv2:=VertexAdd(rtmod.point.worldcoord,rtmod.dist);
CoordInOCS.lbegin:=VertexSub(tv2,tv);
CoordInOCS.lend:=VertexAdd(tv2,tv);
```

## Solution Plan

### Part 1: Fix Grip Shape
Need to modify grip drawing to:
1. Detect `os_midle` point type
2. Calculate segment direction from adjacent vertices
3. Draw oriented rectangle instead of square point

This requires:
- Store segment direction in control point descriptor OR
- Calculate direction at draw time from parent entity vertices
- Add new drawing method for oriented rectangles

### Part 2: Fix Dragging Distortion
Modify `uzeentpolyline.pas:rtmodifyonepoint`:
1. Calculate segment half-vector in OCS
2. Calculate new center position from `rtmod.point.worldcoord + rtmod.dist`
3. Set both vertices relative to new center using half-vector

## Implementation Files to Modify
1. `cad_source/zengine/core/entities/uzeentpolyline.pas` - Fix rtmodifyonepoint
2. `cad_source/zengine/containers/UGDBControlPointArray.pas` - Add segment direction data
3. `cad_source/zengine/zgl/common/uzglviewareadata.pas` - Extend control point descriptor
4. OpenGL/DX drawer implementations - Add oriented rectangle drawing
