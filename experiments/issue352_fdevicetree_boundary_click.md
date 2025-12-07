# Issue #352: Strange Behavior in velectrnav.pas - Click on Last Node Near Boundary

## Problem Description

**Issue:** https://github.com/veb86/zcadvelecAI/issues/352

When FDeviceTree is not fully stretched vertically and the last node is near the form boundary:
- Clicking the left mouse button on the last node does NOT trigger the click command
- vstDev is NOT populated

When the form is expanded and the last node is moved away from the boundary:
- Everything works correctly

## Root Cause Analysis

The issue is caused by using `OnClick` event for handling node selection in `FDeviceTree`. The `OnClick` event has limitations when clicking near form boundaries:

1. **Event timing**: `OnClick` fires only when both mouse down and mouse up occur on the same control at the same position
2. **Boundary interference**: Near form boundaries, slight mouse movement between down and up can prevent the click from being registered
3. **Control interference**: Splitters, scrollbars, or resize handles can intercept mouse events near boundaries

### Current Implementation (velectrnav.pas:274)

```pascal
FDeviceTree.OnClick := @TreeClick;
```

The `TreeClick` procedure (lines 1137-1165) uses `FDeviceTree.GetFirstSelected` to get the selected node. However, if the click doesn't register properly, the selection doesn't change and `vstDev` doesn't get updated.

### Comparison with vstDev

`vstDev` uses both `OnClick` (line 289) and `OnMouseUp` (line 291) handlers:
- `OnClick` for general interaction
- `OnMouseUp` for reliable handling of right-clicks and special cases (lines 663-690)

This pattern provides more reliable event handling, especially near boundaries.

## Solution

Replace the unreliable `OnClick` event with `OnMouseUp` event for `FDeviceTree`. The `OnMouseUp` event:
- Always fires when the mouse button is released, regardless of minor position changes
- Provides X, Y coordinates for hit testing
- Is not affected by form boundary issues

### Implementation Steps

1. Add `TreeMouseUp` procedure to handle mouse up events on FDeviceTree
2. Use hit testing to determine which node was clicked
3. Manually trigger the selection and filtering logic
4. Keep the existing `TreeClick` as a fallback for keyboard navigation

### Code Changes

#### In private section (around line 84):

```pascal
procedure TreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer); // Mouse up handler for reliable click detection
```

#### In AllSelActionExecute (around line 274):

```pascal
// Keep existing OnClick for keyboard navigation
FDeviceTree.OnClick := @TreeClick;
// Add OnMouseUp for reliable mouse click detection (especially near boundaries)
FDeviceTree.OnMouseUp := @TreeMouseUp;
```

#### New procedure implementation (after TreeClick):

```pascal
// Mouse up handler for FDeviceTree
// Uses hit testing for reliable node selection even when clicking near form boundaries
// This fixes the issue where clicking on the last node near the bottom edge
// doesn't trigger the selection and doesn't populate vstDev
procedure TVElectrNav.TreeMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  Node: PVirtualNode;
  HitInfo: THitInfo;
begin
  // Only handle left mouse button
  if Button <> mbLeft then Exit;

  // Get the node at the click position using hit testing
  FDeviceTree.GetHitTestInfoAt(X, Y, True, HitInfo);

  // Check if we hit a node
  if Assigned(HitInfo.HitNode) then
  begin
    Node := HitInfo.HitNode;

    // Select the clicked node
    FDeviceTree.ClearSelection;
    FDeviceTree.Selected[Node] := True;
    FDeviceTree.FocusedNode := Node;

    // Trigger the filtering logic (same as TreeClick)
    TreeClick(Sender);
  end;
end;
```

## Why This Fix Works

1. **Hit testing**: Uses `GetHitTestInfoAt(X, Y, ...)` to reliably determine which node was clicked, even near boundaries
2. **Explicit selection**: Manually sets the selection and focus, ensuring the node is properly selected
3. **OnMouseUp reliability**: Mouse up events fire consistently, regardless of minor mouse movements
4. **Boundary independence**: Works correctly even when the node is near the bottom edge of the control

## Testing Checklist

To verify the fix:

1. Load devices using the "*" button (AllSelActionExecute)
2. Resize the form so FDeviceTree is not fully stretched vertically
3. Click on the last visible node near the bottom boundary
4. ✅ Expected: Node gets selected and vstDev is populated
5. Expand the form and click on nodes away from the boundary
6. ✅ Expected: Normal behavior continues to work

## Alternative Solutions Considered

### Alternative 1: Add ScrollBarOptions to FDeviceTree
- **Pros**: Might reduce boundary issues
- **Cons**: Doesn't address the root cause, wastes screen space

### Alternative 2: Adjust panel constraints
- **Pros**: Simple configuration change
- **Cons**: Doesn't fix the actual event handling issue

### Alternative 3: Use OnNodeClick event
- **Pros**: VirtualStringTree-specific event
- **Cons**: Still has the same timing issues as OnClick

## Related Code Patterns

This fix follows the same pattern used in `vstDev`:
- vstDevMouseUp (lines 663-690) uses hit testing with mouse coordinates
- Handles edge cases where OnClick might not fire properly
- Provides reliable event handling for user interactions

## Files to Modify

- `cad_source/zcad/velec/connectmanager/gui/velectrnav.pas` - Add TreeMouseUp handler
