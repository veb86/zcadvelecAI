# Issue 1470: polyface mesh display mode

Objective: implement and verify all requirements in upstream PR #1471.
The issue has no comments or required sub-issues; the PR initially has no
conversation comments, inline comments, or reviews.

## Requirements and verification

- [x] Register an editable Boolean polyface mesh display property in
  `uzcoiregistermultiproperties.pas` using the existing inspector mechanism.
- [x] `False` renders the wireframe without submitting filled faces.
- [x] `True` renders filled mesh faces, including triangles and quadrilaterals.
- [x] Preserve existing edge visibility, transformed geometry, and cloning.
- [x] Reproduce the missing feature with an automated regression before coding.
- [x] Capture before/after renders using the actual rendering path.
- [x] Run focused compiled regressions and the repository's local CI checks.
- [x] Add compiled and OpenGL visual regressions to CI.

Final PR review, latest CI results, and the issue closing reference are recorded
in https://github.com/veb86/zcadvelecAI/pull/1471.

## Working assumptions

The requested Boolean is an entity property (consistent with the issue's
explicit inspector registration location). New and imported meshes default to
wireframe to retain existing performance. Filled display must use absolute DXF
vertex indices because negative indices hide edges rather than faces. No
release workflow/package version was found in the initial workflow inventory.

## Reproduction evidence

The compiled FPCUnit regression `FilledFacesAreSubmitted` ran before the
implementation and failed with `"filled triangle" expected: <1> but was: <0>`;
the same mesh submitted its three visible edges. The reproduction used the
real `GDBObjPolyFaceMesh.DrawGeometry` with a recording drawer. An unused
`uzcinterface` import was removed to avoid pulling the application UI into this
engine-only regression. Initial documentation CI passed on commit `323d24c07`.

## Implementation and evidence

| Requirement | Implementation | Verification |
| --- | --- | --- |
| Editable Boolean in the inspector | `ShowSolid` / **Solid faces**, Russian **Заполнение граней**; existing editable Boolean handlers | Actual `uzcoiregistermultiproperties.pas` compiles for GTK2 with `compile_inspector.sh` |
| False means wireframe | Both constructors default `ShowSolid` to False; triangle submission is gated | `ConstructorsDefaultToWireframe`, `SwitchingBackStopsSubmittingFaces`; captured default commands equal pre-fix commands byte for byte |
| True fills faces | One triangle per triangular face, two per quad in contour order, using existing SOLID triangle drawing API | `FilledFacesAreSubmitted`, `QuadUsesContourOrder`, OpenGL pixel comparison |
| Retain geometry behavior | Read current WCS vertices, retain original edge path, clone the flag | `HiddenEdgesDoNotHideFaces`, `ClonePreservesDisplayMode`, `FillUsesTransformedVertices` |
| Guard malformed input | Check face size and absolute vertex indices; skip zero-area triangles | `InvalidFacesAreSkipped`, `DegenerateTrianglesAreSkipped` |

The focused suite contains 10 FPCUnit tests, including the existing allocation
regression. All pass. All 13 existing NOD test programs also pass. Optional
`POLYFACEMESH_TRACE` compilation enables `LM_Info` messages for solid drawing;
normal builds keep this tracing disabled.

## Reproduce the checks and images

Use FPC 3.2.2 and Lazarus sources with the submodules listed in `nodtests.sh`:

```sh
make -C cad_source/zengine/tests nodtests
make -C cad_source/zengine/tests meshdisplaytests
bash experiments/issue1470/run_visual.sh
```

The visual check additionally needs Mesa's `libegl1`, `libgl1`, and
`libgl1-mesa-dri`. It compiles `capture_mesh.lpr`, captures the real mesh
`DrawGeometry` calls, and replays those calls in a software OpenGL context.
It verifies an interior pixel and more than 30,000 changed pixels, exactly
12 filled triangles, no wireframe triangles, and identical edge commands.
The observed result is 109,351 filled pixels. Output images and command
streams go to `$FPC_OUT/mesh-renders` (default `/tmp/zcad-nodtests/mesh-renders`).
CI runs the same check and uploads both images as an artifact.

The committed `docs/screenshots/issue1470-before.png` was rendered from the
mesh at the reproducing commit `85faa1523` with `POLYFACEMESH_BASELINE` defined
for the capture program. Its commands and image match the new default
wireframe mode exactly. `issue1470-after.png` uses the filled mode. These are
OpenGL renders of captured entity commands, not screenshots of the desktop UI.

To compile the actual inspector registration too, initialize the extra
submodules listed at the top of `compile_inspector.sh`, then run:

```sh
bash experiments/issue1470/compile_inspector.sh
```

This compiles the desktop unit for GTK2 without launching the application.
`LAZARUS_DIR`, `FPC_OUT`, and `FPC_EXTRA_OPTS` work as in the existing test runner.

## Existing broader-suite failures

`python3 -m pytest -q test_*.py` reports 144 passed and 25 failed on both
upstream master `22ec09195307ab9bce71d9a490614476193007ff` and this branch,
with exactly the same failed test names. Full recursive pytest collection
also finds pre-existing missing NumPy/SciPy dependencies, invalid UTF-8 fixture
input, and experiments with hardcoded absent files.

The older `testzengine.lpr` runner cannot compile on that same baseline:
`uzctmtextwrap.pas(29,7) Error: Argument cannot be assigned to`.
This was reproduced by compiling the unchanged test unit from a separate
upstream checkout with the same FPC and dependencies. The focused mesh runner
avoids that unrelated compile failure. Neither broader suite is part of the
repository's existing CI workflow; the NOD CI checks and the new mesh checks
pass locally.
