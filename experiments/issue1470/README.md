# Issue 1470: polyface mesh display mode

Objective: implement and verify all requirements in upstream PR #1471.
The issue has no comments or required sub-issues; the PR initially has no
conversation comments, inline comments, or reviews.

## Requirements and verification plan

- [ ] Register an editable Boolean polyface mesh display property in
  `uzcoiregistermultiproperties.pas` using the existing inspector mechanism.
- [ ] `False` renders the wireframe without submitting filled faces.
- [ ] `True` renders filled mesh faces, including triangles and quadrilaterals.
- [ ] Preserve existing edge visibility, transformed geometry, and cloning.
- [ ] Reproduce the missing feature with an automated regression before coding.
- [ ] Capture before/after renders using the actual rendering path.
- [ ] Run focused compiled regressions and the repository's local CI checks.
- [ ] Add automated verification to CI and inspect all latest PR check results.
- [ ] Review the complete diff, update PR title/body and closing reference,
  merge current upstream master if necessary, commit and push only the prepared
  branch, mark PR ready, and verify a clean worktree.

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
