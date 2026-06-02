#!/bin/sh
# Build & run the standalone LEADER vertex/undo logic test.
# Validates uzccommand_leader.RebuildLeaderVertices + the Undo menu item
# against the real GDBPoint3dArray container, with no graphics/style deps.
#
# Usage: sh experiments/run_leader_vertex_logic_test.sh
set -e
HERE=$(cd "$(dirname "$0")" && pwd)
SRC="$HERE/.."/cad_source
ZE="$SRC/zengine"
ZM="$SRC/components/zmath/src"
ZC="$SRC/components/zcontainers/src"
ZB="$SRC/components/zbaseutils/src"
OUT=$(mktemp -d)

fpc -Mdelphi \
  -Fu"$ZE/containers" -Fu"$ZM" -Fu"$ZC" -Fu"$ZB" -Fu"$ZE" -Fi"$ZE" \
  -FE"$OUT" -o"$OUT/leader_vertex_logic_test" \
  "$HERE/leader_vertex_logic_test.pas"

"$OUT/leader_vertex_logic_test"
