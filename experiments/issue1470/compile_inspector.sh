#!/usr/bin/env bash
# Проверка настоящего модуля регистрации свойств тем же FPC, что и nodtests.
# Сначала запустить make -C cad_source/zengine/tests meshdisplaytests.
# Нужны также zscriptbase, zscript, zbaseutilsgui, zobjectinspector, fphunspell,
# ztoolbars, lape. Это проверка компиляции GUI, без запуска приложения.
set -euo pipefail
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
cd "$ROOT"
L=${LAZARUS_DIR:-$(ls -d /usr/lib/lazarus/*/ | sort -V | tail -1)}
OUT=${FPC_OUT:-/tmp/zcad-nodtests}
mkdir -p "$OUT"
ARGS=(-Fu"$OUT" -FU"$OUT" -FE"$OUT" -Fucad_source/other)
while IFS= read -r DIR; do ARGS+=("-Fu$DIR"); done < <(
  find cad_source/zengine cad_source/zcad cad_source/components/z* \
    cad_source/components/fphunspell cad_source/components/lape -type d)
while IFS= read -r DIR; do ARGS+=("-Fi$DIR"); done < <(
  find cad_source -name '*.inc' -printf '%h\n' | sort -u)
for DIR in components/lazutils components/buildintf components/codetools \
  components/freetype components/lazcontrols components/anchordocking \
  ide/packages/ideconfig lcl lcl/widgetset lcl/interfaces/nogui; do
  ARGS+=("-Fu$L/$DIR" "-Fi$L/$DIR")
done
ARGS+=("-Fi$L/lcl/include" "-Fi$L/ide/packages/ideconfig/include/linux"
  "-Fi$L/ide/packages/ideconfig/include/unix")
# Приложение использует расширенный fpexprpars из репозитория, а не FCL.
# shellcheck disable=SC2086
fpc -Mobjfpc ${FPC_EXTRA_OPTS:-} "${ARGS[@]}" cad_source/other/fpexprpars.pp \
  > "$OUT/build-mesh-inspector-fpexprpars.log" 2>&1
# Этот контейнер нужен только приложению и должен собираться в режиме Delphi.
# shellcheck disable=SC2086
fpc -Mdelphi ${FPC_EXTRA_OPTS:-} "${ARGS[@]}" \
  cad_source/components/zcontainers/src/uzctnrtree.pas \
  > "$OUT/build-mesh-inspector-tree.log" 2>&1
# shellcheck disable=SC2086
fpc -Mobjfpc -Sh ${FPC_EXTRA_OPTS:-} "${ARGS[@]}" \
  cad_source/components/zscriptbase/src/uzsbvarmandef.pas \
  > "$OUT/build-mesh-inspector-zscriptbase.log" 2>&1
# shellcheck disable=SC2086
fpc -Mobjfpc -Sh ${FPC_EXTRA_OPTS:-} "${ARGS[@]}" \
  cad_source/components/zscript/src/varman.pas \
  > "$OUT/build-mesh-inspector-varman.log" 2>&1
# shellcheck disable=SC2086
# Для TZToolButton.Paint приложение требует конкретный GUI-виджетсет.
fpc -Mdelphi -dLCLgtk2 ${FPC_EXTRA_OPTS:-} "${ARGS[@]}" \
  cad_source/zcad/gui/odjectinspector/uzcoiregistermultiproperties.pas \
  > "$OUT/build-mesh-inspector.log" 2>&1
printf 'Polyface mesh inspector: compiled successfully\n'
