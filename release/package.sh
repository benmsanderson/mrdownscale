#!/usr/bin/env bash
# Package a finished build as release datasets, and check it against a
# reference build if one is given.
#
#   release/package.sh                      # all markers of WORK, into WORK/release
#   REFERENCE=~/madrat/output REFTAG=x7 release/package.sh
#
# Uses graft at GRAFT_REF (release/config.sh), as build.sh does.
set -euo pipefail
FORK=$(cd "$(dirname "$0")/.." && pwd)
source "$FORK/release/config.sh"
MARKERS=${*:-$MARKERS_DEFAULT}
export HDF5_USE_FILE_LOCKING=FALSE OMP_NUM_THREADS=2
unset PYTHONPATH
OUT=${OUT:-$WORK/release}
mkdir -p "$OUT" "$WORK/logs"
git -C "$GRAFT_DIR" fetch -q origin 2>/dev/null || true
git -C "$GRAFT_DIR" checkout -q "$GRAFT_REF"
# the build's manifest plus this packaging, written afresh so repackaging does not accumulate lines
{ cat "$WORK/manifest.txt"; echo "packaged with graft $(git -C "$GRAFT_DIR" rev-parse HEAD), $(date -u +%FT%TZ)"; } \
  > "$WORK/package_manifest.txt"
one() {
  local k=$1 build=$WORK/madrat/output/${1}${TAG}_iamc
  (cd "$GRAFT_DIR" &&
   if [ -n "${REFERENCE:-}" ]; then
     "$GRAFT_PY" scripts/compare_products.py "$build" "$REFERENCE/${k}${REFTAG}_iamc" --stride 25
   fi &&
   "$GRAFT_PY" scripts/package_release.py "$build" "$k" "$OUT" --version "${RELEASE#v}" --manifest "$WORK/package_manifest.txt" &&
   "$GRAFT_PY" scripts/check_closure.py "$OUT/CICERO-graft-landState-$k-${RELEASE#v}" --stride 20 &&
   "$GRAFT_PY" scripts/check_closure.py "$OUT/CICERO-graft-landState-$k-ext-${RELEASE#v}" --stride 100) \
    > "$WORK/logs/package_$k.log" 2>&1
  echo "$k: exit $? $(date)"
}
running=0
for k in $MARKERS; do
  one "$k" &
  running=$((running + 1))
  if [ "$running" -ge 3 ]; then wait -n; running=$((running - 1)); fi
done
wait
echo "packaged into $OUT"
