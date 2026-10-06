#!/usr/bin/env bash
# Build the release products from scratch: every marker 2020-2500, from the
# public IAMC release and LUH3 history, with this fork at its current commit
# and graft at GRAFT_REF (release/config.sh). A fresh madrat main folder means
# an empty cache, so nothing computed by earlier code is reused.
#
#   release/build.sh               # all seven markers
#   release/build.sh vl h          # some
#
# Writes WORK/madrat/output/<marker><TAG>_iamc/{annual,extension}, a log per
# marker in WORK/logs, and WORK/manifest.txt (code, inputs, environment).
set -euo pipefail
FORK=$(cd "$(dirname "$0")/.." && pwd)
source "$FORK/release/config.sh"
MARKERS=${*:-$MARKERS_DEFAULT}
eval "$R_SETUP" >/dev/null 2>&1 || true
export LC_ALL=C.UTF-8 HDF5_USE_FILE_LOCKING=FALSE
export OMP_NUM_THREADS=$THREADS OPENBLAS_NUM_THREADS=$THREADS MKL_NUM_THREADS=$THREADS \
       R_DATATABLE_NUM_THREADS=$THREADS MC_CORES=$THREADS

# --- code: this fork, clean; graft at its pinned commit, clean ---------------
if [ -n "$(git -C "$FORK" status --porcelain -- R inst DESCRIPTION NAMESPACE)" ]; then
  echo "the fork has uncommitted changes to its package; commit them first" >&2; exit 1
fi
[ -d "$GRAFT_DIR/.git" ] || git clone -q "$GRAFT_REPO" "$GRAFT_DIR"
git -C "$GRAFT_DIR" fetch -q origin 2>/dev/null || true
git -C "$GRAFT_DIR" checkout -q "$GRAFT_REF"
if [ -n "$(git -C "$GRAFT_DIR" status --porcelain --untracked-files=no)" ]; then
  echo "the graft checkout has uncommitted changes" >&2; exit 1
fi
if [ ! -x "$GRAFT_PY" ]; then
  python3 -m venv "$GRAFT_DIR/.venv"
  "$GRAFT_DIR/.venv/bin/pip" install -q -r "$GRAFT_DIR/requirements-lock.txt"
  "$GRAFT_DIR/.venv/bin/pip" install -q -e "$GRAFT_DIR"
fi

# --- a fresh madrat main folder: LUH3 history linked in, empty cache ---------
mkdir -p "$WORK/madrat/sources/LUH3" "$WORK/logs"
for f in "$GRAFT_LUH3"/multiple-*_input4MIPs_landState_CMIP_UofMD-landState-3-1-1_gn*.nc; do
  ln -sf "$f" "$WORK/madrat/sources/LUH3/"
done
export MADRAT_MAINFOLDER=$WORK/madrat

# --- inputs: the R10 export, carried to 2150 on the extension ramp -----------
(cd "$GRAFT_DIR" && "$GRAFT_PY" scripts/iamc_coverage.py "$IAMC_XLSX" --export "$WORK/land_r10.csv" \
   > "$WORK/logs/inputs.log" 2>&1 &&
 "$GRAFT_PY" scripts/extend_iamc.py "$WORK/land_r10.csv" "$WORK/land_r10_ext.csv" >> "$WORK/logs/inputs.log" 2>&1)

# --- manifest ------------------------------------------------------------------
{
  echo "release $RELEASE, tag $TAG, built $(date -u +%FT%TZ) on $(hostname)"
  echo "fork   $(git -C "$FORK" rev-parse HEAD) ($(git -C "$FORK" rev-parse --abbrev-ref HEAD))"
  echo "graft  $(git -C "$GRAFT_DIR" rev-parse HEAD)"
  echo "options $OPTS, to $YEAR_END, then 2500"
  echo "R      $(Rscript -e 'cat(R.version.string)' 2>/dev/null); packages: release/renv.lock"
  echo "python $("$GRAFT_PY" --version 2>&1); packages: graft/requirements-lock.txt"
  echo "inputs (sha256):"
  sha256sum "$IAMC_XLSX" "$GRAFT_DIR/data/country_cell.csv.gz" "$GRAFT_DIR"/data/region_mappings/*.csv \
            "$GRAFT_DIR/src/graft/data/phbio.average.7states.txt" | sed 's/^/  /'
  echo "  LUH3 history: $(ls "$GRAFT_LUH3"/*.nc | xargs -n1 basename | tr '\n' ' ')"
} > "$WORK/manifest.txt"

# --- the markers, PARALLEL at a time ------------------------------------------
build_one() {
  local k=$1
  (cd "$GRAFT_DIR" && PY="$GRAFT_PY" PERMARKER=1 OPTS="$OPTS" TAGSUFFIX="$TAG" YEAR_END="$YEAR_END" \
     EXPORT="$WORK/land_r10_ext.csv" MRDOWNSCALE="$FORK" scripts/run_markers.sh "$k" &&
   PY="$GRAFT_PY" scripts/extend_marker.sh "$MADRAT_MAINFOLDER/output/${k}${TAG}_iamc") \
    > "$WORK/logs/$k.log" 2>&1
  echo "$k: exit $? $(date)"
}
running=0
for k in $MARKERS; do
  build_one "$k" &
  running=$((running + 1))
  if [ "$running" -ge "$PARALLEL" ]; then wait -n; running=$((running - 1)); fi
  sleep 60   # staggered, so two runs do not compute the same LUH3 target at once
done
wait
echo "done $(date); manifest: $WORK/manifest.txt"
