# Configuration for release/build.sh. Every value can be overridden from the
# environment; the defaults are CICERO's cic-nac cluster.

# this fork's release, and the graft commit it is built with
RELEASE=${RELEASE:-v0.1}
TAG=${TAG:-r01}                       # suffix of the output folders, <marker><TAG>_iamc
GRAFT_REPO=${GRAFT_REPO:-https://github.com/benmsanderson/graft.git}
GRAFT_REF=${GRAFT_REF:-50b012396274976f3c5608980359e2f6790f060e}
GRAFT_DIR=${GRAFT_DIR:-$PWD/graft}    # a checkout at GRAFT_REF, cloned if missing
GRAFT_PY=${GRAFT_PY:-$GRAFT_DIR/.venv/bin/python}

# inputs
IAMC_XLSX=${IAMC_XLSX:-/storage/no-backup-nac/users/bensan/graft/ScenarioMIP_v0.1_R10.xlsx}
export GRAFT_LUH3=${GRAFT_LUH3:-/storage/no-backup-nac/LUH2/UofMD-landState-3-1-1}

# where the build writes: a fresh madrat main folder (sources, cache, output)
WORK=${WORK:-$PWD/work-$TAG}

# R on the cluster; leave empty where R, GDAL and netCDF are on the path
R_SETUP=${R_SETUP:-"source /etc/profile.d/z00_lmod.sh; module load R/4.4.2-gfbf-2024a GDAL/3.10.0-foss-2024a GEOS/3.12.2-GCC-13.3.0 PROJ/9.4.1-GCCcore-13.3.0 netCDF/4.9.2-gompi-2024a"}

# the product: gross transitions, plantations folded into secondary forest,
# primary forest from wood demand; to 2150 on the extension ramp, then 2500
OPTS=${OPTS:-gross,fold,harvest}
YEAR_END=${YEAR_END:-2150}
MARKERS_DEFAULT="vl l ln m ml h hl"
PARALLEL=${PARALLEL:-2}               # marker runs at a time; HDF5 is unreliable beyond 2
THREADS=${THREADS:-12}
