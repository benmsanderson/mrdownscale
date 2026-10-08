# Building the v0.1 ScenarioMIP land-use products

This folder rebuilds the products from scratch: seven ScenarioMIP markers,
annual, on the LUH3 0.25 degree grid, 2020-2500, from the public IIASA
ScenarioMIP R10 release and LUH3 history. It is fork-only; nothing here is
proposed upstream.

The products are **not LUH3** and not an official CMIP7 forcing. They are a
documented backup built with this fork of mrdownscale (branch
`graft/primary-forest-harvest`) and graft (Python post-processing and
diagnostics, pinned in `config.sh`).

## Run it

```bash
release/build.sh            # all seven markers, two at a time (~10 h)
release/build.sh vl h       # some
```

Edit `config.sh`, or override its variables from the environment, for other
machines: the IAMC release file, the LUH3 history folder (`GRAFT_LUH3`), the
work folder, and how R is set up. The build refuses to run with uncommitted
changes to the fork's package or to the graft checkout, uses a fresh madrat
main folder (so an empty cache), and writes `WORK/manifest.txt` with the code
commits, input checksums and environment.

To package a finished build as release datasets (split at 2100, LUH3's
conventions, README and checksums per dataset), checking it against an
earlier build on the way:

```bash
REFERENCE=<earlier>/output REFTAG=x7 release/package.sh
```

To check that a build reproduces an earlier one by hand:

```bash
python graft/scripts/compare_products.py WORK/madrat/output/vlr01_iamc <earlier>/vlx7_iamc
```

## Inputs

| input | where | pinned by |
|---|---|---|
| IIASA ScenarioMIP release, R10 regions (`ScenarioMIP_v0.1_R10.xlsx`, "ScenarioMIP/CMIP7 Ensemble - Data at the R10 region level", Release v0.1, September 2026) | ScenarioMIP Explorer, https://scenariomip.apps.ece.iiasa.ac.at, after accepting its licence; set `IAMC_XLSX` to the downloaded file | sha256 `73bc5c5c972e8dd80d8456e57c528bb2930787547f27c2e869a8273ddfece92b` (also in the manifest) |
| LUH3 history, `UofMD-landState-3-1-1` (states, transitions, management, static) | input4MIPs / ESGF | file names in the manifest; `graft/scripts/fetch_luh3.py` downloads them checksum-verified |
| R10 region membership per model | `graft/data/region_mappings` | graft commit |
| country of each grid cell | `graft/data/country_cell.csv.gz` (built by `graft/scripts/luh_country_mask.py` from LUH3's `ccode` and Natural Earth) | graft commit |
| GLM's harvest probability table | `graft/src/graft/data/phbio.average.7states.txt` (CMIP6 GLM; Hurtt et al. 2020) | graft commit |

**The IIASA release is not redistributed here.** Its licence
(https://scenariomip.apps.ece.iiasa.ac.at/license) permits reuse for
scientific analysis but prohibits redistributing substantial parts of the
data, and asks users to work from the current release; the file must be
downloaded by whoever runs the build. A later release changes the checksum,
and the build then makes a different product.

## Environment

- R 4.4.2; packages as `renv.lock` (written from the library the products
  were made with). PIK's packages, served by r-universe at their latest
  version only, are pinned to their exact GitHub commits, so the lock still
  restores once newer versions are out:
  `renv::restore(lockfile = "release/renv.lock", library = <empty library>)`;
  a GitHub token (`GITHUB_PAT`) avoids API rate limits.
- Python 3.12; packages as `graft/requirements-lock.txt`.

## Verified

- 6-7 October 2026: `build.sh` from an empty cache (fresh graft clone, Python
  environment from the lock) reproduced all seven markers bit for bit
  against the products analysed during development (11 files per marker,
  largest difference 0).
- 8 October 2026: the R lock restored into an empty library (94 packages
  installed, the other 59 supplied at the locked versions by the cluster's R
  installation), and VL rebuilt with only that library: bit for bit again.
  A machine without those 59 packages installs them from the lock too; that
  path is untested.
- The fork's test suite passes at `graft-v0.1` (43 tests, 176 expectations).

## The flow

For each marker:

1. **Input** (graft): the R10 export of the IAMC release
   (`iamc_coverage.py --export`), carried to 2150 on the extension protocol's
   ramp (`extend_iamc.py`): land-use change ramps to zero in 2149 from the
   2060-2100 mean rate; wood demand to half its 2100 value; a roundwood index
   for models that report none (COFFEE).
2. **Staging** (graft `prepare_iamc_source.py`, via `run_markers.sh`): one
   marker's data, its model's region mapping and the country mask, as madrat's
   IAMC source.
3. **Harmonization and downscaling** (this fork, `fullSCENARIOMIP` via
   `graft/scripts/run_scenariomip.R`): harmonized to LUH3 history at 2025,
   primary forest from the scenario's wood demand (`fadeForestHarvest`), gross
   transitions with plantations folded into secondary forest, to 2150.
4. **Post-processing** (graft `extend_marker.sh`): annual output; closure
   check under LUH3's rule; secondary age and biomass; the static period
   2150-2500 with the maintenance wood harvest; a summary.

Every logic change in the fork, with its reason and evidence, is in the
fork's `README.md` ledger. Decisions and their measurements are in graft's
`docs/16-review-plan.md`.
