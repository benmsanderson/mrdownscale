# Downscale and harmonize land use data

R package **mrdownscale**, version **0.51.2**

  [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.11244475.svg)](https://doi.org/10.5281/zenodo.11244475) [![R build status](https://github.com/pik-piam/mrdownscale/workflows/check/badge.svg)](https://github.com/pik-piam/mrdownscale/actions) [![codecov](https://codecov.io/gh/pik-piam/mrdownscale/branch/master/graph/badge.svg)](https://app.codecov.io/gh/pik-piam/mrdownscale) [![r-universe](https://pik-piam.r-universe.dev/badges/mrdownscale)](https://pik-piam.r-universe.dev/builds)

## Purpose and Functionality

Downscale and harmonize land use data (e.g. MAgPIE or WITCH)
    using high resolution reference data (e.g. LUH2v2h or LUH3).


## Changes in this fork

This fork (`benmsanderson/mrdownscale`) runs the pipeline on public IAMC
releases, e.g. the ScenarioMIP R10 data, with LUH3 as the target. It is
maintained with the [graft](https://github.com/benmsanderson/graft) project
and is being prepared as a series of single-purpose pull requests to
`pik-piam/mrdownscale` (graft `docs/16-review-plan.md`). The ledger below
lists every change to upstream behaviour, so each can be reviewed on its own.
Options marked *opt-in* leave upstream's default output unchanged.

### Ledger

| purpose | commit(s) | what changes |
|---|---|---|
| IAMC input | `d564f82` `f891f95` `4877f5e` `0fed242` | `readIAMC` and its land categories: land states from an IAMC release |
| country per cell | `d61dfed` | country of each grid cell from LUH3's `ccode`, no MAgPIE clustermap |
| IAMC wood harvest and fertilizer | `64afe7a` `65b24eb` `ad69db6` `53dd312` `ccaa796` | nonland input from reported roundwood and nitrogen, split with LUH3's history |
| forest definition | `976d009` `335629b` | IAMC forest put on LUH's forest definition before harmonizing |
| secondary land | `8088012` | LUH's rule for secondary land when downscaling IAMC input |
| fadeForest time step | `fe778bf` | primary forest's extrapolated decline per year, not per output step (upstream PR #72) |
| primary forest from wood demand | `7e3cd0a` | *opt-in* harmonization `fadeForestHarvest`, below |
| ScenarioMIP writer: gross transitions | `dcbece9` `51a86a4` | *opt-in* gross transitions, and plantations folded into secondary forest to match the states |
| ScenarioMIP writer: LUH3 harvest semantics | `373451d` `4b32916` | with gross transitions only; below |
| ScenarioMIP writer: time axis, history splice | `c5d4e9f` | a decodable time axis; output starts from LUH3 history |
| attribution | `3340489` | CICERO/graft attribution and declared limitations; fork only |

### Branch `graft/primary-forest-harvest`

Three changes, each tested (`tests/testthat/test-toolPrimfFromHarvest.R`,
`test-toolHarvestConventionLUH3.R`) and checked on the VL and H markers
against the published LUH3 scenarios.

**1. Primary-to-secondary flow is booked as harvest (`373451d`).** LUH3 has no
`primf_to_secdf` or `primn_to_secdn`; primary land becomes secondary through
`primf_harv` and `primn_harv`. The gross transitions derived from states carried
that flow as a transition, while the harvest area was the whole primary
decline, so read with LUH3's rule, as an ESM reads it, every hectare moved
twice: VL primary forest fell at 21 Mha/yr where the states said 10.6.
`toolHarvestConventionLUH3` sets the primary harvest area cell by cell to the
flow and drops the transition. Harvest area and carbon that no flow backs go
to the same cell's secondary source. Total harvested carbon is unchanged.

**2. Primary land is harvested before it is cleared (`4b32916`).** Net state
changes cannot tell "primary cleared for crops" from "primary harvested to
secondary, secondary cleared for crops" in one cell, and book the former. In
cells whose secondary land gains, primary conversion to other uses now goes
through secondary, up to the harvest area the cell carries. States are
unchanged.

**3. Primary forest follows the scenario's wood demand (`7e3cd0a`, opt-in).**
`fadeForest` caps primary forest at an extrapolation of its historical decline;
the cap binds every step, so all seven markers lost 10.5 Mha/yr whatever the
scenario. With `harmonization = "fadeForestHarvest"`, `toolPrimfFromHarvest`
takes the primary harvest area the scenario's own roundwood implies at the
target's historical primary share (from `calcNonlandInputRecategorized`),
scales it by the primary fraction of forest relative to the harmonization
year, and adds the primary share of any net forest loss. In LUH3's history and
both scenarios the primary share of harvest stays within 0.7-1.0 of the
primary fraction of forest. There is no free parameter, and at the
harmonization year the split is the target's own.

**Effect, against LUH3** (2025-2095 means; areas at 2100):

| | H before | H after | LUH3-H | VL before | VL after | LUH3-VL |
|---|---|---|---|---|---|---|
| primary forest loss, Mha/yr, LUH3 rule | 20.9 | 8.96 | 8.99 | 20.9 | 7.43 | 5.25 |
| of which cleared for other uses | 4.41 | 2.51 | 2.08 | 1.58 | 0.36 | 0.60 |
| primary share of harvested carbon | 0.48 | 0.33 | 0.36 | 0.51 | 0.40 | 0.45 |
| primary forest 2100, gap | -9.0% | -0.7% | | -23.9% | -10.3% | |
| secondary forest 2100, gap | -1.8% | -7.0% | | +8.3% | -0.9% | |

States and transitions close under LUH3's rule, harvest included, to 0.0001
Mha in every checked year. What remains is not from these changes. VL keeps
primary forest 10% low because LUH3-VL's total wood harvest halves in the
2030s, which REMIND-MAgPIE's published roundwood does not show. H's secondary
forest is 7% low because the harmonized forest total is 4.4% low; with primary
forest now right, the whole deficit falls on secondary.

## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r
install.packages("mrdownscale")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r
update.packages()
```

## Tutorial

The package comes with vignettes describing the basic functionality of the package and how to use it. You can load them with the following command (the package needs to be installed):

```r
vignette("basicUsage")        # Basic Usage of mrdownscale
vignette("downscaleNewModel") # How to downscale data from a new model
```

## Questions / Problems

In case of questions / problems please contact Pascal Sauer <pascal.sauer@pik-potsdam.de>.

## Citation

To cite package **mrdownscale** in publications use:

Sauer P, Dietrich J (2026). "mrdownscale: Downscale and harmonize land use data." doi:10.5281/zenodo.11244475 <https://doi.org/10.5281/zenodo.11244475>. Version: 0.51.2, <https://github.com/pik-piam/mrdownscale>.

A BibTeX entry for LaTeX users is

 ```latex
@Misc{,
  title = {mrdownscale: Downscale and harmonize land use data},
  author = {Pascal Sauer and Jan Philipp Dietrich},
  doi = {10.5281/zenodo.11244475},
  date = {2026-09-22},
  year = {2026},
  url = {https://github.com/pik-piam/mrdownscale},
  note = {Version: 0.51.2},
}
```

## Funding

This research constitutes a contribution to the project OptimESM, which has received funding from the European Union’s Horizon Europe research and innovation programme under grant agreement No 101081193.
