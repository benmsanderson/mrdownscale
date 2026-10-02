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
| forest and urban definitions | `976d009` `335629b` `e310623` | IAMC forest and urban land put on LUH's definitions before harmonizing (crosswalks) |
| secondary land | `8088012` | LUH's rule for secondary land when downscaling IAMC input |
| fadeForest time step | `fe778bf` | primary forest's extrapolated decline per year, not per output step (upstream PR #72) |
| primary forest from wood demand | `7e3cd0a` | *opt-in* harmonization `fadeForestHarvest`, below |
| ScenarioMIP writer: gross transitions | `dcbece9` `51a86a4` | *opt-in* gross transitions, and plantations folded into secondary forest to match the states |
| ScenarioMIP writer: LUH3 harvest semantics | `373451d` `3e3d041` `58781ef` | with gross transitions only; below (`4b32916`, a re-routing of primary clearing, was tried and withdrawn in `58da5c2`; the cell-by-cell rule of `58781ef` restores it on a different ground) |
| ScenarioMIP writer: time axis, history splice | `c5d4e9f` | a decodable time axis; output starts from LUH3 history |
| attribution | `3340489` | CICERO/graft attribution and declared limitations; fork only |

### Branch `graft/primary-forest-harvest`

Three changes, each tested (`tests/testthat/test-toolPrimfFromHarvest.R`,
`test-toolHarvestConventionLUH3.R`, `test-toolUrbanCrosswalk.R`) and run on
all seven ScenarioMIP markers; VL and H are checked against the published
LUH3 scenarios.

**1. Primary-to-secondary flow is booked as harvest (`373451d`).** LUH3 has no
`primf_to_secdf` or `primn_to_secdn`; primary land becomes secondary through
`primf_harv` and `primn_harv`. The gross transitions derived from states carried
that flow as a transition, while the harvest area was the whole primary
decline, so read with LUH3's rule, as an ESM reads it, every hectare moved
twice: VL primary forest fell at 21 Mha/yr where the states said 10.6.
`toolHarvestConventionLUH3` drops the transition and books the flow as harvest.

*Revised 2 October (`3e3d041`, `58781ef`).* The first version set the harvest
area to the derived flow and moved the rest of the harvest, with its carbon,
to secondary forest. That emptied primary harvest in the target's own history
(2024: 0.214 PgC against LUH3's 0.658, totals matching), because gross
transitions derived from net states cannot see primary land harvested and
then cleared in one step, and because the wood-harvest chain spreads its
regional harvest over cells independently of where primary land declines.
mrdownscale's chain defines primary harvest as the whole primary decline; the
rule now applies that definition cell by cell: each cell's primary harvest
area is its decline, all its primary conversions go through secondary land
(`primf_to_c3ann` becomes `secdf_to_c3ann`), and harvest carbon in cells with
no decline moves, at its harvested density, to the region's cells whose
decline the chain did not cover; only what a region cannot place goes to
secondary forest (8-12% of primary harvest carbon in VL and H). States are
unchanged. Primary harvest in the history years now matches LUH3 within 4%
(H 2020, 2023, 2024: 0.642, 0.663, 0.658 PgC against 0.665, 0.658, about
0.66), and it is smooth through the harmonization start (H 2025, 2030, 2050:
0.670, 0.660, 0.723). The cost: no primary land is cleared directly, where
LUH3 clears 2-3 Mha/yr in H; it is harvested first, then cleared as
secondary land.

**2. Primary forest follows the scenario's wood demand (`7e3cd0a`, opt-in).**
`fadeForest` caps primary forest at an extrapolation of its historical decline;
the cap binds every step, so all seven markers lost 10.5 Mha/yr whatever the
scenario. With `harmonization = "fadeForestHarvest"`, `toolPrimfFromHarvest`
takes the primary harvest area the scenario's own roundwood implies at the
target's historical primary share (from `calcNonlandInputRecategorized`),
scales it by the primary fraction of forest relative to the harmonization
year, and adds the primary share of any net forest loss. It is a reduced form
of GLM's own rule, which splits harvest between primary and mature secondary
forest in proportion to their biomass. There is no free parameter, and at the
harmonization year the split is the target's own. Land harmonization now reads
the nonland chain for the primary harvest area; that is the method, and it is
opt-in.

**3. IAMC urban land on LUH's level (`e310623`).** Models report built-up area
on their own definitions or not at all: against LUH3's 86 Mha in 2025, GCAM
reports a flat 59, COFFEE 55, and MESSAGE and WITCH none, so harmonization
emptied a third, or all, of LUH's urban land by 2050. `toolUrbanCrosswalk`
shifts each region's urban land by its gap to the target in 2025, from
non-forest natural land, as `toolForestCrosswalk` does for forest; where a
model leaves too little natural land in some year, the rest comes from its
other land in proportion to area. The model's urban change is kept, on LUH's
level; a model that reports none holds LUH's.

**Effect** (2025-2095 means; areas at 2100):

| | H before | H after | LUH3-H | VL before | VL after | LUH3-VL |
|---|---|---|---|---|---|---|
| primary forest loss, Mha/yr, LUH3 rule | 20.9 | 8.96 | 8.99 | 20.9 | 7.43 | 5.25 |
| of which cleared for other uses | 4.41 | 4.92 | 2.08 | 1.58 | 1.33 | 0.60 |
| primary share of harvested carbon | 0.48 | 0.20 | 0.36 | 0.51 | 0.34 | 0.45 |
| primary forest 2100, gap | -9.0% | -0.7% | | -23.9% | -10.3% | |
| secondary forest 2100, gap | -1.8% | -7.5% | | +8.3% | -1.1% | |
| urban 2100, gap | -33.2% | +1.7% | | +0.1% | +14.5% | |

Primary-forest loss across the seven markers now spans 5.3-9.0 Mha/yr, where
it was 10.53-10.57 in all of them. On all seven, states and transitions close
under LUH3's rule, harvest included, to 0.0001 Mha in every checked year;
primary land never expands; urban never falls below its 2025 level; forest
joins LUH3 history within 0.6 Mha.

Known and not changed here:

- Primary clearing is attributed by mrdownscale's net transitions: where a cell
  loses primary forest and gains cropland in the same year it is booked as
  clearing, so H clears 4.92 Mha/yr of primary against LUH3's 2.08 with the
  same total loss. GLM would split such clearing by the cell's primary and
  secondary shares. A fitted re-routing (`4b32916`) was withdrawn.
- VL keeps primary forest 10% low: LUH3-VL's own wood harvest input halves in
  the 2030s (zero in India, DR Congo and Myanmar from 2040) while
  REMIND-MAgPIE's published roundwood falls 3%; this follows the published
  roundwood.
- H's secondary forest is 7.5% low because forest in total is 4.6% low; part
  may be the secondary-land rule (`8088012`) moving land after downscaling.
- VL urban is 14.5% high: the crosswalk keeps REMIND-MAgPIE's urban growth
  (+33 Mha) on LUH's level, where LUH3-VL ends at REMIND's own 2100 level.
- Pasture is 17% high in VL: mrdownscale's pasture/rangeland weights.
- mrdownscale's own soft checks fail as they did before this branch (harvest
  area over available land by up to 0.2 Mha, downscaling changing a category's
  global sum by up to ~1.2%).

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
