#' toolPrimfFromHarvest
#'
#' Primary forest after the harmonization start, driven by the scenario's own
#' wood demand and forest loss rather than extrapolated from history.
#'
#' In LUH3, primary forest is forest never harvested, and most of what it loses
#' is harvest: 85 percent in VL and 77 percent in H over 2025-2100. How much of
#' a region's wood comes from primary forest follows how much of its forest is
#' still primary. Across LUH3's history and its two published scenarios the
#' primary share of harvested carbon stays within 0.7 to 1.0 times the primary
#' fraction of forest, while secondary forest's carbon per hectare stays flat
#' (doc 10, issue 13 of graft). So, per region and step,
#' \itemize{
#'   \item primary harvest area is the area the scenario's wood demand implies
#'   at the target's historical primary share, scaled by the primary fraction
#'   of forest relative to the harmonization year: as primary forest thins,
#'   more of the wood comes from secondary forest. At the harmonization year
#'   this is the target's own split, so there is no step and no free parameter;
#'   \item a net loss of forest takes primary forest in proportion to the
#'   primary fraction; forest gains are secondary;
#'   \item primary forest never exceeds forest, and never grows.
#' }
#' The rate is per year and applied over each step's length, so it does not
#' depend on the output's time step.
#'
#' @param forest magpie object, regions by years from the harmonization start
#' on, the harmonized forest total (primf + secdf) in Mha
#' @param primf0 magpie object, regions by the harmonization start year, the
#' target's primary forest in Mha
#' @param primfHarvest magpie object, regions by years, primary forest harvest
#' area in Mha per year at the historical primary share; interpolated to the
#' years of \code{forest} and held constant beyond its ends
#' @return magpie object, regions by years of \code{forest}, primary forest in Mha
#' @author Ben Sanderson
toolPrimfFromHarvest <- function(forest, primf0, primfHarvest) {
  regions <- getItems(forest, dim = 1)
  stopifnot(setequal(regions, getItems(primfHarvest, dim = 1)),
            setequal(regions, getItems(primf0, dim = 1)),
            nyears(primf0) == 1, primfHarvest >= 0)
  years <- getYears(forest, as.integer = TRUE)
  hp1 <- getYears(primf0, as.integer = TRUE)
  stopifnot(hp1 == years[1])

  primfHarvest <- magclass::time_interpolate(primfHarvest[regions, , ], years,
                                             integrate_interpolated_years = TRUE,
                                             extrapolation_type = "constant")[, years, ]
  primfHarvest <- magclass::setNames(primfHarvest, "primf")
  forest <- magclass::setNames(forest, "primf")

  fraction <- function(p, f) {
    out <- p / f
    out[is.na(out) | is.infinite(out)] <- 0
    out
  }
  primf <- pmin(magclass::setNames(primf0[regions, , ], "primf"), forest[, hp1, ])
  fraction0 <- fraction(primf, forest[, hp1, ])

  previousYear <- hp1
  for (year in years[years > hp1]) {
    p <- setYears(primf[, previousYear, ], NULL)
    forestBefore <- setYears(forest[, previousYear, ], NULL)
    forestNow <- setYears(forest[, year, ], NULL)
    primaryFraction <- fraction(p, forestBefore)

    relative <- fraction(primaryFraction, setYears(fraction0, NULL))
    harvest <- setYears(primfHarvest[, previousYear, ], NULL) * relative
    cleared <- forestBefore - forestNow
    cleared[cleared < 0] <- 0
    loss <- (year - previousYear) * harvest + cleared * primaryFraction

    next_ <- p - loss
    next_ <- pmin(next_, forestNow, p)
    next_[next_ < 0] <- 0
    primf <- mbind(primf, setYears(next_, year))
    previousYear <- year
  }
  toolStatusMessage("note", paste0("primary forest from the scenario's wood demand: global loss ",
                                   round(sum(primf[, hp1, ]) - sum(primf[, max(years), ]), 1),
                                   " Mha over ", hp1, "-", max(years)))
  return(primf)
}
