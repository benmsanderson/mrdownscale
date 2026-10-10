#' toolPrimnFromConversion
#'
#' Primary non-forest land after harmonization, declining only through
#' conversion and harvest, by the rule primary forest follows.
#'
#' The harmonizers converge on the input's primn and secdn. For IAMC input
#' that split is not the model's: models report only other natural land,
#' which is split by the target's composition, and the forest crosswalk then
#' moves a model's surplus forest into it by the model's forest composition -
#' all of it into secdn for models that report no primary forest. Fading to
#' that split relabels primary land as secondary without any land use, by up
#' to 270 Mha in a region.
#'
#' Here the total of primn and secdn is kept and the split is recomputed with
#' \code{\link{toolPrimfFromHarvest}} applied to natural non-forest land: primn
#' loses its proportional share of any net loss of natural non-forest land and
#' the harvest area the scenario's wood demand implies from it, and never
#' grows. Before \code{year} the input is unchanged.
#'
#' @param x magpie object, regions by years by categories, with primn and secdn
#' @param year the first year of harmonization; from the next year on primn is recomputed
#' @param primnHarvest optional magpie object, regions by years, the primary
#' non-forest harvest area in Mha per year at the target's historical share
#' (\code{wood_harvest_area} of primn from
#' \code{\link{calcNonlandInputRecategorized}}); NULL for conversion only
#' @return x with primn and secdn re-split, their sum unchanged
#' @author Ben Sanderson
toolPrimnFromConversion <- function(x, year, primnHarvest = NULL) {
  stopifnot(c("primn", "secdn") %in% getItems(x, dim = 3),
            year %in% getYears(x, as.integer = TRUE))
  years <- getYears(x, as.integer = TRUE)
  after <- years >= year
  natural <- dimSums(x[, after, c("primn", "secdn")], dim = 3)
  if (is.null(primnHarvest)) {
    primnHarvest <- new.magpie(getItems(x, dim = 1), year, "primn", fill = 0)
  }
  primn <- toolPrimfFromHarvest(natural, x[, year, "primn"], collapseDim(primnHarvest, dim = 3))
  primn <- magclass::setNames(primn, "primn")
  x[, after, "primn"] <- primn
  x[, after, "secdn"] <- magclass::setNames(natural, "secdn") - magclass::setNames(primn, "secdn")
  return(x)
}
