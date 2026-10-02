#' toolHarvestDensity
#'
#' Carbon harvested per unit of harvest area, kg C per Mha, for each region and
#' source forest, as at the end of the history.
#'
#' The density is taken from the last historical year, where that year has
#' harvest area, and from the whole history pooled only where it has none.
#' Pooling over the whole history would set the extrapolated target to the
#' history's average: LUH3's density more than halved over 1995-2024, as
#' harvest of young secondary forest grew, so a pooled density raised
#' harvested carbon by about 15\% at the first extrapolated year, before
#' harmonization had begun.
#'
#' @param bioh magpie object, harvested carbon (kg C yr-1), regions x years x source
#' @param area magpie object, harvest area (Mha yr-1), the same dimensions
#' @return magpie object, kg C per Mha, regions x source, without a year
#' @author Ben Sanderson
toolHarvestDensity <- function(bioh, area) {
  last <- utils::tail(getYears(area), 1)
  # order is important here for correct dims: the area keeps its category
  pooled <- 1 / dimSums(area, dim = 2) * collapseDim(dimSums(bioh, dim = 2), dim = 3.1)
  lastArea <- area[, last, ]
  getYears(lastArea) <- NULL
  lastBioh <- bioh[, last, ]
  getYears(lastBioh) <- NULL
  out <- 1 / lastArea * collapseDim(lastBioh, dim = 3.1)
  useLast <- lastArea > 0 & is.finite(out)
  out[!useLast] <- pooled[!useLast]
  out[!is.finite(out)] <- 0
  return(out)
}
