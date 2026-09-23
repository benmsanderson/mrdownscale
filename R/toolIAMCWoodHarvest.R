#' toolIAMCWoodHarvest
#'
#' Split a reported wood harvest demand into the wood harvest weight and area
#' of each source forest, using LUH's own history to supply what IAMC does
#' not report.
#'
#' IAMC reports one number, roundwood production, as a volume. LUH needs
#' harvested carbon and harvested area from each of primf, primn, secmf,
#' secyf, secnf and pltns. Three things therefore come from the target's
#' history, per region, and each is a prior the output should declare:
#' \itemize{
#'   \item the volume-to-carbon factor, taken as harvested carbon over
#'   reported volume in the calibration year. It absorbs wood density, carbon
#'   fraction, and the difference between roundwood production and everything
#'   LUH counts as harvested, so the level matches LUH at that year and the
#'   scenario supplies only the trajectory;
#'   \item the share of harvest taken from each source forest, held at its
#'   historical mean;
#'   \item the carbon harvested per hectare of each source, which turns weight
#'   into area.
#' }
#' A region with no historical harvest of a source gets no harvest from it. A
#' region with no historical harvest at all, or no reported demand, gets none.
#'
#' @param demand magpie object with reported harvest, regions by years, one
#' data column, in the unit of \code{historicVolume}
#' @param historicWeight magpie object, harvested carbon per source forest in
#' the calibration year, kg C per year
#' @param historicArea magpie object, harvested area per source forest in the
#' calibration year, Mha per year
#' @param historicVolume magpie object, the reported harvest in the
#' calibration year, same unit as \code{demand}
#' @param factorCap a region whose carbon per unit volume exceeds this
#' multiple of the global one reports too little harvest to calibrate against
#' the target, and takes the global factor instead
#' @return list with weight (kg C per year) and area (Mha per year), regions
#' by years by source forest
#' @author Ben Sanderson
toolIAMCWoodHarvest <- function(demand, historicWeight, historicArea, historicVolume,
                                factorCap = 5) {
  sources <- getItems(historicWeight, dim = 3)
  stopifnot(setequal(sources, getItems(historicArea, dim = 3)),
            setequal(getItems(demand, dim = 1), getItems(historicWeight, dim = 1)),
            ndata(demand) == 1, ndata(historicVolume) == 1,
            demand >= 0, historicWeight >= 0, historicArea >= 0)

  regions <- getItems(demand, dim = 1)
  historicWeight <- historicWeight[regions, , ]
  historicArea <- historicArea[regions, , ]
  volume <- as.vector(historicVolume[regions, , ])
  totalWeight <- as.vector(dimSums(historicWeight, dim = 3))

  # carbon per unit of reported volume, so the calibration year matches the
  # target's level and the scenario contributes the trajectory. A region that
  # reports next to no harvest cannot support its own factor - the Middle East
  # reports 0.4 million m3 against LUH's harvest, which alone would imply a
  # thousand times the carbon per cubic metre of wood - so those fall back to
  # the global factor, and outliers are capped at a multiple of it.
  globalFactor <- sum(totalWeight) / max(sum(volume), .Machine$double.eps)
  carbonPerVolume <- ifelse(volume > 0, totalWeight / pmax(volume, .Machine$double.eps), globalFactor)
  implausible <- carbonPerVolume > factorCap * globalFactor | carbonPerVolume == 0
  if (any(implausible)) {
    toolStatusMessage("note", paste0(sum(implausible), " of ", length(volume), " regions report too ",
                                     "little harvest to calibrate against the target and take the ",
                                     "global carbon per unit volume instead: ",
                                     paste(regions[implausible], collapse = ", ")))
    carbonPerVolume[implausible] <- globalFactor
  }
  share <- as.array(historicWeight)[, 1, ] / pmax(totalWeight, .Machine$double.eps)
  # carbon per hectare harvested, from the target's own history
  yield <- as.array(historicWeight)[, 1, ] / pmax(as.array(historicArea)[, 1, ], .Machine$double.eps)

  years <- getYears(demand)
  weight <- new.magpie(regions, years, sources, fill = 0)
  area <- new.magpie(regions, years, sources, fill = 0)
  for (year in years) {
    carbon <- as.vector(demand[, year, ]) * carbonPerVolume
    for (k in seq_along(sources)) {
      weight[, year, sources[k]] <- carbon * share[, k]
      area[, year, sources[k]] <- ifelse(yield[, k] > 0, carbon * share[, k] / yield[, k], 0)
    }
  }
  toolStatusMessage("note", paste0("wood harvest split across ", length(sources),
                                   " source forests using the target's historical shares; ",
                                   "carbon per reported volume ",
                                   round(stats::median(carbonPerVolume[carbonPerVolume > 0]), 1),
                                   " kg C per unit at the median region"))
  return(list(weight = weight, area = area))
}
