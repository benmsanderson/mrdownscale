#' toolIAMCNonlandRecategorized
#'
#' Turn the reported wood harvest volume and fertilizer total of an IAMC
#' release into the nonland categories the target uses.
#'
#' IAMC reports a harvest volume, its industrial and fuel parts, and a
#' nitrogen total. The target wants harvested carbon and harvested area from
#' each of primf, primn, secmf, secyf, secnf and pltns, and fertilizer per
#' crop type. What IAMC does not report is taken from the target's own
#' history, aggregated to the input's regions:
#' \itemize{
#'   \item the split of harvest across source forests, and the carbon
#'   harvested per hectare of each, from the mean of the last ten historical
#'   years (\code{\link{toolIAMCWoodHarvest}});
#'   \item the carbon per unit of reported volume, at the last year that is
#'   both reported and historical, so the level matches the target there and
#'   the scenario supplies the trajectory;
#'   \item fertilizer per crop type, splitting the reported total by each
#'   crop's share of cropland in that region and year.
#' }
#'
#' @inheritParams calcNonlandInput
#' @param target name of the target dataset; only luh3 is supported, since
#' the priors come from its transitions
#' @return nonland input data in the target's categories
#' @author Ben Sanderson
toolIAMCNonlandRecategorized <- function(input, target) {
  if (target != "luh3") {
    stop("iamc nonland input needs target = \"luh3\", its harvest history supplies the priors")
  }
  landMha <- calcOutput("LandInputRecategorized", input = input, target = target, aggregate = FALSE)
  nonland <- calcOutput("NonlandInput", input = input, aggregate = FALSE)
  nonland <- nonland[getItems(landMha, dim = 1), , ]
  geometry <- attr(landMha, "geometry")
  crs <- attr(landMha, "crs")

  sources <- c("primf", "primn", "secmf", "secyf", "secnf", "pltns")
  transitions <- readSource("LUH3", subtype = "transitions", subset = 1995:2024, convert = FALSE)
  cellAreaKm2 <- readSource("LUH3", subtype = "cellArea", convert = FALSE)
  historicYears <- as.integer(terra::time(transitions))
  reported <- getYears(nonland, as.integer = TRUE)
  calibrationYear <- max(intersect(reported, historicYears))
  window <- intersect(seq(calibrationYear - 9, calibrationYear), historicYears)
  toolStatusMessage("note", paste0("iamc nonland priors from the target's history: shares and ",
                                   "yields averaged over ", min(window), "-", max(window),
                                   ", level matched at ", calibrationYear))

  # the target's harvest, per source forest, aggregated to the input's regions
  regions <- as.SpatVector(landMha[, 1, 1])[, c(".region", ".id")]
  # layers are named "y2020..primf_bioh", so match on the variable, not the year
  perRegion <- function(layer) {
    extracted <- terra::extract(layer, regions, sum, na.rm = TRUE)
    pmax(extracted[[2]], 0)  # LUH carries a few negative values of rounding size
  }
  historicWeight <- new.magpie(getItems(landMha, dim = 1), calibrationYear, sources, fill = 0)
  historicArea <- new.magpie(getItems(landMha, dim = 1), calibrationYear, sources, fill = 0)
  for (source in sources) {
    inWindow <- which(endsWith(names(transitions), paste0(source, "_bioh")) & historicYears %in% window)
    stopifnot(length(inWindow) == length(window))
    historicWeight[, , source] <- rowMeans(vapply(inWindow, function(i) perRegion(transitions[[i]]),
                                                  numeric(nrow(regions))))
    inWindow <- which(endsWith(names(transitions), paste0(source, "_harv")) & historicYears %in% window)
    historicArea[, , source] <- rowMeans(vapply(inWindow,
                                                function(i) perRegion(transitions[[i]] * cellAreaKm2 / 10^4),
                                                numeric(nrow(regions))))
  }

  harvest <- toolIAMCWoodHarvest(demand = nonland[, , "wood_harvest_demand.roundwood"],
                                 historicWeight = historicWeight,
                                 historicArea = historicArea,
                                 historicVolume = nonland[, calibrationYear, "wood_harvest_demand.roundwood"])
  bioh <- add_dimension(harvest$weight, dim = 3.1, add = "category", "bioh")
  harvestArea <- add_dimension(harvest$area, dim = 3.1, add = "category", "wood_harvest_area")

  # roundwood and fuelwood shares, as the target names them
  industrial <- collapseDim(nonland[, , "wood_harvest_demand.industrial"], dim = 3)
  fuel <- collapseDim(nonland[, , "wood_harvest_demand.fuel"], dim = 3)
  total <- pmax(industrial + fuel, .Machine$double.eps)
  weightType <- mbind(magclass::setNames(industrial / total, "roundwood"),
                      magclass::setNames(fuel / total, "fuelwood"))
  weightType <- add_dimension(weightType, dim = 3.1, add = "category", "harvest_weight_type")

  # fertilizer, split by each crop's share of that region's cropland
  cropTypes <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per")
  cropland <- toolAggregateCropland(landMha, cropTypes = cropTypes, keepOthers = FALSE)
  cropland <- cropland[, reported, cropTypes]
  share <- cropland / pmax(dimSums(cropland, dim = 3), .Machine$double.eps)
  fertilizer <- collapseDim(nonland[, , "fertilizer.nitrogen"], dim = 3) * share
  fertilizer <- add_dimension(fertilizer, dim = 3.1, add = "category", "fertilizer")

  out <- mbind(bioh, harvestArea, weightType, fertilizer)
  names(dimnames(out)) <- c("region.id", "year", "category.data")
  attr(out, "geometry") <- geometry
  attr(out, "crs") <- crs
  return(out)
}
