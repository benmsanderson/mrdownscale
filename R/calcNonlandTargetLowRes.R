#' calcNonlandTargetLowRes
#'
#' Aggregate target nonland data to the spatial resolution of the input data in
#' preparation for harmonization. Fertilizer is converted to Tg yr-1, then aggregated and
#' converted back to kg ha-1 yr-1.
#'
#' @inheritParams calcNonlandInput
#' @param target name of a target dataset
#' @param endOfHistory years later than this are not returned
#' @return low resolution target nonland data
#' @author Pascal Sauer
calcNonlandTargetLowRes <- function(input, target, endOfHistory) {
  if (target %in% c("luh2", "luh2mod")) {
    cellAreaKm2 <- readSource("LUH2v2h", subtype = "cellArea", convert = FALSE)
    states <- readSource("LUH2v2h", subtype = "states", convert = FALSE)
  } else {
    cellAreaKm2 <- readSource("LUH3", subtype = "cellArea", convert = FALSE)
    states <- readSource("LUH3", subtype = "states", subset = 1995:2024, convert = FALSE)
    states <- states[[terra::time(states) <= endOfHistory]]
  }
  cellAreaHa <- cellAreaKm2 * 100

  xInput <- calcOutput("NonlandInputRecategorized", input = input, target = target, aggregate = FALSE)
  ref <- as.SpatVector(xInput[, 1, 1])[, c(".region", ".id")]

  # get target data in spatial resolution of input data
  xTarget <- calcOutput("NonlandTarget", target = target, endOfHistory = endOfHistory, aggregate = FALSE)

  # need absolute values for aggregating, convert from kg ha-1 yr-1 to Tg yr-1, assuming ha-1 refers to cropland
  cropland <- states[[sub("_fertilizer", "", names(xTarget["fertilizer"]))]]
  fertilizer <- xTarget["fertilizer"] * cellAreaHa * cropland / 10^9
  terra::units(fertilizer) <- "Tg yr-1"
  xTarget <- c(xTarget[[grep("fertilizer", names(xTarget), invert = TRUE)]], fertilizer)

  out <- terra::extract(xTarget, ref, sum, na.rm = TRUE, bind = TRUE)
  out <- as.magpie(out)
  getItems(out, 3, raw = TRUE) <- sub("^(.+?)_(.+)$", "\\2.\\1", getItems(out, 3))
  names(dimnames(out))[3] <- "category.data"

  landTargetLowRes <- calcOutput("LandTargetLowRes", input = input, target = target,
                                 endOfHistory = endOfHistory, aggregate = FALSE)

  lastYear <- terra::time(xTarget)[length(terra::time(xTarget))]
  mTarget <- as.magpie(xTarget[[terra::time(xTarget) == lastYear]])
  getItems(mTarget, 3, raw = TRUE) <- sub("^(.+?)_(.+)$", "\\2.\\1", getItems(mTarget, 3))
  names(dimnames(mTarget))[3] <- "category.data"
  toolExpectLessDiff(dimSums(mTarget, 1), dimSums(out[, lastYear, ], 1), 10^-5,
                     "total sum is not changed by aggregation")

  out[, , "fertilizer"] <- toolFertilizerKgPerHa(out[, , "fertilizer"], landTargetLowRes)

  stopifnot(setequal(getItems(xInput, 3), getItems(out, 3)))
  out <- out[, , getItems(xInput, 3)] # harmonize order


  toolExpectLessDiff(dimSums(out[, , "bioh"], 3),
                     dimSums(out[, , "harvest_weight_type"], 3),
                     10^5, "Harvest weight types are consistent")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")
  toolCheckFertilizer(out[, , "fertilizer"])

  return(list(x = out,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg ha-1 yr-1",
              min = 0,
              description = "Land target data at the same low resolution as the input dataset for harmonization",
              # iamc regions carry an id, as the Land functions keep;
              # cleaning drops it and the two sides stop matching
              clean_magpie = !startsWith(input, "iamc")))
}
