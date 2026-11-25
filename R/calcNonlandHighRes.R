#' calcNonlandHighRes
#'
#' Calculate a high resolution dataset from the low resolution input dataset
#' and high resolution data.
#'
#' Wood harvest area is disaggregated using the maximum possible harvest per
#' year which is based on the downscaled land data. Bioh
#' is disaggregated using the just disaggregated wood harvest area as weight.
#' Fertilizer is in kg ha-1 yr-1, so we simply use the low res/region value for each
#' cell corresponding to that region. Harvest weight type is disaggregated
#' using the nonland target data in the first year of
#' the harmonization period as weight.
#'
#' @inheritParams calcNonlandInput
#' @param target name of a target dataset, see \code{\link{calcLandTarget}}
#' for available target datasets
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset vector of years to keep in the output dataset
#' @param harmonization name of harmonization method, see \code{\link{toolGetHarmonizer}}
#' @param downscaling name of downscaling method, currently only "magpieClassic"
#' @return downscaled nonland data
#'
#' @examples
#' \dontrun{
#'   calcOutput("NonlandHighRes", input = "magpie", target = "luh2mod",
#'              harmonizationPeriod = c(2015, 2050), yearsSubset = 2015:2100,
#'              harmonization = "fade", downscaling = "magpieClassic")
#' }
#' @author Pascal Sauer
calcNonlandHighRes <- function(input, target, harmonizationPeriod, yearsSubset, harmonization, downscaling) {
  hp <- harmonizationPeriod
  x <- calcOutput("NonlandHarmonized", input = input, target = target,
                  harmonizationPeriod = hp, harmonization = harmonization, aggregate = FALSE)
  x <- x[, getYears(x, as.integer = TRUE) %in% yearsSubset, ]

  nonlandTarget <- calcOutput("NonlandTarget", target = target, endOfHistory = hp[1], aggregate = FALSE)
  # IAMC input: the target's harvest area over its last ten years, to downscale harvest where it happens
  if (startsWith(input, "iamc")) {
    histYears <- intersect(seq(hp[1] - 9, hp[1]), terra::time(nonlandTarget))
    histHarvest <- nonlandTarget[[terra::time(nonlandTarget) %in% histYears &
                                    grepl("wood_harvest_area$", names(nonlandTarget))]]
    histHarvest <- as.magpie(histHarvest)
    getItems(histHarvest, 3, raw = TRUE) <- sub("^(.+?)_(.+)$", "\\2.\\1", getItems(histHarvest, 3))
    names(dimnames(histHarvest))[3] <- "category.data"
    histHarvest <- dimSums(histHarvest, dim = 2) / length(histYears)
  }
  nonlandTarget <- as.magpie(nonlandTarget[[terra::time(nonlandTarget) %in% yearsSubset]])
  getItems(nonlandTarget, 3, raw = TRUE) <- sub("^(.+?)_(.+)$", "\\2.\\1", getItems(nonlandTarget, 3))
  names(dimnames(nonlandTarget))[3] <- "category.data"

  futureYears <- setdiff(getYears(x, TRUE), getYears(nonlandTarget, TRUE))

  resmap <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)

  landHighRes <- calcOutput("LandHighRes", input = input, target = target,
                            harmonizationPeriod = hp, yearsSubset = yearsSubset,
                            harmonization = harmonization, downscaling = downscaling, aggregate = FALSE)

  message("downscaling wood harvest area...")
  harvestArea <- x[, futureYears, "wood_harvest_area"]
  harvestAreaWeight <- toolMaxHarvestPerYear(landHighRes)[, futureYears, ]
  harvestAreaDownscaled <- toolAggregate(harvestArea, resmap, weight = harvestAreaWeight,
                                         from = "lowRes", to = "cell", dim = 1, zeroWeight = "fix")
  if (startsWith(input, "iamc")) {
    # an IAM region is large: weighting by land alone would move its secondary harvest into
    # countries that barely harvest, so keep the target's pattern (harvest per unit of land at the
    # end of history); primary harvest stays with the land, where the downscaled primary decline is
    sec <- grep("\\.(sec|pltns)", getItems(harvestArea, 3), value = TRUE)
    landYears <- getYears(landHighRes, as.integer = TRUE)
    landEnd <- toolMaxHarvestPerYear(landHighRes, timestepAdjust = FALSE)
    landEnd <- landEnd[, landYears[which(landYears == hp[1]) + 1], sec]
    histHarvest <- histHarvest[getItems(landEnd, 1), , sec]
    intensity <- setYears(histHarvest, NULL) / setYears(landEnd, NULL)
    intensity[!is.finite(intensity)] <- 0
    # LUH3 has no plantation harvest; plantations are carved out of its secdf, so they take secdf's
    pltns <- grep("\\.pltns$", sec, value = TRUE)
    secdf <- grep("\\.(secyf|secmf)$", sec, value = TRUE)
    if (length(pltns) == 1 && length(secdf) > 0 && sum(histHarvest[, , pltns]) <= 0) {
      secdfLand <- setYears(landEnd[, , secdf[1]], NULL)
      secdfIntensity <- dimSums(setYears(histHarvest[, , secdf], NULL), dim = 3) / secdfLand
      secdfIntensity[!is.finite(secdfIntensity)] <- 0
      intensity[, , pltns] <- as.vector(secdfIntensity)
    }
    secDownscaled <- toolHarvestAreaDownscale(harvestArea[, , sec], resmap, harvestAreaWeight[, , sec], intensity)
    harvestAreaDownscaled[, , sec] <- secDownscaled[getItems(harvestAreaDownscaled, 1),
                                                    getYears(harvestAreaDownscaled), sec]
  }

  message("downscaling wood harvest mass...")
  bioh <- x[, futureYears, "bioh"]
  weightBioh <- harvestAreaDownscaled
  getItems(weightBioh, 3.1) <- sub("wood_harvest_area$", "bioh", getItems(weightBioh, 3.1))
  stopifnot(setequal(getItems(weightBioh, 3), getItems(bioh, 3)))
  biohDownscaled <- toolAggregate(bioh, resmap, weight = weightBioh, from = "lowRes", to = "cell",
                                  dim = 1, zeroWeight = "fix")

  # fertilizer is in kg ha-1 yr-1 already, use low res/region value for each cell corresponding to that region
  fertilizerDownscaled <- setCells(x[resmap$lowRes, futureYears, "fertilizer"], resmap$cell)

  message("downscaling wood harvest weight type...")
  harvestType <- x[, futureYears, "harvest_weight_type"]
  weightHarvestType <- collapseDim(nonlandTarget[, nyears(nonlandTarget), "harvest_weight_type"])
  harvestTypeDownscaled <- toolAggregate(harvestType, resmap, weight = weightHarvestType,
                                         from = "lowRes", to = "cell", dim = 1, zeroWeight = "fix")


  out <- mbind(nonlandTarget,
               mbind(harvestAreaDownscaled, biohDownscaled, fertilizerDownscaled, harvestTypeDownscaled))

  # checks
  inSum <- dimSums(x[, , "fertilizer", invert = TRUE], dim = 1)
  outSum <- dimSums(out[, , "fertilizer", invert = TRUE], dim = 1)
  stopifnot(identical(getYears(inSum), getYears(outSum)),
            setequal(getItems(inSum, 3), getItems(outSum, 3)))
  toolExpectLessDiff(inSum, outSum, 10^-3,
                     "No significant global sum difference per category before and after downscaling")

  # for years after harmonization make sure that total global fertilizer matches input
  # not applicable for absoluteChanges, which deviates from the input by design
  if (harmonization != "absoluteChanges") {
    years <- getYears(out, TRUE)
    years <- years[years >= hp[2]]
    fertilizerInput <- calcOutput("NonlandInputRecategorized", input = input, target = target, aggregate = FALSE)
    fertilizerInput <- dimSums(fertilizerInput[, years, "fertilizer"], 1)
    fertilizerOutput <- toolFertilizerTg(out[, years, "fertilizer"], landHighRes[, years, ])
    toolExpectLessDiff(fertilizerInput, dimSums(fertilizerOutput, 1), 10^-5,
                       "Total global fertilizer after harmonization period matches input data")
  }
  toolCheckFertilizer(out[, , "fertilizer"])

  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(x, dim = 3)),
                 "Nonland categories remain unchanged")
  toolExpectTrue(min(out) >= 0, "All values are >= 0")

  toolCheckWoodHarvestArea(out[, , "wood_harvest_area"], landHighRes, hp[1])

  return(list(x = out,
              min = 0,
              isocountries = FALSE,
              unit = "harvest_weight & bioh: kg C yr-1; harvest_area: Mha yr-1; fertilizer: kg ha-1 yr-1",
              description = "Downscaled nonland data",
              # iamc regions carry an id, as the Land functions keep;
              # cleaning drops it and the two sides stop matching
              clean_magpie = !startsWith(input, "iamc")))
}
