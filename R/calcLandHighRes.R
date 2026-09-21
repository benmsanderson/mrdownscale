#' calcLandHighRes
#'
#' This function performs the downscaling: It calculates a high resolution dataset
#' from the low resolution input dataset and the high resolution target dataset
#' using the given downscaling method.
#'
#' @inheritParams calcLandInput
#' @param target name of a target dataset
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset vector of years to keep in the output dataset
#' @param harmonization name of harmonization method, see \code{\link{toolGetHarmonizer}}
#' @param downscaling name of downscaling method, currently only "magpieClassic"
#' @return downscaled land use data
#' @author Jan Philipp Dietrich, Pascal Sauer
calcLandHighRes <- function(input, target, harmonizationPeriod, yearsSubset, harmonization, downscaling) {
  x <- calcOutput("LandHarmonized", input = input, target = target,
                  harmonizationPeriod = harmonizationPeriod, harmonization = harmonization,
                  aggregate = FALSE)
  x <- x[, getYears(x, as.integer = TRUE) %in% yearsSubset, ]

  hp1 <- harmonizationPeriod[1]

  xTarget <- calcOutput("LandTarget", target = target, endOfHistory = hp1, aggregate = FALSE)
  xTarget <- as.magpie(xTarget[[terra::time(xTarget) %in% yearsSubset]])

  histYears <- intersect(getYears(x, as.integer = TRUE),
                         getYears(xTarget, as.integer = TRUE))

  x <- x[, , getItems(xTarget, 3)]

  landTargetLowRes <- calcOutput("LandTargetLowRes", input = input, target = target,
                                 endOfHistory = hp1, aggregate = FALSE)
  landTargetLowRes <- landTargetLowRes[, max(histYears), getItems(x, 3)]
  stopifnot(setequal(getItems(landTargetLowRes, 1), getItems(x, 1)))

  mapping <- calcOutput("ResolutionMapping", input = input, target = target, aggregate = FALSE)

  if (downscaling == "magpieClassic") {
    out <- toolDownscaleMagpieClassic(x[, getYears(x, as.integer = TRUE) >= max(histYears), ],
                                      xTarget[, max(histYears), ],
                                      xTargetLowRes = landTargetLowRes,
                                      mapping = mapping)
    out <- out[, getYears(out, as.integer = TRUE) > max(histYears), ]
  } else {
    stop("Unsupported downscaling method \"", downscaling, "\"")
  }

  out <- mbind(xTarget[, histYears, ], out)

  # IAMC input carries the reporting model's forest dynamics, which ignore
  # where LUH's potential vegetation allows forest; apply LUH's rule for
  # secondary land so gains land where LUH would put them
  secondaryLandRule <- startsWith(input, "iamc") && target == "luh3"
  if (secondaryLandRule) {
    potentialForest <- readSource("LUH3", subtype = "potentialForest", convert = FALSE)
    potentialForest <- collapseDim(as.magpie(potentialForest), 3)
    out <- toolSecondaryLandRule(out, potentialForest, baseYear = max(histYears))
  }

  out <- toolReplaceExpansion(out, "primf", "secdf", warnThreshold = 100)
  out <- toolReplaceExpansion(out, "primn", "secdn", warnThreshold = 100)

  toolExpectTrue(identical(unname(getSets(out)), c("x", "y", "year", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(setequal(getItems(out, dim = 3), getItems(x, dim = 3)),
                 "Land categories remain unchanged")
  toolExpectLessDiff(out[, max(histYears), ], xTarget[, max(histYears), ], 10^-5,
                     paste("In", max(histYears), "output equals target"))
  toolExpectTrue(all(out >= 0), "All values are >= 0")

  outSum <- dimSums(out, dim = 3)
  toolExpectLessDiff(outSum, outSum[, 1, ], 10^-5,
                     "Total land area per cell in output stays constant over time")

  globalSumIn <- dimSums(x[, getYears(out), ], dim = 1)
  globalSumOut <- dimSums(out, dim = 1)
  toolExpectLessDiff(dimSums(globalSumIn, 3), dimSums(globalSumOut, 3), 10^-5,
                     "Total global land area remains unchanged")
  if (secondaryLandRule) {
    # the rule moves area between secdf, pltns and secdn by design, so those
    # are checked together; every other land type must be unchanged
    secondary <- c("secdf", "pltns", "secdn")
    regroup <- function(z) {
      mbind(z[, , secondary, invert = TRUE],
            magclass::setNames(dimSums(z[, , secondary], dim = 3), "secondary"))
    }
    toolExpectLessDiff(regroup(globalSumIn), regroup(globalSumOut), 10^-5,
                       "Global area of each land type remains unchanged (secdf, pltns and secdn together)")
  } else {
    toolExpectLessDiff(globalSumIn, globalSumOut, 10^-5,
                       "Global area of each land type remains unchanged")
  }
  toolPrimExpansionCheck(out)

  return(list(x = out,
              class = "magpie",
              isocountries = FALSE,
              unit = "Mha",
              min = 0,
              description = "Downscaled land use data"))
}
