#' calcStatesNC
#'
#' Prepare data to be written as LUH-style states.nc file. Call this via calcOutput
#' in a full function, and set calcOutput's file argument to a .nc file path.
#'
#' @param outputFormat options: ESM, ScenarioMIP
#' @inheritParams calcLandInput
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset remove years from the returned data which are not in yearsSubset
#' @param harmonization name of harmonization method, see \code{\link{toolGetHarmonizer}}
#' @param downscaling name of downscaling method, currently only "magpieClassic"
#' @param foldPlantations add plantations into secondary forest and drop the
#' pltns state, so the output can be spliced onto LUH3 history without a step
#' at the join. The plantation share is then reported as manaf by
#' \code{\link{calcManagementNC}}.
#' @return data prepared to be written as a LUH-style states.nc file
#' @author Pascal Sauer, Jan Philipp Dietrich
calcStatesNC <- function(outputFormat, input, harmonizationPeriod, yearsSubset, harmonization,
                         downscaling, foldPlantations = FALSE) {
  x <- calcOutput("LandReport", outputFormat = outputFormat, input = input,
                  harmonizationPeriod = harmonizationPeriod, yearsSubset = yearsSubset,
                  harmonization = harmonization, downscaling = downscaling, aggregate = FALSE)

  statesVariables <- c("c3ann", "c3nfx", "c3per", "c4ann", "c4per", "pastr",
                       "primf", "primn", "range", "secdf", "secdn", "urban")
  if (outputFormat == "ScenarioMIP" && !foldPlantations) {
    statesVariables <- c(statesVariables, "pltns")
  }
  if (outputFormat == "ScenarioMIP" && foldPlantations) {
    # LUH3 carries no plantation state, so a product that does cannot be
    # spliced onto LUH3 history: at the join secondary forest drops by the
    # plantation area and a state that was zero throughout history appears.
    # Folding them back in removes the step; calcManagementNC keeps the share
    # as manaf so the information is not lost.
    x[, , "secdf"] <- x[, , "secdf"] + setNames(x[, , "pltns"], "secdf")
    toolStatusMessage("note", paste0("plantations folded into secondary forest for LUH3 ",
                                     "compatibility (max ", round(max(x[, , "pltns"]), 4),
                                     " of a cell); the share is reported as manaf"))
  }
  x <- x[, , statesVariables]

  # years since the epoch; toolAddMetadataNC converts the written axis to
  # days, which is what a 365-day calendar can actually express
  x <- setYears(x, getYears(x, as.integer = TRUE) - 1970)

  if (outputFormat == "ScenarioMIP") {
    expectedVariables <- c("primf", "primn", "secdf", "secdn", "pastr", "range", "urban",
                           "c3ann", "c3per", "c4ann", "c4per", "c3nfx")
    if (!foldPlantations) {
      expectedVariables <- c(expectedVariables, "pltns")
    }
    toolExpectTrue(setequal(getItems(x, 3), expectedVariables), "variable names are as expected")
  }

  return(list(x = x,
              isocountries = FALSE,
              unit = "1",
              min = 0,
              max = 1,
              cache = FALSE,
              description = "data prepared to be written as a LUH-style states.nc file"))
}
