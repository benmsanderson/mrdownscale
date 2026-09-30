#' calcTransitionsNC
#'
#' Prepared data to be written as a LUH-style transitions.nc file
#'
#' @param outputFormat options: ESM, ScenarioMIP
#' @inheritParams calcLandInput
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset remove years from the returned data which are not in yearsSubset
#' @param harmonization name of harmonization method, see \code{\link{toolGetHarmonizer}}
#' @param downscaling name of downscaling method, currently only "magpieClassic"
#' @param grossTransitions write the gross land transitions alongside wood
#' harvest. The ScenarioMIP deliverable carries only harvest, so this is off by
#' default and changes nothing for that product; it exists because anything
#' that tracks secondary land - stand age, biomass, a bookkeeping model - needs
#' the gross flows, which net state differences cannot give.
#' @param foldPlantations fold plantation gross transitions into secondary
#' forest, to match the states fold in \code{\link{calcStatesNC}}. Without it
#' the folded secdf state carries plantation dynamics that its own transitions
#' do not, so an annual state change no longer reconciles with the gross flows.
#' Only affects the gross transitions; wood harvest (pltns_harv, pltns_bioh) is
#' left untouched. With gross transitions in the ScenarioMIP format, the
#' primary-to-secondary flow is booked as harvest, as LUH3 does, see
#' \code{\link{toolHarvestConventionLUH3}}.
#' @return data prepared to be written as a LUH-style transitions.nc file
#' @author Pascal Sauer, Jan Philipp Dietrich
calcTransitionsNC <- function(outputFormat, input, harmonizationPeriod, yearsSubset, harmonization,
                              downscaling, grossTransitions = FALSE, foldPlantations = FALSE) {
  nonland <- calcOutput("NonlandReport", outputFormat = outputFormat, input = input,
                        harmonizationPeriod = harmonizationPeriod, yearsSubset = yearsSubset,
                        harmonization = harmonization, downscaling = downscaling, aggregate = FALSE)

  nonland <- nonland[, , grep("_(bioh|harv)$", getItems(nonland, 3), value = TRUE)]

  x <- nonland[, getYears(nonland, as.integer = TRUE) %in% yearsSubset, ]

  if (outputFormat == "ESM" || grossTransitions) {
    transitions <- calcOutput("LandTransitions", outputFormat = outputFormat,
                              harmonizationPeriod = harmonizationPeriod, yearsSubset = yearsSubset,
                              harmonization = harmonization, downscaling = downscaling,
                              input = input, gross = TRUE, aggregate = FALSE)
    getItems(transitions, raw = TRUE, dim = 3) <- sub("\\.", "_to_", getItems(transitions, dim = 3))
    getSets(transitions, fulldim = FALSE)[3] <- "transitions"
    # we use to-semantics for transitions (value for 1994 describes what happens from 1993 to 1994)
    # by subtracting 1 we get from-semantics (value for 1994 describes what happens from 1994 to 1995)
    # which is what LUH uses
    getYears(transitions) <- getYears(transitions, as.integer = TRUE) - 1
    transitions <- transitions[, getYears(transitions, as.integer = TRUE) %in% yearsSubset, ]

    if (foldPlantations) {
      # calcStatesNC adds pltns into secdf and drops the pltns state, so every
      # gross flow into or out of pltns has to be redirected into secdf to keep
      # the transitions consistent with the folded states. Flows between pltns
      # and secdf become secdf-to-secdf self-transitions, which carry no
      # information and are dropped.
      items <- getItems(transitions, dim = 3)
      folded <- sub("^pltns_to_", "secdf_to_", sub("_to_pltns$", "_to_secdf", items))
      transitions <- toolAggregate(transitions, data.frame(from = items, to = folded), dim = 3)
      self <- grep("^(.+)_to_\\1$", getItems(transitions, dim = 3), value = TRUE)
      if (length(self) > 0) {
        transitions <- transitions[, , self, invert = TRUE]
      }
      getSets(transitions, fulldim = FALSE)[3] <- "transitions"
    }

    x <- mbind(x, transitions)

    if (outputFormat == "ScenarioMIP") {
      # the ScenarioMIP format claims LUH3's semantics, where harvest - not a
      # transition - carries primary land to secondary
      x <- toolHarvestConventionLUH3(x)
    }
  }

  # years since the epoch; toolAddMetadataNC converts the written axis to
  # days, which is what a 365-day calendar can actually express
  x <- setYears(x, getYears(x, as.integer = TRUE) - 1970)

  if (outputFormat == "ScenarioMIP") {
    expectedVariables <- c("primf_harv", "secdf_harv", "primn_harv", "secnf_harv", "pltns_harv",
                           "primf_bioh", "secdf_bioh", "primn_bioh", "secnf_bioh", "pltns_bioh")
    if (grossTransitions) {
      expectedVariables <- c(expectedVariables,
                             grep("_to_", getItems(x, 3), value = TRUE))
      toolStatusMessage("note", paste0("writing ", sum(grepl("_to_", getItems(x, 3))),
                                       " gross land transitions alongside wood harvest; ",
                                       "the ScenarioMIP deliverable itself carries only harvest"))
    }
    toolExpectTrue(setequal(getItems(x, 3), expectedVariables), "variable names are as expected")
  }

  return(list(x = x,
              isocountries = FALSE,
              unit = "1 or kg C yr-1",
              min = 0,
              cache = FALSE,
              description = "data prepared to be written as a LUH-style transitions.nc file"))
}
