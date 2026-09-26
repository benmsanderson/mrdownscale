#' fullSCENARIOMIP
#'
#' Run the pipeline to generate harmonized and downscaled data to report for ScenarioMIP.
#' LUH3 is used as historical reference dataset for harmonization and downscaling.
#' Write .nc files, print full report on consistency checks and write it to report.log.
#'
#' @param rev revision number of the data. If not provided the current date will be used instead.
#' When called via madrat::retrieveData rev will be converted to numeric_version.
#' @inheritParams calcLandInput
#' @param scenario scenario name to be included in filenames
#' @param harmonizationPeriod Two integer values, before the first given
#' year the target dataset is used, after the second given year the input
#' dataset is used, in between harmonize between the two datasets
#' @param yearsSubset remove years from the returned data which are not in yearsSubset
#' @param harmonization name of harmonization method, see \code{\link{toolGetHarmonizer}}
#' @param downscaling name of downscaling method, currently only "magpieClassic"
#' @param compression compression level of the resulting .nc files, possible values are integers from 1-9,
#' 1 = fastest, 9 = best compression
#' @param progress boolean defining whether progress should be printed
#' @param grossTransitions write the gross land transitions alongside wood
#' harvest, which the ScenarioMIP deliverable itself does not carry
#' @param foldPlantations add plantations into secondary forest and report
#' their share as manaf, so the output splices onto LUH3 history without a
#' step at the join
#' @param institution institution attribute written to the nc files
#' @param institutionId institution_id attribute written to the nc files
#' @param contact contact attribute written to the nc files
#' @param ncTitle title attribute written to the nc files
#' @param references references attribute written to the nc files
#' @param furtherInfoUrl further_info_url attribute written to the nc files
#' @param downscalingTool harmonization_downscaling_tool attribute written to
#' the nc files
#' @param comment optional comment attribute written to the nc files, used to
#' declare known limitations of the product; omitted if NULL
#'
#' @author Pascal Sauer
fullSCENARIOMIP <- function(rev = numeric_version("0"), input = "magpie", scenario = "",
                            harmonizationPeriod = c(2025, 2050),
                            yearsSubset = 1995:2100,
                            harmonization = "fadeForest", downscaling = "magpieClassic",
                            compression = 2, progress = TRUE, grossTransitions = FALSE,
                            foldPlantations = FALSE,
                            institution = "CICERO Center for International Climate Research",
                            institutionId = "CICERO",
                            contact = "benjamin.sanderson@cicero.oslo.no",
                            ncTitle = paste0("ScenarioMIP land-use forcing harmonized and ",
                                             "downscaled onto LUH3 using mrdownscale and graft"),
                            references = paste0("https://github.com/benmsanderson/graft, ",
                                                "https://github.com/benmsanderson/mrdownscale and ",
                                                "https://wcrp-cmip.org/mips/scenariomip/"),
                            furtherInfoUrl = "https://github.com/benmsanderson/graft",
                            downscalingTool = paste0("https://github.com/benmsanderson/mrdownscale ",
                                                     "(fork of https://github.com/pik-piam/mrdownscale)"),
                            comment = paste0(
                              "Research product derived from IAMC-format ScenarioMIP land-use ",
                              "projections downscaled onto LUH3 by graft using a fork of mrdownscale. ",
                              "Not an official UofMD/PIK LUH3 dataset. Known limitations: ",
                              "(1) primary forest declines at ~10.5 Mha/yr near-identically across ",
                              "all seven ScenarioMIP markers, carrying little scenario differentiation; ",
                              "(2) the land-carbon flux implied by this forcing reconciles only ~1/5 ",
                              "of the land sink reported by the source IAMs. Not suitable for ",
                              "applications requiring scenario-differentiated primary forest or ",
                              "closed carbon budgets without independent verification.")) {
  revision <- if (identical(rev, numeric_version("0"))) format(Sys.time(), "%Y-%m-%d") else rev

  fileSuffix <- paste0("_input4MIPs_landState_ScenarioMIP_",
                       scenario, if (scenario != "") "-",
                       revision, "_gn_", min(yearsSubset), "-", max(yearsSubset), ".nc")

  writeArgs <- list(compression = compression, missval = 1e20, progress = progress,
                    gridDefinition = c(-179.875, 179.875, -89.875, 89.875, 0.25))

  metadataArgs <- list(revision = revision, missingValue = 1e20, resolution = 0.25,
                       compression = compression, harmonizationPeriod = harmonizationPeriod,
                       activityId = "ScenarioMIP",
                       references = references,
                       targetMIP = "ScenarioMIP",
                       ncTitle = ncTitle,
                       referenceDataset = paste0("LUH3 historic from https://aims2.llnl.gov/search/input4mips/ ",
                                                 "(institution_id = 'UofMD' and mip_era = 'CMIP7')"),
                       furtherInfoUrl = furtherInfoUrl,
                       institution = institution, institutionId = institutionId,
                       contact = contact, host = institution,
                       downscalingTool = downscalingTool, comment = comment)

  ncFile <- paste0("multiple-states", fileSuffix)
  calcOutput("StatesNC", outputFormat = "ScenarioMIP", input = input,
             harmonizationPeriod = harmonizationPeriod,
             yearsSubset = yearsSubset,
             harmonization = harmonization, downscaling = downscaling,
             foldPlantations = foldPlantations,
             aggregate = FALSE, file = ncFile, writeArgs = writeArgs)
  do.call(toolAddMetadataNC, c(ncFile = ncFile, metadataArgs))

  ncFile <- paste0("multiple-management", fileSuffix)
  calcOutput("ManagementNC", outputFormat = "ScenarioMIP", input = input,
             harmonizationPeriod = harmonizationPeriod,
             yearsSubset = yearsSubset,
             harmonization = harmonization, downscaling = downscaling,
             foldPlantations = foldPlantations,
             aggregate = FALSE, file = ncFile, writeArgs = writeArgs)
  do.call(toolAddMetadataNC, c(ncFile = ncFile, metadataArgs))

  # iamc input carries wood harvest, which is what the ScenarioMIP
  # transitions file holds
  if (input == "magpie" || startsWith(input, "iamc")) {
    ncFile <- paste0("multiple-transitions", fileSuffix)
    calcOutput("TransitionsNC", outputFormat = "ScenarioMIP", input = input,
               harmonizationPeriod = harmonizationPeriod,
               yearsSubset = yearsSubset,
               harmonization = harmonization, downscaling = downscaling,
               grossTransitions = grossTransitions,
               foldPlantations = foldPlantations,
               aggregate = FALSE, file = ncFile, writeArgs = writeArgs)
    do.call(toolAddMetadataNC, c(ncFile = ncFile, metadataArgs))
  }

  toolWriteMadratLog(logPath = "consistencyCheck.log")
}
