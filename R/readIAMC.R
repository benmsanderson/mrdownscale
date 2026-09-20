#' readIAMC
#'
#' Read land data from an IAMC-format scenario release, such as the public
#' ScenarioMIP releases, plus the region mapping that says which countries
#' each of its regions covers.
#'
#' Expects two files in the source folder:
#' \itemize{
#'   \item \code{data.csv}: IAMC wide format, one row per model, scenario,
#'   region, variable and unit, with one column per year. May contain several
#'   models and scenarios; \code{\link{calcLandInput}} selects one.
#'   \item \code{region_mapping.csv}: columns region and country, where region
#'   matches the Region column of data.csv and country is an ISO3 code. The
#'   region names of a common region set (R5, R10, ...) cover different
#'   countries in different models, so this file must be the one belonging to
#'   the model being read.
#' }
#'
#' @param subtype data or regionMapping
#' @return for data, a data.frame in IAMC wide format; for regionMapping, a
#' data.frame with columns region, country and lowRes
#'
#' @author Ben Sanderson
readIAMC <- function(subtype = "data") {
  if (subtype == "data") {
    x <- utils::read.csv("data.csv", check.names = FALSE)
    colnames(x) <- make.names(colnames(x))
    return(list(x = x,
                class = "data.frame",
                unit = paste(unique(x$Unit), collapse = ", "),
                description = "IAMC land data"))
  } else if (subtype == "regionMapping") {
    mapping <- utils::read.csv("region_mapping.csv")
    stopifnot(c("region", "country") %in% colnames(mapping),
              !anyDuplicated(mapping$country))
    mapping <- mapping[, c("region", "country")]

    # artificial region ids, as expected downstream; see readCOFFEE
    regions <- unique(mapping$region)
    addId <- data.frame(region = regions, lowRes = paste0(regions, ".", seq_along(regions)))
    mapping <- merge(mapping, addId, "region")

    return(list(x = mapping,
                class = "data.frame",
                description = "IAMC region mapping"))
  } else {
    stop("Unexpected subtype, only data and regionMapping are accepted")
  }
}
