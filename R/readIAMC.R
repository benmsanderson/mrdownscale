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
#'   \item \code{country_cell.csv}: columns x, y and country (ISO3), the
#'   country per grid cell of the target grid. Built from LUH3's own static
#'   ccode field, so this path needs no MAgPIE run; see
#'   scripts/luh_country_mask.py in the graft repository. May be gzipped.
#'   \item \code{region_mapping.csv}: columns region and country, where region
#'   matches the Region column of data.csv and country is an ISO3 code. The
#'   region names of a common region set (R5, R10, ...) cover different
#'   countries in different models, so this file must be the one belonging to
#'   the model being read.
#' }
#'
#' @param subtype data, regionMapping or countryCell
#' @return for data, a data.frame in IAMC wide format; for regionMapping, a
#' data.frame with columns region, country and lowRes; for countryCell, a
#' data.frame with columns x, y and country
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
  } else if (subtype == "countryCell") {
    file <- if (file.exists("country_cell.csv")) "country_cell.csv" else "country_cell.csv.gz"
    if (!file.exists(file)) {
      stop("country_cell.csv(.gz) not found. It holds the country of each grid cell, ",
           "built from LUH3's static ccode field by scripts/luh_country_mask.py ",
           "in the graft repository.")
    }
    cells <- utils::read.csv(file)
    stopifnot(c("x", "y", "country") %in% colnames(cells))
    return(list(x = cells[, c("x", "y", "country")],
                class = "data.frame",
                description = "IAMC country per grid cell"))
  } else {
    stop("Unexpected subtype, only data, regionMapping and countryCell are accepted")
  }
}
