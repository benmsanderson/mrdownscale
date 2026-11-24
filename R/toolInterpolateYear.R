#' toolInterpolateYear
#'
#' Add a year to data reported on a coarser grid, linearly interpolated
#' between the reported years around it. IAMC releases report 2020 and 2025,
#' while LUH3's history ends in 2024; absoluteChanges needs one year present
#' in both, so the input gets 2024 rather than the target being extrapolated.
#'
#' @param x magpie object
#' @param year integer year to add; left unchanged if already present, and an
#' error if it is not between two reported years
#' @return x with the year added
#' @author Ben Sanderson
toolInterpolateYear <- function(x, year) {
  years <- getYears(x, as.integer = TRUE)
  if (year %in% years) {
    return(x)
  }
  stopifnot(any(years < year), any(years > year))
  out <- magclass::time_interpolate(x, year, integrate_interpolated_years = TRUE)
  return(out[, sort(c(years, year)), ])
}
