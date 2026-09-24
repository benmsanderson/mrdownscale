#' toolTimeAxis
#'
#' Convert a written netCDF time axis from years to days, so that it decodes.
#'
#' The files declare \code{calendar = "365_day"}, and CF does not allow "years"
#' as the unit of such an axis: udunits defines a year as 365.2425 days, which
#' a 365-day calendar does not have, so the two cannot both be true. cftime
#' refuses the combination outright, which means \code{xarray.open_dataset} and
#' everything built on it cannot open the files at all unless the caller passes
#' \code{decode_times = FALSE}. The published LUH3 files use
#' \code{days since 850-1-1} on \code{noleap} and decode cleanly.
#'
#' magclass will not carry the day numbers - it rejects years outside the
#' y0000 format - so the axis is written in years and converted here, in the
#' one place that already has the file open. 365 days to the year is exact on
#' this calendar.
#'
#' @param nc open ncdf4 handle, in write mode
#' @param daysPerYear length of a year on the declared calendar
#' @return the unit string that was written, invisibly
#' @author Ben Sanderson
toolTimeAxis <- function(nc, daysPerYear = 365) {
  years <- ncdf4::ncvar_get(nc, "time")
  ncdf4::ncvar_put(nc, "time", years * daysPerYear)
  units <- "days since 1970-01-01 0:0:0"
  ncdf4::ncatt_put(nc, "time", "units", units)
  invisible(units)
}
