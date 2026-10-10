#' toolPrimnFromConversion
#'
#' Primary non-forest land after harmonization, declining only where natural
#' non-forest land is converted.
#'
#' The harmonizers converge on the input's primn and secdn. For IAMC input
#' that split is not the model's: models report only other natural land,
#' which is split by the target's composition, and the forest crosswalk then
#' moves a model's surplus forest into it by the model's forest composition -
#' all of it into secdn for models that report no primary forest. Fading to
#' that split relabels primary land as secondary without any land use, by up
#' to 270 Mha in a region.
#'
#' Here the total of primn and secdn is kept and the split is recomputed: in
#' each step primn loses its proportional share of any net loss of natural
#' non-forest land, and gains nothing when it grows, so primary land declines
#' only as land is converted. Before \code{year} the input is unchanged.
#'
#' @param x magpie object, regions by years by categories, with primn and secdn
#' @param year the first year of harmonization; from the next year on primn is recomputed
#' @return x with primn and secdn re-split, their sum unchanged
#' @author Ben Sanderson
toolPrimnFromConversion <- function(x, year) {
  stopifnot(c("primn", "secdn") %in% getItems(x, dim = 3),
            year %in% getYears(x, as.integer = TRUE))
  years <- getYears(x, as.integer = TRUE)
  natural <- dimSums(x[, , c("primn", "secdn")], dim = 3)
  primn <- as.vector(x[, year, "primn"])
  for (t in which(years > year)) {
    before <- as.vector(natural[, t - 1, ])
    now <- as.vector(natural[, t, ])
    loss <- pmax(before - now, 0)
    primn <- primn - loss * ifelse(before > 0, primn / before, 0)
    primn <- pmax(pmin(primn, now), 0)
    x[, t, "primn"] <- primn
    x[, t, "secdn"] <- now - primn
  }
  return(x)
}
