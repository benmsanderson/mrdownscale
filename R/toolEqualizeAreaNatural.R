#' toolEqualizeAreaNatural
#'
#' Give each region of the input the total area of the target by changing its
#' natural land only.
#'
#' A model's region and the target's version of it can differ in land area:
#' Greenland, ice and inland water are counted differently, and country areas
#' differ between sources. \code{\link{toolEqualizeArea}} scales every
#' category by the ratio of the two totals, which also scales cropland and
#' pasture and their changes - by 0.79 for GCAM's Europe, which turns its
#' 50 Mha of cropland expansion into none. The difference is not managed land,
#' so here it is added to or taken from the natural categories instead, as a
#' fixed amount in every year, shared in proportion to each category's
#' smallest value over time. Every category's change over time is kept, and
#' managed land is untouched. Where the natural land cannot absorb the
#' difference in some year, the region falls back to scaling.
#'
#' @param x magpie object with the input, regions by years by categories
#' @param y magpie object with the target's area per region and category, a single year
#' @param natural the natural categories that absorb the difference
#' @return x with each region's total equal to the target's
#' @author Ben Sanderson
toolEqualizeAreaNatural <- function(x, y, natural = c("primf", "secdf", "primn", "secdn")) {
  stopifnot(nyears(y) == 1,
            natural %in% getItems(x, dim = 3))
  difference <- as.vector(dimSums(y, dim = 3)) - as.vector(dimSums(x[, 1, ], dim = 3))
  names(difference) <- getItems(x, dim = 1)

  scaled <- character(0)
  for (region in names(difference)[abs(difference) >= 10^-5]) {
    smallest <- vapply(natural, function(category) min(x[region, , category]), numeric(1))
    if (difference[[region]] < 0 && -difference[[region]] > sum(smallest)) {
      scaled <- c(scaled, region)
      next
    }
    share <- if (sum(smallest) > 0) smallest / sum(smallest) else rep(1 / length(natural), length(natural))
    for (k in seq_along(natural)) {
      x[region, , natural[k]] <- x[region, , natural[k]] + difference[[region]] * share[k]
    }
  }

  if (length(scaled) > 0) {
    toolStatusMessage("warn", paste0("natural land too small to absorb the area difference, scaling instead: ",
                                     paste(scaled, collapse = ", ")))
    x[scaled, , ] <- toolEqualizeArea(x[scaled, , ], y[scaled, , ])
  }
  toolStatusMessage("note", paste0("area difference absorbed by natural land: ",
                                   round(sum(difference[difference > 0]), 1), " Mha added, ",
                                   round(-sum(difference[difference < 0]), 1), " Mha taken; largest ",
                                   round(max(abs(difference)), 1), " Mha"))
  stopifnot(abs(dimSums(x[, 1, ], dim = 3) - dimSums(y, dim = 3)) < 10^-5,
            x >= -10^-10)
  x[x < 0] <- 0
  return(x)
}
