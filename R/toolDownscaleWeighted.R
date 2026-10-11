#' toolDownscaleWeighted
#'
#' The classic MAgPIE downscaling (\code{luscale::interpolate2}) with weighted
#' losses for some categories. interpolate2 takes a shrinking category from
#' every cell of a region by the same fraction, so expansion lands wherever
#' that category is, in proportion to its amount. For the categories in
#' \code{weighted} the fraction taken from a cell is instead proportional to
#' the cell's weight, capped at the whole stock, with the region's total loss
#' unchanged. With equal weights this is interpolate2.
#'
#' Used to keep expansion off land that cannot support it: LUH counts desert
#' and tundra as primary non-forest land, so with equal weights a region
#' converting natural land puts much of its new cropland there.
#'
#' @inheritParams toolDownscaleMagpieClassic
#' @param weight magpie object with a non-negative weight per cell, for at
#' least the cells of xTarget; missing values count as zero
#' @param weighted categories whose losses are weighted
#' @return downscaled land use dataset, including the year of xTarget
#' @author Ben Sanderson
toolDownscaleWeighted <- function(x, xTarget, xTargetLowRes, mapping, weight, weighted = c("primn", "secdn")) {
  stopifnot(setequal(mapping$lowRes, getItems(x, 1)),
            setequal(mapping$cell, getItems(xTarget, 1)),
            nyears(xTarget) == 1,
            getYears(xTargetLowRes) == getYears(xTarget),
            setequal(getItems(x, 3), getItems(xTarget, 3)),
            weighted %in% getItems(xTarget, 3))

  x <- x[, -1, getItems(xTarget, 3)]
  categories <- getItems(xTarget, 3)
  cells <- getItems(xTarget, 1)
  region <- match(mapping$lowRes[match(cells, mapping$cell)], getItems(x, 1))
  w <- as.vector(weight[cells, , ])
  w[is.na(w) | w < 0] <- 0

  lowRes <- mbind(xTargetLowRes[getItems(x, 1), , categories], x)
  test <- dimSums(lowRes, dim = 3)
  if (any(abs(test - test[, 1, ]) > 1e-4)) {
    stop("Total stock is not constant over time")
  }

  h <- matrix(as.vector(xTarget[, , categories]), ncol = length(categories))
  out <- mbind(lapply(getYears(lowRes), function(y) setYears(xTarget[, , categories], y)))

  for (t in 2:nyears(lowRes)) {
    before <- matrix(as.vector(lowRes[, t - 1, ]), ncol = length(categories))
    change <- matrix(as.vector(lowRes[, t, ]), ncol = length(categories)) - before
    reduction <- pmax(-change, 0) / (before + 10^-100)
    extent <- pmax(change, 0) / (rowSums(pmax(-change, 0)) + 10^-100)

    taken <- reduction[region, , drop = FALSE]
    for (k in match(weighted, categories)) {
      for (r in which(reduction[, k] > 0)) {
        inRegion <- which(region == r)
        taken[inRegion, k] <- toolWeightedShares(h[inRegion, k], w[inRegion], reduction[r, k])
      }
    }
    h <- (1 - taken) * h + rowSums(taken * h) * extent[region, , drop = FALSE]
    out[, t, ] <- array(h, dim = c(nrow(h), 1, ncol(h)))
  }
  getSets(out)[1:2] <- c("x", "y")
  return(out)
}

#' toolWeightedShares
#'
#' Fractions of each cell's stock to take so that, in total, \code{share} of
#' the region's stock is taken, each cell's fraction proportional to its
#' weight and at most 1. Where the cells with weight cannot supply it all,
#' they give everything and the rest is taken evenly from the others; where no
#' cell has weight, evenly from all.
#'
#' @param stock stock per cell
#' @param weight weight per cell, non-negative
#' @param share fraction of the total stock to take
#' @return fraction taken per cell
#' @author Ben Sanderson
toolWeightedShares <- function(stock, weight, share) {
  total <- share * sum(stock)
  withWeight <- weight > 0 & stock > 0
  if (!any(withWeight)) {
    return(rep(share, length(stock)))
  }
  available <- sum(stock[withWeight])
  if (total >= available) {
    rest <- (total - available) / max(sum(stock[!withWeight]), 10^-100)
    return(ifelse(withWeight, 1, min(rest, 1)))
  }
  taken <- function(lambda) sum(pmin(1, lambda * weight) * stock)
  lower <- 0
  upper <- 1 / min(weight[withWeight])
  for (i in 1:60) {
    lambda <- (lower + upper) / 2
    if (taken(lambda) < total) {
      lower <- lambda
    } else {
      upper <- lambda
    }
  }
  return(pmin(1, upper * weight))
}
