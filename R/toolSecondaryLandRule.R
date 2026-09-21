#' toolSecondaryLandRule
#'
#' Apply LUH's rule for secondary land to downscaled land use: land that
#' becomes secondary natural land is secondary forest where the cell's
#' potential vegetation is forest, and secondary non-forest where it is not.
#'
#' LUH3 enforces this through its potential-forest mask (fstnf): every
#' hectare of forest change in the published LUH3-VL falls in cells the mask
#' marks as forest-potential, and none in the rest. The downscaler knows
#' nothing of the mask, so it places afforestation on savanna and leaves
#' land abandoned on forest-potential cells as non-forest. Relative to the
#' base year, per cell and per year, this
#' \itemize{
#'   \item holds secondary forest (secdf and pltns together) at its base-year
#'   level on cells that cannot carry forest, moving any gain to secdn, and
#'   \item turns any gain in secdn on cells that can carry forest into secdf,
#'   as regrowth.
#' }
#' Area only moves within a cell, and only between secdf, pltns and secdn,
#' so every cell's total is kept and primary land is untouched. Losses are
#' left as they are; the rule is about where gains go.
#'
#' @param x magpie object with downscaled land use in Mha, cells by years by
#' categories, including the base year
#' @param potentialForest magpie object, 1 where the cell's potential
#' vegetation is forest and 0 elsewhere, for at least the cells of x
#' @param baseYear year to measure gains from, usually the last historical
#' year, which is left unchanged
#' @return x with secondary land allocated by potential vegetation
#' @author Ben Sanderson
toolSecondaryLandRule <- function(x, potentialForest, baseYear) {
  years <- getYears(x, as.integer = TRUE)
  forestParts <- intersect(c("secdf", "pltns"), getItems(x, dim = 3))
  stopifnot(baseYear %in% years,
            "secdf" %in% forestParts,
            "secdn" %in% getItems(x, dim = 3),
            getItems(x, dim = 1) %in% getItems(potentialForest, dim = 1))

  canBeForest <- as.vector(potentialForest[getItems(x, dim = 1), , ]) == 1
  canBeForest[is.na(canBeForest)] <- FALSE
  nCells <- length(canBeForest)

  matrixOf <- function(year, categories) {
    matrix(as.vector(x[, year, categories]), nrow = nCells, ncol = length(categories))
  }
  baseForest <- matrixOf(baseYear, forestParts)
  baseSecdn <- as.vector(x[, baseYear, "secdn"])

  toNonForest <- 0
  toForest <- 0
  for (year in years[years > baseYear]) {
    forest <- matrixOf(year, forestParts)
    secdn <- as.vector(x[, year, "secdn"])

    # no secondary forest gain where the potential vegetation is not forest;
    # take the excess from the parts that gained, in proportion to their gain
    excess <- pmax(rowSums(forest) - rowSums(baseForest), 0)
    excess[canBeForest] <- 0
    gains <- pmax(forest - baseForest, 0)
    shares <- gains / pmax(rowSums(gains), .Machine$double.eps)
    # removing a whole gain leaves rounding residue below zero (~1e-18); clamp it
    forest <- pmax(forest - excess * shares, 0)
    secdn <- secdn + excess

    # secondary non-forest gained where the potential vegetation is forest
    # regrows as secondary forest
    regrowth <- pmax(secdn - baseSecdn, 0)
    regrowth[!canBeForest] <- 0
    secdn <- secdn - regrowth
    forest[, 1] <- forest[, 1] + regrowth  # secdf is the first forest part

    for (k in seq_along(forestParts)) {
      x[, year, forestParts[k]] <- forest[, k]
    }
    x[, year, "secdn"] <- secdn
    toNonForest <- max(toNonForest, sum(excess))
    toForest <- max(toForest, sum(regrowth))
  }

  toolStatusMessage("note", paste0("secondary-land rule: up to ", round(toNonForest, 1),
                                   " Mha of forest gain on cells that cannot carry forest moved to secdn, up to ",
                                   round(toForest, 1), " Mha of secdn gain on forest-potential cells regrown as secdf"))
  return(x)
}
