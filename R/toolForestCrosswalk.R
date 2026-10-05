#' toolForestCrosswalk
#'
#' Put the input's forest onto the target's definition of forest, keeping the
#' input's land-use changes.
#'
#' IAMs and LUH disagree about where forest ends. LUH constrains forest to its
#' potential-forest mask, so tropical woodland an IAM reports as forest sits in
#' LUH's non-forested natural land, while at high latitudes the disagreement
#' runs the other way - for REMIND-MAgPIE, LUH has 110 Mha more forest than the
#' IAM across Canada, Australia and New Zealand, which the IAM counts as
#' primary non-forest. Harmonization converges on the input's forest total
#' after the harmonization period, so without a crosswalk that definitional
#' difference is carried into the output as if it were land-use change.
#'
#' Per region, the crosswalk measures how much more forest (primf, secdf and
#' pltns together, since LUH counts plantations as secondary forest) the input
#' has than the target at the calibration year, and moves that difference
#' between forest and non-forested natural land, primary to primary and
#' secondary to secondary: surplus forest goes from primf to primn and from
#' secdf to secdn, a shortfall comes back the other way. The split between
#' primary and secondary follows the source group's composition at the
#' calibration year. Each category is shifted by a fixed amount in every year,
#' so every category's change over time is kept exactly - including that
#' primary land never expands. Plantations are forest under both definitions
#' and are left alone. Total area is unchanged.
#'
#' @param xInput magpie object with the input in target categories
#' @param xTarget magpie object with the target at the same regions
#' @param year calibration year, present in both, usually the first year of
#' the harmonization period
#' @return xInput with its forest level shifted to the target's definition
#' @author Ben Sanderson
toolForestCrosswalk <- function(xInput, xTarget, year) {
  natural <- c("primf", "secdf", "primn", "secdn")
  stopifnot(natural %in% getItems(xInput, dim = 3),
            year %in% getYears(xInput, as.integer = TRUE),
            year %in% getYears(xTarget, as.integer = TRUE))
  forest <- intersect(c("primf", "secdf", "pltns"), getItems(xInput, dim = 3))
  targetForest <- intersect(c("primf", "secdf", "pltns"), getItems(xTarget, dim = 3))
  regions <- getItems(xInput, dim = 1)

  excess <- as.vector(dimSums(xInput[, year, forest], dim = 3)) -
    as.vector(dimSums(xTarget[regions, year, targetForest], dim = 3))
  names(excess) <- regions

  toolStatusMessage("note", paste0("forest crosswalk at ", year, ": input forest exceeds target by ",
                                   round(sum(excess[excess > 0]), 1), " Mha in total, falls short by ",
                                   round(-sum(excess[excess < 0]), 1), " Mha; largest regional shift ",
                                   round(max(abs(excess)), 1), " Mha"))

  shortfall <- 0
  for (region in regions) {
    e <- excess[[region]]
    if (e == 0) next
    # surplus forest leaves forest for non-forest, a shortfall the other way
    from <- if (e > 0) c(primary = "primf", secondary = "secdf") else c(primary = "primn", secondary = "secdn")
    to <- if (e > 0) c(primary = "primn", secondary = "secdn") else c(primary = "primf", secondary = "secdf")
    base <- as.vector(xInput[region, year, from])
    share <- if (sum(base) > 0) base / sum(base) else c(0.5, 0.5)
    for (k in 1:2) {
      amount <- abs(e) * share[k]
      available <- as.vector(xInput[region, , from[k]])
      moved <- pmin(amount, available)
      shortfall <- max(shortfall, amount - min(moved))
      xInput[region, , from[k]] <- available - moved
      xInput[region, , to[k]] <- as.vector(xInput[region, , to[k]]) + moved
    }
  }
  if (shortfall > 10^-6) {
    toolStatusMessage("warn", paste0("forest crosswalk limited by available area, up to ",
                                     round(shortfall, 1), " Mha of the difference could not be moved"))
  }
  return(xInput)
}
