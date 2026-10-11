#' toolUrbanCrosswalk
#'
#' Put the input's urban land onto the target's level, keeping the input's
#' urban changes.
#'
#' IAMC models report built-up area on their own definitions, or not at all:
#' against LUH3's 86 Mha in 2025, GCAM reports 59 and holds it flat, COFFEE 55,
#' and MESSAGE and WITCH report nothing, which reads as zero. Harmonization
#' converges on the input after the harmonization period, so without a
#' crosswalk those models' products lose a third, or all, of LUH's urban land
#' by 2050 - a definitional difference carried into the output as land-use
#' change.
#'
#' Per region, the crosswalk measures the input's urban land against the
#' target's at the calibration year and shifts urban by that fixed amount in
#' every year, so the input's urban change is kept exactly and a model that
#' reports none holds the target's level. The difference comes from, or goes
#' to, non-forested natural land (primn, secdn), split by their composition at
#' the calibration year, as in \code{\link{toolForestCrosswalk}}. Where the model
#' leaves too little natural land in some year, the rest comes from its other
#' land in proportion to area, so urban keeps its level. Total area is unchanged.
#'
#' @param xInput magpie object with the input in target categories
#' @param xTarget magpie object with the target at the same regions
#' @param year calibration year, present in both, usually the first year of
#' the harmonization period
#' @return xInput with its urban level shifted to the target's
#' @author Ben Sanderson
toolUrbanCrosswalk <- function(xInput, xTarget, year) {
  natural <- c("primn", "secdn")
  stopifnot(c("urban", natural) %in% getItems(xInput, dim = 3),
            year %in% getYears(xInput, as.integer = TRUE),
            year %in% getYears(xTarget, as.integer = TRUE))
  regions <- getItems(xInput, dim = 1)

  # positive: the input has less urban land than the target
  gap <- as.vector(xTarget[regions, year, "urban"]) - as.vector(xInput[, year, "urban"])
  names(gap) <- regions
  toolStatusMessage("note", paste0("urban crosswalk at ", year, ": input urban below target by ",
                                   round(sum(gap[gap > 0]), 1), " Mha in total, above by ",
                                   round(-sum(gap[gap < 0]), 1), " Mha"))

  fromOther <- 0
  others <- setdiff(getItems(xInput, dim = 3), c("urban", natural))
  for (region in regions) {
    g <- gap[[region]]
    if (g == 0) next
    base <- as.vector(xInput[region, year, natural])
    share <- if (sum(base) > 0) base / sum(base) else c(0.5, 0.5)
    short <- 0
    for (k in 1:2) {
      amount <- g * share[k]
      available <- as.vector(xInput[region, , natural[k]])
      # urban grows by taking natural land; a surplus of urban goes back to it
      moved <- if (amount > 0) pmin(amount, available) else rep(amount, length(available))
      short <- short + (amount - moved)
      xInput[region, , natural[k]] <- available - moved
      xInput[region, , "urban"] <- as.vector(xInput[region, , "urban"]) + moved
    }
    # where the model has left too little natural land in some year, the rest
    # comes from its other land in proportion to area, so urban keeps its level
    if (any(short > 10^-9)) {
      rest <- matrix(as.vector(xInput[region, , others]), ncol = length(others))
      taken <- rest / pmax(rowSums(rest), .Machine$double.eps) * pmin(short, rowSums(rest))
      xInput[region, , others] <- as.vector(rest - taken)
      xInput[region, , "urban"] <- as.vector(xInput[region, , "urban"]) + rowSums(taken)
      fromOther <- max(fromOther, max(short))
    }
  }
  if (fromOther > 10^-6) {
    toolStatusMessage("note", paste0("urban crosswalk: natural land ran short in some years, up to ",
                                     round(fromOther, 1), " Mha taken from other land in proportion to area"))
  }
  return(xInput)
}
