toolFertilizerKgPerHa <- function(fertilizerTg, landMha) {
  landMha <- landMha[getItems(fertilizerTg, 1), getItems(fertilizerTg, 2), ]
  cropMha <- toolAggregateCropland(landMha, getItems(fertilizerTg, 3.2), keepOthers = FALSE)
  # convert from Tg yr-1 to kg ha-1 yr-1
  fertilizerKgPerHa <- fertilizerTg / cropMha * (10^9 / 10^6)
  fertilizerKgPerHa[is.nan(fertilizerKgPerHa)] <- 0
  # fertilizer on a crop type without area, possible when land and fertilizer
  # are harmonized differently (absoluteChanges land, faded fertilizer), is dropped
  noArea <- is.infinite(fertilizerKgPerHa)
  if (any(noArea)) {
    toolStatusMessage("note", paste0("dropping fertilizer on crop types without area: ",
                                     signif(sum(fertilizerTg[noArea]), 3), " Tg over all years"))
    fertilizerKgPerHa[noArea] <- 0
  }
  return(fertilizerKgPerHa)
}
