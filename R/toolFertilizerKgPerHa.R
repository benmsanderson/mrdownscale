toolFertilizerKgPerHa <- function(fertilizerTg, landMha) {
  landMha <- landMha[getItems(fertilizerTg, 1), getItems(fertilizerTg, 2), ]
  cropMha <- toolAggregateCropland(landMha, getItems(fertilizerTg, 3.2), keepOthers = FALSE)
  # convert from Tg yr-1 to kg ha-1 yr-1
  fertilizerKgPerHa <- fertilizerTg / cropMha * (10^9 / 10^6)
  fertilizerKgPerHa[is.nan(fertilizerKgPerHa)] <- 0
  return(fertilizerKgPerHa)
}
