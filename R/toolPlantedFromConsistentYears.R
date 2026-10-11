#' toolPlantedFromConsistentYears
#'
#' Planted forest where a model's forest parts do not add up to its forest total.
#'
#' In a region-year whose planted, primary and secondary forest differ from the
#' forest total by more than 1\% and 1 Mha, planted forest takes its share of
#' the total from the region's nearest year in which they agree (the earlier
#' year on a tie). Regions in which no year agrees keep what they report.
#' Other region-years are returned unchanged.
#'
#' @param planted magpie object, planted forest (Mha), regions x years
#' @param forest magpie object, forest total (Mha), the same dimensions
#' @param parts magpie object, sum of the reported forest parts (Mha)
#' @return planted, with inconsistent region-years replaced
#' @author Ben Sanderson
toolPlantedFromConsistentYears <- function(planted, forest, parts) {
  years <- getYears(forest, as.integer = TRUE)
  offset <- abs(as.array(parts)[, , 1] - as.array(forest)[, , 1])
  total <- as.array(forest)[, , 1]
  bad <- offset > 0.01 * total & offset > 1
  bad <- matrix(bad, nrow = length(getItems(forest, dim = 1)))
  share <- matrix(as.array(planted)[, , 1] / pmax(total, .Machine$double.eps), nrow = nrow(bad))
  replaced <- 0
  for (r in seq_len(nrow(bad))) {
    good <- which(!bad[r, ])
    if (length(good) == 0 || !any(bad[r, ])) next
    for (k in which(bad[r, ])) {
      nearest <- good[which.min(abs(years[good] - years[k]) + 1e-6 * (years[good] > years[k]))]
      planted[r, k, ] <- share[r, nearest] * total[r, k]
      replaced <- replaced + 1
    }
  }
  if (replaced > 0) {
    toolStatusMessage("note", paste0("forest parts do not add up to the forest total in ", replaced,
                                     " region-years; planted forest there takes its share of the total ",
                                     "from the nearest year where they do"))
  }
  return(planted)
}
