#' toolHarmonizeAbsoluteChangesHarvest
#'
#' Land: the input's changes since the first year of the harmonization period
#' applied to the target's level in that year
#' (\code{\link{toolHarmonizeAbsoluteChanges}}), with primary forest then
#' following the scenario's wood demand as under fadeForestHarvest
#' (\code{\link{toolPrimfFromHarvest}}), keeping the forest total. Data without
#' primf and secdf (non-land) are faded over the harmonization period as under
#' fadeForest.
#'
#' @inheritParams toolHarmonizeFadeForest
#' @return harmonized data set as magpie object
#' @author Ben Sanderson
toolHarmonizeAbsoluteChangesHarvest <- function(xInput, xTarget, harmonizationPeriod, primfHarvest = NULL) {
  psf <- c("primf", "secdf")
  if (!all(psf %in% getItems(xInput, 3))) {
    return(toolHarmonizeFade(xInput, xTarget, harmonizationPeriod))
  }
  hp1 <- harmonizationPeriod[1]
  x <- toolHarmonizeAbsoluteChanges(xInput, xTarget[, getYears(xTarget, as.integer = TRUE) <= hp1, ],
                                    harmonizationPeriod = c(hp1, hp1))
  if (is.null(primfHarvest)) {
    return(x)
  }
  years <- getYears(x, as.integer = TRUE)
  forest <- dimSums(x[, years >= hp1, psf], 3)
  primf <- toolPrimfFromHarvest(forest, xTarget[, hp1, "primf"], primfHarvest)
  x[, years >= hp1, psf] <- mbind(primf, magclass::setNames(forest - primf, "secdf"))
  stopifnot(x >= 0)
  return(x)
}
