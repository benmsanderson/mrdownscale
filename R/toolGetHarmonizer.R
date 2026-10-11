#' toolGetHarmonizer
#'
#' Get a harmonizer function by name.
#'
#' @param harmonizerName name of a harmonizer function, currently offset, fade, fadeForest,
#' fadeForestHarvest, absoluteChanges, absoluteChangesHarvest
#' @return harmonizer function
#' @seealso \code{\link{toolHarmonizeOffset}}, \code{\link{toolHarmonizeFade}},
#' \code{\link{toolHarmonizeFadeForest}}, \code{\link{toolHarmonizeAbsoluteChanges}},
#' \code{\link{toolHarmonizeAbsoluteChangesHarvest}}
#' @author Pascal Sauer
toolGetHarmonizer <- function(harmonizerName) {
  # function(...) toolHarmonizeOffset(...) instead of passing
  # toolHarmonizeOffset directly so madrat recognizes it as dependency
  harmonizers <- list(offset = function(...) toolHarmonizeOffset(...),
                      fade = function(...) toolHarmonizeFade(...),
                      fadeForest = function(...) toolHarmonizeFadeForest(...),
                      # calcLandHarmonized passes the primary harvest this
                      # needs for land; non-land data carry no primf/secdf and
                      # are faded exactly as under fadeForest
                      fadeForestHarvest = function(...) toolHarmonizeFadeForest(...),
                      absoluteChanges = function(...) toolHarmonizeAbsoluteChanges(...),
                      absoluteChangesHarvest = function(...) toolHarmonizeAbsoluteChangesHarvest(...))
  stopifnot(harmonizerName %in% names(harmonizers))
  return(harmonizers[[harmonizerName]])
}
