#' toolHarvestConventionLUH3
#'
#' Book the primary-to-secondary flow the way LUH3 does: through wood harvest,
#' not through a land transition.
#'
#' LUH3 carries no \code{primf_to_secdf} or \code{primn_to_secdn}. Harvested
#' primary land becomes secondary through \code{primf_harv} and
#' \code{primn_harv}, so a state change closes as
#' \deqn{\Delta primf = -\sum primf\_to\_* - primf\_harv}
#' and secondary forest gains \code{primf_harv}. The gross transitions derived
#' from the states carry that flow as \code{primf_to_secdf} as well, while the
#' harvest area is the whole primary decline, land cleared for crops included.
#' Read with LUH3's rule, every hectare moves twice.
#'
#' Cell by cell, the harvest area becomes exactly the flow it stands for and the
#' transition is dropped. The harvested carbon that area still backs stays with
#' the primary source, at the cell's own carbon per hectare; harvest area and
#' carbon that no primary-to-secondary flow backs go to the secondary source of
#' the same kind in the same cell. Secondary harvest moves no area, so the
#' states still close, and total harvested carbon is unchanged.
#'
#' @param x magpie object with wood harvest (\code{*_harv}, \code{*_bioh}) and
#' gross transitions (\code{*_to_*}), as assembled in \code{\link{calcTransitionsNC}}
#' @return x in LUH3's convention: no primary-to-secondary transitions, primary
#' harvest area equal to them
#' @author Ben Sanderson
toolHarvestConventionLUH3 <- function(x) {
  # primary source -> (transition carrying its flow to secondary, secondary harvest source)
  pairs <- list(primf = c(flow = "primf_to_secdf", secondary = "secdf"),
                primn = c(flow = "primn_to_secdn", secondary = "secnf"))
  items <- getItems(x, dim = 3)
  for (primary in names(pairs)) {
    flowName <- pairs[[primary]][["flow"]]
    secondary <- pairs[[primary]][["secondary"]]
    needed <- c(flowName, paste0(c(primary, secondary), "_harv"), paste0(c(primary, secondary), "_bioh"))
    if (!all(needed %in% items)) {
      next
    }
    harv <- x[, , paste0(primary, "_harv")]
    bioh <- x[, , paste0(primary, "_bioh")]
    flow <- magclass::setNames(x[, , flowName], paste0(primary, "_harv"))
    harv[is.na(harv)] <- 0
    bioh[is.na(bioh)] <- 0
    flow[is.na(flow)] <- 0

    # the share of the old harvest area the flow still backs, and the carbon it keeps
    kept <- flow / harv
    kept[is.na(kept) | is.infinite(kept)] <- 0
    kept[kept > 1] <- 1
    keptBioh <- bioh * magclass::setNames(kept, paste0(primary, "_bioh"))

    movedHarv <- harv - flow
    movedHarv[movedHarv < 0] <- 0
    movedBioh <- bioh - keptBioh

    for (kind in c("harv", "bioh")) {
      name <- paste0(secondary, "_", kind)
      target <- x[, , name]
      target[is.na(target)] <- 0
      moved <- if (kind == "harv") movedHarv else movedBioh
      x[, , name] <- target + magclass::setNames(moved, name)
    }
    x[, , paste0(primary, "_harv")] <- flow
    x[, , paste0(primary, "_bioh")] <- keptBioh
    x <- x[, , flowName, invert = TRUE]

    toolStatusMessage("note", paste0(
      "LUH3 harvest convention: ", flowName, " booked as ", primary, "_harv; ",
      signif(sum(movedBioh) / max(sum(bioh), .Machine$double.eps) * 100, 3),
      "% of ", primary, " harvested carbon moved to ", secondary,
      " where no primary-to-secondary flow backs it"))
  }
  return(x)
}
