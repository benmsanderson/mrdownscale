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
  # primary source -> (transition carrying its flow to secondary, secondary harvest
  # source, secondary state)
  pairs <- list(primf = c(flow = "primf_to_secdf", secondary = "secdf", state = "secdf"),
                primn = c(flow = "primn_to_secdn", secondary = "secnf", state = "secdn"))
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

    # Transitions derived from net state changes cannot tell "primary cleared
    # for crops" from "primary harvested to secondary, secondary cleared for
    # crops" in a cell where both happen, and name it the former. LUH harvests
    # first. So where a cell's secondary land gains, primary conversion to
    # other uses goes through secondary instead, up to the harvest area the
    # cell already carries. The states are unchanged.
    state <- pairs[[primary]][["state"]]
    out <- setdiff(grep(paste0("^", primary, "_to_"), items, value = TRUE), flowName)
    via <- sub(paste0("^", primary, "_to_"), paste0(state, "_to_"), out)
    out <- out[via %in% items]
    via <- via[via %in% items]
    if (length(out) > 0) {
      values <- function(names) {
        a <- as.array(x[, , names, drop = FALSE])
        a[is.na(a)] <- 0
        a
      }
      gain <- rowSums(values(grep(paste0("_to_", state, "$"), items, value = TRUE)), dims = 2) -
        rowSums(values(grep(paste0("^", state, "_to_"), items, value = TRUE)), dims = 2)
      conv <- values(out)
      total <- rowSums(conv, dims = 2)
      room <- pmax(as.array(harv)[, , 1] - as.array(flow)[, , 1], 0)
      reroute <- ifelse(gain > 0, pmin(total, room), 0)
      moved <- conv * array(ifelse(total > 0, reroute / total, 0), dim(conv))
      x[, , out] <- conv - moved
      x[, , via] <- values(via) + moved
      flow[, , ] <- as.array(flow) + array(reroute, dim(as.array(flow)))
      toolStatusMessage("note", paste0(
        "LUH3 harvest convention: ", signif(sum(reroute) / max(sum(total), .Machine$double.eps) * 100, 3),
        "% of ", primary, " conversion to other uses routed through ", state,
        " in cells where ", state, " gains"))
    }

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
