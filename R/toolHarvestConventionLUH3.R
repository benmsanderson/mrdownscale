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
#' The harvest area is the input here: it comes from the wood-harvest chain
#' (\code{\link{calcWoodHarvestAreaHarmonized}}), which sets primary harvest
#' to the whole primary decline, in the target's history as in the scenario.
#' The transitions are reconciled to it, not the other way round. Gross
#' transitions derived from net state changes cannot see primary land that is
#' harvested and then cleared in the same step: where a cell's secondary land
#' loses what harvest gave it, the netting books primary land cleared directly,
#' and the primary-to-secondary flow vanishes. So, cell by cell, as much of the
#' cell's primary conversions to other land as its harvest area exceeds that
#' flow is routed through secondary land - \code{primf_to_c3ann} becomes
#' \code{secdf_to_c3ann}, in proportion to each conversion - and the harvest
#' area keeps all of its carbon. States are unchanged. Only harvest area that
#' even this cannot back - more than the cell's primary land lost - goes, with
#' its carbon, to the secondary source of the same kind in the same cell.
#' Secondary harvest moves no area, so the states still close, and total
#' harvested carbon is unchanged.
#'
#' @param x magpie object with wood harvest (\code{*_harv}, \code{*_bioh}) and
#' gross transitions (\code{*_to_*}), as assembled in \code{\link{calcTransitionsNC}}
#' @return x in LUH3's convention: no primary-to-secondary transitions, primary
#' harvest area equal to them
#' @author Ben Sanderson
toolHarvestConventionLUH3 <- function(x) {
  # primary source -> (transition carrying its flow to secondary, secondary
  # harvest source, secondary state)
  pairs <- list(primf = c(flow = "primf_to_secdf", secondary = "secdf", state = "secdf"),
                primn = c(flow = "primn_to_secdn", secondary = "secnf", state = "secdn"))
  for (primary in names(pairs)) {
    items <- getItems(x, dim = 3)
    flowName <- pairs[[primary]][["flow"]]
    secondary <- pairs[[primary]][["secondary"]]
    state <- pairs[[primary]][["state"]]
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

    # route primary conversions through secondary land, up to the harvest area
    # the flow leaves unbacked
    conversions <- setdiff(grep(paste0("^", primary, "_to_"), items, value = TRUE), flowName)
    if (length(conversions) > 0) {
      converted <- x[, , conversions]
      converted[is.na(converted)] <- 0
      total <- magclass::setNames(dimSums(converted, dim = 3), paste0(primary, "_harv"))
      unbacked <- harv - flow
      unbacked[unbacked < 0] <- 0
      routed <- pmin(unbacked, total)
      share <- routed / total
      share[is.na(share) | is.infinite(share)] <- 0
      for (conversion in conversions) {
        moved <- converted[, , conversion] * magclass::setNames(share, conversion)
        missing <- is.na(x[, , conversion])
        x[, , conversion] <- converted[, , conversion] - moved
        x[, , conversion][missing] <- NA
        via <- sub(paste0("^", primary), state, conversion)
        if (!via %in% getItems(x, dim = 3)) {
          x <- add_columns(x, via, fill = 0)
        }
        target <- x[, , via]
        target[is.na(target)] <- 0
        x[, , via] <- target + magclass::setNames(moved, via)
        x[, , via][missing & is.na(x[, , via])] <- NA
      }
      flow <- flow + routed
      toolStatusMessage("note", paste0(
        "LUH3 harvest convention: ", signif(sum(routed) / max(sum(total), .Machine$double.eps) * 100, 3),
        "% of ", primary, " conversions routed through ", state, " to back the harvest area"))
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
