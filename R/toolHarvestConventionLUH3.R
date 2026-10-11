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
#' mrdownscale's wood-harvest chain (\code{\link{calcWoodHarvestAreaHarmonized}})
#' defines primary harvest as the whole primary decline, in the target's history
#' as in the scenario, but sets it per region and spreads it over cells by its
#' own weights, independently of where primary land actually declines. Gross
#' transitions derived from net state changes, in turn, cannot see primary land
#' harvested and then cleared in the same step.
#'
#' So the definition is applied cell by cell. A cell's primary harvest area is
#' its primary decline, and all of the cell's primary conversions go through
#' secondary land (\code{primf_to_c3ann} becomes \code{secdf_to_c3ann}); the
#' states are unchanged. The harvested carbon follows: a cell keeps the carbon
#' of the harvest area its decline backs, and carbon whose harvest area no
#' primary decline backs is moved, at the density it was harvested at, to the
#' cells of the same region whose decline the chain's harvest area does not
#' cover. Only what the region has no such cells for goes, with its area, to
#' the secondary source of the same kind in the same cell. Total harvested
#' carbon is unchanged.
#'
#' @param x magpie object with wood harvest (\code{*_harv}, \code{*_bioh}) and
#' gross transitions (\code{*_to_*}), as assembled in \code{\link{calcTransitionsNC}}
#' @param region the region of each cell of x, in order; carbon moves only
#' within a region. NULL treats all cells as one region.
#' @return x in LUH3's convention: no primary-to-secondary transitions, primary
#' harvest area equal to them
#' @author Ben Sanderson
toolHarvestConventionLUH3 <- function(x, region = NULL) {
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

    conversions <- setdiff(grep(paste0("^", primary, "_to_"), items, value = TRUE), flowName)
    # the cell's primary decline: the flow to secondary and every conversion
    decline <- flow
    for (conversion in conversions) {
      converted <- x[, , conversion]
      converted[is.na(converted)] <- 0
      decline <- decline + magclass::setNames(converted, paste0(primary, "_harv"))
      missing <- is.na(x[, , conversion])
      via <- sub(paste0("^", primary), state, conversion)
      if (!via %in% getItems(x, dim = 3)) {
        x <- add_columns(x, via, fill = 0)
      }
      target <- x[, , via]
      target[is.na(target)] <- 0
      x[, , via] <- target + magclass::setNames(converted, via)
      x[, , via][missing & is.na(x[, , via])] <- NA
      x[, , conversion] <- 0
      x[, , conversion][missing] <- NA
    }

    # carbon the cell's decline backs stays; the rest is orphaned, with its area
    backed <- pmin(harv, decline)
    kept <- backed / harv
    kept[is.na(kept) | is.infinite(kept)] <- 0
    keptBioh <- bioh * magclass::setNames(kept, paste0(primary, "_bioh"))
    orphanBioh <- bioh - keptBioh
    orphanHarv <- harv - backed
    uncovered <- decline - backed

    # within each region and year, orphaned carbon moves to uncovered decline at
    # the density it was harvested at, as far as that decline reaches
    groups <- if (is.null(region)) rep("all", length(getItems(x, dim = 1))) else as.character(region)
    movedBioh <- orphanBioh * 0
    movedHarv <- orphanHarv * 0
    newBioh <- keptBioh
    for (g in unique(groups)) {
      cells <- which(groups == g)
      for (year in getYears(x)) {
        oc <- sum(orphanBioh[cells, year, ])
        oa <- sum(orphanHarv[cells, year, ])
        ua <- sum(uncovered[cells, year, ])
        if (oc <= 0) next
        density <- if (oa > 0) oc / oa else 0
        placed <- if (ua > 0 && density > 0) min(oc, density * ua) else 0
        if (placed > 0) {
          newBioh[cells, year, ] <- newBioh[cells, year, ] +
            placed * as.vector(uncovered[cells, year, ]) / ua
        }
        left <- if (oc > 0) (oc - placed) / oc else 0
        movedBioh[cells, year, ] <- orphanBioh[cells, year, ] * left
        movedHarv[cells, year, ] <- orphanHarv[cells, year, ] * left
      }
    }

    for (kind in c("harv", "bioh")) {
      name <- paste0(secondary, "_", kind)
      target <- x[, , name]
      target[is.na(target)] <- 0
      moved <- if (kind == "harv") movedHarv else movedBioh
      x[, , name] <- target + magclass::setNames(moved, name)
    }
    x[, , paste0(primary, "_harv")] <- decline
    x[, , paste0(primary, "_bioh")] <- newBioh
    x <- x[, , flowName, invert = TRUE]

    toolStatusMessage("note", paste0(
      "LUH3 harvest convention: ", primary, " harvest area set to each cell's ", primary,
      " decline, its conversions routed through ", state, "; ",
      signif(sum(orphanBioh) / max(sum(bioh), .Machine$double.eps) * 100, 3), "% of ", primary,
      " harvested carbon sat where no decline backs it, ",
      signif(sum(movedBioh) / max(sum(bioh), .Machine$double.eps) * 100, 3), "% went to ", secondary))
  }
  return(x)
}
