#' toolHarvestAreaDownscale
#'
#' Downscale low resolution wood harvest area to cells so that harvest stays
#' where the target's history puts it. Each cell's weight is the land it can
#' harvest (maxHarvest) times its historical harvest intensity, the target's
#' recent harvest area over its land of that type at the end of history. So a
#' region's harvest goes to the cells that harvest now, in proportion to how
#' much they harvest, and follows their land as it changes. Where no cell of a
#' region harvested a category in the target's history, the weight is the land
#' itself. No cell harvests more than maxHarvest; what a cap holds back goes to
#' the region's other cells, by weight, and once those are full to its
#' remaining land. secyf and secmf are both harvested from secdf, so they share
#' one cap: secyf gets what secmf leaves.
#'
#' Downscaling by land alone moves harvest wherever land of the right type is,
#' which for a large low resolution unit (an IAM region) can put a region's
#' harvest into countries that barely harvest, from the first year after
#' history on.
#'
#' @param harvestArea magpie object, low resolution wood harvest area
#' (category "wood_harvest_area") for the years to downscale
#' @param resmap data.frame with columns cell and lowRes
#' @param maxHarvest magpie object, cells, same years and items as harvestArea,
#' as returned by toolMaxHarvestPerYear
#' @param intensity magpie object, cells, one year, same items as harvestArea:
#' historical harvest area per unit of land (0 where there was none)
#' @param rounds iterations to place what the caps hold back
#' @return magpie object with harvest area per cell
#' @author Benjamin Sanderson
toolHarvestAreaDownscale <- function(harvestArea, resmap, maxHarvest, intensity, rounds = 20) {
  items <- getItems(harvestArea, 3)
  years <- getYears(harvestArea)
  stopifnot(setequal(getItems(maxHarvest, 3), items), setequal(getItems(intensity, 3), items),
            all(years %in% getYears(maxHarvest)), nyears(intensity) == 1)
  cells <- getItems(maxHarvest, 1)
  lowRes <- resmap$lowRes[match(cells, resmap$cell)]
  stopifnot(!anyNA(lowRes), all(unique(lowRes) %in% getItems(harvestArea, 1)))
  region <- match(lowRes, getItems(harvestArea, 1))
  sumBy <- function(v) {
    total <- numeric(nregions(harvestArea))
    s <- rowsum(v, region)
    total[as.integer(rownames(s))] <- s[, 1]
    return(total)
  }

  out <- maxHarvest[, years, items]
  out[] <- 0
  # secyf and secmf both come out of secdf: do secmf first, secyf takes the rest
  pool <- sub("^.*\\.", "", items)
  items <- items[order(pool == "secyf")]
  shared <- items[sub("^.*\\.", "", items) %in% c("secyf", "secmf")]
  for (item in items) {
    rate <- as.vector(intensity[, , item])
    rate[!is.finite(rate) | rate < 0] <- 0
    for (year in years) {
      cap <- pmax(as.vector(maxHarvest[, year, item]), 0)
      if (item %in% shared && length(shared) > 1) {
        cap <- pmax(cap - as.vector(dimSums(out[, year, setdiff(shared, item)], dim = 3)), 0)
      }
      target <- pmax(as.vector(harvestArea[, year, item]), 0)
      target[is.na(target)] <- 0
      weight <- cap * rate
      noHistory <- sumBy(weight) <= 0
      weight[noHistory[region]] <- cap[noHistory[region]]
      total <- sumBy(weight)
      share <- ifelse(total[region] > 0, weight / total[region], 0)
      area <- pmin(target[region] * share, cap)
      for (i in seq_len(rounds)) {
        short <- target - sumBy(area)
        room <- ifelse(area < cap, weight, 0)
        # once the cells that harvested are full, the region's remaining land takes the rest
        full <- sumBy(room) <= 0
        room[full[region]] <- (cap - area)[full[region]]
        roomTotal <- sumBy(room)
        if (!any(short > 1e-9 * pmax(target, 1e-12) & roomTotal > 0)) break
        add <- ifelse(roomTotal[region] > 0, pmax(short[region], 0) * room / roomTotal[region], 0)
        area <- pmin(area + add, cap)
      }
      out[, year, item] <- area
    }
  }
  return(out)
}
