#' calcResolutionMapping
#'
#' Calculate a complete mapping from low (input dataset, clusters/countries/regions)
#' to high resolution (target dataset, grid). As a basis the mapping from the low
#' resolution clusters/countries/regions to grid cells is used.
#' Cells which are present in that mapping, but not in the target
#' dataset are discarded. Cells which are present in the target dataset, but not in the mapping are
#' added using a nearest-neighbor approach: These cells are mapped to the same low resolution
#' cluster/country/region as the closest cell which is already present in the mapping.
#'
#' @inheritParams calcLandInput
#' @param target character, the target dataset
#' @return a list including a data.frame with columns x, y, lowRes, country
#'
#' @author Pascal Sauer
calcResolutionMapping <- function(input, target) {
  if (target == "luh3") {
    targetGrid <- readSource("LUH3", subtype = "states", subset = 2000)
  } else {
    targetGrid <- calcOutput("LandTargetComplete", target = target, aggregate = FALSE)
  }

  if (input == "magpie") {
    clustermap <- readSource("MagpieFulldataGdx", subtype = "clustermap")
    coords <- strsplit(clustermap$cell, "\\.")
    xCoords <- vapply(coords, function(x) as.double(sub("p", ".", x[1])), double(1))
    yCoords <- vapply(coords, function(x) as.double(sub("p", ".", x[2])), double(1))
    clustermap$cellOriginal <- sub("\\.[A-Z]{3}$", "", clustermap$cell)
    clustermap <- cbind(x = xCoords, y = yCoords, clustermap[, -which(colnames(clustermap) == "cell")])
    colnames(clustermap)[colnames(clustermap) == "cluster"] <- "lowRes"

    mapping <- toolResolutionMapping(clustermap, targetGrid)

    toolExpectTrue(all(mapping$cellOriginal %in% clustermap$cellOriginal),
                   "a subset of input cells is mapped")
  } else if (input == "witch") {
    mapping <- readSource("WITCH", subtype = "resolutionMapping")
    mapping <- mapping[, setdiff(colnames(mapping), "witch17")]
  } else if (input == "coffee") {
    mapping <- readSource("COFFEE", subtype = "regionMapping", convert = FALSE)

    magpie <- calcOutput("ResolutionMapping", input = "magpie", target = target, aggregate = FALSE)
    magpie <- magpie[, setdiff(colnames(magpie), c("region", "lowRes"))]

    mapping <- merge(magpie, mapping, by = "country")
  } else if (input == "iamc" || startsWith(input, "iamc:")) {
    # Country per grid cell comes from LUH3's own static ccode field rather
    # than a MAgPIE clustermap, so an IAMC release can be downscaled from
    # public data alone. The region mapping then says which region each
    # country belongs to for this model.
    mapping <- readSource("IAMC", subtype = "countryCell", convert = FALSE)
    regionMapping <- readSource("IAMC", subtype = "regionMapping", convert = FALSE)

    unmapped <- setdiff(unique(mapping$country), unique(regionMapping$country))
    if (length(unmapped) > 0) {
      toolStatusMessage("warn", paste0(length(unmapped), " countries are not in the region ",
                                       "mapping, their cells are dropped: ",
                                       paste(sort(unmapped), collapse = ", ")))
    }
    mapping <- merge(mapping, regionMapping, by = "country")

    mapping$cellOriginal <- paste0(sub("\\.", "p", mapping$x), ".", sub("\\.", "p", mapping$y))
    mapping$global <- "GLO"

    mapping <- toolResolutionMapping(mapping, targetGrid)
  } else {
    stop("Unsupported input type \"", input, "\"")
  }

  toolExpectTrue(setequal(colnames(mapping), c("x", "y", "lowRes", "region", "country",
                                               "global", "cellOriginal", "cell")),
                 "resolution mapping has the expected columns")
  coords <- terra::crds(targetGrid, df = TRUE)
  allTargetCells <- paste0(sub("\\.", "p", coords$x),
                           ".",
                           sub("\\.", "p", coords$y))
  toolExpectTrue(setequal(mapping$cell, allTargetCells),
                 "all target cells are mapped")

  return(list(x = mapping,
              class = "data.frame",
              unit = NA,
              description = "mapping of high to low resolution and country"))
}
