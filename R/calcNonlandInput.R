#' calcNonlandInput
#'
#' Prepare the nonland input data for category mapping, checking data for consistency before returning.
#'
#' All "Land" functions deal with area data, as opposed to "Nonland" functions which deal with non-area
#' data such as the amount of applied fertilizer. These are treated differently, because for area
#' data other constraints apply, e.g. the total area must be constant over time.
#' Fertilizer on regional level is disaggregated to cluster level using cropland as weight.
#'
#' input = "iamc": the reported wood harvest volume and nitrogen fertilizer
#' total. IAMC says nothing about which forest the wood comes from or the area
#' it takes, so those come from the target's history in
#' \code{\link{calcNonlandInputRecategorized}}.
#'
#' @param input name of an input dataset, "magpie" or "iamc[:<scenario>]"
#' @return nonland input data
#' @author Pascal Sauer
calcNonlandInput <- function(input) { # before adding args, consider: many functions @inheritParams from this function
  if (input == "magpie") {
    woodHarvestWeight <- readSource("MagpieFulldataGdx", subtype = "woodHarvestWeight")
    # convert from Pg DM yr-1 to kg C yr-1
    woodHarvestWeight <- woodHarvestWeight * 10^9 * 0.5

    woodHarvestWeightSource <- dimSums(woodHarvestWeight, dim = "woodType")
    woodHarvestWeightSource <- add_dimension(woodHarvestWeightSource, dim = 3.1,
                                             add = "category", "wood_harvest_weight")
    getSets(woodHarvestWeightSource)[["d3.2"]] <- "data"

    woodHarvestWeightType <- dimSums(woodHarvestWeight, dim = "source")
    woodHarvestWeightType <- add_dimension(woodHarvestWeightType, dim = 3.1,
                                           add = "category", "wood_harvest_weight_type")
    getSets(woodHarvestWeightType)[["d3.2"]] <- "data"

    woodHarvestArea <- readSource("MagpieFulldataGdx", subtype = "woodHarvestArea")
    woodHarvestArea <- dimSums(woodHarvestArea, dim = "ageClass")
    woodHarvestArea <- add_dimension(woodHarvestArea, dim = 3.1,
                                     add = "category", "wood_harvest_area")
    getSets(woodHarvestArea)[["d3.2"]] <- "data"

    # check wood harvest area * time step length (as unit is Mha yr-1) <=
    # land of the correponding type (in the previous timestep)
    land <- calcOutput("LandInput", input = input, aggregate = FALSE)

    # only forestry_plant is harvested, so we can just rename
    land <- add_columns(land, "forestry")
    land[, , "forestry"] <- dimSums(land[, , "forestry_plant"], 3)

    stopifnot(identical(getYears(woodHarvestArea), getYears(land)))
    years <- getYears(land, as.integer = TRUE)
    timestepLengths <- new.magpie(years = years[-1], fill = diff(years))
    woodland <- setYears(land[, -nyears(land), getItems(woodHarvestArea, 3.2)], years[-1])
    toolExpectTrue(min(woodland - timestepLengths * collapseDim(woodHarvestArea)[, -1, ]) >= -10^-10,
                   "Wood harvest area is smaller than land of the corresponding type")

    # get fertilizer on regional level, then disaggregate to cluster level using cropland as weight
    fertilizerRaw <- readSource("MagpieFulldataGdx", subtype = "fertilizer")
    fertilizerRaw[is.nan(fertilizerRaw) | fertilizerRaw < 0] <- 0
    fertilizer <- fertilizerRaw
    # set fertilizer to zero where there is no cropland
    cropland <- toolAggregateCropland(land, cropTypes = getItems(fertilizer, 3), keepOthers = FALSE)
    fertilizer[cropland == 0] <- 0
    fertilizer <- add_dimension(fertilizer, dim = 3.1, add = "category", "fertilizer")
    stopifnot(min(fertilizer) >= 0)

    out <- mbind(woodHarvestWeightSource, woodHarvestWeightType, woodHarvestArea, fertilizer)
    unit <- "harvest_weight: kg C yr-1; harvest_area: Mha yr-1; fertilizer: Tg yr-1"
  } else if (startsWith(input, "iamc")) {
    # IAMC reports a harvest volume and a fertilizer total, and nothing about
    # which forest the wood comes from or how much area it takes. Those are
    # supplied in calcNonlandInputRecategorized from the target's own history,
    # so what is reported is kept as reported here.
    x <- toolSelectIAMCScenario(readSource("IAMC"), input)
    wanted <- c(wood_harvest_demand.roundwood = "Forestry_Production_Roundwood",
                wood_harvest_demand.industrial = "Forestry_Production_Roundwood_Industrial_Roundwood",
                wood_harvest_demand.fuel = "Forestry_Production_Roundwood_Wood_Fuel",
                fertilizer.nitrogen = "Fertilizer_Use_Nitrogen_Synthetic")
    # LUH's fertl layer is a synthetic fertilization rate - its 2020 history,
    # 114.7 Tg N, matches REMIND's synthetic nitrogen (112.1) rather than its
    # total nitrogen (148.7), which would step up by 16% at the harmonization
    # join. Fall back to the total where synthetic is not reported.
    if (!wanted[["fertilizer.nitrogen"]] %in% x$Variable) {
      toolStatusMessage("note", paste0("synthetic nitrogen is not reported, using total nitrogen; ",
                                       "LUH's fertilization rate is synthetic, so this overstates it"))
      wanted[["fertilizer.nitrogen"]] <- "Fertilizer_Use_Nitrogen"
    }
    missingVariables <- setdiff(wanted, x$Variable)
    if (length(missingVariables) > 0) {
      stop("Missing required variables: \"", paste(missingVariables, collapse = "\", \""), "\"")
    }
    x <- x[x$Variable %in% wanted & x$Region != "World", ]
    stopifnot(x$Unit[x$Variable == wanted[["fertilizer.nitrogen"]]] == "Tg N/yr",
              x$Unit[startsWith(x$Variable, "Forestry")] == "million m3/yr")

    x <- x[, c("Region", "Year", "Variable", "Value")]
    out <- as.magpie(x, spatial = "Region", temporal = "Year")
    out <- out[, , wanted]
    getItems(out, dim = 3, raw = TRUE) <- names(wanted)
    names(dimnames(out))[3] <- "category.data"

    if (anyNA(out)) {
      # the harvest and fertilizer variables can report on a coarser year grid
      # than the land tree; keep the years all of them have
      complete <- getYears(out)[!apply(is.na(as.array(out)), 2, any)]
      toolStatusMessage("note", paste0("nonland input reports ", length(complete), " years, ",
                                       min(getYears(complete, as.integer = TRUE)), " to ",
                                       max(getYears(complete, as.integer = TRUE))))
      out <- out[, complete, ]
    }
    if (any(out < 0)) {
      toolStatusMessage("warn", "Negative values detected, replacing with 0.")
      out[out < 0] <- 0
    }

    mapping <- readSource("IAMC", subtype = "regionMapping", convert = FALSE)
    out <- toolAggregate(out, unique(mapping[, c("region", "lowRes")]))
    names(dimnames(out)) <- c("region.id", "year", "category.data")
    unit <- "harvest_demand: million m3 yr-1; fertilizer: Tg yr-1"
  } else {
    stop("Unsupported input dataset \"", input, "\"")
  }

  # check data for consistency
  toolExpectTrue(identical(unname(getSets(out)), c("region", "id", "year", "category", "data")),
                 "Dimensions are named correctly")
  toolExpectTrue(all(out >= 0), "All values are >= 0")
  if (input == "magpie") {
    toolExpectLessDiff(fertilizerRaw, fertilizer, 10^-5,
                       paste0("Setting fertilizer to zero where there is no cropland ",
                              "does not change fertilizer significantly"))
    toolCheckFertilizer(out[, , "fertilizer"], land)
  }

  return(list(x = out,
              isocountries = FALSE,
              unit = unit,
              min = 0,
              description = "Nonland input data for data harmonization and downscaling pipeline",
              # iamc regions carry an id, as in calcLandInput; cleaning drops it
              clean_magpie = !startsWith(input, "iamc")))
}
