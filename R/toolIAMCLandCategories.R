#' toolIAMCLandCategories
#'
#' Turn the land tree of an IAMC-format release into the land categories the
#' downscaling pipeline expects, as a magclass object in Mha.
#'
#' The IAMC land tree is not a clean partition, and models differ in how they
#' break it down, so three things are handled here:
#' \itemize{
#'   \item Primary, secondary and planted forest are rescaled to add up to the
#'   reported Land Cover|Forest total where they are reported and consistent.
#'   Where they are not - IMAGE overshoots by up to 9%, AIM reports secondary
#'   forest equal to its total with primary inside it, GCAM reports no split -
#'   the split is set aside and the forest total kept, since harmonization
#'   derives primary forest from the target regardless of what the input said.
#'   \item Other Land is ignored, because models disagree on whether it sits
#'   beside the other categories or inside one of them. Instead whatever
#'   Land Cover does not otherwise account for is added to other natural
#'   land, which recategorizes to primn/secdn. LUH has no barren category,
#'   so that is where this area belongs.
#'   \item Primary forest cannot expand, so any increase between reported
#'   years, which is usually rounding, is moved into secondary forest.
#'   \item Built-up area and energy crops are not reported by every model and
#'   are filled with zeros when missing. That area is not lost, it stays in
#'   the Land Cover total and so ends up in other natural land.
#' }
#'
#' @param x long format IAMC data for a single model and scenario, with
#' columns Region, Year, Variable and Value, as returned by
#' \code{\link{convertIAMC}}
#' @return magclass object with the categories of referenceMappings/iamc.csv
#'
#' @author Ben Sanderson
toolIAMCLandCategories <- function(x) {
  required <- c("Land_Cover", "Land_Cover_Cropland", "Land_Cover_Pasture",
                "Land_Cover_Forest", "Land_Cover_Other_Natural")
  optional <- c("Land_Cover_Built_Up_Area", "Land_Cover_Cropland_Energy_Crops",
                "Land_Cover_Forest_Primary", "Land_Cover_Forest_Secondary",
                "Land_Cover_Forest_Planted")

  missingVariables <- setdiff(required, x$Variable)
  if (length(missingVariables) > 0) {
    stop("Missing required variables: \"", paste(missingVariables, collapse = "\", \""), "\"")
  }
  x <- x[x$Variable %in% c(required, optional), ]

  # models report 5- or 10-yearly on their own grid; keep the years where
  # every variable is present, harmonization interpolates from there
  reportedPerYear <- table(unique(x[, c("Year", "Variable")])$Year)
  years <- as.integer(names(reportedPerYear)[reportedPerYear == max(reportedPerYear)])
  x <- x[x$Year %in% years, ]
  toolStatusMessage("note", paste0("IAMC input reports ", length(years), " years, ",
                                   min(years), " to ", max(years)))

  out <- as.magpie(x[, c("Region", "Year", "Variable", "Value")],
                   spatial = "Region", temporal = "Year")

  for (variable in setdiff(grep("Forest", optional, invert = TRUE, value = TRUE),
                           getItems(out, dim = 3))) {
    toolStatusMessage("note", paste0(variable, " is not reported, filling with zeros"))
    out <- add_columns(out, variable, fill = 0)
  }

  if (anyNA(out)) {
    toolStatusMessage("warn", "NAs detected, replacing with 0.")
    out[is.na(out)] <- 0
  }
  if (any(out < 0)) {
    toolStatusMessage("warn", "Negative values detected, replacing with 0.")
    out[out < 0] <- 0
  }

  # Forest. Models report the split inconsistently: IMAGE's parts exceed its
  # forest total by up to 9%, AIM reports secondary forest equal to the total
  # with primary inside it, GCAM reports no split at all. None of that need
  # stop a run, because the split does not survive harmonization - for years
  # from the harmonization start on, toolHarmonizeFadeForest derives primary
  # forest from the target's own trajectory and the harmonized forest total,
  # whatever the input said (see its tests). So a usable split is rescaled to
  # the reported total, and an unusable one is set aside, with the forest
  # total kept either way.
  forestParts <- c("Land_Cover_Forest_Primary", "Land_Cover_Forest_Secondary")
  forest <- collapseDim(out[, , "Land_Cover_Forest"], dim = 3)
  planted <- if ("Land_Cover_Forest_Planted" %in% getItems(out, dim = 3)) {
    pmin(collapseDim(out[, , "Land_Cover_Forest_Planted"], dim = 3), forest)
  } else {
    forest * 0
  }
  reportedSplit <- all(forestParts %in% getItems(out, dim = 3))
  usableSplit <- FALSE
  if (reportedSplit) {
    partSum <- dimSums(out[, , c(forestParts, "Land_Cover_Forest_Planted")[
      c(forestParts, "Land_Cover_Forest_Planted") %in% getItems(out, dim = 3)]], dim = 3)
    offset <- abs(partSum - forest)
    usableSplit <- !any(offset > 0.01 * forest & offset > 1)
    if (!usableSplit) {
      toolStatusMessage("warn", paste0("the reported forest split does not add up to the forest ",
                                       "total, by up to ", round(max(offset), 1), " Mha; setting it ",
                                       "aside and keeping the total, which is what harmonization uses"))
    }
  } else {
    toolStatusMessage("note", "no forest split is reported; keeping the forest total, which is what harmonization uses")
  }

  if (usableSplit) {
    parts <- c(forestParts, "Land_Cover_Forest_Planted")
    parts <- parts[parts %in% getItems(out, dim = 3)]
    partSum <- dimSums(out[, , parts], dim = 3)
    out[, , parts] <- out[, , parts] * ifelse(partSum > 0, forest / partSum, 1)
  } else {
    # all forest that is not a plantation becomes secondary; primary forest
    # is left to harmonization
    out <- add_columns(out, setdiff(c(forestParts, "Land_Cover_Forest_Planted"),
                                    getItems(out, dim = 3)), fill = 0)
    out[, , "Land_Cover_Forest_Primary"] <- 0
    out[, , "Land_Cover_Forest_Secondary"] <- forest - planted
    out[, , "Land_Cover_Forest_Planted"] <- planted
  }
  forestParts <- c(forestParts, "Land_Cover_Forest_Planted")

  # primary forest cannot expand by definition, but reported values drift up
  # by rounding (0.085 Mha in one VL region-step); move any increase into
  # secondary forest, which keeps the forest total. Above 1 Mha it is not
  # rounding and still warns.
  out <- toolReplaceExpansion(out, "Land_Cover_Forest_Primary", "Land_Cover_Forest_Secondary",
                              warnThreshold = 1)

  cropland <- collapseDim(out[, , "Land_Cover_Cropland"], dim = 3)
  energyCrops <- collapseDim(out[, , "Land_Cover_Cropland_Energy_Crops"], dim = 3)
  cropOther <- setNames(cropland - energyCrops, "Land_Cover_Cropland_Other")
  if (any(cropOther < 0)) {
    toolStatusMessage("warn", "Energy crops exceed cropland, replacing with 0.")
    cropOther[cropOther < 0] <- 0
  }

  reported <- c("Land_Cover_Cropland", "Land_Cover_Pasture", "Land_Cover_Forest",
                "Land_Cover_Built_Up_Area", "Land_Cover_Other_Natural")
  landCover <- collapseDim(out[, , "Land_Cover"], dim = 3)
  # land area cannot change, but a reported total can drift - IMAGE's moves by
  # 0.42 Mha - and the downscaler refuses a stock that is not constant. Hold
  # the total at its first year; the difference joins the residual below.
  firstYear <- getYears(landCover)[1]
  drift <- max(abs(landCover - as.vector(landCover[, firstYear, ])))
  if (drift > 10^-3) {  # below a thousand hectares it is floating point, not drift
    toolStatusMessage("note", paste0("the reported land total drifts by up to ", round(drift, 2),
                                     " Mha; holding it at its ", firstYear, " value"))
    for (year in getYears(landCover)) {
      landCover[, year, ] <- as.vector(landCover[, firstYear, ])
    }
  }
  residual <- landCover - dimSums(out[, , reported], dim = 3)
  if (any(abs(residual) > 0.01 * landCover)) {
    toolStatusMessage("warn", paste0("Land categories miss the Land_Cover total by up to ",
                                     round(max(abs(residual)), 1),
                                     " Mha, adding the difference to other natural land"))
  }
  otherNatural <- setNames(collapseDim(out[, , "Land_Cover_Other_Natural"], dim = 3) + residual,
                           "Land_Cover_Other_Natural")
  if (any(otherNatural < 0)) {
    toolStatusMessage("warn", "Other natural land is negative after adding the residual, replacing with 0.")
    otherNatural[otherNatural < 0] <- 0
  }

  return(mbind(out[, , c(forestParts, "Land_Cover_Pasture", "Land_Cover_Built_Up_Area",
                         "Land_Cover_Cropland_Energy_Crops")],
               cropOther, otherNatural))
}
