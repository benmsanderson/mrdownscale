#' toolIAMCLandCategories
#'
#' Turn the land tree of an IAMC-format release into the land categories the
#' downscaling pipeline expects, as a magclass object in Mha.
#'
#' The IAMC land tree is not a clean partition, and models differ in how they
#' break it down, so three things are handled here:
#' \itemize{
#'   \item Primary, secondary and planted forest are rescaled to add up to the
#'   reported Land Cover|Forest total. Some models nest them instead of
#'   partitioning them (AIM reports secondary forest equal to its forest
#'   total, with primary inside it); that is an error rather than a rescale,
#'   because the forest state would otherwise be silently wrong.
#'   \item Other Land is ignored, because models disagree on whether it sits
#'   beside the other categories or inside one of them. Instead whatever
#'   Land Cover does not otherwise account for is added to other natural
#'   land, which recategorizes to primn/secdn. LUH has no barren category,
#'   so that is where this area belongs.
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
                "Land_Cover_Forest", "Land_Cover_Forest_Primary",
                "Land_Cover_Forest_Secondary", "Land_Cover_Forest_Planted",
                "Land_Cover_Other_Natural")
  optional <- c("Land_Cover_Built_Up_Area", "Land_Cover_Cropland_Energy_Crops")

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

  for (variable in setdiff(optional, getItems(out, dim = 3))) {
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

  forestParts <- c("Land_Cover_Forest_Primary", "Land_Cover_Forest_Secondary",
                   "Land_Cover_Forest_Planted")
  partSum <- dimSums(out[, , forestParts], dim = 3)
  forest <- collapseDim(out[, , "Land_Cover_Forest"], dim = 3)
  offset <- abs(partSum - forest)
  if (any(offset > 0.01 * forest & offset > 1)) {
    stop("Forest subcategories do not partition Land_Cover_Forest, worst case ",
         round(max(offset), 1), " Mha. This model needs a prior for the forest split.")
  }
  out[, , forestParts] <- out[, , forestParts] * ifelse(partSum > 0, forest / partSum, 1)

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
