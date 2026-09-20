#' convertIAMC
#'
#' Convert IAMC wide format to long format, one row per model, scenario,
#' region, variable, unit and year. Variable names are made syntactically
#' safe, so "Land Cover|Built-Up Area" becomes "Land_Cover_Built_Up_Area",
#' matching referenceMappings/iamc.csv.
#'
#' @param x IAMC wide format data.frame, as returned by \code{\link{readIAMC}}
#' @param subtype data; for regionMapping pass convert = FALSE
#' @return long format data.frame
#'
#' @author Ben Sanderson
convertIAMC <- function(x, subtype = "data") {
  if (subtype != "data") {
    stop("for subtype != data pass convert = FALSE")
  }
  columns <- c("Model", "Scenario", "Region", "Variable", "Unit")
  stopifnot(columns %in% colnames(x),
            colnames(x) %in% columns | startsWith(colnames(x), "X"))

  years <- grep("^X[0-9]+$", colnames(x), value = TRUE)
  stopifnot(length(years) > 0)

  x <- Reduce(rbind, lapply(years, function(year) {
    a <- x[, c(columns, year)]
    a$Year <- as.integer(sub("^X", "", year))
    colnames(a)[colnames(a) == year] <- "Value"
    return(a[, c(columns, "Year", "Value")])
  }))

  # models report different year grids, 5- or 10-yearly; drop the gaps
  x <- x[!is.na(x$Value), ]

  x$Variable <- gsub("_+$", "", gsub("[^A-Za-z0-9]+", "_", x$Variable))

  return(list(x = x,
              class = "data.frame",
              unit = paste(unique(x$Unit), collapse = ", "),
              description = "IAMC land data"))
}
