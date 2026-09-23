#' toolSelectIAMCScenario
#'
#' Pick the one model and scenario an iamc input names.
#'
#' An IAMC release holds many scenarios - the public ScenarioMIP release has
#' seven markers and a good many more besides - so `input = "iamc:<scenario>"`
#' names one, e.g. `input = "iamc:Very Low - SSP1 (Marker)"`. Plain `"iamc"`
#' is accepted when the release holds a single model and scenario already.
#'
#' @param x long format IAMC data, as returned by \code{\link{convertIAMC}}
#' @param input the input name, "iamc" or "iamc:<scenario>"
#' @return x, restricted to one model and scenario
#' @author Ben Sanderson
toolSelectIAMCScenario <- function(x, input) {
  if (startsWith(input, "iamc:")) {
    scenario <- sub("^iamc:", "", input)
    if (!scenario %in% x$Scenario) {
      stop("Scenario \"", scenario, "\" not found, available: \"",
           paste(unique(x$Scenario), collapse = "\", \""), "\"")
    }
    x <- x[x$Scenario == scenario, ]
  }
  if (length(unique(x$Scenario)) != 1 || length(unique(x$Model)) != 1) {
    stop("Expected exactly one model and scenario, found \"",
         paste(unique(paste(x$Model, x$Scenario)), collapse = "\", \""),
         "\"; select one with input = \"iamc:<scenario>\"")
  }
  return(x)
}
