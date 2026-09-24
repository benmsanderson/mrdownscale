#' toolAddMetadataNC
#'
#' Add metdata nc output files
#'
#' @param ncFile file name of the respective nc file
#' @param activityId a string to store in the nc attribute activityId
#' @param revision a string to store in the nc attribute revision
#' @param harmonizationPeriod a string to store in the nc attribute harmonizationPeriod
#' @param missingValue a string to store in the nc attribute missingValue
#' @param compression a string to store in the nc attribute compression
#' @param resolution a string to store in the nc attribute resolution
#' @param references a string to store in the nc attribute references
#' @param targetMIP a string to store in the nc attribute targetMIP
#' @param ncTitle a string to store in the nc attribute ncTitle
#' @param referenceDataset a string to store in the nc attribute referenceDataset
#' @param furtherInfoUrl a string to store in the nc attribute furtherInfoUrl
#' @author Pascal Sauer, Jan Philipp Dietrich
toolAddMetadataNC <- function(ncFile, activityId, revision, harmonizationPeriod,
                              missingValue, compression, resolution, references,
                              targetMIP, ncTitle, referenceDataset, furtherInfoUrl) {
  variableId <- sub("^(multiple-[^_]+).+$", "\\1", basename(ncFile))
  stopifnot(variableId %in% c("multiple-states", "multiple-management", "multiple-transitions"))
  nc <- ncdf4::nc_open(ncFile, write = TRUE)
  withr::defer({
    ncdf4::nc_close(nc)
  })
  # global
  dateTime <- strftime(Sys.time(), format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ncdf4::ncatt_put(nc, 0, "activity_id", activityId)
  ncdf4::ncatt_put(nc, 0, "contact", "pascal.sauer@pik-potsdam.de, dietrich@pik-potsdam.de")
  ncdf4::ncatt_put(nc, 0, "Conventions", "CF-1.6")
  ncdf4::ncatt_put(nc, 0, "creation_date", dateTime)
  ncdf4::ncatt_put(nc, 0, "data_structure", "grid")
  ncdf4::ncatt_put(nc, 0, "dataset_category", "landState")
  ncdf4::ncatt_put(nc, 0, "dataset_version_number", as.character(revision))
  ncdf4::ncatt_put(nc, 0, "date", dateTime)
  ncdf4::ncatt_put(nc, 0, "frequency", "yr")
  ncdf4::ncatt_put(nc, 0, "further_info_url", furtherInfoUrl)
  ncdf4::ncatt_put(nc, 0, "grid_label", "gn")
  ncdf4::ncatt_put(nc, 0, "host", "Potsdam Institute for Climate Impact Research")
  ncdf4::ncatt_put(nc, 0, "institution_id", "PIK")
  ncdf4::ncatt_put(nc, 0, "institution", "Potsdam Institute for Climate Impact Research")
  ncdf4::ncatt_put(nc, 0, "license", "CC BY 4.0")
  ncdf4::ncatt_put(nc, 0, "nominal_resolution", "50 km")
  ncdf4::ncatt_put(nc, 0, "realm", "land")
  ncdf4::ncatt_put(nc, 0, "references", references)
  ncdf4::ncatt_put(nc, 0, "source_id", sub(paste0("^", variableId, "_"), "",
                                           sub("\\.nc$", "",
                                               basename(ncFile))))
  ncdf4::ncatt_put(nc, 0, "target_mip", targetMIP)
  ncdf4::ncatt_put(nc, 0, "title", ncTitle)
  ncdf4::ncatt_put(nc, 0, "variable_id", variableId)

  # added by us
  ncdf4::ncatt_put(nc, 0, "harmonization_period", paste(harmonizationPeriod, collapse = "-"))
  ncdf4::ncatt_put(nc, 0, "harmonization_downscaling_tool", "https://github.com/pik-piam/mrdownscale")
  ncdf4::ncatt_put(nc, 0, "reference_dataset", referenceDataset)
  ncdf4::ncatt_put(nc, 0, "source_version", as.character(revision))

  # time
  ncdf4::ncatt_put(nc, "time", "axis", "T")
  ncdf4::ncatt_put(nc, "time", "calendar", "365_day")
  ncdf4::ncatt_put(nc, "time", "long_name", "time")
  ncdf4::ncatt_put(nc, "time", "realtopology", "linear")
  ncdf4::ncatt_put(nc, "time", "standard_name", "time")
  toolTimeAxis(nc)

  # lon
  ncdf4::ncatt_put(nc, "lon", "realtopology", "circular")
  ncdf4::ncatt_put(nc, "lon", "topology", "circular")
  ncdf4::ncatt_put(nc, "lon", "modulo", 360, prec = "double")
  ncdf4::ncatt_put(nc, "lon", "long_name", "longitude")
  ncdf4::ncatt_put(nc, "lon", "standard_name", "longitude")
  ncdf4::ncatt_put(nc, "lon", "axis", "X")
  ncdf4::ncatt_put(nc, "lon", "bounds", "bounds_lon")

  # lat
  ncdf4::ncatt_put(nc, "lat", "realtopology", "linear")
  ncdf4::ncatt_put(nc, "lat", "long_name", "latitude")
  ncdf4::ncatt_put(nc, "lat", "standard_name", "latitude")
  ncdf4::ncatt_put(nc, "lat", "axis", "Y")
  ncdf4::ncatt_put(nc, "lat", "bounds", "bounds_lat")

  # variable attributes
  luhNames <- toolGetMapping("luhNames.csv", where = "mrdownscale")
  for (varname in names(nc$var)) {
    ncdf4::ncatt_put(nc, varname, "_Fillvalue", missingValue, prec = "float")
    ncdf4::ncatt_put(nc, varname, "missing_value", missingValue, prec = "float")
    ncdf4::ncatt_put(nc, varname, "cell_methods", "time:mean")

    if (varname %in% luhNames$name) {
      varnameLuhNames <- as.vector(luhNames[luhNames$name == varname, ])
      ncdf4::ncatt_put(nc, varname, "units", varnameLuhNames$unit)
      ncdf4::ncatt_put(nc, varname, "long_name", varnameLuhNames$long_name)
      ncdf4::ncatt_put(nc, varname, "standard_name", varnameLuhNames$standard_name)
      if (varnameLuhNames$standard_name_description != "") {
        ncdf4::ncatt_put(nc, varname, "standard_name_description", varnameLuhNames$standard_name_description)
      }
      if (varnameLuhNames$long_name_description != "") {
        ncdf4::ncatt_put(nc, varname, "long_name_description", varnameLuhNames$long_name_description)
      }
    } else {
      warning("metadata for variable ", varname, " missing in luhNames.csv")
    }
  }

  # add bounds
  boundsDim <- ncdf4::ncdim_def("bounds", "", 1:2, create_dimvar = FALSE)

  withr::with_options(list(warnPartialMatchDollar = FALSE), {
    nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("bounds_lon", units = "",
                                                dim = list(boundsDim, nc$dim$lon),
                                                prec = "double", compression = compression))
    nc <- ncdf4::ncvar_add(nc, ncdf4::ncvar_def("bounds_lat", units = "",
                                                dim = list(boundsDim, nc$dim$lat),
                                                prec = "double", compression = compression))
  })

  ncdf4::ncvar_put(nc, "bounds_lon", rbind(nc$dim$lon$vals - resolution / 2,
                                           nc$dim$lon$vals + resolution / 2))
  ncdf4::ncvar_put(nc, "bounds_lat", rbind(nc$dim$lat$vals + resolution / 2,
                                           nc$dim$lat$vals - resolution / 2))
}
