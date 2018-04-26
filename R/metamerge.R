#' Add metadata to form data
#' @description For a given form, combine the form's header and detail data then merge with site, plot, and line data
#' @param dima.tables A list of data frames. The output from \code{read.dima()} can be provided directly as this argument.
#' @param form A string specifying which form to work with. This can either be the form name in the background tables of DIMA or the human version, e.g. \code{"CanopyGap"}, \code{"Canopy Gap"}, \code{"LPI"}, \code{"Line Point Intercept"}, \code{"Species Richness"}, etc. This is case insensitive.
#' @param minimum Logical. If \code{TRUE} then only the keys and names for the site, plot, and line will be included. Defaults to \code{TRUE}
#' @return A data frame created by merging \code{tblSites}, \code{tblPlots}, \code{tblLines}, and the header and detail tables for the selected form.
#' @export
metamerge <- function(dima.tables = list(),
                      form = "",
                      minimum = TRUE
){
  forms <- c("CANOPY GAP" = "CanopyGap",
             "COMPACTION" = "Compact",
             "DK" = "DK",
             "DRY WEIGHT" = "DryWt",
             "GAP" = "Gap",
             "INFILTRATION" = "Infiltration",
             "LIC" = "LIC",
             "LINE POINT INTERCEPT" = "LPI",
             "NESTED FREQUENCY" = "NestedFreq",
             "OCULAR COVER" = "OcularCov",
             "OCULAR COVER ESTIMATE" = "OcularCov",
             "PASTURE CONDITION" = "PastCond",
             "PASTURE CONDITION RATING" = "PastCond",
             "DENSITY" = "PlantDen",
             "PLANT DENSITY" = "PlantDen",
             "PRODUCTIVITY" = "PlantProd",
             "PLANT PRODUCTIVITY" = "PlantProd",
             "PLOT MANAGEMENT" = "PlotMgt",
             "POINT FRAME" = "PTFrame",
             "QUALITATIVE" = "Qual",
             "RIPARIAN PRODUCTIVITY" = "RiparPro",
             "RIPARIAN SURVEY" = "RiparSurv",
             "SOIL STABILITY" = "SoilStab",
             "SPECIES RICHNESS" = "SpecRich",
             "SPECIES INVENTORY" = "SpecRich",
             "SPECIES RICHNESS INVENTORY" = "SpecRich",
             "TREE DENSITY" = "TreeDen",
             "UTILIZATION" = "Util",
             "VEGETATION STRUCTURE" = "VegStruct")

  ## Search the values of forms to see if the argument string form matches
  if (any(grepl(forms, pattern = paste0("^", form, "$"), ignore.case = TRUE))) {
    form <- unique(forms[grepl(forms, pattern = paste0("^", form, "$"), ignore.case = TRUE)])
    ## If that finds nothing, then try the names
  } else if (any(grepl(names(forms), pattern = paste0("^", form, "$"), ignore.case = TRUE))) {
    form <- unique(forms[names(forms)[grepl(names(forms), pattern = paste0("^", form, "$"), ignore.case = TRUE)]])
  } else {
    message("The string specifying the form must match one and only one form. It can either be the name of the form or the name of the table containing the form data. The following are valid values:")
    stop(paste(paste(names(forms), collapse = ", "), paste(forms, collapse = ", "), sep = ","))
  }
  ## If there were no matches or too many, stop!
  if (length(form) != 1) {
    message("The string specifying the form must match one and only one form. It can either be the name of the form or the name of the table containing the form data. The following are valid values:")
    stop(paste(paste(names(forms), collapse = ", "), paste(forms, collapse = ", "), sep = ","))
  }

  ## The minimum tables for this to work
  tables.required <- c("tblSites", "tblPlots", "tblLines", paste0("tbl", form, c("Header", "Detail")))

  ## They need to all be in dima.tables
  if (!all(tables.required %in% names(dima.tables))) {
    stop(paste0("The list dima.tables is missing the following data frames: ",
                paste(tables.required[!(tables.required %in% names(dima.tables))], collapse = ", ")))
  }
  ## They also need to be data frames
  if (!all(sapply(tables.required, FUN = function(X, table.list){class(table.list[[X]])}, table.list = dima.tables) == "data.frame")) {
    stop(paste0("The following in dima.list aren't data frames and need to be: ",
                paste(tables.required[sapply(tables.required, FUN = function(X, table.list){class(table.list[[X]])}, table.list = dima.tables) == "data.frame"], collapse = ", ")))
  }

  ## The metadata lookup table
  sites.plots.lines <- merge(x = merge(x = dplyr::select(dima.tables$tblSites, -DateModified),
                                       y = dplyr::select(dima.tables$tblPlots, -DateModified),
                                       by = "SiteKey"),
                             y = dplyr::select(dima.tables$tblLines, -DateModified),
                             by = "PlotKey")

  if (minimum) {
    sites.plots.lines <- dplyr::select(sites.plots.lines,
                                       dplyr::starts_with("site", ignore.case = TRUE),
                                       dplyr::starts_with("plot", ignore.case = TRUE),
                                       dplyr::starts_with("line", ignore.case = TRUE))
  }

  ## Combine the Header and Detail tables for this form
  header.details <- merge(x = dima.tables[[tables.required[4]]],
                          y = dima.tables[[tables.required[5]]])

  ## Combine the metadata with the form data
  sites.plots.lines.header.details <-  merge(x = sites.plots.lines,
                                             y = header.details,
                                             by = if ("PlotKey" %in% names(header.details)) {
                                               "PlotKey"
                                             } else if ("LineKey" %in% names(header.details)) {
                                               "LineKey"
                                             } else {
                                               stop(paste0("Somehow there's neither PlotKey nor LineKey in the header and detail table for ", form))
                                             })

  return(sites.plots.lines.header.details)
}

