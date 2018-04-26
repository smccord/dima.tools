#' Identify Unknown Plant Codes and Their Growth Habits and Durations
#' @description Check a character string to see if it's a valid DIMA/AIM unknown plant code. Additionally, try to assign growth habit and duration for that code if possible.
#' @param code Character string. The string to check against the unknown plant code patterns
#' @param valid.structure Regular expression as a character string. Define what the acceptable structure is for unknown plant codes. Defaults to the AIM format of \code{"^(AF|AG|PF|PG|SH|TR)\\d{2,3}$"}.
#' @return A named list with three values. \code{valid} is logical and indicates if the code was a valid unknown. The remaining values match the style of DIMA's \code{tblSpeciesGrowthHabit}. \code{habit}, a character string, will be \code{NULL} if the code was not valid and either \code{"Woody"} or \code{"Non-woody"} as appropriate if it was valid. \code{subhabit}, a character string, will be \code{NULL} if the code was not valid and \code{"Forb/herb"}, \code{"Graminoid"}, \code{"Shrub"}, or \code{"Tree"} as appropriate if it was valid. \code{duration}, a character string, will be \code{NULL} if the code was not valid and either \code{"Annual"} or \code{"Perennial"} as appropriate if it was valid.
check.code <- function(code,
                       valid.structure = "^(AF|AG|PF|PG|SH|TR)\\d{2,3}$") {
  if (!is.character(code)) {
    stop("The code must be a character string.")
  }
  if (!is.character(valid.structure)) {
    stop("valid.structure must be a regular expression as a character string.")
  }

  # Okay, so is this a valid code????
  valid <- grepl(code, pattern = valid.structure)

  # If it wasn't a valid code, then just assign everything to NULL
  if (!valid) {
    growthhabit <- NULL
    growthhabitsub <- NULL
    duration <- NULL
  } else {
    # Check to see if it starts with one of our woody prefixes
    if (grepl(code, pattern = "^(SH|TR)")) {
      # If it's woody, it's also perennial
      growthhabit <- "Woody"
      duration <- "Perennial"
      # There are only two categories, so if it's not a tree it's a shrub
      if (grepl(code, pattern = "^TR")){
        growthhabitsub <- "Tree"
      } else {
        growthhabitsub <- "Shrub"
      }
    } else {
      # If it wasn't a woody, then it was non-woody
      growthhabit <- "Non-woody"
      # We only have forbs and graminoids, so we can just use if/else
      if (grepl(code, pattern = "F")) {
        growthhabitsub <- "Forb/herb"
      } else {
        growthhabitsub <- "Graminoid"
      }
      # Same for annual/perennial
      if (grepl(code, pattern = "^A")) {
        duration <- "Annual"
      } else {
        duration <- "Perennial"
      }
    }
  }
  # Assemble the output list
  return(list("Valid" = valid,
              "GrowthHabit" = growthhabit,
              "GrowthHabitSub" = growthhabitsub,
              "Duration" = duration))
}

#' Gather LPI data into tall/long data frames
#'
#' @description Given a list of data frames containing tblSites, tblPlots, tblLines, tblLPIHeader, and tblLPIDetail, create a tall format data frame for canopy data from LPI and one for heights from the specialized height fields.
#' @param dima.tables A list of data frames. Recommended to use the output from\code{read.dima()}. Must contain data frames called tblLPIHeader and tblLPIDetail which have the same fields as the tables in DIMA with the same names. If \code{meta} is \code{TRUE} then it must also include data frames called tblSites, tblPlots, and tblLines which have the same fields as the tables in DIMA with the same names. If \code{species.characteristics} is \code{TRUE} then it must also include data frames called tblSpecies and tblSpeciesGrowthHabit (and optionally tblSpeciesGroups) which have the same fields as the tables in DIMA with the same names.
#' @param meta Logical. If \code{TRUE} then the site, plot, and line names and keys will be added to the output data frames from \code{dima.list} using the data frames called tblSites, tblPlots, and tblLines. Defaults to \code{TRUE}
#' @param species.characteristics Logical. If \code{TRUE} then the available species information will be added from \code{dima.list} using the data frames tblSpecies, tblSpeciesGrowthHabit, and, if available, tblSpeciesGroups. Defaults to \code{TRUE}.
#' @return A list of two data frames: one containing the data from the LPI pin intercepts and one containing the data from the height methd done alongside pin drops.
#' @export
gather.lpi <- function(dima.tables,
                       meta = TRUE,
                       species.characteristics = TRUE){
  if (meta) {
    ## Merge the site, plot, and line details with the LPI data
    lpi <- metamerge(dima.tables = dima.tables, "LPI", minimum = TRUE)
  } else {
    lpi <- merge(dima.tables$tblLPIHeader, dima.tables$tblLPIDetail)
  }


  ## Strip out most of the variables because they're not relevant
  lpi.restricted <- dplyr::select(.data = lpi,
                                  FormDate,
                                  dplyr::starts_with("site", ignore.case = TRUE),
                                  dplyr::starts_with("plot", ignore.case = TRUE),
                                  dplyr::starts_with("line", ignore.case = TRUE),
                                  PointLoc,
                                  PointNbr,
                                  dplyr::contains("Top"),
                                  dplyr::contains("Lower"),
                                  dplyr::contains("Surface"),
                                  dplyr::ends_with("Woody"),
                                  dplyr::ends_with("Herbaceous"),
                                  dplyr::starts_with("Chkbox"),
                                  -dplyr::starts_with("avg"),
                                  -dplyr::contains("option", ignore.case = TRUE))

  ## Make a tall data frame with the site-plot-line-point identifiers and the hit codes by layer
  lpi.hits.tall <- tidyr::gather(data = dplyr::select(.data = lpi.restricted,
                                                      FormDate,
                                                      dplyr::starts_with("site", ignore.case = TRUE),
                                                      dplyr::starts_with("plot", ignore.case = TRUE),
                                                      dplyr::starts_with("line", ignore.case = TRUE),
                                                      PointLoc,
                                                      PointNbr,
                                                      TopCanopy,
                                                      dplyr::matches("^Lower[1-7]$"),
                                                      SoilSurface),
                                 key = "layer",
                                 value = "code",
                                 -(FormDate:PointNbr))

  ## Make a tall data frame with the site-plot-line-point identifiers and the checkbox status by layer
  lpi.chkbox.tall <- tidyr::gather(data = dplyr::select(.data = lpi.restricted,
                                                        FormDate,
                                                        dplyr::starts_with("site", ignore.case = TRUE),
                                                        dplyr::starts_with("plot", ignore.case = TRUE),
                                                        dplyr::starts_with("line", ignore.case = TRUE),
                                                        PointLoc,
                                                        PointNbr,
                                                        dplyr::starts_with("Chkbox"),
                                                        -dplyr::matches("Woody$|Herbaceous$")),
                                   key = "layer",
                                   value = "checkbox",
                                   -(FormDate:PointNbr))

  ## Make the names in the layer variable match
  lpi.chkbox.tall$layer <- stringr::str_replace_all(string = lpi.chkbox.tall$layer,
                                                    pattern = "^Chkbox",
                                                    replacement = "")

  lpi.chkbox.tall$layer[lpi.chkbox.tall$layer == "Top"] <- "TopCanopy"
  lpi.chkbox.tall$layer[lpi.chkbox.tall$layer == "Soil"] <- "SoilSurface"

  ## Make a tall data frame with the site-plot-line-point identifiers and the heights by layer
  lpi.layerheight.tall <- tidyr::gather(data = dplyr::select(.data = lpi.restricted,
                                                             FormDate,
                                                             dplyr::starts_with("site", ignore.case = TRUE),
                                                             dplyr::starts_with("plot", ignore.case = TRUE),
                                                             dplyr::starts_with("line", ignore.case = TRUE),
                                                             PointLoc,
                                                             PointNbr,
                                                             dplyr::starts_with("Height"),
                                                             -dplyr::matches("Woody$|Herbaceous$")),
                                        key = "layer",
                                        value = "layer.height",
                                        -(FormDate:PointNbr))

  ## Make the names in the layer variable match
  lpi.layerheight.tall$layer <- stringr::str_replace_all(string = lpi.layerheight.tall$layer,
                                                         pattern = "^Height",
                                                         replacement = "")

  lpi.layerheight.tall$layer[lpi.layerheight.tall$layer == "Top"] <- "TopCanopy"
  lpi.layerheight.tall$layer[lpi.layerheight.tall$layer == "Surface"] <- "SoilSurface"

  ## Merge the three tall data frames
  lpi.tall <- dplyr::distinct(merge(x = merge(x = lpi.hits.tall,
                                              y = lpi.chkbox.tall,
                                              all = TRUE),
                                    y = lpi.layerheight.tall,
                                    all = TRUE))

  ## Make an AIM height data frame
  lpi.habitheight.tall.woody <- dplyr::select(.data = lpi.restricted,
                                              FormDate,
                                              dplyr::starts_with("site", ignore.case = TRUE),
                                              dplyr::starts_with("plot", ignore.case = TRUE),
                                              dplyr::starts_with("line", ignore.case = TRUE),
                                              PointLoc,
                                              PointNbr,
                                              dplyr::matches("Woody$")) %>% dplyr::mutate(type = "woody")
  ## Strip out the extra name stuff so woody and herbaceous variable names will match.
  names(lpi.habitheight.tall.woody) <- stringr::str_replace_all(string = names(lpi.habitheight.tall.woody),
                                                                pattern = "Woody$",
                                                                replacement = "")

  lpi.habitheight.tall.herb <- dplyr::select(.data = lpi.restricted,
                                             FormDate,
                                             dplyr::starts_with("site", ignore.case = TRUE),
                                             dplyr::starts_with("plot", ignore.case = TRUE),
                                             dplyr::starts_with("line", ignore.case = TRUE),
                                             PointLoc,
                                             PointNbr,
                                             dplyr::matches("Herbaceous$")) %>% dplyr::mutate(type = "herbaceous")
  names(lpi.habitheight.tall.herb) <- stringr::str_replace_all(string = names(lpi.habitheight.tall.herb),
                                                               pattern = "Herbaceous$",
                                                               replacement = "")

  lpi.habit.height <- rbind(lpi.habitheight.tall.woody, lpi.habitheight.tall.herb)

  ## If we're adding species
  if (species.characteristics) {
    ## Make a species characteristics data frame
    species <- merge(x = dima.tables$tblSpecies,
                     y = dima.tables$tblSpeciesGrowthHabit,
                     by.x = "GrowthHabitCode",
                     by.y = "Code",
                     all.x = TRUE)
    ## If this is a recent enough version of DIMA, bring in the group information
    if (class(dima.tables$tblSpeciesGroups) == "data.frame") {
      species <- merge(x = species,
                       y = dima.tables$tblSpeciesGroups,
                       by.x = "Group",
                       by.y = "RecKey",
                       all.x = TRUE)
    }

    # Add the species characteristics for unknown codes that show up here
    # Which codes don't show up in the species characteristics table?
    unaccounted.codes <- unique(lpi.tall$code[!(lpi.tall$code %in% species$SpeciesCode)])
    # Make a data frame of the result of running those through check.code()
    unknowns.df <- dplyr::bind_rows(lapply(lapply(unaccounted.codes, check.code), data.frame))
    # Slice that to only the ones that were valid unknown codes
    unknowns.df <- unknowns.df[unknowns.df$Valid,]
    # Smash those into the species table
    species <- dplyr::bind_rows(list(species, unknowns.df))

    ## Add species information
    lpi.tall <- merge(x = lpi.tall,
                      y = species,
                      by.x = "code",
                      by.y = "SpeciesCode",
                      all.x = TRUE)

    ## Do the same for the habitat height data, but woody then herbaceous
    lpi.habit.height <- merge(x = lpi.habit.height,
                                  y = species,
                                  by.x = "Species",
                                  by.y = "SpeciesCode",
                                  all.x = TRUE)
  }

  ## Output the list
  output <- list("layers" = lpi.tall, "heights" = lpi.habit.height)
}
