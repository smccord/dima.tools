#' Calculating percent cover from LPI data.
#' @description Calculate the percent cover by plot for variables or combinations of variables. Percent cover will be calculated for every combination of the variables requested, so if the variables are \code{GrowthHabitSub} and \code{Duration} then the output will contain fields like \code{Graminoid.Perennial}, \code{Graminoid.Annual}, \code{Shrub.Perennial}, etc. whereas using just the variable \code{code} will produce one column per species code. Any number of indicator variables can be used. These are calculated as cover from anywhere in the canopy column or as only the first hit in the canopy column. Any groupings where all the variable values were \code{NA} will be dropped.
#' @param lpi.tall A tall/long-format data frame. Use the data frame \code{"layers"} from the \code{gather.lpi()} output.
#' @param ... One or more bare variable name from \code{lpi.tall} to calculate percent cover for, e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @param hit Character string. If \code{"any"} then percent cover will be calculated using any hit in the canopy column (so a single pin drop record may be counted more than once if it had hits that corresponded to different groups). If \code{"first"} then only the first canopy hit at a pin drop will be used to calculate cover. Defaults to \code{"any"}.
#' @param by.year Logical. If \code{TRUE} then results will be reported further grouped by year using the \code{DateModified} field from the data forms. Defaults to \code{FALSE}.
#' @param by.line Logical. If \code{TRUR} then results will be reported further grouped by line using the \code{LineID} and \code{LineKey} fields from the data forms. Defaults to \code{FALSE}.
#' @export
pct.cover <- function(lpi.tall,
                      tall = FALSE,
                      hit = "any",
                      by.year = FALSE,
                      by.line = FALSE,
                      ...){
  ## Get a list of the variables the user wants to group by.
  grouping.variables <- rlang::quos(...)

  # For how deep to group. Always by plot, sometimes by line
  if (by.line) {
    level <- rlang::quos(PlotKey, PlotID, LineKey, LineID)
  } else {
    level <- rlang::quos(PlotKey, PlotID)
  }

  # Drop data where there is no code value (i.e. layers where there was no recorded hit)
  lpi.tall <- dplyr::filter(.data = lpi.tall,
                            !is.na(code),
                            code != "",
                            code != "None")

  # Within a plot, we need the number of pin drops, which we'll calculate by sorting the
  # values then taking the last [number of lines on the plot] values
  point.totals <- lpi.tall %>%
    dplyr::group_by(SiteKey, SiteID, SiteName, !!!level) %>%
    dplyr::summarize(point.count = sum(tail(sort(PointNbr), length(unique(LineID)))))

  # Add the point.counts field (it'll be the same for every record associated with a plot)
  lpi.tall <- merge(x = lpi.tall,
                    y = point.totals,
                    all.x = TRUE)

  # Get the layers into the correct order
  lpi.tall <- dplyr::mutate(.data = lpi.tall,
                            layer = factor(layer,
                                 levels = c("TopCanopy",
                                            unique(lpi.tall$layer)[grepl(unique(lpi.tall$layer), pattern = "^Lower[1-7]")],
                                            "SoilSurface"))) %>% dplyr::arrange(layer)

  if (hit == "basal") {
    hit <- "any"
    lpi.tall <- dplyr::filter(.data = lpi.tall,
                              layer == "SoilSurface")
  }

  summary <- switch(hit,
                    "any" = {
                      if (by.year) {
                        summary <- lpi.tall %>% dplyr::mutate(Year = format(lubridate::as_date(FormDate), "%Y")) %>%
                          dplyr::group_by(Year, SiteKey, SiteID, SiteName, PlotKey, PlotID, LineKey, LineID, PointNbr, point.count,
                                          !!!grouping.variables) %>%
                          ## Here's the breakdown of the gnarly parts:
                          # Because this is a tall format, we want just presence/absence for the grouping at a given point
                          # so we'll write in 1 if any of the layers within that grouping has a non-NA and non-"" value
                          dplyr::summarize(present = if(any(!is.na(code) & code != "")){1} else {0}) %>%
                          tidyr::unite(indicator, !!!grouping.variables, sep = ".") %>%
                          dplyr::ungroup() %>% dplyr::group_by(SiteKey, SiteID, SiteName, !!!level, indicator) %>%
                          # Within a plot, find the sum of all the "presents" then divide by the number of possible hits, which
                          # we added in point.count
                          dplyr::summarize(percent = 100*sum(present, na.rm = TRUE)/first(point.count)) %>%
                          ## Remove the empty groupings—that is the ones where all the indicator variable values were NA
                          dplyr::filter(!grepl(indicator, pattern = "^[NA.]{0,100}NA$"))
                      } else {
                        summary <- lpi.tall %>%
                          dplyr::group_by(SiteKey, SiteID, SiteName, PlotKey, PlotID, LineKey, LineID, PointNbr, point.count,
                                          !!!grouping.variables) %>%
                          ## Here's the breakdown of the gnarly parts:
                          # Because this is a tall format, we want just presence/absence for the indicator at a given point
                          # so we'll write in 1 if any of the layers within that indicator has a non-NA and non-"" value
                          dplyr::summarize(present = if(any(!is.na(code) & code != "")){1} else {0}) %>%
                          tidyr::unite(indicator, !!!grouping.variables, sep = ".") %>%
                          dplyr::ungroup() %>% dplyr::group_by(SiteKey, SiteID, SiteName, !!!level, indicator) %>%
                          # Within a plot, find the sum of all the "presents" then divide by the number of possible hits, which
                          # we added in point.count
                          dplyr::summarize(percent = 100*sum(present, na.rm = TRUE)/first(point.count)) %>%
                          ## Remove the empty groupings—that is the ones where all the indicator variable values were NA
                          dplyr::filter(!grepl(indicator, pattern = "^[NA.]{0,100}NA$"))
                      }
                    },
                    "first" = {
                      if (by.year) {
                        summary <- lpi.tall %>% dplyr::mutate(Year = format(lubridate::as_date(FormDate), "%Y")) %>%
                          # Strip out all the non-hit codes
                          dplyr::filter(!(code %in% c("", NA, "None"))) %>%
                          dplyr::group_by(Year, SiteKey, PlotKey, LineID, PointNbr, point.count) %>%
                          # Get the first hit at a point
                          dplyr::summarize(code = first(code)) %>%
                          # Get all the other fields back
                          merge(x = dplyr::distinct(dplyr::select(lpi.tall, SiteKey, SiteID, SiteName, PlotKey, PlotID, LineKey, LineID, PointNbr, code, !!!grouping.variables)),
                                y = .,
                                all.y = TRUE) %>%
                          tidyr::unite(indicator, !!!grouping.variables, sep = ".") %>%
                          dplyr::ungroup() %>% dplyr::group_by(Year, SiteKey, SiteID, SiteName, !!!level, indicator) %>%
                          dplyr::summarize(percent = 100*n()/first(point.count)) %>%
                          dplyr::filter(!grepl(indicator, pattern = "^[NA.]{0,100}NA$"))
                      } else {
                        summary <- lpi.tall %>%
                          # Strip out all the non-hit codes
                          dplyr::filter(!(code %in% c("", NA, "None"))) %>%
                          dplyr::group_by(SiteKey, PlotKey, LineID, PointNbr, point.count) %>%
                          # Get the first hit at a point
                          dplyr::summarize(code = first(code)) %>%
                          # Get all the other fields back
                          merge(x = dplyr::distinct(dplyr::select(lpi.tall, SiteKey, SiteID, SiteName, PlotKey, PlotID, LineKey, LineID, PointNbr, code, !!!grouping.variables)),
                                y = .,
                                all.y = TRUE) %>%
                          tidyr::unite(indicator, !!!grouping.variables, sep = ".") %>%
                          dplyr::ungroup() %>% dplyr::group_by(SiteKey, SiteID, SiteName, !!!level, indicator) %>%
                          dplyr::summarize(percent = 100*n()/first(point.count)) %>%
                          dplyr::filter(!grepl(indicator, pattern = "^[NA.]{0,100}NA$"))
                      }
                    })



  if (!tall) {
    summary <- tidyr::spread(summary, key = indicator, value = percent, fill = 0)
  }

  return(summary)
}
