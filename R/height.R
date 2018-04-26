#' Calculate the vegetation height
#' @param lpi.tall A tall/long-format data frame. Use the data frame \code{"height"} from the \code{gather.lpi()} output.
#' @param ... One or more bare variable name from \code{lpi.tall} to calculate percent cover for, e.g. \code{GrowthHabitSub} to calculate percent cover by growth habits or \code{GrowthHabitSub, Duration} to calculate percent cover for categories like perennial forbs, annual graminoids, etc.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @param hit Character string. If \code{"any"} then percent cover will be calculated using any hit in the canopy column (so a single pin drop record may be counted more than once if it had hits that corresponded to different groups). If \code{"first"} then only the first canopy hit at a pin drop will be used to calculate cover. Defaults to \code{"any"}.
#' @param by.year Logical. If \code{TRUE} then results will be reported further grouped by year using the \code{DateModified} field from the data forms. Defaults to \code{FALSE}.
#' @param by.line Logical. If \code{TRUE} then results will be reported further grouped by line using the \code{LineID} and \code{LineKey} fields from the data forms. Defaults to \code{FALSE}.
#' @param by.species Logical. If \code{TRUE} the results will be reported by species and any growth habit assignments in \code {...}
#' @export


mean.height<-function(lpi.tall,
                      by.line = FALSE,
                      tall = FALSE,
                      by.year=FALSE,
                      by.max=FALSE,
                      by.species=FALSE,
                      ...){
  ## Get a list of the variables the user wants to group by.
  grouping.variables <- rlang::quos(...)

  # For grouping by line
  if (by.line) {
      level <- rlang::quos(SiteKey, SiteID, SiteName, PlotKey, PlotID, LineKey, LineID)
    } else {
      level <- rlang::quos(SiteKey, SiteID, SiteName, PlotKey, PlotID)
    }


  # For grouping by year
  if (by.year){
    height<-dplyr::mutate(height, Year = format(lubridate::as_date(FormDate), "%Y"))
    level<-rlang::quos(!!!level, Year)
  }

  # Calculate Mean woody height including zeros
  summary<-lpi.tall %>% dplyr::filter(!is.na(Height)) %>%
    dplyr::group_by(!!!level) %>%
    dplyr::filter(type=="woody")%>%
    dplyr::summarize(mean.woody.height.all=mean(Height))
  if(sum(summary$mean.woody.height.all) > 0){
    #Calculate mean woody height not including zeros
    summary<-lpi.tall %>% dplyr::filter(!is.na(Height)) %>%
      dplyr::group_by(!!!level) %>%
      dplyr::filter(type=="woody")%>%
      dplyr::filter(Height!=0)%>%
      dplyr::summarize(mean.woody.height=mean(Height))%>% merge(x=summary)
  }else {
      summary$mean.woody.height<-0
    }

  # Calculate mean herbaceous height including zeros
  summary<-lpi.tall %>% dplyr::filter(!is.na(Height)) %>%
    dplyr::group_by(!!!level) %>%
    dplyr::filter(type=="herbaceous")%>%
    dplyr::summarize(mean.herbaceous.height.all=mean(Height))%>% merge(x=summary)
  if(sum(summary$mean.herbaceous.height.all) > 0) {
  # Calculate mean woody height not including zeros
  summary<-lpi.tall %>% dplyr::filter(!is.na(Height)) %>%
    dplyr::group_by(!!!level) %>%
    dplyr::filter(type=="herbaceous")%>%
    dplyr::filter(Height!=0)%>%
    dplyr::summarize(mean.herbaceous.height=mean(Height))%>% merge(x=summary)

  }else {
    summary$mean.herbaceous.height<-0
  }

  if (by.max) {
    # Calculate mean max height  (tallest of woody or herbaceous at each point)
    summary<-lpi.tall %>% dplyr::filter(!is.na(Height)) %>%
      dplyr::group_by(!!!level, PointNbr )%>%
      dplyr::summarize(max.height=max(Height))%>%
      dplyr::group_by(!!!level) %>% dplyr::summarize(max.height=mean(max.height))%>%merge(x=summary)
  }

  if (by.species){
    summary<-lpi.tall %>% dplyr::filter(!is.na(Height), Species!="None") %>%
      dplyr::group_by(!!!level,!!!grouping.variables, Species) %>%
      dplyr::summarize(mean.height=mean(Height))%>%
      tidyr::spread(key=Species, value=mean.height)%>% merge(x=summary)


  }

  return(summary)
}



