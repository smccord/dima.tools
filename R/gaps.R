#' Calculate the number, length, and percent of gaps
#' @description Calculate the number, length, and percent of gaps by plot or line.
#' @param dima.tables Raw tables as imported from a DIMA. Use data from \code{read.dima( all=T)}.
#' @param tall Logical. If \code{TRUE} then the returned data frame will be tall rather than wide and will not have observations for non-existent values e.g., if no data fell into a group on a plot, there will be no row for that group on that plot. Defaults to \code{FALSE}.
#' @param by.year Logical. If \code{TRUE} then results will be reported further grouped by year using the \code{DateModified} field from the data forms. Defaults to \code{FALSE}.
#' @param by.line Logical. If \code{TRUR} then results will be reported further grouped by line using the \code{LineID} and \code{LineKey} fields from the data forms. Defaults to \code{FALSE}.
#' @export


#Percent Gap
gap.cover <- function(dima.tables,
                 by.line = FALSE,
                 tall = FALSE,
                 by.year=FALSE,
                 breaks=c(20,25,50, 100, 200),
                 type="canopy") {

  gap.data <- metamerge(dima.tables = dima.tables,
                        form = "Gap",
                        minimum = TRUE)



  # For how deep to group. Always by plot, sometimes by line
  if (by.line) {
    level <- rlang::quos(SiteKey, SiteID, SiteName, PlotKey, PlotID, LineKey, LineID)
  } else {
    level <- rlang::quos(SiteKey, SiteID, SiteName, PlotKey, PlotID)
  }


  # For grouping by year
  if (by.year){
    gap.data<-dplyr::mutate(gap.data, Year = format(lubridate::as_date(FormDate), "%Y"))
    level<-rlang::quos(!!!level, Year)
  }

  ## Convert the line lengths to the same units as the gaps
  #if metric (gap$Measure==1) then multiply by 100 to put the line length in centimeters
  gap.data$LineLengthAmount[gap.data$Measure == 1] <- 100 * gap.data$LineLengthAmount[gap.data$Measure == 1]
  #if English (gap$Measure==2) then multiply by 12 to put the line length in inches, then convert both the line length and measured gaps to metric
  if(unique(gap.data$Measure) %in% 2){
    gap.data$LineLengthAmount[gap.data$Measure == 2]<-gap.data$LineLengthAmount[gap.data$Measure == 2]*2.54
    gap.data$Gap[gap.data$Measure == 2]<-gap.data$Gap[gap.data$Measure == 2]*2.54
    gap.data$GapMin[gap.data$Measure == 2]<-gap.data$MinGap[gap.data$Measure == 2]*2.54
  }
  ##Note if this is Basal or Canopy Gap
  if (type=="canopy"){
    gap.data<-gap.data[gap.data$RecType=="C",]
    if (nrow(gap.data)==0) {
      stop("There are no canopy gap records. Did you intend to summarize basal gap?")
      }
  }
  if (type=="basal"){
    gap.data<-gap.data[gap.data$RecType=="B",]
    if (nrow(gap.data)==0) {
      stop("There are no basal gap records. Did you intend to summarize basal gap?")
    }
  }
  #Gap lookup table
  class.min<-c(0, breaks)
  class.max<-c(breaks, max(gap.data$LineLengthAmount))
  class<-data.frame(class.min, class.max)
  class$indicator<-paste(class.min, "to", class.max, sep=".")
  class$indicator[length(class$indicator)]<-gsub("\\..*]*", "plus", dplyr::last(class$indicator))

  #Merge the indicator names into the gap data table
  plots.indicator<-expand.grid(PlotKey=unique(gap.data$PlotKey), indicator=class$indicator)
  gap.data.indicator<-merge(gap.data, plots.indicator)%>% merge(., class)


  ## Calculate indicators
  gap.summary <- gap.data.indicator  %>% dplyr::group_by(!!!level, LineKey, LineID, LineLengthAmount, indicator)%>%
    #calculate number of gaps,total length of gaps, and percent of gaps in each indicator category
    dplyr::summarize(n = length(Gap[Gap >= class.min & Gap < class.max]),
                     length = sum(Gap[Gap >= class.min & Gap < class.max]))%>%
    dplyr::mutate(.,percent=100*(length/LineLengthAmount))

  ## If by.line=FALSE, then take the mean of the lines to summarize the gap indicators to the plot level
  if(by.line){
  gap.summary<-gap.summary%>%dplyr::group_by(!!!level, LineLengthAmount, indicator)%>%
    dplyr::summarize(n=mean(n),length=mean(length), percent=mean(percent))
  }

  ##If tall=FALSE, then convert to wide format
  if (!tall) {
    percent <- tidyr::spread(gap.summary, key = indicator, value = percent, fill=0)%>% select(., -n,-length)
    n<-tidyr::spread(gap.summary, key = indicator, value = n, fill=0)%>% select(., -percent,-length)
    length<-tidyr::spread(gap.summary, key = indicator, value = length, fill=0)%>% select(., -n,-percent)
    gap.summary<-list("percent"=percent, "n"=n, "length"=length)
    }

  return(gap.summary)
}

