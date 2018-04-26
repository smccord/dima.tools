gather.soil.stability<-function(dima.tables, source="AIM"){


  #read in tabular data###something is happening in metamerge so that the LineKey's are merging properly
  soil.stability<-metamerge(dima.tables = dima.tables, form="soil stability")


  #Morph soil stability detail into a tidy format

  gathered<-soil.stability %>%
    #Remove standard columns (In and Dip Times and Date Downloaded in DB)
    dplyr::select(., match= -dplyr::starts_with("In"), -dplyr::starts_with("Dip"))%>%
    #Convert to tall format
    tidyr::gather(., key=variable, value=value, -c(PlotKey:BoxNum))

  #Remove NAs
  gathered<-gathered[!is.na(gathered$value),]

  #Separate numerical suffixes from field type
  gathered$key<-stringr::str_extract(string=gathered$variable, pattern = "^[A-z]+")

  gathered<-subset(gathered, select=-c(variable,BoxNum))

  #Remove duplicates
  gathered<-unique(gathered)

  #Spread the gathered data so that Line, Rating, Vegetation, and Hydro are all different variables

  soil.stability.tall<-lapply(X=as.list(unique(gathered$key)),
                                     FUN=function(k=as.list(unique(gathered$key)),df=gathered ){
                                       df[df$key==k,] %>% mutate(id=1:n())%>%
                                         tidyr::spread(key=key, value=value)%>% select(-id)
                                     })%>% purrr::reduce(dplyr::full_join)

  soil.stability.tall$Rating<-as.numeric(soil.stability.tall$Rating)


  #Return final merged file
  return(soil.stability.tall)
}
