#' @title Create a mapvizier object
#'
#' @description
#' \code{mapvizier()} returns an object with class \code{mapvizier}
#'
#' @details 
#' This method takes a long form CDF, addess columns for growth calculations and 
#' subsetting and joins assessemnts by student ID and measurement scale for seasons
#' in appropriate school years.
#' 
#' @param x a data frame from a long CDF that has student data
#' already joined to it 

#' @return a \code{mapvizier} object
#' @examples 
#' 
#' require(mapvizier)
#' 
#' data(nweamap)
#' x<-mapvizier(nweamap)
#' str(x)

#' @export
mapvizier <- function(x) UseMethod("mapvizier")

#' @rdname mapvizier
#' @export
mapvizier.default <- function(x){
  require(dplyr)
  require(data.table)
  # coerce to data.frame to be safe
  x<-as.data.frame(x)
  
  x2 <- cbind(x,
              term_split2(term_name = x$TermName,
                          grade = x$Grade)
  )
  
  # lookout!  here comes some dplyr sugar to get the munging done
  # Adding school abbreviations, TestQuartile, and KIPP Tiered Growth
  x3 <- x2 %>% filter(GrowthMeasureYN=="TRUE") %>%
    mutate(SchoolInitials   = abbrev(SchoolName), 
           TestQuartile     = kipp_quartile(TestPercentile),
           KIPPTieredGrowth = tiered_growth(TestQuartile, Grade)
    )
  
  # let's add some proper growth norms from the growth norms table
  
  map_data <- cbind(x3,
                    nwea_growth(x3$Grade, 
                                x3$TestRITScore, 
                                x3$MeasurementScale
                    )
  )
  #shit, why not just to all the joins we could ever want on this original data
  # Create Seaason to Season Numbers
  year_list<-unique(map_data$Year2)
  
  map.SS<-rbindlist(lapply(year_list, 
                           s2s_match, 
                           .data=map_data, 
                           season1="Spring", 
                           season2="Spring", 
                           typical.growth=T,
                           college.ready=T
  )
  )
  map.FS<-rbindlist(lapply(year_list, 
                           s2s_match,
                           .data=map_data, 
                           season1="Fall", 
                           season2="Spring", 
                           typical.growth=T,
                           college.ready=T
  )
  )
  map.FW<-rbindlist(lapply(year_list, 
                           s2s_match, 
                           .data=map_data, 
                           season1="Fall", 
                           season2="Winter", 
                           typical.growth=T,
                           college.ready=T
  )
  )
  map.WS<-rbindlist(lapply(year_list,
                           s2s_match, 
                           .data=map_data, 
                           season1="Winter", 
                           season2="Spring", 
                           typical.growth=T,
                           college.ready=T
  )
  )
  map.FF<-rbindlist(lapply(year_list, 
                           s2s_match, 
                           .data=map_data, 
                           season1="Fall", 
                           season2="Fall", 
                           typical.growth=T,
                           college.ready=T
  )
  )
  map.SW<-rbindlist(lapply(years, 
                           mapvisuals::s2s_match, 
                           .data=map.all, 
                           season1="Spring", 
                           season2="Winter", 
                           typical.growth=T,
                           college.ready=T
  )
  )
  
  
  map.all.growth<-rbindlist(list(map.SS, map.FS, map.FW, map.WS, map.FF, map.SW))
  
  # add to mapviz object
  
  mapviz<-list(mapData = map_data,
               seasonMatched = map.all.growth
  )
  
  class(mapviz) <- "mapvizier"
  
  mapviz            
}

#' @title Reports whether x is a mapvizier object
#'
#' @description
#' Reports whether x is a mapvizier object
#' @param x an object to test
#' @export
is.mapvizier <- function(x) inherits(x, "mapvizier")