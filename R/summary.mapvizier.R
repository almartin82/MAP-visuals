#' @title summary method for \code{mapvizier} class
#'
#' @description
#'  summarizes growth data from \code{mapvizier} orbect.
#'
#' @details Creates a \code{mapvizierSummary} object of growth data from a \code{mapvizier} 
#' object.  Includes the following summarizations for every growth term available
#' in the \code{mapvizier} object:
#' itemize{
#'  \item number tested in both assessment seasons (i.e., the number of students who 
#'  too a test in both assessment season and for which we are able to calcualate growth stats).
#'  \item Total students making typical growth
#'  \item Percent of students making typical growth
#'  \item Total students making college ready growth
#'  \item Percent of students making college ready  growth
#'  \item Total students with NPR >= 50 percentile in the first assessment season
#'  \item Percent students with NPR >= 50 percentile in the first assessment season
#'  \item Total students with NPR >= 75th percentile in the first assessment season
#'  \item Percent students with NPR >= 75 percentile in the first assessment season
#'  \item Total students with NPR >= 50 percentile in the second assessment season
#'  \item Percent students with NPR >= 50 percentile in the second assessment season
#'  \item Total students with NPR >= 75th percentile in the second assessment season
#'  \item Percent students with NPR >= 75 percentile in the second assessment season
#' } 

#' @param x a \code{mapvizier} object
#' @param ... other arguments to be passed to other functions (not currently supported)
#' @param digits the numbber of digits to round percentatges to. 

#' @return summary stats as a \code{mapvizierSummary} object.
#' @rdname summary
#' @export
#' @examples 
#' data(nweamap)
#' x<-mapvizier(nweamap)
#' summarize(x)

summary.mapvizier <- function(object, ..., digits=2){
  
  mapData<-group_by(as.data.frame(object$seasonMatched), 
                    SY.2, 
                    GrowthSeason, 
                    SchoolInitials, 
                    Grade.2, 
                    MeasurementScale
  )
  mapSummary <- dplyr::summarize(mapData,
                                 N = n(),
                                 N_Typical = sum(MetTypical),
                                 Pct_Typical = round(sum(MetTypical)/N,digits),
                                 N_CollegeReady = sum(MetCollegeReady),
                                 Pct_CollegeReady = round(sum(MetCollegeReady)/N,digits),
                                 N_50th_Pctl_S1 = sum(TestPercentile>=50),
                                 Pct_50th_Pctl_S1= round(sum(TestPercentile>=50)/N,digits),
                                 N_50th_Pctl_S2 = sum(TestPercentile.2>=50),
                                 Pct_50th_Pctl_S2 = round(sum(TestPercentile.2>=50)/N,digits),
                                 N_75th_Pctl_S1 = sum(TestPercentile>=75),
                                 Pct_75th_Pctl_S1 = round(sum(TestPercentile>=75)/N,digits),
                                 N_75th_Pctl_S2 = sum(TestPercentile.2>=75),
                                 Pct_75th_Pctl_S2 = round(sum(TestPercentile.2>=75)/N,digits)
  )
  
  
  setnames(mapSummary, 
           c("SchoolInitials", "Grade.2", "MeasurementScale", "SY.2"),
           c("School", "Grade", "Subject", "SY")
  )
  
  #class(mapSummary)<-"mapvizierSummary"  
  
  class(mapSummary)<-c("mapvizierSummary", class(mapSummary))
  
  #return
  mapSummary
  
}