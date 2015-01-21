#' @title Merge two different assessment seasons (by student and measurement scale)
#' from a single long-format MAP assessment data frame
#'
#' @description
#' \code{s2s_match} a dataframe with two season results matched on student-measurement
#' scale basis. 
#'
#' @details 
#' This function returns a data frame that results from subsetting a long-format data MAP assessment
#' data frame (i.e., where every students' test event occupies a single row), \code{.data} into two seasons 
#' and then mergeing the two subsets with an inner join on student id and measurement 
#' scale (via \code{dplyr::inner_join}) for the school year and seasons passed to it. All columns of \code{.data} are replicated, save of student id
#' and measuremen scale (since these are used to merge on) with the later season's column names
#' indicated wiht .2 suffix (i.e., \code{TestRITScore.2}). 
#' 
#' If indicated (by setting the values of the  \code{typical.growth} and \code{college.ready} 
#' parameters to \code{TRUE}) the function will also calculate the amount of growth (i.e. number of RIT points) and 
#' growth target (i.e., RIT score to attain) college ready growth.  
#' 
#' Note well that for \code{typical.growth=TRUE} the original 
#' data frame, \code{.data}, must have reported norms for the requried growth season.  The reported norm column 
#' must be named using the 2011 NWEA Norms table convention of the season (winter through fall) indicated by corresponding
#' integers (1-4);  For example fall to spring requires \code{.data} has a field names
#' \code{R42} and spring to spring wourld be \code{R22}.  These columns can be easily 
#' added to a CDF by using the \code{\link{nwea_growth}} function. 
#' 
#' Also note that calculating college ready growth and growth targets
#' requires that \code{.data} has a column named \code{KIPPTieredGrowth} containing 
#' a KIPP tiered multiplier for each student-assessment.  These data can be generated
#' using the \code{\link{tiered_growth}} function. 
#' 
#' @param .data a data frame with assessment data, in long-format (i.e., one student-assessment per row).
#' @param season1 a string of either "Fall", "Winter", or "Spring" for the first assessment season by which to subset \code{.data} and join on.
#' @param season2 a string of either "Fall", "Winter", or "Spring" for the first assessment season by which to subset \code{.data} and join on. 
#' Note that if "Spring" to "Fall" is  a valid combination but norms cannot be calcualted for it.
#' @param sy an integer indicating the (second half school year.  For example, enter 2014 for the 2013-2104 school year. For assessment combinations that are ambiguious like 
#' spring to spring, the school year is taken to be the the school year for season 2 and season 1 is taken from 
#' the prior year. 
#' @param typical.growth boolean indicating if typical growth, typical growth target, and typical growth met/exceeded inditor are to be calculted. Requires that \code{.data} have norm columns.
#' @param college.ready boolean indicating if college ready growth, college ready growth target, and college ready growth met/exceeded inditor are to be calculted. Requires that \code{.data} have KIPPTieredGrowth column.
#' 
#' @return a data.frame ith at least 2(m-1) columns (and as many as $(m-1) + 6) and 
#' a row for every student-assessment that occured in both season 1 and season 2. 
#' @export
#' @examples 
#' s2s_match()

s2s_match <- function(.data, 
                      season1="Fall", 
                      season2="Spring", 
                      sy=2013,
                      typical.growth=TRUE,
                      college.ready=FALSE){
  require(dplyr)
  
  stopifnot(season1 %in% c("Fall", "Spring", "Winter"), 
            season2 %in% c("Fall", "Spring", "Winter"),
            is.numeric(sy),
            is.data.frame(.data),
            is.logical(typical.growth),
            is.logical(college.ready)
  )
  
  
  # Filter to Season1
  # If season1=season2, i.e., spring-spring, roll Year2 back one year
  sy1<-sy
  if(season1==season2) sy1 <- sy-1
  # special check for spring to winter growth
  if(season1=="Spring" & season2=="Winter") sy1 <- sy-1
  m.1<-filter(.data, Season==season1, Year2==sy1)
  
  # Filter to Season2
  m.2<-filter(.data, Season==season2, Year2==sy)
  
  
  # Join on ID and MeasurementScale
  m.12<-inner_join(m.1, m.2, by=c("StudentID", "MeasurementScale"))
  
  # growth calculations
  if(typical.growth | college.ready){
    # construct and substitute names
    seasons <-paste0(season1,season2)
    norm.season <- as.name(switch(seasons,
                                  "FallFall"     = "R44.x", #appended x because these appear twice after the join
                                  "FallSpring"   = "R42.x",
                                  "FallWinter"   = "R41.x",
                                  "WinterSpring" = "R12.x",
                                  "SpringSpring" = "R22.x",
                                  "SpringWinter" = "R22.x",
                                  )
                           )
    if(!as.character(norm.season) %in% names(m.12)) stop(paste(".data is missing a column named", 
                                                  gsub(".x","", norm.season),
                                                       ". You can fix this error by running nwea_growth(). See ?nwea_growth for more details"
                                                       )
                                            )
    if(typical.growth){
      q<-substitute(norm.season)
      m.12<-with(m.12, mutate(m.12, 
                              TypicalGrowth=eval(q), 
                              TypicalTarget=TypicalGrowth+TestRITScore.x,
                              MetTypical=TestRITScore.y>=TypicalTarget, 
                              GrowthSeason=paste(Season.x, Season.y, sep=" - ")
                              )
                 )
    }
    if(college.ready) {
      if(!"KIPPTieredGrowth.x" %in% names(m.12)) stop(paste(".data is missing a column named", 
                                                     "KIPPTieredGrowth",
                                                    ". You can fix this error by running tiered_growth(). See ?tiered_growth for more details"
      )
      )
      q<-substitute(norm.season * KIPPTieredGrowth.x)
      m.12 <- with(m.12, mutate(m.12, 
                                CollegeReadyGrowth=eval(q),
                                CollegeReadyTarget=TestRITScore.x+CollegeReadyGrowth,
                                MetCollegeReady=TestRITScore.y>=CollegeReadyTarget
      )
      )
    }
    # Adjust Spring-to-winter growth to be half of spring to spring and 
    # adjust typical and CR goals and targets
    if(seasons=="SpringWinter"){
      m.12 <- m.12 %>%
        mutate(TypicalGrowth = round(TypicalGrowth/2),
               CollegeReadyGrowth = round(CollegeReadyGrowth/2),
               TypicalTarget = TestRITScore.x+TypicalGrowth,
               CollegeReadyTarget=TestRITScore.x+CollegeReadyGrowth,
               MetTypical=TestRITScore.y>=TypicalTarget,
               MetCollegeReady=TestRITScore.y>=CollegeReadyTarget
               )
    }
  }  
  
  # Rename columns with .x or .y suffixes ot have no suffix for season 1
  #  and .2 for season 2 suffixes
  setnames(m.12, gsub("\\.x","",names(m.12)))
  setnames(m.12, gsub("\\.y",".2",names(m.12)))
  
  # return
  m.12
}
