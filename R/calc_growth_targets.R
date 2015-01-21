#' for each student.  This is experimental. 
#'
#' @description
#' \code{cakc_growht_targerts} calculates typical and college ready growth targets.
#'
#' @details Growth target calculator
#' 
#' @param .data the data, which is either a \code{mapvizier} object or a \code{data.frame}
#' @param typical.growth if \code{TRUE} (the default), thene the function calculates and returns
#' typcial growth target
#' @param college.ready if \code{TRUE} (the default), thene the function calculates and returns
#' college ready growth target
#' @param season1 first season to use for growth calculations
#' @param seaosn2 second season to use for growth calculations
#' @return a data.frame with growth columns or something
#' @export
#' @examples 
#' calc_growth_targets()

calc_growth_targets <-  function(.data, 
                                 typical.growth=TRUE,
                                 college.ready=TRUE, 
                                 season1="Spring",
                                 season2="spring"){
  # growth calculations
  if(typical.growth | college.ready){
    # construct and substitute names
    seasons <-paste0(season1,season2)
    if(is.mapvizier(.data)){
      norm.season <- as.name(switch(seasons,
                                    "FallFall"     = "R44.x", #appended x because these appear twice after the join
                                    "FallSpring"   = "R42.x",
                                    "FallWinter"   = "R41.x",
                                    "WinterSpring" = "R12.x",
                                    "SpringSpring" = "R22.x"
                                    )
                             )
    } else {
      norm.season <- as.name(switch(seasons,
                                    "FallFall"     = "R44", #appended x because these appear twice after the join
                                    "FallSpring"   = "R42",
                                    "FallWinter"   = "R41",
                                    "WinterSpring" = "R12",
                                    "SpringSpring" = "R22"
                                    )
                             )
    }
    
    if(!as.character(norm.season) %in% names(.data)) stop(paste(".data is missing a column named", 
                                                               gsub(".x","", norm.season),
                                                               ". You can fix this error by running nwea_growth(). See ?nwea_growth for more details"
                                                               )
                                                          )
    if(typical.growth){
      q<-substitute(norm.season)
      if(is.mapvizier(.data)){
        .data<-with(.data, mutate(.data, 
                                TypicalGrowth=eval(q), 
                                TypicalTarget=TypicalGrowth+TestRITScore.x,
                                MetTypical=TestRITScore.y>=TypicalTarget, 
                                GrowthSeason=paste(Season.x, Season.y, sep=" - ")
                                )
                    )
      } else {
        .data<-with(.data, mutate(.data, 
                                  TypicalGrowth=eval(q), 
                                  TypicalTarget=TypicalGrowth+TestRITScore,
                                 # MetTypical=TestRITScore.y>=TypicalTarget, 
                                  GrowthSeason=paste(season1, season2, sep=" - ")
        )
        )
      }
      
    }
    if(college.ready) {
      if(is.mapvizier(.data)){
        if(!"KIPPTieredGrowth.x" %in% names(.data)) stop(paste(".data is missing a column named", 
                                                              "KIPPTieredGrowth",
                                                              ". You can fix this error by running tiered_growth(). See ?tiered_growth for more details"
                                                              )
                                                        )
        q<-substitute(norm.season * KIPPTieredGrowth.x)
        .data <- with(.data, mutate(.data, 
                                  CollegeReadyGrowth=eval(q),
                                  CollegeReadyTarget=TestRITScore.x+CollegeReadyGrowth,
                                  MetCollegeReady=TestRITScore.y>=CollegeReadyTarget
                                  )
                     )
      } else {
        if(!"KIPPTieredGrowth" %in% names(.data)) stop(paste(".data is missing a column named", 
                                                              "KIPPTieredGrowth",
                                                              ". You can fix this error by running tiered_growth(). See ?tiered_growth for more details"
        )
        )
        q<-substitute(norm.season * KIPPTieredGrowth)
        .data <- with(.data, mutate(.data, 
                                  CollegeReadyGrowth=eval(q),
                                  CollegeReadyTarget=TestRITScore+CollegeReadyGrowth #,
                                  #MetCollegeReady=TestRITScore.y>=CollegeReadyTarget
                                  )
                      )
      }
      
    }
  }  
  #returns
  .data
  
}