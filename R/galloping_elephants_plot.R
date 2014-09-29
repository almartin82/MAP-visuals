#' @title galloping_elephants_plot
#'
#' @description \code{galloping_elephants_plot()} is a method that serves as wrapper for \code{\link{galloping_elephants}}.
#' 
#'
#' @details \code{galloping_elephants_plot} serves as wrapper that dispatches to \code{\link{galloping_elephants}} depending
#' on the class of the object passed to the \code{.data} parameter. 
#' 
#' If the object is a \code{\link{mapvizier}} object, then \code{galloping_elephants}' requirement that the data columns conform 
#' to KIPP NJ (n\'{e}e TEAM) canonical form is unnecessary (since we know which 
#' columns are which in a \code{mapvizier object}). The \code{subsetting} argument must be used
#' when a \code{mapvizier} object is passed, since the data will need to be 
#' properly subsetted. The user should pass this argument a call to \code{\link[dplyr]{filter(...)}}, where 
#' that function's \code{.data} argument is unnecessary. All other arguments (e.g. \code{facets})
#' are passed to \code{galloping_elephants} via the \code{...} argument.
#' 
#' Ff the object is a not a \code{mapvizier} object (like a \code{data.frame}) then as with \code{becca_plot}, the 
#' user needs to identify school, cohort, year, grade-level-season, and percentile columns.
#' 
#' @seealso \code{\link{galloping_elephants}}, \code{link[dplyr]{filter}}
#' @param .data an object to plot
#' @param ... other arguments passed to \code{\link{galloping_elephants}}
#' @param subsetting (required for \code{mapvizier obejcts}) a call to \code{dplyr}'s \code{filter} function, leaving out 
#' the \code{.data} argument (i.e. only containing subsetting arguments)

#' @return a \code{link[ggplot2]{ggplot2}} object
#' @rdname galloping_elephants_plot
#' @export
galloping_elephants_plot <- function(.data, ...) UseMethod("galloping_elephants_plot")


#' @rdname galloping_elephants_plot.default
#' @name galloping_elephants_plot.default
#' @title Wrapper to galloping_elephants for non \code{\link{mapvizier}} objects
#' @aliases galloping_elephants
galloping_elephants.default <- function (.data
                                 ,first_and_spring_only = TRUE
                                 ,entry_grades = c(-0.8, 4.2)
                                 ,title_text = FALSE
                                 ,ho_cutoff = 13
                                 ,bw_adjust=1
                                 ,school_name_column = 'sch_abbrev'
                                 ,academic_year_column = 'map_year_academic'
                                 ,grade_level_season = 'grade_level_season'
                                 ,measurement_scale_column = 'measurementscale'
                                 ,rit_score_column = 'testritscore'
                                 ,test_percentile_column = 'percentile_2011_norms'
                                 ,current_grade_column='cur_grade_level'
                                 ){
  galloping_elephants(.data
                      ,first_and_spring_only
                      ,entry_grades
                      ,title_text
                      ,ho_cutoff 
                      ,bw_adjust
                      ,school_name_column
                      ,academic_year_column
                      ,grade_level_season
                      ,measurement_scale_column
                      ,rit_score_column
                      ,test_percentile_column
                      ,current_grade_column
                      )
}


#' @rdname galloping_elephants_plot.mapvizier
#' @name galloping_elephants_plot.mapvizier
#' @title Wrapper to galloping_elephants for \code{\link{mapvizier}} objects
#' @aliases galloping_elephants_plot
galloping_elephants_plot.mapvizier <- function(.data, ...
                                               ,subsetting=filter(CohortYear==2023, 
                                                                  MeasurementScale=="Reading", 
                                                                  Season!="Winter")
                                               ){
  
  # check to see if subsetting argument is missing
  if(missing(subsetting)){ 
    stop(paste("mapvizier objects must include a subsetting argument.\n",
               "You should pass something like subsetting=filter(CohortYear==2018).\n",
               "Type ?galloping_elephants_plot.mapvizier or ?filter for more information."
    )
    )
  }
  # Extract mapData from mapvizier object
  .data<-as.data.frame(.data$mapData) 
  
  # try filtering
  dplyr_call<-substitute(.data %>% subsetting)
  try(.data <- eval(dplyr_call), silent = TRUE) # just don't do anything if filter() is not passed or fails
  
  #get grade level seasons 
  gls<-unlist(lapply(.data$Season, grade_level_season))
  .data$GradeLevelSeason <- .data$Grade+gls
  
  # 
 p <- galloping_elephants(.data,
                      ,school_name_column = 'SchoolInitials'
                      ,academic_year_column = 'Year2'
                      ,grade_level_season = 'GradeLevelSeason'
                      ,measurement_scale_column = 'MeasurementScale'
                      ,rit_score_column = 'TestRITScore'
                      ,test_percentile_column = 'TestPercentile'
                      ,current_grade_column='Grade'
                      ,...)
  p
}
