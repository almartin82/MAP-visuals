#' @title becca_plot2
#'
#' @description method as wrapper for \code{\link{becca_plot}}
#'
#' @details See \code{\link{becca_plot}}

#' @param x an object to plot
#' @param ... other arguments passed to \code{\link{becca_plot}}

#' @return a \code{link[ggplot2]{ggplot2}} object
#' @rdname becca_plot2
#' @export
becca_plot2 <- function(.data
#                         ,school_name_column = 'sch_abbr'
#                         ,cohort_name_column = 'cohort'
#                         ,academic_year_column = 'map_year_academic'
#                         ,grade_level_season_column = 'grade_level_season'
#                         ,measurement_scale_column = 'measurementscale'
#                         ,percentile_column = 'percentile_2011_norms'
#                         ,first_and_spring_only = TRUE
#                         ,auto_justify_x = TRUE
#                         ,justify_widths = FALSE
#                         ,justify_min = NA
#                         ,justify_max = NA
#                         ,entry_grades = c(-0.7, 4.3)
#                         ,color_scheme = 'KIPP Report Card'
#                         ,facets = FALSE
#                         ,facet_opts = FALSE
#                         ,title_text = FALSE
                        ,...) {UseMethod("becca_plot2")}

#' @rdname becca_plot2.default
becca_plot2.default <- function(.data
                                ,school_name_column = 'sch_abbr'
                                ,cohort_name_column = 'cohort'
                                ,academic_year_column = 'map_year_academic'
                                ,grade_level_season_column = 'grade_level_season'
                                ,measurement_scale_column = 'measurementscale'
                                ,percentile_column = 'percentile_2011_norms'
                                ,first_and_spring_only = TRUE
                                ,auto_justify_x = TRUE
                                ,justify_widths = FALSE
                                ,justify_min = NA
                                ,justify_max = NA
                                ,entry_grades = c(-0.7, 4.3)
                                ,color_scheme = 'KIPP Report Card'
                                ,facets = FALSE
                                ,facet_opts = FALSE
                                ,title_text = FALSE){
  becca_plot(.data
             ,school_name_column
             ,cohort_name_column
             ,academic_year_column
             ,grade_level_season_column
             ,measurement_scale_column
             ,percentile_column
             ,first_and_spring_only
             ,auto_justify_x
             ,justify_widths
             ,justify_min
             ,justify_max
             ,entry_grades
             ,color_scheme
             ,facets
             ,facet_opts
             ,title_text)
}

#' @rdname becca_plot2.mapvizier
becca_plot2.mapvizier <- function(.data, ...
                                  #,first_and_spring_only = TRUE
                                  #,auto_justify_x = TRUE
#                                   ,justify_widths = FALSE
#                                   ,justify_min = NA
#                                   ,justify_max = NA
#                                   ,entry_grades = c(-0.7, 4.3)
#                                   ,color_scheme = 'KIPP Report Card'
#                                   ,facets = FALSE
#                                   ,facet_opts = FALSE
#                                   ,title_text = FALSE
                                  ,subsetting=filter(Year2>=2010,
                                                    MeasurementScale %in% c("Reading", "Mathematics")
                                  )){

    # Extract mapData from mapvizier object
   .data<-as.data.frame(.data$mapData) 
 
  # try filtering
  dplyr_call<-substitute(.data %>% subsetting)
  try(.data <- eval(dplyr_call), silent = TRUE) # just don't do anything if filter() is not passed or fails
  
  #get grade level seasons 
  gls<-unlist(lapply(.data$Season, grade_level_season))
  .data$GradeLevelSeason <- .data$Grade-gls
  

  # check for facets call and change formula to proper formula name
  func_args<-dots(...)
  if ("facets" %in% names(unlist(func_args))){
    
    
    facets<-as.formula(func_args$facets)
    lhs<-as.character(facets[[2]])
    rhs<-as.character(facets[[3]])
    
    
    lhs<-switch(lhs,
                'SchoolIntials' =' SCH_ABBREV',
                'CohortYear' = 'COHORT',
                'Year2'  = 'MAP_YEAR_ACADEMIC',
                'GradeLevelSeason' = 'GRADE_LEVEL_SEASON',
                'MeasurementScale' = 'MEASUREMENTSCALE',
                'TestPercentile' = 'PERCENTILE_2011_NORMS'
    )
    
    rhs<-switch(rhs,
                'SchoolIntials' =' SCH_ABBREV',
                'CohortYear' = 'COHORT',
                'Year2'  = 'MAP_YEAR_ACADEMIC',
                'GradeLevelSeason' = 'GRADE_LEVEL_SEASON',
                'MeasurementScale' = 'MEASUREMENTSCALE',
                'TestPercentile' = 'PERCENTILE_2011_NORMS'
    )
    
    func_args$facets<-as.character(paste(lhs, "~", rhs))
    
    func_args$.data <- .data
    func_args$school_name_column <- 'SchoolInitials'
    func_args$cohort_name_column <-'CohortYear'
    func_args$academic_year_column <- 'Year2'
    func_args$grade_level_season_column <- 'GradeLevelSeason'
    func_args$measurement_scale_column <- 'MeasurementScale'
    func_args$percentile_column <- 'TestPercentile'
    
    # Use do call to pass becca_plot() its arguments in a function
    
    p<-do.call(becca_plot, args = func_args)
    
  } else {
    p<-becca_plot(.data
                  ,school_name_column = 'SchoolInitials'
                  ,cohort_name_column = 'CohortYear'
                  ,academic_year_column = 'Year2'
                  ,grade_level_season_column = 'GradeLevelSeason'
                  ,measurement_scale_column = 'MeasurementScale'
                  ,percentile_column = 'TestPercentile'
                  ,...
    )
  }
  
  p
  
}
