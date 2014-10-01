#' @title Becca Vichniac's Quartile (Floating Bar) Chart 
#'
#' @description
#' \code{becca_plot} returns a ggplot object binned quaritle performonce
#'
#' @details 
#' This function builds and prints a bar graph with 4 bins per bar show MAP data
#' binned by quartile (National Percentile Rank).  Bars are centered at 50th percentile 
#' horizonatally
#' 
#' @param .data the data frame in TEAM canoncical style (long data forms)
#' @param school_name_column column in \code{.data} with school names
#' @param cohort_name_column  column in \code{.data} with cohornt names
#' @param grade_level_season_column column in \code{.data} with numeric indicating grade season (e.g., Fall 4th
#' = 3.3, Winter 4th = 3.7, Spring 4th = 4.0).
#' @param measurement_scale_column column in \code{.data} with subject
#' @param percentile_column = column in \code{.data} with NPR.
#' @param first_and_spring_only indicator for showing shoing Fall-to-Spring rather than Spring-to-Spring
#' @param justify_widths width justification indicator
#' @param justify_min 
#' @param justify_max 
#' @param entry_grades = c(-0.7, 4.3)  GradeSeasons that should use Fall and Spring (i.e., Kinder =-.3 and 5th = 4.3)
#' @param color_scheme only 'KIPP Report Card'
#' @param facets = FALSE
#' @param facet_opts = FALSE
#' @param title_text = FALSE
#' @param small_n_cutoff drop a grade_level_season if less than x% of the max? 
#' (useful when dealing with weird cohort histories)
#' 
#' @return prints a ggplot object
#' @export

becca_plot <- function(
   .data
   #for munging data
  ,school_name_column = 'sch_abbr'
  ,cohort_name_column = 'cohort'
  ,grade_level_season_column = 'grade_level_season'
  ,measurement_scale_column = 'measurementscale'
  ,test_percentile_column = 'percentile_2011_norms'
   #controls if 'extra' data points (fall, winter) get dropped
  ,first_and_spring_only = TRUE
  ,entry_grades = c(-0.8, 4.2)
  ,auto_justify_x = TRUE
  ,justify_widths = FALSE
  ,justify_min = NA
  ,justify_max = NA
  ,color_scheme = 'KIPP Report Card'
  ,facets = FALSE
  ,facet_opts = FALSE
  ,title_text = FALSE
  ,small_n_cutoff = .001
  ) {
  require(dplyr)
    
  # Changed passed dataframes' column names to those used throughout 
  # function
  
  colnames(.data)[colnames(.data) == school_name_column] <- 'SCH_ABBREV'
  colnames(.data)[colnames(.data) == cohort_name_column] <- 'COHORT'
  colnames(.data)[colnames(.data) == grade_level_season_column] <- 'GRADE_LEVEL_SEASON'
  colnames(.data)[colnames(.data) == measurement_scale_column] <- 'MEASUREMENTSCALE'
  colnames(.data)[colnames(.data) == test_percentile_column] <- 'PERCENTILE_2011_NORMS'  

  #TRANSFORMATION 1 - TRIM
  #trim down the C.data - we don't need all the columns
  require(data.table)
  d1 <- as.data.table(as.data.frame(.data[,c(
     'SCH_ABBREV'
    ,'COHORT'
    ,'GRADE_LEVEL_SEASON'
    ,'MEASUREMENTSCALE'
    ,'PERCENTILE_2011_NORMS')]))
    
  #all terms or first & spring only?
  if (first_and_spring_only) {
    #possible entry grades controlled by entry_grades parameter
    #default is Fall K, Fall 5 (aka -0.7, 4.3) - only change if you need to 
    #add an additional entry grade (perhaps 9th?) or to take away 5th
    #(eg for a fully grown KIPP school?)
    d1 <- d1[with(d1, round(GRADE_LEVEL_SEASON, 1) %in% round(entry_grades,1) | GRADE_LEVEL_SEASON %% 1 == 0), ]
  }
  
  #drop small N time periods
  by_grade_season <- group_by(.data, GRADE_LEVEL_SEASON)
  grade_season_counts <- summarize(
    by_grade_season
   ,n=n()
  )
  biggest <- max(grade_season_counts$n)
  grade_season_counts$include <- grade_season_counts$n >= small_n_cutoff * biggest
  use_these <- grade_season_counts[grade_season_counts$include==TRUE, 'GRADE_LEVEL_SEASON']
  
  d1 <- d1[d1$GRADE_LEVEL_SEASON %in% use_these, ]
  
  #calculate quartile from test percentile
  d1[,QUARTILE:=floor((PERCENTILE_2011_NORMS/25) + 1)]
  
  #TRANSFORMATION 2 - COUNT
  #calculate group level averages.  Our final data set should have
  
  #SCHOOL    COHORT    YEAR    SUBJECT     QUARTILE      PCT
  
  #There is definitely a more elegant way to do this that doesn't
  #require 2 ddply calls, but this works for now
  
  d2<-d1[,list(N_Qrtl=.N), 
         keyby=list(SCH_ABBREV, 
                    COHORT,
                    GRADE_LEVEL_SEASON, 
                    MEASUREMENTSCALE, 
                    QUARTILE)][
                      d1[,list(.N), 
                         keyby=list(SCH_ABBREV, 
                                    COHORT,
                                    GRADE_LEVEL_SEASON, 
                                    MEASUREMENTSCALE)]][,PCT:=round(N_Qrtl/N*100,1)]

  #add a column that indicates above/below grade level
  #this simplifies bar chart creation
  #set flags for above and below
  d2[QUARTILE<=2, AT_GRADE_LEVEL_DUMMY:='NO']
  d2[QUARTILE>=3, AT_GRADE_LEVEL_DUMMY:='YES']

  
  #TRANSFORMATION 4 - CUSTOM ORDERING
  #this was tricky (and important!) -- thanks Mike H.
  d2[,ORDER:=QUARTILE]
  
  #stage_3$ORDER <- stage_3$QUARTILE
  #2 becomes placeholder
  
  d2[QUARTILE==2, ORDER:=99]
  
  #stage_3[stage_3$QUARTILE == 2, 'ORDER'] <- 'placeholder'
  #1 becomes 2
  d2[QUARTILE==1, ORDER:=2]
  
  #stage_3[stage_3$QUARTILE == 1, 'ORDER'] <- 2
  #placeholder becomes 1
  d2[ORDER==99,ORDER:=1]
  
  #stage_3[stage_3$ORDER == 'placeholder', 'ORDER'] <- 1
  #finally sort by new order (so midpoint calculation works properly)
  final_data <- copy(d2[order(MEASUREMENTSCALE, 
                         SCH_ABBREV, 
                         COHORT,
                         GRADE_LEVEL_SEASON,
                         ORDER)]) 

  #TRANSFORMATION 5 - TWO .datas FOR CHART
  #super helpful advice from: http://stackoverflow.com/questions/13734368/ggplot2-and-a-stacked-bar-chart-with-negative-values
  #above
  npr_above <- final_data[AT_GRADE_LEVEL_DUMMY == 'YES']
  #below
  npr_below <- final_data[AT_GRADE_LEVEL_DUMMY == 'NO']
  #flip the sign
  npr_below[, PCT:= PCT * -1]

  #TRANSFORMATION 5 - CALCULATE MIDPOINTS (for chart labels)
  #one df for the two quartiles above the national average...
    
  npr_above <- npr_above[,list(N, 
                               PCT, 
                               AT_GRADE_LEVEL_DUMMY, 
                               ORDER,
                               QUARTILE,
                               MIDPOINT=cumsum(PCT) - 0.5*PCT),
                         by=list(SCH_ABBREV, 
                                 COHORT, 
                                 GRADE_LEVEL_SEASON, 
                                 MEASUREMENTSCALE)]
  #...and another for those below.
  npr_below <- npr_below[,list(N, 
                               PCT, 
                               AT_GRADE_LEVEL_DUMMY, 
                               ORDER,
                               QUARTILE,
                               MIDPOINT=cumsum(PCT) - 0.5*PCT),
                         by=list(SCH_ABBREV, 
                                 COHORT, 
                                 GRADE_LEVEL_SEASON, 
                                 MEASUREMENTSCALE)]
  
  npr_below[,QUARTILE:=ordered(QUARTILE, levels = names(sort(-table(QUARTILE))))]
  
  
  #FORMAT X AXIS LABELS
  becca_x_breaks <- sort(unique(final_data$GRADE_LEVEL_SEASON))
  becca_x_labels <- unlist(lapply(becca_x_breaks, fall_spring_me))
  
  if (auto_justify_x == TRUE) {
    becca_x_breaks <- c(min(becca_x_breaks) - 0.35, becca_x_breaks, max(becca_x_breaks) + 0.35)
    becca_x_labels <- c('', becca_x_labels, '')
  }
  
  if (justify_widths == TRUE) {
    becca_x_breaks <- c(justify_min, becca_x_breaks, justify_max)
    becca_x_labels <- c('', becca_x_labels, '')
  }
  
  #PLOT PLOT PLOT PLOT
  p <- ggplot() +
    
    #top half of NPR plots
    geom_bar(
      data = npr_above
     ,aes(
        x = GRADE_LEVEL_SEASON
       ,y = PCT
       ,fill = factor(QUARTILE)
       ,order = ORDER
      )
     ,stat = "identity"
    ) +
    
    #bottom half of NPR plots
    geom_bar(
      data = npr_below
     ,aes(
        x = GRADE_LEVEL_SEASON
       ,y = PCT
       ,fill = factor(QUARTILE)
       ,order = ORDER
      )
     ,stat = "identity"
    ) +
    
    #labels above
    geom_text(
      data = npr_above
     ,aes(
        x = GRADE_LEVEL_SEASON
       ,y = MIDPOINT
       ,label = round(PCT,0)
      )
     ,size = 4
    ) +
    
    #labels below
    geom_text(
      data = npr_below
     ,aes(
        x = GRADE_LEVEL_SEASON
       ,y = MIDPOINT
       ,label = abs(round(PCT, 0))
      )
     ,size = 4
    ) +
    
    #axis labels
    labs(
      x = 'Grade Level'
     ,y = 'Percentage of Cohort'
    ) +
    
    #clean out some default ggplot formatting elements
    theme(
      #zero out cetain formatting
      panel.background = element_blank()
     ,plot.background = element_blank()
     ,panel.grid.major = element_blank()
     ,panel.grid.minor = element_blank()
     ,axis.ticks.y = element_blank()
      
      #title and axis sizes
     ,title = element_text(size = rel(0.9))
     ,axis.title.x = element_text(size = rel(0.9))
     ,axis.text.y = element_blank()
     
     ,plot.margin = rep(unit(0,"null"),4)
    ) +
    
    scale_x_continuous(
      breaks = becca_x_breaks
     ,labels = becca_x_labels
    ) +
    coord_cartesian(
      xlim=c(min(becca_x_breaks),max(becca_x_breaks))  
    )

  legend_labels = c('1st', '2nd', '3rd', '4th')
  
  #color style?
  if(color_scheme == 'KIPP Report Card') {
    p <- p +
      #dark gray, light gray, light orange, dark orange
      scale_fill_manual(
        values = c(
          rgb(207, 204, 193, max = 255)
         ,rgb(230, 230, 230, max = 255)
         ,rgb(254, 188, 17, max = 255)
         ,rgb(247, 148, 30, max = 255)
        )
       ,name = 'Quartiles' 
       ,labels = legend_labels
      )
  } else if (color_scheme == 'Sequential Blues') {
    p <- p + scale_fill_brewer(
      type = "seq"
     ,palette = 1
    ) 
  } else {
    p <- p + scale_fill_manual(
      values = color_scheme
     ,labels = legend_labels
    )
  }
  
  #title?
  if (title_text != FALSE) {
    p <- p +
      labs(
        title = title_text
      )
  }
  
  #facet specified AND facet opts
  if (facets != FALSE & facet_opts != FALSE) {
    p <- p + eval(facet_grid(as.formula(facets), facet_opts))
  #facet specified WITHOUT facet opts
  } else if (facets != FALSE & facet_opts == FALSE) {
    p <- p + facet_grid(as.formula(facets))
  }
  #no facet specified = no need to do anything (implicit)

  #legend formatting stuff
  p <- p + guides(
     fill = guide_legend(
      #make it a little smaller
       title.theme = element_text(
         angle = 0
        ,size = rel(8)
       )
      ,keywidth = .5
      ,keyheight = .5
      #flips the order
      ,reverse = TRUE)
  )
  
  p

}

