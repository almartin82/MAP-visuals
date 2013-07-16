becca_plot <- function(
   df
  ,first_and_spring_only = TRUE
  ,entry_grades = c(-0.7, 4.3)
  ,color_scheme = 'KIPP Report Card'
  ,facets = FALSE
  ,facet_opts = FALSE
  ,title_text = FALSE) {
  
  #TRANSFORMATION 1 - TRIM
  #trim down the CDF - we don't need all the columns
  stage_1 <- df[,c(
     'SCH_ABBREV'
    ,'COHORT'
    ,'MAP_YEAR_ACADEMIC'
    ,'GRADE_LEVEL_SEASON'
    ,'MEASUREMENTSCALE'
    ,'PERCENTILE_2011_NORMS')]
    
  #all terms or first & spring only?
  if (first_and_spring_only) {
    #possible entry grades controlled by entry_grades parameter
    #default is Fall K, Fall 5 (aka -0.7, 4.3) - only change if you need to 
    #add an additional entry grade (perhaps 9th?) or to take away 5th
    #(eg for a fully grown KIPP school?)
    stage_1 <- stage_1[stage_1$GRADE_LEVEL_SEASON %in% entry_grades |
                       stage_1$GRADE_LEVEL_SEASON %% 1 == 0,]
  }
  
  #tag each observation w/ appropriate quartile
  stage_1$QUARTILE <- floor((stage_1$PERCENTILE_2011_NORMS/25) + 1)
  
  #just the number 1, so that we can SUM up the rows when we ddply them up
  #there must be a better way than this... right?
  stage_1$DUMMY <- 1

  #TRANSFORMATION 2 - COUNT
    #calculate group level averages.  Our final data set should have
  
    #SCHOOL    COHORT    YEAR    SUBJECT     QUARTILE      PCT
  
    #There is definitely a more elegant way to do this that doesn't
    #require 2 ddply calls, but this works for now
  
  #ddply into counts by quartile
  stage_2 <- ddply(
    stage_1
   ,.(SCH_ABBREV, COHORT, MAP_YEAR_ACADEMIC, GRADE_LEVEL_SEASON
     ,MEASUREMENTSCALE, QUARTILE)
   ,summarise
   ,n = sum(DUMMY)
  )

  #TRANSFORMATION 3 - PERCENTAGES
  #use ddply to calculate percentages by quartile
  stage_3 <- ddply(
    stage_2
   ,.(SCH_ABBREV, COHORT, MAP_YEAR_ACADEMIC, GRADE_LEVEL_SEASON
     ,MEASUREMENTSCALE)
   ,summarise
   ,QUARTILE = QUARTILE
   ,PCT = round((n / sum(n)) * 100, 1)
  )

  #add a column that indicates above/below grade level
  #this simplifies bar chart creation
  #preallocate column
  stage_3$AT_GRADE_LEVEL_DUMMY <- ''
  #set flags for above and below
  stage_3[stage_3$QUARTILE <= 2 , 'AT_GRADE_LEVEL_DUMMY'] <- 'NO'
  stage_3[stage_3$QUARTILE >= 3 , 'AT_GRADE_LEVEL_DUMMY'] <- 'YES'
  
  #TRANSFORMATION 4 - CUSTOM ORDERING
  #this was tricky (and important!) -- thanks Mike H.
  stage_3$ORDER <- stage_3$QUARTILE
  #2 becomes placeholder
  stage_3[stage_3$QUARTILE == 2, 'ORDER'] <- 'placeholder'
  #1 becomes 2
  stage_3[stage_3$QUARTILE == 1, 'ORDER'] <- 2
  #placeholder becomes 1
  stage_3[stage_3$ORDER == 'placeholder', 'ORDER'] <- 1
  #finally sort by new order (so midpoint calculation works properly)
  final_df <- stage_3[with(stage_3, order(MEASUREMENTSCALE, SCH_ABBREV, COHORT,
                                         MAP_YEAR_ACADEMIC, GRADE_LEVEL_SEASON,
                                         ORDER)), ] 

  #TRANSFORMATION 5 - TWO DFs FOR CHART
  #super helpful advice from: http://stackoverflow.com/questions/13734368/ggplot2-and-a-stacked-bar-chart-with-negative-values
  #above
  npr_above <- subset(final_df, AT_GRADE_LEVEL_DUMMY == 'YES')
  #below
  npr_below <- subset(final_df, AT_GRADE_LEVEL_DUMMY == 'NO')
  #flip the sign
  npr_below$PCT <- npr_below$PCT * -1

  #TRANSFORMATION 5 - CALCULATE MIDPOINTS (for chart labels)
  #one df for the two quartiles above the national average...
    
  npr_above = ddply(
    npr_above
   ,.(SCH_ABBREV, COHORT, MAP_YEAR_ACADEMIC, GRADE_LEVEL_SEASON, MEASUREMENTSCALE)
   ,transform
   ,MIDPOINT = cumsum(PCT) - 0.5*PCT
  )
  #...and another for those below.
  npr_below = ddply(
    npr_below
   ,.(SCH_ABBREV, COHORT, MAP_YEAR_ACADEMIC, GRADE_LEVEL_SEASON, MEASUREMENTSCALE)
   ,transform
   #,MIDPOINT = sum(ifelse(ORDER %in% c(1,2), PCT, 0))
   ,MIDPOINT = cumsum(PCT) - 0.5*PCT
  )
  
  npr_below <- transform(
    npr_below
   ,QUARTILE = ordered(QUARTILE, levels = names(sort(-table(QUARTILE))))
  )
  
  #FORMAT X AXIS LABELS
  becca_x_breaks <- sort(unique(final_df$GRADE_LEVEL_SEASON))
  becca_x_labels <- unlist(lapply(becca_x_breaks, fall_spring_me))  
  
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
     #,legend.title=element_blank()
      
      #title and axis sizes
     ,title = element_text(size = rel(0.9))
     ,axis.title.x = element_text(size = rel(0.9))
     ,axis.text.y = element_blank()
     
     ,plot.margin = rep(unit(0,"null"),4)
     #,legend.margin = rep(unit(0,"null"),4)
    ) +
    
    #format text
    theme(
     #,axis.title = element_text(size = rel(1.75))
    ) + scale_x_continuous(
      breaks = becca_x_breaks
     ,labels = becca_x_labels
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
  
  return(
    list(p, final_df)
  )
}
