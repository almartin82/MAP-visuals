galloping_elephants <- function (
    df
   ,first_and_spring_only = TRUE
   ,entry_grades = c(-0.7, 4.3)
   ,title_text = FALSE
  ) {
  
   #data transformations
    stage_1 <- df[,c(
     'SCH_ABBREV'
    ,'MAP_YEAR_ACADEMIC'
    ,'GRADE_LEVEL_SEASON'
    ,'MEASUREMENTSCALE'
    ,'TESTRITSCORE'
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
   
  stage_1$grade_season_label <-unlist(lapply(stage_1$GRADE_LEVEL_SEASON, fall_spring_me))  
  
  stage_1 <- stage_1[with(stage_1, order(GRADE_LEVEL_SEASON)), ]
  
  stage_1$grade_season_label <- factor(stage_1$grade_season_label, ordered = TRUE)
   
  #grab appropriate norms data
  subj <- as.character(unique(stage_1$MEASUREMENTSCALE))
  
  norms_slim <- norms[norms$MEASUREMENTSCALE == subj
          & norms$RIT >= min(stage_1$TESTRITSCORE)
          & norms$RIT <= max(stage_1$TESTRITSCORE)
          #norms after 8th grade are stupid
          & norms$GRADE_LEVEL <= 8, ]

  norms_slim$chart_label <- paste("nat'l Gr.", norms_slim$GRADE_LEVEL, 'mean')

  #dummy, to get height of density plot
  dummy <- ggplot(
    data = stage_1
   ,aes(
      x = TESTRITSCORE
     ,group = grade_season_label
    )
  ) + 
    geom_density(adjust = 0.8)

  points <- ggplot_build(dummy)
    
  #head(points$data[[1]])    
  #just get the data
  density_raw <- points$data[[1]]
  #extract the max per group
  max_points <- ddply(density_raw, "group", summarise, y = max(y))
    
  #THE ACTUAL PLOT
  p <- ggplot(
    data = stage_1
   ,aes(
      x = TESTRITSCORE
     ,group = grade_season_label
     ,fill = grade_season_label
     ,alpha = GRADE_LEVEL_SEASON
    )
  ) + 
  geom_point(
    aes(
      y = 0
    )
   ,alpha = 0
  )
    
  #annotation lines (behind everything else)
  p <- p + annotate(
    geom = 'segment'
   ,x = norms_slim$RIT
   ,xend = norms_slim$RIT
   ,y = 0
   ,yend = 1
   ,color = 'gray20'
   ,alpha = .3
  )

  #annotation text (behind everything else)
  p <- p + annotate(
    geom = 'text'
   ,x = norms_slim$RIT
   ,y = .8 * max(max_points$y)
   ,label = norms_slim$chart_label
   ,color = 'gray20'
   ,alpha = .3
   ,size = 3
   ,vjust = 1
   ,angle = 90
  )

  p <- p +
  geom_density(
    adjust = 0.8
  ) + scale_alpha(
    range = c(0.5, 0.85)
  ) + theme(
    #zero out formats
    panel.background = element_blank()
   ,plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   ,legend.position = 'none'
    
   ,axis.text.y = element_blank()
   ,axis.ticks.y = element_blank()
   ,plot.margin = rep(unit(0,"null"),4)
   ,axis.title.x = element_blank()
   ,axis.title.y = element_blank()
  ) + 
  scale_fill_brewer(
    #type = 'div', palette = 'RdYlBu'
    type = 'seq', palette = 'Blues'
    #start = 0.2, end = 0.8, na.value = "red"
  ) 
  
  #join a DF with extracted data & max values - this tags all the max rows in the df
  full_max <- join(density_raw, max_points, type = 'inner')
  
  #cbind in the factor names (ie the group names)
  grade_labels <- levels(stage_1$grade_season_label)  
    
  full_max <- cbind(full_max, grade_labels)
  
  #ANNOTATE
  p <- p + annotate(
    geom = 'text'
   ,x = full_max$x
   ,y = full_max$y
   ,label = full_max$grade_labels
   ,size = 7
  )

    
  #title?
  if (title_text != FALSE) {
    p <- p +
      labs(
        title = title_text
      )
  }
  return(p)
}