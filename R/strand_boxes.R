strand_boxes <- function (df) {
  
  #DATA TRANSFORMATIONS
  #first, trim down the big CDF.
  stage_1a <- df[,c(
   'PS_STUDENTID'
  ,'COHORT'
  ,'MEASUREMENTSCALE'
  ,'SCH_ABBREV'
  ,'MAP_YEAR_ACADEMIC'
  ,'FALLWINTERSPRING'
  ,'TESTRITSCORE'
  ,'GOAL1RITSCORE'
  ,'GOAL2RITSCORE'
  ,'GOAL3RITSCORE'
  ,'GOAL4RITSCORE'
  ,'GOAL5RITSCORE'
  ,'GOAL6RITSCORE'
  ,'GOAL7RITSCORE'
  ,'GOAL8RITSCORE')]

  stage_1b <- df[,c(
   'PS_STUDENTID'
  ,'COHORT'
  ,'MEASUREMENTSCALE'
  ,'SCH_ABBREV'
  ,'MAP_YEAR_ACADEMIC'
  ,'FALLWINTERSPRING'
  ,'TESTRITSCORE'
  ,'GOAL1NAME'
  ,'GOAL2NAME'
  ,'GOAL3NAME'
  ,'GOAL4NAME'
  ,'GOAL5NAME'
  ,'GOAL6NAME'
  ,'GOAL7NAME'
  ,'GOAL8NAME')]

  #melt RIT values
  stage_2a <- melt(
      data = stage_1a
     ,id.vars = names(stage_1)[1:7]
     ,value.name = names(stage_1)[8:15]
    )
  
  #melt names
  stage_2b <- melt(
      data = stage_1b
     ,id.vars = names(stage_1)[1:7]
     ,value.name = names(stage_1)[8:15]
  )
  
  head(stage_2a)
  head(stage_2b)
  
  #these should be the same length
  nrow(stage_2a)
  nrow(stage_2b)  
  
  #cbind them together
  stage_3 <- stage_2a
  stage_3$GOAL_NAME <- stage_2b$value
  
  head(stage_3)
  
  #drop NA goals
  stage_4 <- stage_3[!is.na(stage_3$GOAL_NAME),]
  
  #now make a boxplot
  p <- ggplot(
    data = stage_4
   ,aes(
      x = factor(GOAL_NAME)
     ,y = value
     ,fill = factor(GOAL_NAME)
    )
  ) + 
  geom_boxplot(
    outlier.size = 0) +
  coord_flip() + 
  geom_jitter(
    position = position_jitter(width = .15)
   ,color = 'gray60'
   ,alpha = 0.8
  ) + 
  stat_summary(
   aes(
     label = round(..y..,1)
     )
  ,fun.y = mean
  ,geom = 'text'
  ,size = 8
  ) +
  labs(
    x = 'RIT Score'
   ,y = 'Goal Name'
  ) +
  theme(
    panel.background = element_blank()
   ,plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
  )
  
  return(p)
}