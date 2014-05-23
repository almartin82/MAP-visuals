#' @title Strand boxes
#'
#' @description
#' \code{strand_boxes} returns a ggplot boxplot of MAP strand scores
#'
#' @param df long data frame in TEAM canonical style
#' 
#' @return returns a ggplot object
#' @export

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
     ,id.vars = names(stage_1a)[1:7]
     ,value.name = names(stage_1a)[8:15]
    )
  
  #melt names
  stage_2b <- melt(
      data = stage_1b
     ,id.vars = names(stage_1a)[1:7]
     ,value.name = names(stage_1a)[8:15]
  )
  
  #head(stage_2a)
  #head(stage_2b)
  
  #these should be the same length
  #nrow(stage_2a)
  #nrow(stage_2b)  
  
  #cbind them together
  stage_3 <- stage_2a
  stage_3$GOAL_NAME <- stage_2b$value
  
  #head(stage_3)
  
  #drop NA goals
  stage_4 <- stage_3[!is.na(stage_3$GOAL_NAME),]
  #head(stage_4)
  y_center <<- min(stage_4$value, na.rm = TRUE) + 0.5 * (max(stage_4$value, na.rm = TRUE) - min(stage_4$value, na.rm = TRUE)) 
  goal_names <- attributes(factor(stage_4$GOAL_NAME))$levels
  
  more_30 <- nchar(goal_names) > 30
  
  smart_breaks <- ifelse(more_30, '-\n', '')
  
  goal_names_format <<- paste(
    substr(goal_names, start = 1, stop = 30)
   ,smart_breaks
   ,substr(goal_names, start = 31, stop = 100)
   ,sep = ''
  )
  #a data frame of labels!
  #first - how many non-null goals are there?
  
  #str(stage_4$GOAL_NAME)
  
  
  #unique(stage_4[stage_4$value > 0,]
  
  #now make a boxplot
  p <- ggplot(
    data = stage_4
   ,aes(
      x = factor(GOAL_NAME)
     ,y = value
     ,fill = factor(GOAL_NAME)
    )
  ) +
  #empty
  geom_jitter(
    alpha = 0
  ) + 
  annotate(
    "text"
   ,x = seq(1,length(goal_names_format))
   ,y = rep(y_center, length(goal_names_format))
   ,label = goal_names_format
   ,angle = 90
   ,size = 8
   ,color = 'gray80'
   ,alpha = .9
  ) +
  geom_boxplot(
    alpha = 0.6
  ) +
  #coord_flip() + 
  geom_jitter(
    position = position_jitter(width = .15)
   ,color = 'gray85'
   ,alpha = 0.6
   ,shape = 1
  ) + 
  stat_summary(
   aes(
     label = round(..y..,1)
   )
  ,fun.y = mean
  ,geom = 'text'
  ,size = 7
  ) +
  labs(
    x = 'Goal Name'
   ,y = 'RIT Score'
  ) +
  theme(
    panel.background = element_blank()
   ,plot.background = element_blank()
   ,panel.grid.major = element_blank()
   ,panel.grid.minor = element_blank()
   #,axis.text.x = element_text(size = rel(0.7))
   ,axis.title.y = element_blank()
   ,axis.text.x = element_blank()
   ,panel.margin = unit(0,"null")
   ,plot.margin = rep(unit(0,"null"),4)
   ,axis.ticks.margin = unit(0,"null")
  )
  
  return(p)
}