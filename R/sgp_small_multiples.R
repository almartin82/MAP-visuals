sgp_small_multiples <- function (
  df
  #to-do - make 'order by' a parameter
 ,stu_per_row = 12) {

  #TRANSFORMATION
  #order by SGP, need to order factors
  df$STUDENT_NAME <- factor(
    df$STUDENT_NAME
   ,levels = as.character(unique(df[order(-df$GROWTH_PERCENTILE),]$STUDENT_NAME))
  )
  
  #TRANSFORM GRADE LEVEL FOR PLOTTING
  df$GRADE_START_TRANSFORM <- 0
  
  mask <- df$START_TERM_STRING == 'Winter'
  df[mask]$GRADE_START_TRANSFORM <- 0.65
  
  mask <- df$START_TERM_STRING == 'Fall'
  df[mask]$GRADE_START_TRANSFORM <- 0.3
  
  #limit decimals
  df$GROWTH_PERCENTILE <- trunc(df$GROWTH_PERCENTILE, 1)
  
  #get ymin and ymax
  growth_min <<- round_any(
     #minimum of the df
     min(df$RIT_CHANGE, na.rm = TRUE)
    ,5
    ,f = floor
  )
  growth_max <<- round_any(
    #minimum of the df
     max(df$RIT_CHANGE, na.rm = TRUE)
    ,5
    ,f = ceiling
  )
  x_transf_min <<- min(df$GRADE_START_TRANSFORM)
    
  #bin out into SGP thirds
  #empty vector
  df$SGP_BIN <- ''
  #spoofing ifelse
  df[GROWTH_PERCENTILE < 34, ]$SGP_BIN <- 'Low Growth'
  df[GROWTH_PERCENTILE < 67 & GROWTH_PERCENTILE >=34 , ]$SGP_BIN <- 'Average Growth'
  df[GROWTH_PERCENTILE >=67 , ]$SGP_BIN <- 'High Growth'

  p <- ggplot(
    data = df[!is.na(GROWTH_PERCENTILE),]
   ,aes(
        x = GRADE_START_TRANSFORM
       ,y = 0
       ,xend = 1
       ,yend = RIT_CHANGE
       ,label = GROWTH_PERCENTILE
       ,group = STUDENT_NAME
       ,color = SGP_BIN
      )
  ) +
    #start TERM
    geom_text(
      aes(
        x = GRADE_START_TRANSFORM
       ,y = 0
       ,label = START_TERM_VERIF
      )
     ,size = rel(2.75)
     ,color = 'gray20'
     ,alpha = .3
     #annotate down and to the right of start RIT
     ,vjust = 1
     ,hjust = 0
    ) + 
    geom_segment(
      size = rel(1)
     ,color = 'gray20'
     ,alpha = .6
    ) + 
    #end TERM
    geom_text(
      aes(
        x = 1
       ,y = RIT_CHANGE
       ,label = END_TERM_VERIF
      )
     ,size = rel(2.75)
     ,color = 'gray20'
     ,alpha = .3
     #annotate down and to the left of start RIT
     ,vjust = 1
     ,hjust = 1
    ) + 
    #TEXTUAL ANNOTATIONS
    #alpha the SGP
    geom_text(
      aes(
        x = x_transf_min + 0.5 * (1 - x_transf_min)
       ,y = growth_min + (0.6 * (growth_max - growth_min))
       ,color = SGP_BIN
      )
     ,size = rel(15)
     ,alpha = 0.5
    ) +
    #start RIT
    geom_text(
      aes(
        x = GRADE_START_TRANSFORM
       ,y = 0
       ,label = START_RIT
      )
     ,color = 'gray20'
     ,size = rel(4)
     #annotate up and to the right of start RIT
     ,vjust = 0
     ,hjust = 0
    ) + 
    #end RIT
    geom_text(
      aes(
        #arguably this is cheating; possible that a kid could have only F-W data
        #(and thus you'd want this to come from the data)
        x = 1
       ,y = RIT_CHANGE
       ,label = END_RIT
      )
     ,color = 'gray20'
     ,size = rel(4)
     #annotate down and to the left of end RIT
     ,vjust = 0
     ,hjust = 1
    ) + 
    #axis stuff
    scale_y_continuous(
      c(growth_min, growth_max)
    ) +
    scale_color_manual(
      values = c('orange', 'lightgreen', 'firebrick1')
    ) +
    scale_x_continuous(
      c(-.05, 1.05)
    ) +
    labs(
      color = "Student Growth Percentile"
    ) +
    #facets  
    facet_wrap(
      ~ STUDENT_NAME
     #,ncol = stu_per_row
    ) + 
    theme(
     axis.title.y = element_blank()
    ,axis.title.x = element_blank()
     #facet strips
    ,strip.text.x = element_text(size=rel(0.9))
    ,panel.border =  element_rect(fill=NA, colour = "gray80", size=0.5)
    #,plot.background = element_rect(color = 'black', size = 1)
    ,panel.background = element_blank()
    ,strip.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()

) 
   
  return(p)
}