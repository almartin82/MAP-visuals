growth_status_scatter <- function (
    df
   ,show_title = FALSE
   ,school
   ,subject
   ,grade
  ) {
  
  #data frame for annotations
  lab_x <- c(33/2, 50, 66 + 33/2
            ,33/2, 50, 66 + 33/2)
  lab_y <- c(75, 75, 75
            ,25, 25, 25)
  lab_text <- c('Low Growth\nAbove Gr. Lev.'
                ,'Avg Growth\nAbove Gr. Lev.'
                ,'High Growth\nAbove Gr. Lev.'
                ,'Low Growth\nBelow Gr. Lev.'
                ,'Avg Growth\nBelow Gr. Lev.'
                ,'High Growth\nBelow Gr. Lev.')
  
  annotation_df <- data.frame(
    lab_x = lab_x
   ,lab_y = lab_y
   ,lab_text = lab_text
  )
  
  #plot
  p <- ggplot(
     data = df
    ,aes(
       x = GROWTH_PERCENTILE
      ,y = END_NPR
      ,label = STUDENT_NAME
     )
   ) +
   #I am definitely going to hell for this
   geom_point(
    alpha = 0 
   ) 
  #annotation layer (behind everything else)
  p <- p + annotate(
    geom = 'text'
   ,x = annotation_df$lab_x
   ,y = annotation_df$lab_y
   ,label = annotation_df$lab_text
   ,size = 12
   ,color = 'gray80'
   ,alpha = 0.8
  )

  p <- p +              
   #classify
   geom_vline(
     xintercept=c(34,66)
    ,size = 1
    ,color = 'gray50'
    ,alpha = .6
   ) +
   geom_hline(
     yintercept=c(50)
    ,size = 1
    ,color = 'gray50'
    ,alpha = .6 
   ) +
   #chart elements
   geom_text(
     size = rel(3)
    ,alpha = .5
   ) +
   geom_jitter(
     size = 3
    ,shape = 1
    ,position = position_jitter(height = 0.75, width = .75)
   ) +
   #scale
   coord_cartesian(
     ylim = c(0, 100), xlim = c(0,100)
   ) +
   scale_x_continuous(
     breaks = seq(10, 90, by=10)
    ,minor_breaks = NULL
   ) +
   scale_y_continuous(
     breaks = seq(10, 90, by=10)
    ,minor_breaks = NULL
   ) +
   #labels
   labs(
     x = paste(academic_abbrev, 'Growth Percentile')
    ,y = paste(year_ends, 'EOY National Percentile Rank')
   ) +
   theme(
     plot.title = element_text(hjust=0, face="bold", size = 20)
    ,panel.background = element_blank()
    ,panel.grid.major = element_line(
       color = 'gray95'
      ,linetype = 'longdash'
     )
   ) 

  if(show_title != FALSE) {
    p <- p +
    labs(
      title = paste(academic_year, sch, 'gr.', grade, subj,  'EOY Growth vs Status')
    )
  }
   
  return(p)
}
