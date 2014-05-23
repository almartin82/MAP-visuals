#' @title One Year SGO histogram
#'
#' @description
#' \code{one_year_sgp} returns a ggplot histogram that shows the distribution of SGPs
#'
#' @param df long data frame in TEAM canonical style
#' @param title_text desired plot title
#' 
#' @return returns a ggplot object
#' @export

one_year_sgp <- function (
    df
   ,title_text = FALSE  
  ) {
  #helper
  bins <- c(1, seq(10, 90, by = 10), 99)
  bins_7 <- seq(0, 105, by = 7.5)

  #get the count by bin?
  simple_hist <- hist(df$GROWTH_PERCENTILE, breaks = bins_7, plot=FALSE)
  
  chart_max <<- max(simple_hist$counts) + 5
  
  #calculate median SGP
  med_sgp <<- median(df$GROWTH_PERCENTILE, na.rm = TRUE)
  
  #plot
  p <- ggplot(
    data = df
   ,aes(
      x = GROWTH_PERCENTILE
    )
  ) +
  geom_text (
    data = NULL
   ,aes(
      x = 50
     ,y = .5 * chart_max
     ,label = med_sgp
     ,alpha = 0.7
    )
   ,size = 26
   ,color = if(med_sgp > 55) {
         'lightgreen'
         #NOLA 'lightgreen'
         #rgb(186, 255, 117, max = 255)
       } else if (med_sgp >= 45) {
         'orange'
         #NOLA 'lightyellow'
         #rgb(255, 255, 166, max = 255)
       } else if (med_sgp < 45) {
         'firebrick1'
         #NOLA 'lightred'
         #rgb(255, 155, 155, max = 255)
       }
  ) +
  geom_histogram(
   ,binwidth = 7.5
   ,alpha = 0.85
   #'TEAM blue'
   #,color = '#0067AC'
   #,fill = '#0067AC'
   ,fill = 'gray60'
  ) +    
  #labels
   labs(
     x = 'Student Growth Percentile'
    ,y = 'Number of Students'
   ) +
   theme(    #zero out cetain formatting
     panel.background = element_blank()
    ,plot.background = element_blank()
    ,panel.grid.major = element_blank()
    ,panel.grid.minor = element_blank()
    ,legend.position="none"
   ) + 
  scale_x_continuous(
    breaks = bins
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