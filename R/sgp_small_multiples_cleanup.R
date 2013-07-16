sgp_small_multiples_cleanup <- function(plot) {

  #grob-ify it
  g = ggplotGrob(plot)
  
  #get the grob no
  grob_no <- grep('guide-box', g$layout$name)
         
  #access that grob in the list
  #g$grobs[[grob_no]]
  
  #the ones we want are named 'key' and have 'GRID.text' in grob
  #g$grobs[[grob_no]][[1]][[1]]
  
  #empty vector, see if the grob matches, tag number if so
  target_grobs <- c()
  for (z in c(1:length(g$grobs[[grob_no]][[1]][[1]]))) {
    #print(g$grobs[[grob_no]][[1]][[1]][[1]][[z]]$name)  
    test <- grep('GRID.text', g$grobs[[grob_no]][[1]][[1]][[1]][[z]]$name)[1] == 1
  
    if (!is.na(test)) {
      target_grobs <- c(target_grobs, z)
    }
    
  }
  
  #to do - legend numbers should be 10, 50, 90
  #need sgp_bin as factor, for starters...
  #iterate over the grobs I tagged, change them from 'a' to whatever
  for (q in target_grobs) {
    #text itself
    g$grobs[[grob_no]][[1]][[1]][[1]][[q]]$label <- '10'
    #little smaller
    g$grobs[[grob_no]][[1]][[1]][[1]][[q]]$gp$fontsize <- 36
  }
  
  return(g)
}