#' @title RIT "Height Weight" Growth, with NPR bands
#'
#' @description
#' \code{height_weight_npr} returns a ggplot object meant to be used as a template
#'
#' 
#' @param desired_subj
#' 
#' @return a ggplot object
#' @export


height_weight_npr <- function(
  desired_subj
 #this lives in /data/norms_2011_dense.rda
 ,norms_dense
 ,color_list = rainbow_colors()
 ,ribbon_alpha = .35
 ,annotation_style = 'points'
 ,line_style = 'none'
) {
  require(ggplot2)
  e <- new.env()
  
  #only spring, desired percentiles
  norms_dense <- norms_dense[norms_dense$measurementscale==desired_subj &
    (norms_dense$percentile %in% c(1,5,95,99) | norms_dense$percentile %% 10==0) &
    (norms_dense$fallwinterspring == 'Spring' | (norms_dense$grade==0 & norms_dense$fallwinterspring == 'Fall')), ]
    
  #modify grade level
  norms_dense$grade_mod <- sapply(norms_dense$fallwinterspring, grade_level_season)
  norms_dense$grade <- norms_dense$grade + norms_dense$grade_mod
  
  #add y axis margins
  placeholder1 <- norms_dense[norms_dense$grade == 11.0,]
  #arbitrary, just needs to be bigger than 11
  placeholder1$grade <- 14
  norms_dense <- rbind(norms_dense, placeholder1)
  
  placeholder2 <- norms_dense[norms_dense$grade == -0.8,]
  #arbitrary, just needs to be smaller than -0.8
  placeholder2$grade <- -3
  norms_dense <- rbind(norms_dense, placeholder2)
  e$norms_dense <- norms_dense[with(norms_dense, order(measurementscale, grade)), ]

  #had a lot of trouble here. 
    #cutting into ribbon bins
    e$npr_grades <- c(-3,-0.8,0,1,2,3,4,5,6,7,8,9,10,11,14)
    e$nprs <- c(1,5,10,20,30,40,50,60,70,80,90,95,99)
    
    e$npr_band01 <- subset(e$norms_dense, percentile == e$nprs[1])
    e$npr_band05 <- subset(e$norms_dense, percentile == e$nprs[2])
    e$npr_band10 <- subset(e$norms_dense, percentile == e$nprs[3])
    e$npr_band20 <- subset(e$norms_dense, percentile == e$nprs[4])
    e$npr_band30 <- subset(e$norms_dense, percentile == e$nprs[5])
    e$npr_band40 <- subset(e$norms_dense, percentile == e$nprs[6])
    e$npr_band50 <- subset(e$norms_dense, percentile == e$nprs[7])
    e$npr_band60 <- subset(e$norms_dense, percentile == e$nprs[8])
    e$npr_band70 <- subset(e$norms_dense, percentile == e$nprs[9])
    e$npr_band80 <- subset(e$norms_dense, percentile == e$nprs[10])
    e$npr_band90 <- subset(e$norms_dense, percentile == e$nprs[11])
    e$npr_band95 <- subset(e$norms_dense, percentile == e$nprs[12])
    e$npr_band99 <- subset(e$norms_dense, percentile == e$nprs[13])

    #what is needed is a data frame with ribbon, x, ymin, and ymax
    #make them per band, then rbind  
      #first make the top and bottom - custom
      e$df_npr1 <- data.frame(
        rib = rep('below_1', 15)
       ,x = e$npr_band01$grade
        #dummy value - just needs to be small
       ,ymin = rep(100, 15)
       ,ymax = e$npr_band01$RIT
      )
      e$df_npr99 <- data.frame(
        rib = rep('above_99', 15)
       ,x = e$npr_band99$grade
        #dummy value - just needs to be big
       ,ymin = e$npr_band99$RIT
       ,ymax = rep(300, 15)
      )
     e$df <- rbind(e$df_npr1, e$df_npr99)
   
     #then generate the others in a loop
     bands <- ls(pattern="npr_band*", envir=e)
     
     #list to hold ribbon names
     e$ribbons <- rep(NA, 12)
     
     for (i in 1:(length(bands)-1)) {
       new_df_name <- paste(bands[i], bands[i+1], sep='_')
       #remove 'band'
       new_df_name <- gsub('band', '', new_df_name)
       
       #lower and upper df
       lower <- get(bands[i], envir=e)
       upper <- get(bands[i+1], envir=e)
       
       #make a new df for this ribbon
       inner_df <- data.frame(
         rib = rep(new_df_name, 15)
        ,x = e$npr_grades
        ,ymin = lower$RIT
        ,ymax = upper$RIT
       )
       
       #rbind to existing df
       e$df <- rbind(e$df, inner_df)
       #update list of ribbons
       e$ribbons[i] <- new_df_name
     }
          
    #now make the geom_ribbons
      #first make top & bottom
      e$rib_under_1 <- geom_ribbon(
        data = e$df[e$df$rib == 'below_1', ]
       ,aes(
          x = x
         ,ymin = ymin
         ,ymax = ymax
        )
       ,fill = color_list[1]
       ,alpha = ribbon_alpha
       ,environment = e
      )
      e$rib_above_99 <- geom_ribbon(
        data = e$df[e$df$rib == 'above_99', ]
       ,aes(
          x = x
         ,ymin = ymin
         ,ymax = ymax
        )
       ,fill = color_list[14]
       ,alpha = ribbon_alpha
       ,environment = e
      )
 
   for (i in 1:length(e$ribbons)) {
     new_rib_name <- paste('rib', e$ribbons[i], sep='_')
     #make ribbon
     inner_ribbon <- geom_ribbon(
       data = e$df[e$df$rib == e$ribbons[i], ]
      ,aes(
          x = x
         ,ymin = ymin
         ,ymax = ymax
        )
       ,fill = color_list[i+1]
       ,alpha = ribbon_alpha
       ,environment = e       
     )
     
     #appropriate df
     assign(new_rib_name, inner_ribbon, envir=e)
   }
 
  #base ggplot 
  p <- ggplot(
    data = norms_dense
   ,environment = e
  )
  
  #annotation style options
  if (grepl('points', annotation_style)) {
    npr_annotation <- geom_point(
      aes(
        x = grade
       ,y = RIT
      )
    )
  } else if (grepl('big numbers', annotation_style)) {
    npr_annotation <- geom_text(
      aes(
        x = grade
       ,y = RIT
       ,label = percentile
      )
    )
  } else if (grepl('small numbers', annotation_style)) {
    npr_annotation <- geom_text(
      aes(
        x = GRADE
       ,y = RIT
       ,label = percentile
      )
     ,size = 3  
     ,fontface="italic"
     ,color = 'gray40'
     ,alpha = 0.8
    ) 
  } else {
    npr_annotation <- NULL
  } 
  
  #lines
  if (grepl('gray lines', line_style)) {
    npr_lines <- geom_line(
        aes(
          x = grade
         ,y = RIT
         ,group = percentile        
        )
       ,size = 0.5
       ,color = 'gray80'
      )
  } else {
    npr_lines <- NULL
  }
  
  #put it all together
  p <- p + 
  e$rib_under_1 + 
  e$rib_npr_01_npr_05 +
  e$rib_npr_05_npr_10 +
  e$rib_npr_10_npr_20 +
  e$rib_npr_20_npr_30 +
  e$rib_npr_30_npr_40 +
  e$rib_npr_40_npr_50 +
  e$rib_npr_50_npr_60 +
  e$rib_npr_60_npr_70 +
  e$rib_npr_70_npr_80 +
  e$rib_npr_80_npr_90 +
  e$rib_npr_90_npr_95 +
  e$rib_npr_95_npr_99 +
  e$rib_above_99 +   
  npr_annotation +
  npr_lines
  
  return(p)
}
