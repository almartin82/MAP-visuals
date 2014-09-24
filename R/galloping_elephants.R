#' @title RIT distribution change (affectionately titled 'Galloping Elephants')
#'
#' @description
#' \code{one_year_sgp} returns ggplot density distributions that show change in RIT over time
#'
#' @param df long data frame in TEAM canonical style
#' @param first_and_spring_only should fall/winter scores be shown, or only endpoint scores?
#' @param entry_grades the entry grades that are considered 'first' for a cohort (default is KF and 5F)
#' @param title_text desired plot title
#' 
#' @return returns a ggplot object
#' @export

galloping_elephants <- function (
  df
 ,first_and_spring_only = TRUE
 ,entry_grades = c(-0.8, 4.2)
 ,title_text = FALSE
 ,ho_cutoff = 13
) {

#data transformations
stage_1 <- df[,c(
 'sch_abbrev'
,'map_year_academic'
,'grade_level_season'
,'measurementscale'
,'testritscore'
,'percentile_2011_norms'
,'cur_grade_level')]

#kill holdover data
stage_1 <- stage_1[stage_1$grade_level_season < ho_cutoff, ]

#all terms or first & spring only?
if (first_and_spring_only) {
  #possible entry grades controlled by entry_grades parameter
  #default is Fall K, Fall 5 (aka -0.7, 4.3) - only change if you need to 
  #add an additional entry grade (perhaps 9th?) or to take away 5th
  #(eg for a fully grown KIPP school?)
  stage_1 <- stage_1[stage_1$grade_level_season %in% entry_grades |
                     stage_1$grade_level_season %% 1 == 0 |
                     #winter this year
                     stage_1$grade_level_season == (stage_1$cur_grade_level - 0.5),]
}
 
print(nrow(stage_1))
stage_1$grade_season_label <- unlist(lapply(stage_1$grade_level_season, fall_spring_me))
stage_1$label_sorter <- unlist(lapply(stage_1$grade_level_season, fall_spring_sort_me))


term_counts <- ddply(stage_1,'grade_season_label'
 ,function(x) data.frame(
    count = length(x[,'grade_season_label'])
  )        
)

target_terms <- term_counts[term_counts$count >= 3, 1]
  
stage_1 <- stage_1[stage_1$grade_season_label %in% target_terms, ]

stage_1 <- stage_1[with(stage_1, order(grade_level_season)), ]
stage_1$grade_season_label <- factor(stage_1$grade_season_label, ordered = TRUE)

stage_1 <- stage_1[with(stage_1, order(label_sorter)), ]
stage_1$label_sorter <- factor(stage_1$label_sorter, ordered = TRUE)

 
#grab appropriate norms data
subj <- as.character(unique(stage_1$measurementscale))

          
#inconsistent MAP naming conentions
if (subj == 'Science - General Science') {
  subj <- 'General Science'
}

norms_slim <- norms[norms$MEASUREMENTSCALE == subj
        & norms$RIT >= min(stage_1$testritscore)
        & norms$RIT <= max(stage_1$testritscore)
        #norms after 8th grade are stupid
        & norms$GRADE_LEVEL <= 8, ]

norms_slim$chart_label <- paste("nat'l Gr.", norms_slim$GRADE_LEVEL, 'mean')

#dummy, to get height of density plot
dummy <- ggplot(
  data = stage_1
 ,aes(
    x = testritscore
   ,group = grade_season_label
  )
) + 
  geom_density(adjust = 0.8)

points <- ggplot_build(dummy)
  
#head(points$data[[1]])    
#just get the data
density_raw <- points$data[[1]]
#extract the max per group
max_points <- ddply(density_raw, "group", summarise, y = max(y, na.rm=T))
  
#THE ACTUAL PLOT
p <- ggplot(
  data = stage_1
 ,aes(
    x = testritscore
   ,group = label_sorter
   ,fill = label_sorter
   ,alpha = grade_level_season
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
) +
scale_x_continuous(
  limits=c(floor(stage_1$testritscore), ceiling(stage_1$testritscore))
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

plot_min <- round_any(
  x = min(stage_1$testritscore, na.rm=T)
 ,accuracy = 10
 ,f = floor
)  
plot_max <- round_any(
  x = max(stage_1$testritscore, na.rm=T)
 ,accuracy = 10
 ,f = ceiling
)

#limits and breaks
p <- p + scale_x_continuous(
  limits=c(plot_min, plot_max)
 ,breaks=seq(plot_min, plot_max, 10)
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
