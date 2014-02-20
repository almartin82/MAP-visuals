#' @title Calculate quartiles stats for \code{haid_plot}
#'
#' @description
#' \code{get_group_stats} calculates counts and percentages used in 
#' \code{haid_plot}
#' 
#' @param df a data frames with individual RIT scores and grouping variable
#' @param grp the variable to group by
#' @param RIT the column in the data frame with RIT scores
#' @param dummy_y column used to establish data placement on \code{haid_plot}
#' 
#' @return A data frame with aggregate values grouped by grp for count 
#' of students, y-axis placement, group average RIT, percent of students
#' in group (relative to total students), and the total count of students. 

get_group_stats <- function(
  df
  ,grp
  ,RIT
  ,dummy_y
) {
  require(plyr)
  #total rows
  dftotal <- nrow(df)
  
  ddply(
    df
    ,grp
    ,function(x) data.frame(
      count_students = length(x[,RIT])
      ,avg_y_dummy = mean(x[,dummy_y])
      ,avg_rit = mean(x[,RIT])
      ,pct_of_total = length(x[,RIT]) / dftotal
      ,total_count = dftotal
    )        
  )
}