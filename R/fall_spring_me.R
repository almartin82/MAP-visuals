#' @title Fall-Spring Me
#'
#' @description
#' \code{fall_spring_me} tranforms grade levels into labels for charts; eg 4.2 -> F5
#'
#' @param grade_level a grade level between -1 and 12
#' 
#' @return a string
#' @export

fall_spring_me <- function(grade_level) {
  #K is weird edge case
  if(grade_level == -0.8) {
    return('KF')
  } else if(grade_level == -0.5) {
    return('KW')
  } else if(grade_level == 0) {
    return('KS')
  #S observations are decimal 0s
  } else if(grade_level %% 1 == 0) {
    return(paste(round(grade_level, 0), 'S', sep = ''))
  #test for F and W
  } else if(round(grade_level %% 1,1) == 0.2) {
    return(paste(ceiling(grade_level), 'F', sep = ''))
  } else if(round(grade_level %% 1,2) == 0.5) {
    return(paste(ceiling(grade_level), 'W', sep = ''))
  } else {
    return(NA)
  }
}
