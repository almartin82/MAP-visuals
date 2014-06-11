#' @title Fall-Spring Sort Me
#'
#' @description
#' \code{fall_spring_me} tranforms grade levels into labels that will sort properly; 
#' eg, fall / winter/ spring
#'
#' @param grade_level a grade level between -1 and 12
#' 
#' @return a string
#' @export

fall_spring_sort_me <- function(grade_level) {
  #K is weird edge case
  if(grade_level == -0.8) {
    return('K_1')
  } else if(grade_level == -0.5) {
    return('K_2')
  } else if(grade_level == 0) {
    return('K_3')
  #S observations are decimal 0s
  } else if(grade_level %% 1 == 0) {
    return(paste0(round(grade_level, 0), '_3'))
  #test for F and W; note that rounding is required or this misbehaves (?)
  } else if(round(grade_level %% 1,1) == 0.2) {
    return(paste0(ceiling(grade_level), '_1'))
  } else if(round(grade_level %% 1,2) == 0.5) {
    return(paste0(ceiling(grade_level), '_2'))
  } else {
    return(NA)
  }  
}
