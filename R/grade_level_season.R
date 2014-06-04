#' @title Grade Level Season
#'
#' @description
#' \code{grade_level_season} takes a map_season and returns a decimal representation of 
#' what approximate portion of the year it represents
#'
#' @param fws_string Fall, Winter or Spring
#' 
#' @export

grade_level_season <- function(fws_string) {
  
  if (fws_string == 'Spring') {
    return(0)
  } else if (fws_string == 'Winter') {
    return(-0.5)
  } else if (fws_string == 'Fall') {
    return(-0.8)
  } 
}
