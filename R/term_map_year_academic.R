#' @title term_map_year_academic
#'
#' @description
#' \code{term_map_year_academic} takes a MAP CDF and returns munges the termname field to
#' return season and year seperately.
#'
#' @param term_names a vector of term names
#' 
#' @export

term_map_year_academic <- function(term_names) {
  require(stringr)
  
  years <- str_sub(regular_cdf$TermName, str_locate(term_names, fixed(' '))[,1]+1)

  map_year_academic <- str_sub(years, 1, str_locate(years, fixed('-'))[,1]-1)

  return(map_year_academic)
}
