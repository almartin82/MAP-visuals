#' @title term_fallwinterspring
#'
#' @description
#' \code{term_fallwinterspring} takes a MAP CDF and returns munges the termname field to
#' return season and year seperately.
#'
#' @param term_names a vector of term names
#' 
#' @export

term_fallwinterspring <- function(term_names) {
  require(stringr)
  
  fallwinterspring <- str_sub(
    term_names, 1, str_locate(term_names, fixed(' '))[,1]-1)

  return(fallwinterspring)
}
