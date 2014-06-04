#' @title Prep CDF 
#'
#' @description
#' \code{prep_cdf} is a wrapper around \code{term_fallwinterspring} and \code {term_map_year_academic}
#' 
#' @param regular_cdf a NWEA MAP cdf loaded in as a data frame
#' 
#' @export

prep_cdf <- function(regular_cdf) {
 
  #split
  regular_cdf$fallwinterspring <- term_fallwinterspring(regular_cdf$termname)
  regular_cdf$map_year_academic <- term_map_year_academic(regular_cdf$termname)
  
  return(regular_cdf) 
}
