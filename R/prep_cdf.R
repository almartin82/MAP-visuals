#' @title Prep CDF 
#'
#' @description
#' \code{prep_cdf} is a utility function that preps a standard NWEA MAP CDF for use with MAPVizier
#'
#' @param regular_cdf a NWEA MAP cdf loaded in as a data frame
#' 
#' @export

prep_cdf <- function(regular_cdf) {
 
  #split termnames to get fallwinterspring by itself
  prepped_cdf <- term_split(regular_cdf)
  return(prepped_cdf)
  
}
