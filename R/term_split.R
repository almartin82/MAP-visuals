#' @title term_split
#'
#' @description
#' \code{term_splot} takes a MAP CDF and returns munges the termname field to
#' return season and year seperately.
#'
#' @param regular_cdf a NWEA MAP cdf loaded in as a data frame
#' 
#' @export

term_split <- function(regular_cdf) {
  require(stringr)
  
  regular_cdf$fallwinterspring <- str_sub(
    regular_cdf$TermName, 1, str_locate(regular_cdf$TermName, fixed(' '))[,1]-1)

  years <- str_sub(regular_cdf$TermName, str_locate(regular_cdf$TermName, fixed(' '))[,1]+1)

  regular_cdf$map_year_academic <- str_sub(years, 1, str_locate(years, fixed('-'))[,1]-1)

  return(regular_cdf)
}
