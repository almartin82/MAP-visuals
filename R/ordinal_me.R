#' @title Ordinal me
#'
#' @description
#' \code{ordinal_me} appends the appropriate ending ('st', 'nd' etc) to make an integer into an ordinal.
#'
#' @param number a number
#' 
#' @return returns a string
#' @export

ordinal_me <- function(number) {
  if (is.na(number))
    return(NA)
   else if (number == 1) 
    return('st')
  else if (number == 2)
    return('nd')
  else if (number == 3)
    return('rd')
  else
    return('th')
}