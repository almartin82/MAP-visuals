##' @title An internal utility function to capture dots (\code{...}) arguments.
#'
#' @description
#' \code{dots} capturees dots 
#'  

#' @param \code{...} arguments to be caputred
#' 
#' @return a list of captured dots
dots <- function(...) {
  eval(substitute(alist(...)))
}