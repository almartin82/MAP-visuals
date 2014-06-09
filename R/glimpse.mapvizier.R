#' @title glimpse method for \code{mapvizier} class
#'
#' @description
#'  prints to console glimpse of a the first lines of a \code{mapvizier} object's
#'  components.
#'
#' @details Essentialy extendls Hadley Wickham's \code{glimpse} function from 
#' the \code{dplyr} package to work with \code{mapvizier} objects.  \code{mapvizier}
#' objects are lists of two data frames (\code{mapData} and \code{seasonMatched}). This
#' method works over both data frames. 

#' @param x a \code{mapvizier} object

#' @return a list of glimpses to the console
#' @rdname glimpse
#' @export
#' @examples 
#' data(nweamap)
#' x<-mapvizier(nweamap)
#' glimpse(x)

# create generic
glimpse<-function(x) UseMethod("glimpse")

# assign dplry's glimpse to glimpse.default to preserve glimpse default 
# behavior
#' @export
glimpse.default <- function(tbl, width) dplyr::glimpse(tbl, width=getOption("width"))

# Now create method for mapvizier class
#' @export
glimpse.mapvizier <- function(tbl, width=getOption("width")){
  #require(dplry)
  cat("mapData:\n")
  print(dplyr::glimpse(tbl$mapData, width))
  cat("seasonMatched:\n")
  print(dplyr::glimpse(tbl$seasonMatched, width))
}