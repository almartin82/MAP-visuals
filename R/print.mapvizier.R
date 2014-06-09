#' @title print method for \code{mapvizier} class
#'
#' @description
#'  prints to console
#'
#' @details Prints a summary fo the a \code{mapvizier} object. 

#' @param x a \code{mapvizier} object

#' @return some details about the object to the console.
#' @rdname print
#' @export
#' @examples 
#' data(nweamap)
#' x<-mapvizier(nweamap)
#' x

print.mapvizier <-  function(x, ...) {
  require(dplyr)
  
  #gather some summary stats
  n_sy <- length(unique(x$mapData$SY))
  min_sy <- min(x$mapData$SY)
  max_sy <- max(x$mapData$SY)
  n_students <- length(unique(x$mapData$StudentID))
  n_schools <- length(unique(x$mapData$SchoolName))
  growthseasons <- unique(x$seasonMatched$GrowthSeason)
  n_growthseasons <- length(growthseasons)
  
  cat("A mapvizier object repesenting:\n- ")
  cat(paste(n_sy))
  cat(" school years from SY")
  cat(paste(min_sy))
  cat(" to SY")
  cat(paste(max_sy))
  cat(";\n- ")
  cat(paste(n_students))
  cat(" students from ")
  cat(paste(n_schools))
  cat(" schools;\n- and, ")
  cat(paste(n_growthseasons))
  cat(" growth seasons:\n    ")
  cat(paste(growthseasons, collapse = ",\n    "))
    
}
