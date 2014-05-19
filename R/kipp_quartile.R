
#
# Args:
#   x: a vector of percentiles 
#   return.factor: boolean indicating wether to return quartiles as a factor, default is TRUE.
#   proper.quartile: booliean indicating wehter to fix Foundations crazy quaritle, defaul is FALSE

#
# Returns:
#   a vector of quartiles 

#' @title Calcualte KIPP Foundation style quartiles from percentile vector
#'
#' @description
#' \code{kipp_quartile} returns an integer or factor vector quartiles.  
#'
#' @details 
#' This function calculates the KIPP Foundation's (kinda fucked up) quartile (i.e., the foundation
#' breaks with stanard mathematical pracitce and puts the 50th percenile
#' in the  3rd rather than the 2nd quartile). It takes a vector of percentiles and translates 
#' those into quartiles, where the 25th, 50th, and 75th percentils are shifted up 
#' into the 2nd, 3rd, and 4th quartiles, respectively. You can revert to a traditional 
#' quartile calculation by setting the \code{proper.quartile} argument to \code{TRUE}.
#' 
#' @param x vector of percentiles to be converted to quartiels
#' @param return.factor  default is \code{TRUE}.  If set to \code{FALSE} returns integers rather than factors. 
#' @param proper.quartile defaul is \code{FALSE}.  If set to \code{TRUE} returns traditional quartiles rather then KIPP Foundation quartiles. 
#' 
#' @return a vector of \code{length(x)}.
#' @export
#' @examples 
#' x <- sample(x=1:99, 100,replace = T)
#' kipp_quartile(x)
#' kipp_quartile(x, proper.quartile=TRUE)
#' kipp_quartile(x, proper.quartile=TRUE, return.factor=FALSE)

kipp_quartile<- function(x, 
                         return.factor=TRUE, 
                         proper.quartile=FALSE){
  
  
  #defactor factors
  if(is.factor(x)) x<-as.numeric(as.character(x))
  
  # Error handling 
  stopifnot(x>0 | is.na(x), x<100 | is.na(x))
  
  # if proper.quartile is false adjust x's to return Foundation quartile 
  if(!proper.quartile) x<-x+1
  #calculate quartile
  y<-ceiling(x/25)
  
  #transform to factor
  if(return.factor) y<-factor(y, levels=c(1:4))
  
  #return
  y
}