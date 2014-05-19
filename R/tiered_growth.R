#' @title Calculate KIPP Tiered Growth factors 
#'
#' @description
#' \code{tiered growth} takes grade level and quartile data and returns a vector of KIPP 
#' Tiered Growth factors (or multipliers, if you prefer).
#' @details 
#'  # Function takes two vectors---one containing student grade levels and the other 
#'  containing student pre-test/season 1 quartiles---and returns a same-length vector of 
#'  KIPP Tired Growth factors.  These factors are multiplied by a students typical 
#'  (i.e., expected) growth to generate college ready growth. 
 
#' 
#' @param quartile a vector of student quartiles 
#' @param grade vector of student grade-levels
#' 
#' @return a vector of \code{length(quartile)} of KIPP Tiered Growth factors.
#' @export
#' @examples 
#' tiered_growth()

tiered_growth<- function(quartile, grade){
 
  #
  # Args:
  #   quartile:  vector student quartiles
  #   grade:     vector of grade levels, mus be same length as quartile
  #
  # Returns:
  #   a vector of tiered growth facts of lenght nrow(quartile)
  
  require(dplyr)
  #Error handling 
  stopifnot(length(quartile)==length(grade))
  
  # Create data.frame lookup of KIPP Foundation Growth Targts
  tgrowth<-data.frame(grade.type=c(rep(0,4),rep(1,4)), 
                      quartile = as.factor(rep(1:4, 2)), 
                      KIPPTieredGrowth=c(1.5,1.5,1.25,1.25,2,1.75,1.5,1.25)
  )
  
  #
  grade.type<-rep(NA,times=length(quartile))
  
  # Create Grade Type column
  grade.type<-ifelse(grade<=3, 0,1)
  
  df<-data.frame(grade, grade.type, quartile=as.factor(quartile))
  
  df2<-left_join(df, tgrowth, by=c("quartile", "grade.type"))
  
  #return
  df2$KIPPTieredGrowth
  
}