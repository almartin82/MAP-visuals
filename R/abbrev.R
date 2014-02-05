#' @title Create vector of Initials
#'
#' @description
#' \code{abbrev} returns a character vector of each words first capital letters for each element of \code{x}.
#'
#' @details 
#' This function returns a same-length character vector
#' that abbrevs an initial character vector, \code{x}.  Abbreviation returns the first
#' capital letter of any words in each element of \code{x}.  Users may additionally pass
#' \code{abbrev} and option list of exceptions that overides the default abbreviations.
#' The list of exceptions requires a vector of "old" values to be replaced by "new" values
#' 
#' @param x vector of strings to be abbreviated.
#' @param exceptions list with two names vectors:  \code{old}, a vector abbreviations to 
#' be replaced and  \code{new}, a vector of replacment values.
#' 
#' @return a character vector of \code{length(x)}.
#' @export
#' @examples 
#' x<-c("KIPP Ascend Middle School", "KIPP Ascend Primary School", 
#'      "KIPP Create College Prep", 
#'      "KIPP Bloom College Prep" ,
#'      "KIPP One Academy")
#' 
#' abbrev(x)
#' 
#' altnames<-list(old=c("KAPS", "KBCP", "KOA"), 
#'                  new=c("KAP", "Bloom", "One"))
#' 
#' abbrev(x, excpetions=altnames)

abbrev<-function(x, exceptions=NULL){
  x.out<- gsub(pattern="(\\w)\\w*\\W*", 
               replacement="\\1",
               x=x)
  
  # pass list of exceptions to the abbrev function
  if(!is.null(exceptions)){
    x.changed<-with(exceptions, new[match(x.out, old)]) # create changes vector (non changes = NA)
    x.changed[is.na(x.changed)]<-x.out[is.na(x.changed)] # replace NAs with non-changed values
    x.out<-x.changed # replace original vector with changed vector               
  }
    
    
  x.out
}