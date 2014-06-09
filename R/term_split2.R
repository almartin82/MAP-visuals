#' @title Split vector like NWEA CDF TermName column into usefule components
#'
#' @description
#' \code{term_split2} returns a data frame splitting a TermName
#'
#' @details 
#' This function returns a data framre with \code{length(x)} rows that separates a TermName character
#' vector into the following components (here we use "Spring 2012-2013" as en example 
#' element of the vector:
#' \itemize{
#' \item  Season ("Spring"),
#' \item  Year1 (2012), 
#' \item  Year2 (2013), 
#' \item  SY (i.e School Year, "2013-2014").
#' }
#' In addition, if a \code{length(x)} vector of grade levels is passed to the optional second argument
#' then an additional column will be added to the data frame:
#' \itemize{
#' \item  CohortYear (HS graduation year based on grade and year2, so an 7th grader in spring 2013 would be 
#' in the class of 2018).
#' }
#' 
#' @param term_name character vector of term names from NWEA CDF. 
#' @param grade (optional, default = \code{NULL}) integer vector of grade levels
#' @return a data frame  of with \code{length(x)} rows and 4 (5 if grade vector supplied) columns
#' @export
#' @examples 
#' x<-c("Spring 2012-2013", 
#' "Fall 2013-2014", 
#'      "Winter 2012-2013", 
#'      "Fall 2012-2013" ,
#'      "Spring 2013-2014")
#' 
#' grades<-c(3:7)
#' term_split2(term_name=x,
#'             grade=grades )
#'             

term_split2 <- function(term_name, grade=NULL){
  
  #require(stringr)
  #require(assertthat)

  # validation
  
  # coerce term_name to character vector
  if(!is.character(term_name)) {
    warning("Coercing term_name to characer vector.")
    term_name <- as.character(term_name)
  }
  # extract Season
  Season<-str_extract(term_name, 
                      "[[:alpha:]]+"
  )
  # extract Year1
  Year1<-as.integer(str_extract(term_name, 
                                "[[:digit:]]+"
  )
  )
  
  # extract Year2
  Year2<-as.integer(gsub("([a-zA-Z]+[[:space:]][[:digit:]]+-)([[:digit:]]+)",
                         "\\2", 
                         term_name
  )
  )
  # construct School Year (e.g, "2012-2013")
  SY<-paste(Year1, Year2, sep="-")
  
  # calculate Cohort Year
  if(!is.null(grade)) {
    # validation
    assert_that(length(term_name)==length(grade))
    
  CohortYear<-(12-grade)+as.numeric(Year2)
    #return df
    x<-data.frame(Season, Year1, Year2, SY, CohortYear, stringsAsFactors = FALSE)
    x
  } else {
    x<-data.frame(Season, Year1, Year2, SY, stringsAsFactors = FALSE)
    x
  }
}