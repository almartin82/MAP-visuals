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