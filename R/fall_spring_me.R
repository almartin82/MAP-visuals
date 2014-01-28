#tranforms grade levels into labels for charts; eg 4.3 -> F5
fall_spring_me <- function(grade_level) {
  #K is weird edge case
  if(grade_level == -0.7) {
    return('KF')
  } else if(grade_level == -0.35) {
    return('KW')
  } else if(grade_level == 0) {
    return('KS')
  #S observations are decimal 0s
  } else if(grade_level %% 1 == 0) {
    return(paste(round(grade_level, 0), 'S', sep = ''))
  #test for F and W; note that rounding is required or this misbehaves (?)
  } else if(round(grade_level %% 1,1) == 0.3) {
    return(paste(round(grade_level, 0) + 1, 'F', sep = ''))
  } else if(round(grade_level %% 1,2) == 0.65) {
    return(paste(round(grade_level, 0), 'W', sep = ''))
  } else {
    return(NA)
  }
}
