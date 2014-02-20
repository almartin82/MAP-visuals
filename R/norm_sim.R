#' @title Simulate Growth paths based on norms tables
#'
#' @description
#' \code{norm_sim} simulates \code{n.samples} student growth paths using the mean and 
#' standard deviation estimates provided in NWEA's 2011 Student Growth Norms data
#' tables. 
#'
#' @details 
#' This function builds a growth path over specified intervales (e.g., 
#' spring-to-spring, fall-to-winter followed by winter-spring) as well as
#' grades (eg. 5-8, K-11) for a given start grade, measurement scale (i.e., subject)
#' and starting RIT value.  All simulations start wiht the same RIT value
#' and then iterativelydraw the next assessments realization from \code{rnorm}
#' using the means and standard deviations for appropriate intervals as
#' provided by the \code{norms_student_2011}. The function iterates over
#' grades and intervals within grades and construct a data.table in 
#' long format.
#' 
#' 
#' @param start.grade start grade as integer
#' @param end.grade end grade as integer
#' @param start.subject measurement scale 
#' @param start.rit starting RIT value as integer for simulation
#' @param yearly.cycle season intervals used to identify typical growth and standard deviations. These are passed as 
#' a vector of two digit intergers in the style of the 2011 Student Growth Norms Table where each season
#' recieves an integer value corresponding to Winter = 1, Spring = 2, Fall = 4.  Integer pairs indicate 
#' the appropriate interval to simulate growth.  For example, to simulate fall-to-winter followed
#' by winter-to-spring growth for each grade level the vector \code{c(41,12)} is passed to 
#' this paramter (this is the default behavior of the function).  Here 41 indicates fall-to-winter growth 
#' and 12 indicates winter-to-spring.  
#' @param n.samples the number of samples
#' 
#' @return a data frame with \code{n.sample + (n.sample x grades x 
#' length(yearly.cycle)} rows).
#' 
#' @export
#' 
#' @examples 
#' # Run simulations with defaul settings
#' x.t <- norm_sim()
#' 
#' # Create grade-season variable for summary stats and plotting
#' x.t[,GradeSeason:=ifelse(Season==41, StartGrade-.7, StartGrade)]
#' 
#' # Calculate grade-season means
#' x.avg<-x.t[,list(Avg=mean(StartRIT), sigma=sd(StartRIT)), by=list(GradeSeason)]
#' 
#' # Calculate grade-season upper and lower confidence levels
#' x.avg[,ucl:=Avg+1.96*sigma]
#' x.avg[,lcl:=Avg-1.96*sigma]
#' 
#' # Plot!
#' p <- ggplot(x.t, aes(x=GradeSeason, y=StartRIT))  + geom_line(aes(group=ID), alpha=.01) + theme_bw()
#' p + geom_smooth(data=x.avg, aes(x=GradeSeason, y=Avg, ymin=lcl, ymax=ucl), color="orange", fill="orange", size=1.7) 

norm_sim <- function(start.grade = 5,
                     end.grade = 8,
                     start.subject = "Mathematics",
                     start.rit = 190,
                     yearly.cycle = c(41,12), # 4 -> 1 -> 2 -> 4 = Fall -> Winter >- Spring -> Fall
                     n.samples = 5000 # number of samples
) {
  
  
  # first function looks up norm, pulls expectation for proper seasaon 
  if(!exists("norms_students_2011")) data(norms_students_2011)
  norms <- data.table(norms_students_2011)
  
  x.t<-data.table(ID=c(1:n.samples),StartGrade=rep(start.grade, n.samples), 
                  MeasurementScale = rep(start.subject, n.samples),
                  StartRIT =rep(start.rit, n.samples), 
                  Season = yearly.cycle[1])
  
  setkey(x.t, MeasurementScale, StartGrade, StartRIT)
  
  setkey(norms,MeasurementScale, StartGrade, StartRIT)
  
  f <- function(.m,.s){
    .m<-as.name(.m)
    .s<-as.name(.s)
    q<-substitute(list(MeasurementScale, StartGrade, StartRIT, Mean=.m, SD=.s))
    x.var[norms[,eval(q)], nomatch=0][,round(StartRIT+rnorm(Mean, Mean, SD))]
  }
  
  for(g in c(start.grade:end.grade)) {
    s.counter<-1
    for(s in yearly.cycle){
      # subset x.t to the current grade season
      x.var<-copy(x.t[StartGrade==g & Season==s])
      # this only set sets a new grade if g has incremented to the next grade
      #construct columns names to select for correct mean and SD from norms table
      .mean<-paste0("T",s)
      .sd<-paste0("S",s)
      
      # simulate new RIT score
      x.var[,StartRIT:=f(.mean, .sd)]
      
      # increment season
      if (s.counter!=length(yearly.cycle)){ # if not in years last seasone
        s.counter<-s.counter + 1 #set Season to next interval
        x.var[,Season:=yearly.cycle[s.counter]]
      } else { # in last season
        x.var[,Season:=yearly.cycle[1]] #set season to first interval
        x.var[,StartGrade:=g+1] # increment grade to next grade level
      }
      x.t<-rbindlist(list(x.t, x.var))
      # reset key
      setkey(x.t, MeasurementScale, StartGrade, StartRIT)
    }
  }
  x.t
}
