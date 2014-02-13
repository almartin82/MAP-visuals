\name{norm_sim}
\alias{norm_sim}
\title{Simulate Growth paths based on norms tables}
\usage{
norm_sim(start.grade = 5, end.grade = 8, start.subject = "Mathematics",
  start.rit = 190, yearly.cycle = c(41, 12), n.samples = 5000)
}
\arguments{
  \item{start.grade}{start grade as integer}

  \item{end.grade}{end grade as integer}

  \item{start.subject}{measurement scale}

  \item{start.rit}{starting RIT value as integer for
  simulation}

  \item{yearly.cycle}{season intervals used to identify
  typical growth and standard deviations. These are passed
  as a vector of two digit intergers in the style of the
  2011 Student Growth Norms Table where each season
  recieves an integer value corresponding to Winter = 1,
  Spring = 2, Fall = 4.  Integer pairs indicate the
  appropriate interval to simulate growth.  For example, to
  simulate fall-to-winter followed by winter-to-spring
  growth for each grade level the vector \code{c(41,12)} is
  passed to this paramter (this is the default behavior of
  the function).  Here 41 indicates fall-to-winter growth
  and 12 indicates winter-to-spring.}

  \item{n.samples}{the number of samples}
}
\value{
a data frame with \code{n.sample + (n.sample x grades x
length(yearly.cycle)} rows)}.
}
\description{
\code{norm_sim} simulates \code{n.samples} student growth
paths using the mean and standard deviation estimates
provided in NWEA's 2011 Student Growth Norms data tables.
}
\details{
This function builds a growth path over specified
intervales (e.g., spring-to-spring, fall-to-winter followed
by winter-spring) as well as grades (eg. 5-8, K-11) for a
given start grade, measurement scale (i.e., subject) and
starting RIT value.  All simulations start wiht the same
RIT value and then iterativelydraw the next assessments
realization from \code{rnorm} using the means and standard
deviations for appropriate intervals as provided by the
\code{norms_student_2011}. The function iterates over
grades and intervals within grades and construct a
data.table in long format.
}
\examples{
# Run simulations with defaul settings
x.t <- norm_sim()

# Create grade-season variable for summary stats and plotting
x.t[,GradeSeason:=ifelse(Season==41, StartGrade-.7, StartGrade)]

# Calculate grade-season means
x.avg<-x.t[,list(Avg=mean(StartRIT), sigma=sd(StartRIT)), by=list(GradeSeason)]

# Calculate grade-season upper and lower confidence levels
x.avg[,ucl:=Avg+1.96*sigma]
x.avg[,lcl:=Avg-1.96*sigma]

# Plot!
p <- ggplot(x.t, aes(x=GradeSeason, y=StartRIT))  + geom_line(aes(group=ID), alpha=.01) + theme_bw()
p + geom_smooth(data=x.avg, aes(x=GradeSeason, y=Avg, ymin=lcl, ymax=ucl), color="orange", fill="orange", size=1.7)
}

