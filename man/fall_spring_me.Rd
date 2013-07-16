\name{fall_spring_me}
\alias{fall_spring_me}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Converts decimal representations of grade levels (eg 4.3) to F/S style (eg 5F)}
\description{
  Converts decimal representations of grade levels (eg 4.3) to F/S style (eg 5F)
}
\usage{
fall_spring_me(grade_level)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grade_level}{
A decimal representation of a grade level.  fall_spring_me understands spring (.0), fall (0.3), and winter (0.65).  Grade level 0 is interpreted as K.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (grade_level) 
{
    if (grade_level == -0.7) {
        return("KF")
    }
    else if (grade_level == -0.35) {
        return("KW")
    }
    else if (grade_level == 0) {
        return("KS")
    }
    else if (grade_level\%\%1 == 0) {
        return(paste(round(grade_level, 0), "S", sep = ""))
    }
    else if (round(grade_level\%\%1, 1) == 0.3) {
        return(paste(round(grade_level, 0) + 1, "F", sep = ""))
    }
    else if (round(grade_level\%\%1, 2) == 0.65) {
        return(paste(round(grade_level, 0) + 1, "W", sep = ""))
    }
    else {
        return(NA)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
