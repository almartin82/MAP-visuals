\name{ordinal_me}
\alias{ordinal_me}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Returns the appropriate suffix ('rd', 'nd', etc) for an integer}
\description{Returns the appropriate suffix ('rd', 'nd', etc) for an integer}
\usage{
ordinal_me(number)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{number}{
  Takes an integer.
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
function (number) 
{
    if (is.na(number)) 
        return(NA)
    else if (number == 1) 
        return("st")
    else if (number == 2) 
        return("nd")
    else if (number == 3) 
        return("rd")
    else return("th")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
