\name{sgp_small_multiples_cleanup}
\alias{sgp_small_multiples_cleanup}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Makes some direct edits onto the legend of a sgp_small_multiples plot
}
\description{
Gets into the murky depths of grid graphics, and formats the legend box to show numbers (not text)
}
\usage{
sgp_small_multiples_cleanup(plot)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{plot}{
A sgp_small_multiples plot
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
function (plot) 
{
    g = ggplotGrob(plot)
    grob_no <- grep("guide-box", g$layout$name)
    target_grobs <- c()
    for (z in c(1:length(g$grobs[[grob_no]][[1]][[1]]))) {
        test <- grep("GRID.text", g$grobs[[grob_no]][[1]][[1]][[1]][[z]]$name)[1] == 
            1
        if (!is.na(test)) {
            target_grobs <- c(target_grobs, z)
        }
    }
    for (q in target_grobs) {
        g$grobs[[grob_no]][[1]][[1]][[1]][[q]]$label <- "10"
        g$grobs[[grob_no]][[1]][[1]][[1]][[q]]$gp$fontsize <- 36
    }
    return(g)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
