\name{one_year_sgp}
\alias{one_year_sgp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Histogram showing distribution of MAP student growth percentiles
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
one_year_sgp(df, title_text = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
MAP SGP data frame
}
  \item{title_text}{
A title (optional)
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
function (df, title_text = FALSE) 
{
    bins <- c(1, seq(10, 90, by = 10), 99)
    bins_7 <- seq(0, 105, by = 7.5)
    simple_hist <- hist(df$GROWTH_PERCENTILE, breaks = bins_7, 
        plot = FALSE)
    chart_max <<- max(simple_hist$counts) + 5
    med_sgp <<- median(df$GROWTH_PERCENTILE, na.rm = TRUE)
    p <- ggplot(data = df, aes(x = GROWTH_PERCENTILE)) + geom_text(data = NULL, 
        aes(x = 50, y = 0.5 * chart_max, label = med_sgp, alpha = 0.7), 
        size = 26, color = if (med_sgp > 55) {
            "lightgreen"
        }
        else if (med_sgp >= 45) {
            "orange"
        }
        else if (med_sgp < 45) {
            "firebrick1"
        }) + geom_histogram(, binwidth = 7.5, alpha = 0.85, color = "#0067AC", 
        fill = "#0067AC") + labs(x = "Student Growth Percentile", 
        y = "Number of Students") + theme(panel.background = element_blank(), 
        plot.background = element_blank(), panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), legend.position = "none") + 
        scale_x_continuous(breaks = bins)
    if (title_text != FALSE) {
        p <- p + labs(title = title_text)
    }
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
