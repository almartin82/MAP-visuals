\name{sgp_small_multiples}
\alias{sgp_small_multiples}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Small multiples plot of student growth percentiles
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sgp_small_multiples(df, stu_per_row = 12)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A SGP style data frame of MAP data.
}
  \item{stu_per_row}{
How many students to show per row?  Default is 12.
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
function (df, stu_per_row = 12) 
{
    df$STUDENT_NAME <- factor(df$STUDENT_NAME, levels = as.character(unique(df[order(-df$GROWTH_PERCENTILE), 
        ]$STUDENT_NAME)))
    df$GRADE_START_TRANSFORM <- 0
    mask <- df$START_TERM_STRING == "Winter"
    df[mask]$GRADE_START_TRANSFORM <- 0.65
    mask <- df$START_TERM_STRING == "Fall"
    df[mask]$GRADE_START_TRANSFORM <- 0.3
    df$GROWTH_PERCENTILE <- round_any(df$GROWTH_PERCENTILE, 0.1, 
        f = floor)
    growth_min <<- round_any(min(df$RIT_CHANGE, na.rm = TRUE), 
        5, f = floor)
    growth_max <<- round_any(max(df$RIT_CHANGE, na.rm = TRUE), 
        5, f = ceiling)
    x_transf_min <<- min(df$GRADE_START_TRANSFORM)
    df$SGP_BIN <- ""
    df[GROWTH_PERCENTILE < 34, ]$SGP_BIN <- "Low Growth"
    df[GROWTH_PERCENTILE < 67 & GROWTH_PERCENTILE >= 34, ]$SGP_BIN <- "Average Growth"
    df[GROWTH_PERCENTILE >= 67, ]$SGP_BIN <- "High Growth"
    p <- ggplot(data = df[!is.na(GROWTH_PERCENTILE), ], aes(x = GRADE_START_TRANSFORM, 
        y = 0, xend = 1, yend = RIT_CHANGE, label = GROWTH_PERCENTILE, 
        group = STUDENT_NAME)) + geom_text(aes(x = GRADE_START_TRANSFORM, 
        y = 0, label = START_TERM_VERIF), size = rel(2.75), alpha = 0.3, 
        vjust = 1, hjust = 0) + geom_segment(size = rel(1), alpha = 0.6) + 
        geom_text(aes(x = 1, y = RIT_CHANGE, label = END_TERM_VERIF), 
            size = rel(2.75), alpha = 0.3, vjust = 1, hjust = 1) + 
        geom_text(aes(x = x_transf_min + 0.5 * (1 - x_transf_min), 
            y = growth_min + (0.6 * (growth_max - growth_min)), 
            color = SGP_BIN), size = rel(15), alpha = 0.5) + 
        geom_text(aes(x = GRADE_START_TRANSFORM, y = 0, label = START_RIT), 
            size = rel(4), vjust = 0, hjust = 0) + geom_text(aes(x = 1, 
        y = RIT_CHANGE, label = END_RIT), size = rel(4), vjust = 0, 
        hjust = 1) + ylim(growth_min, growth_max) + scale_x_continuous(c(-0.05, 
        1.05)) + scale_fill_discrete() + labs(color = "Student Growth Percentile") + 
        facet_wrap(~STUDENT_NAME) + theme(axis.title.y = element_blank(), 
        strip.text.x = element_text(size = rel(0.9)), panel.background = element_blank(), 
        plot.background = element_blank(), panel.background = element_blank())
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
