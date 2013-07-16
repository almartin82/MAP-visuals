\name{strand_boxes}
\alias{strand_boxes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Makes a boxplot of the MAP goal scores
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
strand_boxes(df)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
  MAP CDF, TEAM-style
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
function (df) 
{
    stage_1a <- df[, c("PS_STUDENTID", "COHORT", "MEASUREMENTSCALE", 
        "SCH_ABBREV", "MAP_YEAR_ACADEMIC", "FALLWINTERSPRING", 
        "TESTRITSCORE", "GOAL1RITSCORE", "GOAL2RITSCORE", "GOAL3RITSCORE", 
        "GOAL4RITSCORE", "GOAL5RITSCORE", "GOAL6RITSCORE", "GOAL7RITSCORE", 
        "GOAL8RITSCORE")]
    stage_1b <- df[, c("PS_STUDENTID", "COHORT", "MEASUREMENTSCALE", 
        "SCH_ABBREV", "MAP_YEAR_ACADEMIC", "FALLWINTERSPRING", 
        "TESTRITSCORE", "GOAL1NAME", "GOAL2NAME", "GOAL3NAME", 
        "GOAL4NAME", "GOAL5NAME", "GOAL6NAME", "GOAL7NAME", "GOAL8NAME")]
    stage_2a <- melt(data = stage_1a, id.vars = names(stage_1)[1:7], 
        value.name = names(stage_1)[8:15])
    stage_2b <- melt(data = stage_1b, id.vars = names(stage_1)[1:7], 
        value.name = names(stage_1)[8:15])
    head(stage_2a)
    head(stage_2b)
    nrow(stage_2a)
    nrow(stage_2b)
    stage_3 <- stage_2a
    stage_3$GOAL_NAME <- stage_2b$value
    head(stage_3)
    stage_4 <- stage_3[!is.na(stage_3$GOAL_NAME), ]
    p <- ggplot(data = stage_4, aes(x = factor(GOAL_NAME), y = value, 
        fill = factor(GOAL_NAME))) + geom_boxplot() + coord_flip() + 
        geom_jitter(position = position_jitter(width = 0.15), 
            color = "gray60", alpha = 0.8) + stat_summary(aes(label = round(..y.., 
        1)), fun.y = mean, geom = "text", size = 8)
    p
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
