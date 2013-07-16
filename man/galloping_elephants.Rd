\name{galloping_elephants}
\alias{galloping_elephants}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Longitudinal change of RIT score distributions, by testing term.  
}
\description{
Density curves of RIT scores, segmented by testing term. They kinda look like stop-motion elephants.
}
\usage{
galloping_elephants(df, first_and_spring_only = TRUE, entry_grades = c(-0.7, 4.3), title_text = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A MAP CDF TEAM-Style.
}
  \item{first_and_spring_only}{
Defaults to TRUE; should we drop intermediate (Fall, Winter?) terms that aren't baseline or EOY?
}
  \item{entry_grades}{
Interprets what grades count as 'first'.  Defaults to K Fall and 5 Fall.
}
  \item{title_text}{
Chart title, if desired.
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
function (df, first_and_spring_only = TRUE, entry_grades = c(-0.7, 
    4.3), title_text = FALSE) 
{
    stage_1 <- df[, c("SCH_ABBREV", "MAP_YEAR_ACADEMIC", "GRADE_LEVEL_SEASON", 
        "MEASUREMENTSCALE", "TESTRITSCORE", "PERCENTILE_2011_NORMS")]
    if (first_and_spring_only) {
        stage_1 <- stage_1[stage_1$GRADE_LEVEL_SEASON \%in\% entry_grades | 
            stage_1$GRADE_LEVEL_SEASON\%\%1 == 0, ]
    }
    stage_1$grade_season_label <- unlist(lapply(stage_1$GRADE_LEVEL_SEASON, 
        fall_spring_me))
    stage_1 <- stage_1[with(stage_1, order(GRADE_LEVEL_SEASON)), 
        ]
    stage_1$grade_season_label <- factor(stage_1$grade_season_label, 
        ordered = TRUE)
    subj <- as.character(unique(stage_1$MEASUREMENTSCALE))
    norms_slim <- norms[norms$MEASUREMENTSCALE == subj & norms$RIT >= 
        min(stage_1$TESTRITSCORE) & norms$RIT <= max(stage_1$TESTRITSCORE) & 
        norms$GRADE_LEVEL <= 8, ]
    norms_slim$chart_label <- paste("nat'l Gr.", norms_slim$GRADE_LEVEL, 
        "mean")
    dummy <- ggplot(data = stage_1, aes(x = TESTRITSCORE, group = grade_season_label)) + 
        geom_density(adjust = 0.8)
    points <- ggplot_build(dummy)
    density_raw <- points$data[[1]]
    max_points <- ddply(density_raw, "group", summarise, y = max(y))
    p <- ggplot(data = stage_1, aes(x = TESTRITSCORE, group = grade_season_label, 
        fill = grade_season_label, alpha = GRADE_LEVEL_SEASON)) + 
        geom_point(aes(y = 0), alpha = 0)
    p <- p + annotate(geom = "segment", x = norms_slim$RIT, xend = norms_slim$RIT, 
        y = 0, yend = 1, color = "gray20", alpha = 0.3)
    p <- p + annotate(geom = "text", x = norms_slim$RIT, y = 0.8 * 
        max(max_points$y), label = norms_slim$chart_label, color = "gray20", 
        alpha = 0.3, size = 3, vjust = 1, angle = 90)
    p <- p + geom_density(adjust = 0.8) + scale_alpha(range = c(0.5, 
        0.85)) + theme(panel.background = element_blank(), plot.background = element_blank(), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.position = "none", axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), plot.margin = rep(unit(0, 
            "null"), 4), axis.title.x = element_blank(), axis.title.y = element_blank()) + 
        scale_fill_brewer(type = "seq", palette = "Blues")
    full_max <- join(density_raw, max_points, type = "inner")
    grade_labels <- levels(stage_1$grade_season_label)
    full_max <- cbind(full_max, grade_labels)
    p <- p + annotate(geom = "text", x = full_max$x, y = full_max$y, 
        label = full_max$grade_labels, size = 7)
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
