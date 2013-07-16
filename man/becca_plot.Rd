\name{becca_plot}
\alias{becca_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Makes Becca's KIPP Report Card plots in R}
\description{
  Takes a CDF 'TEAM Style' and returns a list with 1) a ggplot object and 2) a data frame with aggregate data for each quartile.
}
\usage{
becca_plot(df, first_and_spring_only = TRUE, entry_grades = c(-0.7, 4.3), color_scheme = "KIPP Report Card", facets = FALSE, facet_opts = FALSE, title_text = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
%%     ~~Describe \code{df} here~~
}
  \item{first_and_spring_only}{
%%     ~~Describe \code{first_and_spring_only} here~~
}
  \item{entry_grades}{
%%     ~~Describe \code{entry_grades} here~~
}
  \item{color_scheme}{
%%     ~~Describe \code{color_scheme} here~~
}
  \item{facets}{
%%     ~~Describe \code{facets} here~~
}
  \item{facet_opts}{
%%     ~~Describe \code{facet_opts} here~~
}
  \item{title_text}{
%%     ~~Describe \code{title_text} here~~
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
    4.3), color_scheme = "KIPP Report Card", facets = FALSE, 
    facet_opts = FALSE, title_text = FALSE) 
{
    stage_1 <- df[, c("SCH_ABBREV", "COHORT", "MAP_YEAR_ACADEMIC", 
        "GRADE_LEVEL_SEASON", "MEASUREMENTSCALE", "PERCENTILE_2011_NORMS")]
    if (first_and_spring_only) {
        stage_1 <- stage_1[stage_1$GRADE_LEVEL_SEASON \%in\% entry_grades | 
            stage_1$GRADE_LEVEL_SEASON\%\%1 == 0, ]
    }
    stage_1$QUARTILE <- floor((stage_1$PERCENTILE_2011_NORMS/25) + 
        1)
    stage_1$DUMMY <- 1
    stage_2 <- ddply(stage_1, .(SCH_ABBREV, COHORT, MAP_YEAR_ACADEMIC, 
        GRADE_LEVEL_SEASON, MEASUREMENTSCALE, QUARTILE), summarise, 
        n = sum(DUMMY))
    stage_3 <- ddply(stage_2, .(SCH_ABBREV, COHORT, MAP_YEAR_ACADEMIC, 
        GRADE_LEVEL_SEASON, MEASUREMENTSCALE), summarise, QUARTILE = QUARTILE, 
        PCT = round((n/sum(n)) * 100, 1))
    stage_3$AT_GRADE_LEVEL_DUMMY <- ""
    stage_3[stage_3$QUARTILE <= 2, "AT_GRADE_LEVEL_DUMMY"] <- "NO"
    stage_3[stage_3$QUARTILE >= 3, "AT_GRADE_LEVEL_DUMMY"] <- "YES"
    stage_3$ORDER <- stage_3$QUARTILE
    stage_3[stage_3$QUARTILE == 2, "ORDER"] <- "placeholder"
    stage_3[stage_3$QUARTILE == 1, "ORDER"] <- 2
    stage_3[stage_3$ORDER == "placeholder", "ORDER"] <- 1
    final_df <- stage_3[with(stage_3, order(MEASUREMENTSCALE, 
        SCH_ABBREV, COHORT, MAP_YEAR_ACADEMIC, GRADE_LEVEL_SEASON, 
        ORDER)), ]
    npr_above <- subset(final_df, AT_GRADE_LEVEL_DUMMY == "YES")
    npr_below <- subset(final_df, AT_GRADE_LEVEL_DUMMY == "NO")
    npr_below$PCT <- npr_below$PCT * -1
    npr_above = ddply(npr_above, .(SCH_ABBREV, COHORT, MAP_YEAR_ACADEMIC, 
        GRADE_LEVEL_SEASON, MEASUREMENTSCALE), transform, MIDPOINT = cumsum(PCT) - 
        0.5 * PCT)
    npr_below = ddply(npr_below, .(SCH_ABBREV, COHORT, MAP_YEAR_ACADEMIC, 
        GRADE_LEVEL_SEASON, MEASUREMENTSCALE), transform, MIDPOINT = cumsum(PCT) - 
        0.5 * PCT)
    npr_below <- transform(npr_below, QUARTILE = ordered(QUARTILE, 
        levels = names(sort(-table(QUARTILE)))))
    p <- ggplot() + geom_bar(data = npr_above, aes(x = GRADE_LEVEL_SEASON, 
        y = PCT, fill = factor(QUARTILE), order = ORDER), stat = "identity") + 
        geom_bar(data = npr_below, aes(x = GRADE_LEVEL_SEASON, 
            y = PCT, fill = factor(QUARTILE), order = ORDER), 
            stat = "identity") + geom_text(data = npr_above, 
        aes(x = GRADE_LEVEL_SEASON, y = MIDPOINT, label = round(PCT, 
            0)), size = 4) + geom_text(data = npr_below, aes(x = GRADE_LEVEL_SEASON, 
        y = MIDPOINT, label = abs(round(PCT, 0))), size = 4) + 
        labs(x = "Grade Level", y = "Percentage of Cohort") + 
        theme(panel.background = element_blank(), plot.background = element_blank(), 
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
            axis.text.y = element_blank(), legend.title = element_blank()) + 
        theme(title = element_text(size = rel(1.5)))
    legend_labels = c("Bottom", "Second", "Third", "Top")
    if (color_scheme == "KIPP Report Card") {
        p <- p + scale_fill_manual(values = c(rgb(207, 204, 193, 
            max = 255), rgb(230, 230, 230, max = 255), rgb(254, 
            188, 17, max = 255), rgb(247, 148, 30, max = 255)), 
            labels = legend_labels)
    }
    else if (color_scheme == "Sequential Blues") {
        p <- p + scale_fill_brewer(type = "seq", palette = 1)
    }
    else {
        p <- p + scale_fill_manual(values = color_scheme, labels = legend_labels)
    }
    if (title_text != FALSE) {
        p <- p + labs(title = title_text)
    }
    if (facets != FALSE & facet_opts != FALSE) {
        p <- p + eval(facet_grid(as.formula(facets), facet_opts))
    }
    else if (facets != FALSE & facet_opts == FALSE) {
        p <- p + facet_grid(as.formula(facets))
    }
    return(list(p, final_df))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
