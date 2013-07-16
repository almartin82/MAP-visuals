\name{growth_status_scatter}
\alias{growth_status_scatter}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Scatterplot of single term SGP vs absolute percentile rank
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
growth_status_scatter(df, show_title = FALSE, school, subject, grade)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A SGP style data frame of MAP data
}
  \item{show_title}{
TRUE or FALSE - should the plot have a title?
}
  \item{school}{
Name of school; only used for the title
}
  \item{subject}{
Test subject; only used for the title
}
  \item{grade}{
Grade level; only used for the title
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
function (df, show_title = FALSE, school, subject, grade) 
{
    lab_x <- c(33/2, 50, 66 + 33/2, 33/2, 50, 66 + 33/2)
    lab_y <- c(75, 75, 75, 25, 25, 25)
    lab_text <- c("Low Growth\nAbove Gr. Lev.", "Avg Growth\nAbove Gr. Lev.", 
        "High Growth\nAbove Gr. Lev.", "Low Growth\nBelow Gr. Lev.", 
        "Avg Growth\nBelow Gr. Lev.", "High Growth\nBelow Gr. Lev.")
    annotation_df <- data.frame(lab_x = lab_x, lab_y = lab_y, 
        lab_text = lab_text)
    p <- ggplot(data = df, aes(x = GROWTH_PERCENTILE, y = END_NPR, 
        label = STUDENT_NAME)) + geom_point(alpha = 0)
    p <- p + annotate(geom = "text", x = annotation_df$lab_x, 
        y = annotation_df$lab_y, label = annotation_df$lab_text, 
        size = 12, color = "gray80", alpha = 0.8)
    p <- p + geom_vline(xintercept = c(34, 66), size = 1, color = "gray50", 
        alpha = 0.6) + geom_hline(yintercept = c(50), size = 1, 
        color = "gray50", alpha = 0.6) + geom_text(size = rel(3), 
        alpha = 0.5) + geom_jitter(size = 3, shape = 1, position = position_jitter(height = 0.75, 
        width = 0.75)) + coord_cartesian(ylim = c(0, 100), xlim = c(0, 
        100)) + scale_x_continuous(breaks = seq(10, 90, by = 10), 
        minor_breaks = NULL) + scale_y_continuous(breaks = seq(10, 
        90, by = 10), minor_breaks = NULL) + labs(x = paste(academic_abbrev, 
        "Growth Percentile"), y = paste(year_ends, "EOY National Percentile Rank")) + 
        theme(plot.title = element_text(hjust = 0, face = "bold", 
            size = 20), panel.background = element_blank(), panel.grid.major = element_line(color = "gray95", 
            linetype = "longdash"))
    if (show_title != FALSE) {
        p <- p + labs(title = paste(academic_year, sch, "gr.", 
            grade, subj, "EOY Growth vs Status"))
    }
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
