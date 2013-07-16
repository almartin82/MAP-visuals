\name{best_avail_sgp_df}
\alias{best_avail_sgp_df}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Builds a SGP data frame with best available growth period for each student.}
\description{
  a function that preps a SGP data frame, returning a composite data frame that blends multiple periods   based on your specification
}
\usage{
best_avail_sgp_df(df, prefer_period = "Spring to Spring", backup_period_1 = "Fall to Spring", backup_period_2 = "Winter to Spring")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{df}{
A data frame with SGP headers
}
  \item{prefer_period}{
first choice growth period.  Defaults to 'Spring to Spring'
}
  \item{backup_period_1}{
second choice growth period.  Defaults to 'Fall to Spring'
}
  \item{backup_period_2}{
third choice growth period.  Defaults to 'Winter to Spring'
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
function (df, prefer_period = "Spring to Spring", backup_period_1 = "Fall to Spring", 
    backup_period_2 = "Winter to Spring") 
{
    dt <- data.table(df)
    setkey(dt, PERIOD_STRING)
    preferred <- copy(dt[prefer_period])
    backup_1 <- copy(dt[backup_period_1])
    backup_2 <- copy(dt[backup_period_2])
    unq_key <- c("STUDENTID", "YEAR", "MEASUREMENTSCALE")
    uniques <- unique(df[, unq_key])
    setkey(preferred, STUDENTID, YEAR, MEASUREMENTSCALE)
    setkey(backup_1, STUDENTID, YEAR, MEASUREMENTSCALE)
    setkey(backup_2, STUDENTID, YEAR, MEASUREMENTSCALE)
    cols_to_get <- c("STUDENTID", "LASTFIRST", "STUDENT_NAME", 
        "YEAR", "MEASUREMENTSCALE", "SCH_ABBREV", "START_TERM_VERIF", 
        "END_TERM_VERIF", "START_RIT", "END_RIT", "START_NPR", 
        "END_NPR", "RIT_CHANGE", "START_TERM_STRING", "END_TERM_STRING", 
        "START_GRADE_VERIF", "END_GRADE_VERIF", "REPORTED_GROWTH_PROJECTION", 
        "MET_TYPICAL_GROWTH_TARGET", "GROWTH_PERCENTILE", "VALID_OBSERVATION")
    preferred_merged <- preferred[uniques, cols_to_get, with = FALSE]
    setkey(preferred_merged, VALID_OBSERVATION)
    pref_matched <- preferred_merged[VALID_OBSERVATION == 1, 
        ]
    unmatched <- preferred_merged[VALID_OBSERVATION == 0, ]
    unmatched_key <- unmatched[, list(STUDENTID, YEAR, MEASUREMENTSCALE)]
    nrow(unmatched) + nrow(pref_matched)
    backup_1_merge <- backup_1[unmatched_key, cols_to_get, with = FALSE]
    backup_1_matched <- backup_1_merge[VALID_OBSERVATION == 1, 
        ]
    unmatched <- backup_1_merge[VALID_OBSERVATION == 0, ]
    unmatched_key <- unmatched[, list(STUDENTID, YEAR, MEASUREMENTSCALE)]
    nrow(unmatched) + nrow(backup_1_matched) + nrow(pref_matched)
    backup_2_merge <- backup_2[unmatched_key, cols_to_get, with = FALSE]
    backup_2_matched <- backup_2_merge[VALID_OBSERVATION == 1, 
        ]
    unmatched <- backup_2_merge[VALID_OBSERVATION == 0, ]
    nrow(unmatched) + nrow(backup_1_matched) + nrow(backup_2_matched) + 
        nrow(pref_matched)
    final <- rbind(pref_matched, backup_1_matched, backup_2_matched, 
        unmatched)
    return(final)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
