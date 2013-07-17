\name{network_typ_growth_distro}
\alias{network_typ_growth_distro}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  KIPP Network Variation of pct students making typical growth
}
\description{
  Shows the distribution of KIPP schools for the target subject/grade/year etc.
}
\usage{
network_typ_growth_distro(nat_results_df, measurementscale, academic_year, grade_level, start_season, end_season, comparison_name, comparison_pct_typ_growth, replace_nat_results_match = FALSE, de_kippify_names = TRUE, de_schoolify_names = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nat_results_df}{
  An extract from the R&E cube that has national results in a highly specific format.  Talk to Mike H. to get this.
}
  \item{measurementscale}{
  MAP Subject
}
  \item{academic_year}{
  Year of results (Foundation codes by ending year; eg 2011-2012 becomes 2012)
}
  \item{grade_level}{
  Grade level
}
  \item{start_season}{
  All caps, starting season.  'SPRING'
}
  \item{end_season}{
  All caps, ending season.  'SPRING'
}
  \item{comparison_name}{
  Name of school you want to be shown
}
  \item{comparison_pct_typ_growth}{
  Pct of students who made typical growth.  Input as decimal.
}
  \item{replace_nat_results_match}{
  If you enter 'Rise', do you want it to drop Rise from the national distribution?
  This is a stub/to do, not currently implemented.
}
  \item{de_kippify_names}{
  Drop KIPP from the school names.
}
  \item{de_schoolify_names}{
  Drop assorted other cruft from school names - 'Middle School', 'Academy', etc.
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
function (nat_results_df, measurementscale, academic_year, grade_level, 
    start_season, end_season, comparison_name, comparison_pct_typ_growth, 
    replace_nat_results_match = FALSE, de_kippify_names = TRUE, 
    de_schoolify_names = TRUE) 
{
    nat <- nat_results_df[nat_results_df$Sub_Test_Name == measurementscale & 
        nat_results_df$Growth_Academic_Year == academic_year & 
        nat_results_df$Growth_Grade_Level == grade_level & nat_results_df$Start_Season == 
        start_season & nat_results_df$End_Season == end_season, 
        ]
    if (de_kippify_names == TRUE) {
        nat$School_Display_Name <- gsub(", a KIPP school", "", 
            nat$School_Display_Name)
        nat$School_Display_Name <- gsub("KIPP ", "", nat$School_Display_Name)
    }
    if (de_schoolify_names == TRUE) {
        nat$School_Display_Name <- gsub(" Academy for Girls", 
            "", nat$School_Display_Name)
        nat$School_Display_Name <- gsub(" Academy for Boys", 
            "", nat$School_Display_Name)
        nat$School_Display_Name <- gsub(" Academy of", "", nat$School_Display_Name)
        nat$School_Display_Name <- gsub(" Academy", "", nat$School_Display_Name)
        nat$School_Display_Name <- gsub(" Middle School", "", 
            nat$School_Display_Name)
        nat$School_Display_Name <- gsub(" College Preparatory", 
            "", nat$School_Display_Name)
        nat$School_Display_Name <- gsub(" College Prep", "", 
            nat$School_Display_Name)
        nat$School_Display_Name <- gsub(" Charter School", "", 
            nat$School_Display_Name)
        nat$School_Display_Name <- gsub(" School", "", nat$School_Display_Name)
        nat$School_Display_Name <- gsub(" Preparatory", " Prep", 
            nat$School_Display_Name)
    }
    nat$highlight_me <- 0
    new_row <- c(comparison_name, NA, NA, comparison_pct_typ_growth, 
        NA, grade_level, measurementscale, start_season, end_season, 
        2012, 1)
    nat_plus <- rbind(nat, new_row)
    head(nat)
    tail(nat_plus)
    nat_plus$dummy_x <- rank(as.numeric(nat_plus$Perc_Growth), 
        ties.method = "random")
    nat_plus$Perc_Growth <- as.numeric(nat_plus$Perc_Growth) * 
        100
    kipp_rank <<- sum(comparison_pct_typ_growth * 100 <= nat_plus$Perc_Growth)
    kipp_denom <<- length(nat_plus$Perc_Growth)
    p <- ggplot(data = nat_plus, aes(x = dummy_x, y = as.numeric(Perc_Growth), 
        fill = factor(highlight_me), label = paste(School_Display_Name, 
            " (", round(Perc_Growth, 0), "\%)", sep = ""))) + 
        geom_bar(stat = "identity") + geom_text(y = 0.25, hjust = 0, 
        vjust = 0.3, size = 1, color = "floralwhite") + coord_flip() + 
        guides(fill = FALSE) + labs(y = "Percent Making Typical [Keep Up] Growth", 
        title = "KIPP Network Comparison") + scale_x_continuous(limits = c(0, 
        max(nat_plus$dummy_x))) + theme(panel.background = element_blank(), 
        plot.background = element_blank(), panel.grid.major = element_blank(), 
        title = element_text(size = rel(0.7)), axis.title.y = element_blank(), 
        axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        panel.margin = unit(0, "null"), plot.margin = rep(unit(0, 
            "null"), 4), axis.ticks.margin = unit(0, "null")) + 
        scale_fill_manual(values = c("gray30", "gold1")) + annotate(geom = "text", 
        x = 1, y = 0.85 * nat_plus[nat_plus$dummy_x == max(nat_plus$dummy_x), 
            "Perc_Growth"], label = paste(kipp_rank, ordinal_me(kipp_rank), 
            " of ", kipp_denom, sep = ""), color = "gray20", 
        alpha = 0.8, size = 9, vjust = 1, angle = 0)
    return(p)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
