\name{becca_plot}
\alias{becca_plot}
\title{Becca Vichniac's Quartile (Floating Bar) Chart}
\usage{
becca_plot(.data, school_name_column = "SCH_ABBR",
  cohort_name_column = "COHORT", academic_year_column = "MAP_YEAR_ACADEMIC",
  grade_level_season_column = "GRADE_LEVEL_SEASON",
  measurement_scale_column = "MEASUREMENTSCALE",
  percentile_column = "PERCENTILE_2011_NORMS", first_and_spring_only = TRUE,
  justify_widths = FALSE, justify_min = NA, justify_max = NA,
  entry_grades = c(-0.7, 4.3), color_scheme = "KIPP Report Card",
  facets = FALSE, facet_opts = FALSE, title_text = FALSE)
}
\arguments{
  \item{.data}{the data frame in TEAM canoncical style
  (long data forms)}

  \item{school_name_column}{column in \code{.data} with
  school names}

  \item{cohort_name_column}{column in \code{.data} with
  cohornt names}

  \item{academic_year_column}{column in \code{.data} with
  academic years}

  \item{grade_level_season_column}{column in \code{.data}
  with numeric indicating grade season (e.g., Fall 4th =
  3.3, Winter 4th = 3.7, Spring 4th = 4.0).}

  \item{measurement_scale_column}{column in \code{.data}
  with subject}

  \item{percentile_column}{= column in \code{.data} with
  NPR.}

  \item{first_and_spring_only}{indicator for showing shoing
  Fall-to-Spring rather than Spring-to-Spring}

  \item{justify_widths}{width justification indicator}

  \item{justify_min}{}

  \item{justify_max}{}

  \item{entry_grades}{= c(-0.7, 4.3) GradeSeasons that
  should use Fall and Spring (i.e., Kinder =-.3 and %th =
  4.3)}

  \item{color_scheme}{only 'KIPP Report Card'}

  \item{facets}{= FALSE}

  \item{facet_opts}{= FALSE}

  \item{title_text}{= FALSE}
}
\value{
prints a ggplot object
}
\description{
\code{becca_plot} returns a ggplot object binned quaritle
performonce
}
\details{
This function builds and prints a bar graph with 4 bins per
bar show MAP data binned by quartile (National Percentil
Rank).  Bars are centered at 50th percentile horizonatally
}

