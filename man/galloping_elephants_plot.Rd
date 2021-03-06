% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/galloping_elephants_plot.R
\name{galloping_elephants_plot}
\alias{galloping_elephants_plot}
\title{galloping_elephants_plot}
\usage{
galloping_elephants_plot(.data, ...)
}
\arguments{
\item{.data}{an object to plot}

\item{...}{other arguments passed to \code{\link{galloping_elephants}}}

\item{subsetting}{(required for \code{mapvizier obejcts}) a call to \code{dplyr}'s \code{filter} function, leaving out
the \code{.data} argument (i.e. only containing subsetting arguments)}
}
\value{
a \code{\link[ggplot2]{ggplot2}} object
}
\description{
\code{galloping_elephants_plot()} is a method that serves as wrapper for \code{\link{galloping_elephants}}.
}
\details{
\code{galloping_elephants_plot} serves as wrapper that dispatches to \code{\link{galloping_elephants}} depending
on the class of the object passed to the \code{.data} parameter.

If the object is a \code{\link{mapvizier}} object, then \code{galloping_elephants}' requirement that the data columns conform
to KIPP NJ (n\'{e}e TEAM) canonical form is unnecessary (since we know which
columns are which in a \code{mapvizier object}). The \code{subsetting} argument must be used
when a \code{mapvizier} object is passed, since the data will need to be
properly subsetted. The user should pass this argument a call to \code{\link[dplyr]{filter(...)}}, where
that function's \code{.data} argument is unnecessary. All other arguments (e.g. \code{facets})
are passed to \code{galloping_elephants} via the \code{...} argument.

Ff the object is a not a \code{mapvizier} object (like a \code{data.frame}) then as with \code{becca_plot}, the
user needs to identify school, cohort, year, grade-level-season, and percentile columns.
}
\seealso{
\code{\link{galloping_elephants}}, \code{link[dplyr]{filter}}
}

