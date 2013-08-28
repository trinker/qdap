\name{gantt_rep}
\alias{gantt_rep}
\title{Generate Unit Spans for Repeated Measures}
\usage{
  gantt_rep(rm.var, text.var, grouping.var,
    units = "words", col.sep = "_")
}
\arguments{
  \item{rm.var}{An optional single vector or list of 1 or 2
  of repeated measures to facet by.}

  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables. Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{units}{The unit of measurement to analyze.  One of
  the strings \code{"character"}, \code{"syllable"},
  \code{"word"}, or \code{"sentence"}.}

  \item{col.sep}{The character string to use to separate
  pasted variables in the merged grouping variable
  header/name.}
}
\value{
  Returns a data frame of start and end times by repeated
  measure and grouping variable(s)
}
\description{
  Produces start and end times for occurrences for each
  repeated measure condition.
}
\note{
  For non repeated measures data/plotting use
  \code{\link[qdap]{gantt}}; for a convenient wrapper that
  takes text and generates plots use
  \code{\link[qdap]{gantt_plot}}; and for a flexible gantt
  plot that words with code matrix functions (cm) use
  \code{\link[qdap]{gantt_wrap}}.
}
\examples{
\dontrun{
dat <- with(rajSPLIT, gantt_rep(act, dialogue, list(fam.aff, sex),
    units = "words", col.sep = "_"))
head(dat, 20)
gantt_wrap(dat,  "fam.aff_sex",  facet.vars = "act",
    title = "Repeated Measures Gantt Plot",
    minor.line.freq = 25, major.line.freq = 100)
}
}
\references{
  Clark, W. & Gantt, H. (1922) The Gantt chart, a working
  tool of management. New York, Ronald Press.
}
\seealso{
  \code{\link[qdap]{gantt}},
  \code{\link[qdap]{gantt_wrap}},
  \code{\link[qdap]{gantt_plot}}
}
\keyword{Gantt}

