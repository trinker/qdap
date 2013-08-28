\name{gantt}
\alias{gantt}
\title{Generate Unit Spans}
\usage{
  gantt(text.var, grouping.var, plot = TRUE,
    units = "words", sums = FALSE, plot.colors = NULL,
    box.color = NULL, col.sep = "_")
}
\arguments{
  \item{text.var}{The text variable}

  \item{grouping.var}{The grouping variables. Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{plot}{logical.  If \code{TRUE} plots the start-end
  times as a Gantt plot.}

  \item{units}{The unit of measurement to analyze.  One of
  the strings \code{"character"}, \code{"syllable"},
  \code{"word"}, or \code{"sentence"}.}

  \item{sums}{logical.  If \code{TRUE} reports and
  optionally plots the total units used by grouping
  variable(s).}

  \item{plot.colors}{The colors of the Gantt plot bars.
  Either a single color or a length equal to the number of
  grouping variable(s).}

  \item{box.color}{A single color of the box around the
  Gantt plot bars.}

  \item{col.sep}{The character string to use to separate
  pasted variables in the merged grouping variable
  header/name.}
}
\value{
  Returns a data frame of start and end times by grouping
  variable(s) or optionally returns a list of two: (1) A
  data frame of the total units used by grouping
  variable(s) and (2) a data frame of start and end times
  by grouping variable(s).  Optionally plots a Gantt plot
  of the returned data.
}
\description{
  Generates start and end times of supplied text selections
  (i.e., text selections are determined by any number of
  grouping variables).
}
\note{
  For repeated measures data output use
  \code{\link[qdap]{gantt_rep}}; for a convenient wrapper
  that takes text and generates plots use
  \code{\link[qdap]{gantt_plot}}; and for a flexible gantt
  plot that words with code matrix functions (cm) use
  \code{\link[qdap]{gantt_wrap}}.
}
\examples{
\dontrun{
gantt(DATA$state, DATA$person)
gantt(DATA$state, DATA$person, sums = TRUE)
gantt(DATA$state, list(DATA$sex, DATA$adult))
x <- gantt(mraja1$dialogue, mraja1$person) #hard to see without box color
y <- gantt(mraja1$dialogue, mraja1$person, box.col = "black")
z <- gantt(mraja1$dialogue, mraja1$sex)
m <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex),
    plot.colors = NULL)
n <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex),
    plot.colors = "black")
o <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex),
    plot = FALSE)
p <- gantt(mraja1$dialogue, mraja1$person, units = "characters",
    box.color = "black")
d <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex),
    units = "characters")
e <- with(mraja1, gantt(dialogue, list(fam.aff, sex, died),
   units = "characters", sums = TRUE))
f <- gantt(mraja1$dialogue, mraja1$person, units = "syllables",
   box.color = "black", sums = TRUE)
g <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex),
    units = "syllables")

dat <- gantt(mraja1$dialogue, list(mraja1$fam.aff, mraja1$sex),
    units = "sentences", plot.colors = 'black', sums = TRUE,
    col.sep = "_")$gantt.df
gantt_wrap(dat, "fam.aff_sex", title = "Gantt Plot")
}
}
\author{
  DigEmAll (\url{stackoverflow.com}) and Tyler Rinker
  <tyler.rinker@gmail.com>.
}
\references{
  Clark, W. & Gantt, H. (1922) The Gantt chart, a working
  tool of management. New York, Ronald Press.
}
\seealso{
  \code{\link[qdap]{gantt_rep}},
  \code{\link[qdap]{gantt_wrap}},
  \code{\link[qdap]{gantt_plot}}
}
\keyword{Gantt}

