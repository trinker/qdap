\name{distTab}
\alias{distTab}
\title{SPSS Style Frequency Tables}
\usage{
  distTab(dataframe, breaks = NULL, digits = 2, ...)
}
\arguments{
  \item{dataframe}{A vector or data.frame object.}

  \item{breaks}{Either a numeric vector of two or more cut
  points or a single number (greater than or equal to 2)
  giving the number of intervals into which x is to be
  cut.}

  \item{digits}{Integer indicating the number of decimal
  places (round) or significant digits (signif.) to be
  used. Negative values are allowed}

  \item{\ldots}{Other variables passed to cut.}
}
\value{
  Returns a list of data frames (or singular data frame for
  a vector) of frequencies, cumulative frequencies,
  percentages and cumulative percentages for each interval.
}
\description{
  Generates a distribution table for vectors, matrices and
  dataframes.
}
\examples{
\dontrun{
distTab(rnorm(10000), 10)
distTab(sample(c("red", "blue", "gray"), 100, T), right = FALSE)
distTab(CO2, 4)
distTab(mtcars)
distTab(mtcars, 4)

wdst <- with(mraja1spl, word_stats(dialogue, list(sex, fam.aff, died)))
distTab(wdst$gts)
}
}
\seealso{
  \code{\link[base]{cut}}
}
\keyword{distribution,}
\keyword{frequency}

