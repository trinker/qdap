\name{blank2NA}
\alias{blank2NA}
\title{Replace Blanks in Data Frame}
\usage{
  blank2NA(dataframe, missing = NA)
}
\arguments{
  \item{dataframe}{A dataframe with blank (empty) cells.}

  \item{missing}{Value to replace empty cells with.}
}
\value{
  Returns a dataframe with blank spaces replaced.
}
\description{
  Replaces blank (empty) cells in a dataframe.  generally,
  for internal use.
}
\examples{
\dontrun{
dat <- data.frame(matrix(sample(c(1:4, ""), 50, TRUE),
    10, byrow = TRUE), stringsAsFactors = FALSE)
dat
blank2NA(dat)
}
}
\seealso{
  \code{\link[qdap]{rm_row}}
}

