\name{replacer}
\alias{replacer}
\title{Replace Cells in a Matrix or Data Frame}
\usage{
  replacer(dat, replace = 0, with = "-")
}
\arguments{
  \item{dat}{Data; either a dataframe, matrix or vector.}

  \item{replace}{Element to replace.}

  \item{with}{Replacement element.}
}
\value{
  Returns a dataframe, matrix or vector with the element
  replaced.
}
\description{
  Replace elements of a dataframe, matrix or vector with
  least restrictive class.
}
\examples{
\dontrun{
replacer(mtcars, 0, "REP")
replacer(mtcars, 8, NA)
replacer(c("a", "b"), "a", "foo")
}
}
\keyword{replace}

