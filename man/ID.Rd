\name{ID}
\alias{ID}
\title{ID By Row Number or Sequence Along}
\usage{
  ID(x, pad = TRUE, prefix = FALSE)
}
\arguments{
  \item{x}{A dataframe, matrix, vector, or list object.}

  \item{pad}{logical.  If \code{TRUE} the begining number
  will be padded with zeros.}

  \item{prefix}{logical.  If \code{TRUE} an "X." is place
  before each id.}
}
\value{
  Returns a vector of sequential integers.
}
\description{
  Generate a sequence of integers the
  \code{\link[base]{length}}/\code{\link[base]{ncol}} of an
  object.
}
\examples{
ID(list(1, 4, 6))
ID(matrix(1:10, ncol=1))
ID(mtcars)
ID(mtcars, TRUE)
ID("w")
question_type(DATA.SPLIT$state, ID(DATA.SPLIT, TRUE))
}
\keyword{id}

