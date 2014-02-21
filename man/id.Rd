\name{id}
\alias{id}
\title{ID By Row Number or Sequence Along}
\usage{
id(x, prefix = FALSE, pad = TRUE)
}
\arguments{
  \item{x}{A dataframe, matrix, vector, or list object.}

  \item{prefix}{logical.  If \code{TRUE} an "X." is place
  before each id.}

  \item{pad}{logical.  If \code{TRUE} the beginning number
  will be padded with zeros.}
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
id(list(1, 4, 6))
id(matrix(1:10, ncol=1))
id(mtcars)
id(mtcars, TRUE)
id("w")
question_type(DATA.SPLIT$state, id(DATA.SPLIT, TRUE))
}
\keyword{id}

