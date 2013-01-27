\name{dissimilarity}
\alias{dissimilarity}
\title{Dissimilarity Statistics}
\usage{
  dissimilarity(text.var, grouping.var = NULL,
    method = "prop", diag = FALSE, upper = FALSE, p = 2)
}
\arguments{
  \item{text.var}{A text variable or word frequency matrix
  object.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{method}{Distance methods (see
  \code{\link[stats]{dist}} function). If \code{"prop"}
  (the default; the result is 1 - \code{"binary"}.}

  \item{diag}{logical.  If TRUE returns the diagonals of
  the matrix.  If \code{method = "prop"} diagonals will not
  be returned.}

  \item{upper}{logical.  If TRUE returns the upper triangle
  of the matrix.}

  \item{p}{The power of the Minkowski distance}
}
\value{
  Returns a matrix of dissimilarity values (the agreement
  between text).
}
\description{
  Uses the distance function to calculate dissimilarity
  statistics by grouping variables.
}
\examples{
\dontrun{
with(DATA, dissimilarity(state, list(sex, adult)))
with(DATA, dissimilarity(state, person, diag = TRUE))
}
}
\seealso{
  \code{\link[stats]{dist}}
}
\keyword{dissimilarity}

