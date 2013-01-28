\name{lookup}
\alias{lookup}
\title{Hash Table/Dictionary Lookup}
\usage{
  lookup(terms, key.match, key.reassign = NULL,
    missing = NA)
}
\arguments{
  \item{terms}{A vector of terms to undergo a lookup.}

  \item{key.match}{Either a two column data frame (if data
  frame supplied no key reassign needed) of a match key and
  reassignment column or a single vector match key.}

  \item{key.reassign}{A single reassignment vector supplied
  if key.match is not a two column data frame.}

  \item{missing}{Value to assign to terms not matching the
  key.match.}
}
\value{
  Outputs A new vector with reassigned values.
}
\description{
  Environment based hash table useful for large vector
  lookups.
}
\examples{
\dontrun{
lookup(mtcars$carb, sort(unique(mtcars$carb)),
    c('one', 'two', 'three', 'four', 'six', 'eight'))
lookup(mtcars$carb, sort(unique(mtcars$carb)),
    seq(10, 60, by=10))

lookup(1:5, data.frame(1:4, 11:14))
lookup(LETTERS[1:5], data.frame(LETTERS[1:5], 100:104))

key <- data.frame(x=1:2, y=c("A", "B"))
big.vec <- sample(1:2, 3000000, T)
out <- lookup(big.vec, key)
out[1:20]
}
}
\seealso{
  \code{\link[base]{new.env}}
}
\keyword{dictionary,}
\keyword{hash,}
\keyword{lookup}

