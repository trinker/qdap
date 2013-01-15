\name{clean}
\alias{clean}
\title{Remove Escaped Characters}
\usage{
  clean(text.var)
}
\arguments{
  \item{text.var}{The text variable}
}
\value{
  Returns a vector of character strings with escaped
  characters removed.
}
\description{
  Pre process data to remove escaped characters
}
\examples{
\dontrun{
x <- "I go \\r
    to the \\tnext line"
x
clean(x)
}
}
\keyword{character}
\keyword{escaped}

