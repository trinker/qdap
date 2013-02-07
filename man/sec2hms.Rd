\name{sec2hms}
\alias{sec2hms}
\title{Convert Seconds to h:m:s}
\usage{
  sec2hms(x)
}
\arguments{
  \item{x}{A vector of times in seconds.}
}
\value{
  Returns a vector of times in h:m:s format.  Generally,
  this function is for internal use.
}
\description{
  Converts a vector of seconds to h:m:s.
}
\examples{
\dontrun{
sec2hms(c(256, 3456, 56565))
}
}
\seealso{
  \code{\link[chron]{times}}, \code{\link[qdap]{hms2sec}}
}
\keyword{conversion}
\keyword{time,}

