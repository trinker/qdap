\name{hms2sec}
\alias{hms2sec}
\title{Convert h:m:s to Seconds}
\usage{
  hms2sec(x)
}
\arguments{
  \item{x}{A vector of times in h:m:s.}
}
\value{
  Returns a vector of times in seconds.  Generally, this
  function is for internal use.
}
\description{
  Converts a vector of h:m:s to seconds.
}
\examples{
\dontrun{
hms2sec(c("02:00:03", "04:03:01"))
hms2sec(sec2hms(c(222, 1234, 55)))
}
}
\seealso{
  \code{\link[chron]{times}}, \code{\link[qdap]{sec2hms}}
}
\keyword{conversion}
\keyword{time,}

