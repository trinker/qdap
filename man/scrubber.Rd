\name{scrubber}
\alias{scrubber}
\title{Use to clean text variables when importing a new data set.}
\usage{
  scrubber(text.var, num2word = FALSE, rm.quote = TRUE,
    fix.comma = TRUE, ...)
}
\arguments{
  \item{text.var}{The text variable}

  \item{num2word}{logical If TRUE replaces a numbers with
  text representations.}

  \item{fix.comma}{logical If TRUE removes any spaces
  before a comma.}

  \item{rm.quote}{logical If TRUE removes and \code{\"}.}

  \item{\ldots}{Other arduments passed to
  \code{replace_number}.}
}
\value{
  Returns a parsed character vector.
}
\description{
  Use to clean text variables when importing a new data
  set.  Removes extra white spaces other textual anomalies
  that may cause errors.
}
\examples{
\dontrun{
x <- c("I like 456 dogs  , don't you?\\"")
scrubber(x)
scrubber(x, TRUE)
}
}
\seealso{
  \code{\link[qdap]{strip}}
}
\keyword{clean}
\keyword{parse,}

