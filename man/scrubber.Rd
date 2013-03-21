\name{scrubber}
\alias{scrubber}
\title{Clean Imported Text}
\usage{
  scrubber(text.var, num2word = FALSE, rm.quote = TRUE,
    fix.comma = TRUE, fix.space = TRUE, ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{num2word}{logical If TRUE replaces a numbers with
  text representations.}

  \item{fix.comma}{logical If TRUE removes any spaces
  before a comma.}

  \item{fix.space}{logical.  If TRUE extra spaces before
  endmarks are removed.}

  \item{rm.quote}{logical If TRUE removes any \code{\"}.}

  \item{\ldots}{Other arguments passed to
  \code{\link[qdap]{replace_number}}.}
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
x <- c("I like 456 dogs\\t  , don't you?\\"")
scrubber(x)
scrubber(x, TRUE)
}
}
\seealso{
  \code{\link[qdap]{strip}}
}
\keyword{clean}
\keyword{parse,}

