\name{rm_url}
\alias{rm_url}
\title{Remove/Replace URLs}
\usage{
rm_url(text.var, trim = TRUE, clean = TRUE,
  pattern = "(http[^ ]*)|(www\\\\.[^ ]*)", replacement = "", ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{trim}{logical.  If \code{TRUE} removes leading and
  trailing white spaces.}

  \item{clean}{trim logical.  If \code{TRUE} extra white
  sapces and escaped character will be removed.}

  \item{pattern}{A character string containing a regular
  expression (or character string for \code{fixed = TRUE})
  to be matched in the given character vector.}

  \item{replacement}{Replacement for matched
  \code{pattern}.}

  \item{\dots}{Other arguments passed to
  \code{\link[base]{gsub}}.}
}
\value{
Returns a character string with URLs removed.
}
\description{
Remove/Replace URLs from a string.
}
\examples{
\dontrun{
x <- " I like www.talkstats.com and http://stackoverflow.com"
rm_url(x)
rm_url(x, replacement = '<a href="\\\\1" target="_blank">\\\\1</a>')
}
}
\seealso{
\code{\link[base]{gsub}}
}
\keyword{http}
\keyword{url}
\keyword{www}

