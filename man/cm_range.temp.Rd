\name{cm_range.temp}
\alias{cm_range.temp}
\title{Range Code Sheet}
\usage{
  cm_range.temp(codes, file = NULL, text.var = NULL,
    grouping.var = NULL)
}
\arguments{
  \item{codes}{Character vector of codes.}

  \item{file}{A connection, or a character string naming
  the file to print to (.txt or .doc is recommended).}

  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}
}
\description{
  Generates a range coding sheet for coding words.
}
\examples{
\dontrun{
cm_range.temp(qcv(AA, BB, CC))
with(DATA, cm_range.temp(qcv(AA, BB, CC), , state, list(person, adult)))
## cm_range.temp(qcv(AA, BB, CC), file = "foo.txt")
## library(reports); delete("foo.txt")
}
}
\references{
  Miles, M. B. & Huberman, A. M. (1994). An expanded
  sourcebook: Qualitative data analysis. 2nd ed. Thousand
  Oaks, CA: SAGE Publications.
}
\seealso{
  \code{\link[qdap]{cm_time.temp}}
}
\keyword{coding}

