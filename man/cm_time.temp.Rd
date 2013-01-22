\name{cm_time.temp}
\alias{cm_time.temp}
\title{Time Span Code Sheet}
\usage{
  cm_time.temp(codes, start = ":00", end = NULL,
    file = NULL)
}
\arguments{
  \item{codes}{List of codes.}

  \item{start}{A character string in the form of "00:00"
  indicating start time (default is ":00").}

  \item{end}{A character string in the form of "00:00"
  indicating end time.}

  \item{file}{A connection, or a character string naming
  the file to print to (.txt is recommended).}
}
\description{
  Generates a time span coding sheet and coding format
  sheet.
}
\examples{
\dontrun{
cm_time.temp(qcv(AA, BB, CC), ":30", "7:40", file = "foo.txt")
# delete("foo.txt")
x <- list(
    transcript_time_span = qcv(terms='00:00 - 1:12:00'),
    A = qcv(terms='2.40:3.00, 5.01, 6.52:7.00, 9.00'),
    B = qcv(terms='2.40, 3.01:3.02, 5.01, 6.52:7.00, 9.00, 1.12.00:1.19.01'),
    C = qcv(terms='2.40:3.00, 5.01, 6.52:7.00, 9.00, 17.01')
)
cm_time2long(x)
cm_time.temp(qcv(AA, BB, CC))
}
}
\references{
  Miles, M. B. & Huberman, A. M. (1994). An expanded
  sourcebook: Qualitative data analysis. 2nd ed. Thousand
  Oaks, CA: SAGE Publications.
}
\seealso{
  \code{\link[qdap]{cm_range.temp}},
}
\keyword{coding}

