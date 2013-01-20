\name{cm_code.transform}
\alias{cm_code.transform}
\title{Transform Codes}
\usage{
  cm_code.transform(x2long.obj, overlap.code.list = NULL,
    combine.code.list = NULL, exclude.code.list = NULL,
    rm.var = NULL)
}
\arguments{
  \item{x2long.obj}{An object from \code{cm_range2long},
  \code{cm_time2long} or \code{cm_df2long}.}

  \item{overlap.code.list}{A list of named character
  vectors of at least two code column names to aggregate
  co-occurences.}

  \item{combine.code.list}{A list of named character
  vectors of at least two code column names to combine}

  \item{exclude.code.list}{A list of named character
  vectors of at least two code column names to compare and
  exclude.  The last column name is the one that will be
  excluded.}

  \item{rm.var}{Name of the repeated measures column.}
}
\value{
  Returns a dataframe with overlapping, combined
  occurrences, and/or exclusion of supplied overlapping
  codes added.
}
\description{
  Transform co-occurrences and/or combinations of codes
  into a new code(s).
}
\note{
  The code column must be named code and your start and end
  columns must be named \code{"start"} and \code{"end"}.
}
\examples{
\dontrun{
foo <- list(
    AA = qcv(terms='1:10'),
    BB = qcv(terms='1:2, 3:10, 19'),
    CC = qcv(terms='1:3, 5:6')
)

foo2  <- list(
    AA = qcv(terms='4:8'),
    BB = qcv(terms='1:4, 10:12'),
    CC = qcv(terms='1, 11, 15:20'),
    DD = qcv(terms='')
)

x <- cm_range2long(foo)
z <- cm_range2long(foo, foo2, v.name="time")
overlaps <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
cm_code.transform(x, overlap.code.list=list(AB=qcv(AA, BB)))
cm_code.transform(x, combine.code.list = list(ALL=qcv(AA, BB, CC)))
cm_code.transform(x, overlap.code.list=list(AB=qcv(AA, BB)),
    combine.code.list = list(ALL=qcv(AA, BB, CC)))
cm_code.transform(z, overlaps, rm.var="time")
cm_code.transform(z, overlaps,
   exclude.code.list=list(AABB_no_CC = qcv(AA, BB, CC)), rm.var="time")
#WITH cm_time2long
x <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
)

y <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
)

dat <- cm_time2long(x, y)
cm_code.transform(dat, list(P=qcv(A, B), Q=qcv(B, C), R=qcv(A, B, C)),
    list(S=qcv(A, B), T=qcv(B, C), U=qcv(A, B, C)),
    list(ABnoC = qcv(A, B, C)), rm.var="variable")
}
}
\seealso{
  \code{\link[qdap]{cm_range2long}},
  \code{\link[qdap]{cm_time2long}},
  \code{\link[qdap]{cm_df2long}},
  \code{\link[qdap]{cm_code.blank}},
  \code{\link[qdap]{cm_code.combine}},
  \code{\link[qdap]{cm_code.exclude}},
  \code{\link[qdap]{cm_code.overlap}}
}
\keyword{transform}

