\name{cm_code.blank}
\alias{cm_code.blank}
\title{Blank Code Transformation}
\usage{
  cm_code.blank(x2long.obj, combine.code.list,
    rm.var = NULL, overlap = TRUE)
}
\arguments{
  \item{x2long.obj}{An object from \code{cm_range2long},
  \code{cm_time2long} or \code{cm_df2long}.}

  \item{combine.code.list}{A list of named character
  vectors of at least two code column names to combine.}

  \item{rm.var}{Name of the repeated measures column.}

  \item{overlap}{logical, integer or character of binary
  operator + integer. If TRUE finds the overlap.  If FALSE
  finds anywhere any of the codes occur. If integer finds
  that exact combination of overlaps.  If character must be
  a logical vector c(>, <, =<, =>, ==, !=) followed by an
  integer and wrapped with quotes.}
}
\value{
  Returns a dataframe with transformed occurrences of
  supplied overlapping codes added.
}
\description{
  Transform codes with any binary operator combination.
}
\note{
  For most jobs \code{cm_code.transform} will work.  This
  adds a bit of flexibility in exclusion and partial
  matching.  The code column must be named code and your
  start and end columns must be named \code{"start"} and
  \code{"end"}.
}
\examples{
\dontrun{
foo <- list(
    AA = qcv(terms="1:10"),
    BB = qcv(terms="1:2, 3:10, 19"),
    CC = qcv(terms="1:3, 5:6")
)
foo2  <- list(
    AA = qcv(terms="4:8"),
    BB = qcv(terms="1:4, 10:12"),
    CC = qcv(terms="1, 11, 15:20"),
    DD = qcv(terms="")
)

(x <- cm_range2long(foo))
(z <- cm_range2long(foo, foo2, v.name="time"))
nots <- list(notAABB=qcv(AA, BB), notAACC=qcv(AA, CC),
    notBBCC=qcv(BB, CC))
(z <- cm_code.blank(z, nots, "time", overlap=0))
(z <- cm_code.blank(z, list(atleastAABBCC=qcv(AA, BB, CC)),
    "time", overlap=1))
(z <- cm_code.blank(z, list(AACC=qcv(AA, CC)), "time",
    overlap=FALSE))  #combined
cm_code.blank(z, list(AACCnoAA=qcv(AACC, AA)), "time",
    overlap=1)  #remove the AA part

#WITH cm_time2long
x <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00,
        1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
)

y <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00,
        1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
)

(dat <- cm_time2long(x, y))
out <- cm_code.blank(dat, list(P=qcv(A, B), Q=qcv(B, C), R=qcv(A, B, C)),
    "variable", overlap=TRUE)
head(out, 10)
}
}
\seealso{
  \code{\link[qdap]{cm_range2long}},
  \code{\link[qdap]{cm_time2long}},
  \code{\link[qdap]{cm_df2long}},
  \code{\link[qdap]{cm_code.overlap}},
  \code{\link[qdap]{cm_code.combine}},
  \code{\link[qdap]{cm_code.exclude}},
  \code{\link[qdap]{cm_code.transform}}
}
\keyword{co-occurrence}

