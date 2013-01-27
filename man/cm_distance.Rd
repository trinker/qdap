\name{cm_distance}
\alias{cm_distance}
\title{Distance Matrix Between Codes}
\usage{
  cm_distance(dataframe, time.var = NULL, parallel = FALSE,
    code.var = "code", causal = FALSE, start.var = "start",
    end.var = "end", mean.digits = 2, sd.digits = 2,
    stan.digits = 2)
}
\arguments{
  \item{dataframe}{A data frame from the cm_x2long family
  (\code{cm_range2long}; \code{cm_df2long};
  \code{cm_time2long}).}

  \item{time.var}{An optional variable to split the
  dataframe by (if you have data that is by various times
  this must be supplied).}

  \item{parallel}{logical.  If TRUE runs the cm_distance on
  multiple cores. This is effective with larger data sets
  but may actually be slower with smaller data sets.}

  \item{code.var}{The name of the code variable column.
  Defaults to "codes" as out putted by x2long family.}

  \item{causal}{logical.  If TRUE measures the distance
  between x and y given that x must proceed y.}

  \item{start.var}{The name of the start variable column.
  Defaults to "start" as out putted by x2long family.}

  \item{end.var}{The name of the end variable column.
  Defaults to "end" as out putted by x2long family.}

  \item{mean.digits}{The number of digits to be displayed
  in the mean matrix.}

  \item{sd.digits}{The number of digits to be displayed in
  the sd (standard deviation) matrix.}

  \item{stan.digits}{The number of digits to use in the
  standardized mean difference matrix.}
}
\value{
  An object of the class cm.dist.  This is a list of n
  lists with the following components per each list
  (time.var): \item{mean}{A distance matrix of average
  distances between codes} \item{sd}{A matrix of standard
  deviations of distances between codes} \item{n}{A matrix
  of counts of distances between codes} \item{combined}{A
  matrix of combined mean, sd and n of distances between
  codes} \item{standardized}{A matrix of standardized
  values of distances between codes.  The closer a value is
  to zero the closer two codes relate.}
}
\description{
  Generate distance measures to ascertain a mean distance
  measure between codes.
}
\examples{
\dontrun{
foo <- list(
    AA = qcv(terms="02:03, 05"),
    BB = qcv(terms="1:2, 3:10"),
    CC = qcv(terms="1:9, 100:150")
)

foo2  <- list(
    AA = qcv(terms="40"),
    BB = qcv(terms="50:90"),
    CC = qcv(terms="60:90, 100:120, 150"),
    DD = qcv(terms="")
)

(dat <- cm_range2long(foo, foo2, v.name = "time"))
(out <- cm_distance(dat, time.var = "time", causal=TRUE))
names(out)
names(out$foo2)
out$foo2
#========================================
x <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 6.32:7.00, 9.00,
        10.00:11.00, 59.56"),
    B = qcv(terms = "3.01:3.02, 5.01,  19.00, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.32:7.00, 9.00, 17.01")
)
(dat <- cm_time2long(x))
gantt_wrap(dat, "code", border.color = "black", border.size = 5,
    sig.dig.line.freq = -2)
(a <- cm_distance(dat))
names(a)
names(a$dat)
a$dat
}
}
\keyword{distance}

