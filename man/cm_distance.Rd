\name{cm_distance}
\alias{cm_distance}
\title{Distance Matrix Between Codes}
\usage{
  cm_distance(dataframe, pvals = c(TRUE, FALSE),
    replications = 8000, parallel = TRUE,
    extended.output = TRUE, time.var = TRUE,
    code.var = "code", causal = FALSE, start.var = "start",
    end.var = "end", cores = detectCores()/2)
}
\arguments{
  \item{dataframe}{A data frame from the cm_x2long family
  (\code{cm_range2long}; \code{cm_df2long};
  \code{cm_time2long}).}

  \item{pvals}{A logical vector of length 1 or 2.  If
  element 2 is blank element 1 will be recycled.  If the
  first element is \code{TRUE} pvalues will be calculated
  for the combined (main) output for all repeated measures
  from simulated resampling of the data.  If the second
  element is \code{TRUE} pvalues will be calculated for the
  individual (extended) repeated measures output from
  simulated resampling of the data.  Default is to
  calculate pvalues for the main output but not for the
  extended output.  This process involves multiple
  resampling of the data and is a time consuming process.
  It may take from a few minutes to days to calculate the
  pvalues depending on the number of all codes use, number
  of different codes and number of \code{replications}.}

  \item{replications}{An integer value for the number of
  replications used in resampling the data if any
  \code{pvals} is \code{TRUE}.  It is recommended that this
  value be no lower than 7500. Failure to use enough
  replications may result in unreliable pvalues.}

  \item{parallel}{logical.  If \code{TRUE} runs the
  \code{cm_distance} on multiple cores (if available).
  This will generally be effective with most data sets,
  given there are repeated measures, because of the large
  number of simulations.  Default uses 1/2 of the available
  cores.}

  \item{extended.output}{logical.  If \code{TRUE} the
  information on individual repeated measures is calculated
  in addition to the aggregated repeated measures results
  for the main output.}

  \item{time.var}{An optional variable to split the
  dataframe by (if you have data that is by various times
  this must be supplied).}

  \item{code.var}{The name of the code variable column.
  Defaults to "codes" as out putted by x2long family.}

  \item{causal}{logical.  If \code{TRUE} measures the
  distance between x and y given that x must proceed y.
  That is, only those y_i that begin after the x_i will be
  considered, as it is assumed that x precedes y.}

  \item{start.var}{The name of the start variable column.
  Defaults to "start" as out putted by x2long family.}

  \item{end.var}{The name of the end variable column.
  Defaults to "end" as out putted by x2long family.}

  \item{cores}{An integer value describing the number of
  cores to use if \code{parallel = TRUE}.  Default is to
  use half of the available cores.}
}
\value{
  An object of the class \code{"cm_distance"}.  This is a
  list with the following components:

  \item{pvals}{A logical indication of whether pvalues were
  calculated} \item{replications}{Integer value of number
  of replications used} \item{extended.output}{An optional
  list of individual repeated measures information}
  \item{main.output}{A list of aggregated repeated measures
  information}

  Within the lists of extended.output and list of the
  main.output are the following items:

  \item{mean}{A distance matrix of average distances
  between codes} \item{sd}{A matrix of standard deviations
  of distances between codes} \item{n}{A matrix of counts
  of distances between codes} \item{stan.mean}{A matrix of
  standardized values of distances between codes.  The
  closer a value is to zero the closer two codes relate.}
  \item{pvalue}{A n optional matrix of simulated pvalues
  associated with the mean distances}
}
\description{
  Generate distance measures to ascertain a mean distance
  measure between codes.
}
\section{Warning}{
  Too few replications in simulating pvalues will yield
  unstable results.  It is recommended the researcher uses
  > 7500 replications.  See: Mundform, Schaffer, Kim, Shaw,
  Thongteeraparp, & Supawan (2011)
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

(dat <- cm_2long(foo, foo2, v.name = "time"))
plot(dat)
(out <- cm_distance(dat, replications=10))
names(out)
names(out$main.output)
out$main.output
out$extended.output
print(out, new.order = c(3, 2, 1))
print(out, new.order = 3:2)
#========================================
x <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 6.32:7.00, 9.00,
        10.00:11.00, 59.56"),
    B = qcv(terms = "3.01:3.02, 5.01,  19.00, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.32:7.00, 9.00, 17.01")
)
(dat <- cm_2long(x))
plot(dat)
(a <- cm_distance(dat, causal=TRUE, replications=10))
}
}
\references{
  Mundform, D. J., Schaffer, J., Kim, M., Shaw, D.,
  Thongteeraparp, A., & Supawan, P. (2011) Number of
  Replications Required in Monte Carlo Simulation Studies:
  A Synthesis of Four Studies, Journal of Modern Applied
  Statistical Methods, 10(1), 19-28.

  \url{http://stats.stackexchange.com/a/22333/7482}
}
\seealso{
  \code{\link[qdap]{print.cm_distance}}
}
\keyword{association}
\keyword{codes}
\keyword{distance}

