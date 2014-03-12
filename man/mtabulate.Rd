\name{mtabulate}
\alias{mtabulate}
\title{Tabulate Frequency Counts for Multiple Vectors}
\usage{
mtabulate(vects)
}
\arguments{
  \item{vects}{A list of named/unnamed vectors.}
}
\value{
Returns a dataframe with frequency counts per list item
(levels unused by any vectors in the list are dropped).  If
list of vectors is named these will be the rownames of the
dataframe.
}
\description{
A wrapper for \code{\link[base]{tabulate}} that works on
multiple vectors.
}
\examples{
mtabulate(list(w=letters[1:10], x=letters[1:5], z=letters))
mtabulate(list(mtcars$cyl[1:10]))

## Dummy coding
mtabulate(mtcars$cyl[1:10])
mtabulate(CO2[, "Plant"])

dat <- data.frame(matrix(sample(qcv(A, B), 30, TRUE), ncol=3))
t(mtabulate(dat))
}
\author{
Joran Elias and Tyler Rinker <tyler.rinker@gmail.com>.
}
\references{
\url{http://stackoverflow.com/a/9961324/1000343}
}
\seealso{
\code{\link[base]{tabulate}}
}
\keyword{frequency}
\keyword{tabulate}

