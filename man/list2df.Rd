\name{list2df}
\alias{list2df}
\title{List to Dataframe}
\usage{
  list2df(list.object, col1 = "X1", col2 = "X2")
}
\arguments{
  \item{list.object}{A named \code{\link[base]{list}} of
  vectors..}

  \item{col1}{Name for column 1 (the vector elements).}

  \item{col2}{Name for column 2 (the names of the
  vectors).}
}
\value{
  Returns a dataframe with two columns.
}
\description{
  Convert a named list of vectors to a dataframe.
}
\details{
  generally an internal function used for reshaping data.
}
\examples{
lst1 <- list(x=c("foo", "bar"), y=1:5)
list2df(lst1)

lst2 <- list(a=qcv(hello, everybody), b = mtcars[1:6, 1])
list2df(lst2, "col 1", "col 2")
}
\keyword{collapse}
\keyword{list}

