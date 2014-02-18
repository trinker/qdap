\name{list2df}
\alias{list2df}
\alias{matrix2df}
\title{List/Matrix to Dataframe}
\usage{
list2df(list.object, col1 = "X1", col2 = "X2")

matrix2df(matrix.object, col1 = "var1")
}
\arguments{
  \item{list.object}{A named \code{\link[base]{list}} of
  vectors..}

  \item{col1}{Name for column 1 (the vector elements if
  converting a list or the rownames if converting a
  matrix).}

  \item{col2}{Name for column 2 (the names of the
  vectors).}

  \item{matrix.object}{A matrix object.}
}
\value{
\code{list2df} - Returns a dataframe with two columns.

\code{matrix2df} - Returns a dataframe.
}
\description{
\code{list2df} - Convert a named list of vectors to a
dataframe.

\code{matrix2df} - Convert a matrix to a dataframe and
conver the rownames to the first column.
}
\details{
generally an internal function used for reshaping data.
}
\examples{
lst1 <- list(x=c("foo", "bar"), y=1:5)
list2df(lst1)

lst2 <- list(a=qcv(hello, everybody), b = mtcars[1:6, 1])
list2df(lst2, "col 1", "col 2")

matrix2df(mtcars)
matrix2df(cor(mtcars))
matrix2df(matrix(1:9, ncol=3))
}
\keyword{collapse}
\keyword{list}

