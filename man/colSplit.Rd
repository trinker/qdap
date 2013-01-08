\name{colSplit}
\alias{colSplit}
\title{Separate a Column Pasted by paste2}
\usage{
  colSplit(column, col.sep = ".", name.sep = "&")
}
\arguments{
  \item{column}{The pasted vector.}

  \item{col.sep}{The column separator used in
  \code{paste2}.}

  \item{name.sep}{Name separator used in the column
  (internal use within \code{colsplit2df}).}
}
\value{
  Returns a dataframe of split columns.
}
\description{
  Separates a \code{paste2} column into separate columns.
}
\examples{
\dontrun{
(foo <- paste2(CO2[, 1:3]))
colSplit(foo)
(bar <- paste2(mtcars[, 1:3], sep="|"))
colSplit(bar, col.sep = "|")
}
}
\seealso{
  \code{\link{colsplit2df}}, \code{\link{paste2}}
}
\keyword{column-split}

