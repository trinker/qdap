\name{colsplit2df}
\alias{colsplit2df}
\title{Wrapper for colSplit that Returns a Dataframe}
\usage{
  colsplit2df(dataframe, splitcol = 1, new.names = NULL,
    sep = ".", keep.orig = FALSE)
}
\arguments{
  \item{dataframe}{A dataframe with a column that has been
  pasted together.}

  \item{splitcol}{The name of the column that has been
  pasted together.}

  \item{new.names}{A character vector of new names to
  assign to the columns. Default attempts to extract the
  original names before the paste.}

  \item{sep}{The character that used in \code{paste2} to
  paste the columns.}

  \item{orig.keep}{logical.  If TRUE the original pasted
  column will be retained as well.}
}
\value{
  Returns a dataframe with the pasted column cplit into new
  columns.
}
\description{
  Wrapper for \code{colSplit} that returns a dataframe.
}
\examples{
\dontrun{
CO2$`Plant&Type&Treatment` <- paste2(CO2[, 1:3])
CO2 <- CO2[, -c(1:3)]
head(colsplit2df(CO2, 3))
head(colsplit2df(CO2, 3, qcv(A, B, C)))
head(colsplit2df(CO2, 3, qcv(A, B, C), keep.orig=TRUE))
head(colsplit2df(CO2, "Plant&Type&Treatment"))
CO2 <- datasets::CO2
}
}
\seealso{
  \code{\link{colSplit}}, \code{\link{paste2}}
}
\keyword{column-split}

