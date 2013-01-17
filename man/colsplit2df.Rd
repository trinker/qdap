\name{colsplit2df}
\alias{colsplit2df}
\alias{lcolsplit2df}
\title{Wrapper for colSplit that Returns Dataframe(s)}
\usage{
  colsplit2df(dataframe, splitcol = 1, new.names = NULL,
    sep = ".", keep.orig = FALSE)

  lcolsplit2df(qdap.list, keep.orig = FALSE)
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

  \item{keep.orig}{logical.  If TRUE the original pasted
  column will be retained as well.}

  \item{qdap.list}{A qdap list object that contains
  dataframes with a leading \code{paste2} column.}
}
\value{
  \code{colsplit2df} - returns a dataframe with the
  \code{paste2} column split into new columns.

  \code{lcolsplit2df} - returns a list of dataframes with
  the \code{paste2} column split into new columns.
}
\description{
  \code{colsplit2df} - Wrapper for \code{colSplit} that
  returns a dataframe.

  \code{lcolsplit2df} - Wrapper for \code{colsplit2df}
  designed for qdap lists that returns a list dataframes.
}
\note{
  \code{lcolsplit2df} is a convenience function that is
  less flexible than \code{colsplit2df} but operates on
  multiple dataframes at once.
}
\section{Warning}{
  This wil strip the class of the qdap object.
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

(x <- question_type(DATA$state, list(DATA$sex, DATA$adult)))
lapply(x, head)
lcolsplit2df(x)
}
}
\seealso{
  \code{\link{colSplit}}, \code{\link{paste2}}
}
\keyword{column-split}

