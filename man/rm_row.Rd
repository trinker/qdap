\name{rm_row}
\alias{rm_empty_row}
\alias{rm_row}
\title{Remove Rows That Contain Markers}
\usage{
  rm_row(dataframe, search.column, terms)

  rm_empty_row(dataframe)
}
\arguments{
  \item{dataframe}{A dataframe object.}

  \item{search.column}{Column name to search for
  markers/terms.}

  \item{terms}{Terms/markers of the rows that are to be
  removed from the dataframe.  The term/marker must appear
  at the beginning of the string and is case sensitive.}
}
\value{
  \code{rm_row} - returns a dataframe with the
  termed/markered rows removed.

  \code{rm_empty_row} - returns a dataframe with empty rows
  removed.
}
\description{
  \code{rm_row} - Remove rows from a data set that contain
  a given marker/term.

  \code{rm_empty_row} - Removes the empty rows of a data
  set that are common in reading in data (default method in
  \code{\link[qdap]{read.transcript}}).
}
\examples{
\dontrun{
#rm_row EXAMPLE:
rm_row(DATA, "person", c("sam", "greg"))
rm_row(DATA, 1, c("sam", "greg"))
rm_row(DATA, "state", c("Comp"))

#rm_empty_row EXAMPLE:
x <- matrix(rep(" ", 4), ncol =2)
dat <- DATA[, c(1, 4)]
colnames(x) <- colnames(dat)
(dat <- data.frame(rbind(dat, x)))
rm_empty_row(dat)
}
}

