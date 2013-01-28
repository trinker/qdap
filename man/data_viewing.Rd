\name{htruncdf}
\alias{htruncdf}
\alias{ltruncdf}
\alias{qview}
\alias{truncdf}
\title{Dataframe Viewing}
\usage{
  htruncdf(dataframe, n = 10, width = 10, ...)

  truncdf(dataframe, end = 10, begin = 1)

  ltruncdf(dat.list, n = 6, width = 10, ...)

  qview(dataframe, ...)
}
\arguments{
  \item{dataframe}{A data.frame object.}

  \item{n}{Number of rows to display.}

  \item{width}{The width of the columns to be displayed.}

  \item{end}{The last character to be displayed (width).}

  \item{begin}{The first character to be displayed
  (width).}

  \item{\ldots}{Other arguments passed to
  \code{\link[qdap]{htruncdf}} (\code{\link[qdap]{qview}};
  \code{\link[qdap]{ltruncdf}}) or
  \code{\link[utils]{head}}
  (\code{\link[qdap]{htruncdf}}).}

  \item{dat.list}{A list of data.frame objects.}
}
\value{
  \code{htrundf} - returns n number of rows of a truncated
  dataframe.

  \code{trundf} - returns a truncated dataframe.

  \code{ltruncdf} - returns a list of n number of rows of a
  truncated dataframes.

  \code{qview} - returns a dataframe head with summary
  statistics.
}
\description{
  \code{htruncdf} - Convenience function to view the head
  of a truncated dataframe.

  \code{truncdf} - Convenience function to view a truncated
  dataframe.

  \code{ltruncdf} - Convenience function to view the head
  of a list of truncated dataframes.

  \code{qview} - Convenience function to view a summary and
  head of a dataframe.
}
\examples{
\dontrun{
htruncdf(raj)
htruncdf(raj, 20)
htruncdf(raj, ,20)
truncdf(raj[1:10, ])
truncdf(raj[1:10, ], 40)
qview(raj)
qview(CO2)
}
}
\seealso{
  \code{\link[utils]{head}}
}

