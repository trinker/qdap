\name{htruncdf}
\alias{htruncdf}
\alias{qview}
\alias{truncdf}
\title{Dataframe Viewing}
\usage{
  htruncdf(dataframe, n = 10, width = 10, ...)

  truncdf(dataframe, end = 10, begin = 1)

  qview(dataframe, ...)

  htruncdf(dataframe, n = 10, width = 10, ...)

  truncdf(dataframe, end = 10, begin = 1)

  qview(dataframe, ...)
}
\arguments{
  \item{dataframe}{A data.frame object.}

  \item{n}{Number of rows to display.}

  \item{width}{The width of the columns to be displayed.}

  \item{end}{The last element to be displayed.}

  \item{begin}{The first element to be displayed.}

  \item{\ldots}{Other arguments passed to
  \code{\link[utils]{head}}.}

  \item{dataframe}{A data.frame object.}

  \item{n}{Number of rows to display.}

  \item{width}{The width of the columns to be displayed.}

  \item{end}{The last element to be displayed.}

  \item{begin}{The first element to be displayed.}

  \item{\ldots}{Other arguments passed to
  \code{\link[utils]{head}}.}
}
\value{
  \code{htrundf} - returns n number of rows of a truncated
  dataframe.

  \code{trundf} - returns a truncated dataframe.

  \code{qview} - returns a dataframe head with summary
  statistics.

  \code{htrundf} - returns n number of rows of a truncated
  dataframe.

  \code{trundf} - returns a truncated dataframe.

  \code{qview} - returns a dataframe head with summary
  statistics.
}
\description{
  \code{htruncdf} - Convenience function to view the head
  of a truncated dataframe.

  \code{truncdf} - Convenience function to view a truncated
  dataframe.

  \code{qview} - Convenience function to view a summary and
  head of a dataframe.

  \code{htruncdf} - Convenience function to view the head
  of a truncated dataframe.

  \code{truncdf} - Convenience function to view a truncated
  dataframe.

  \code{qview} - Convenience function to view a summary and
  head of a dataframe.
}
\examples{
\dontrun{
htruncdf(raj)
htruncdf(raj, 20)
htruncdf(raj, ,20)
truncdf(raj)
truncdf(raj, 40)
qview(raj)
qview(CO2)
}
\dontrun{
htruncdf(raj)
htruncdf(raj, 20)
htruncdf(raj, ,20)
truncdf(raj)
truncdf(raj, 40)
qview(raj)
qview(CO2)
}
}
\seealso{
  \code{\link[utils]{head}}

  \code{\link[utils]{head}}
}

