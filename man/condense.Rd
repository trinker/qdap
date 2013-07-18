\name{condense}
\alias{condense}
\title{Condense Dataframe Columns}
\usage{
  condense(dataframe, sep = ", ")
}
\arguments{
  \item{dataframe}{A dataframe with a column(s) that are a
  list of vectors.}

  \item{sep}{A character string to separate the terms.}
}
\value{
  Returns a dataframe with condensed columns that can be
  wrote to csv/xlsx.
}
\description{
  Condense dataframe columns that are a list of vectors to
  a single vector of strings.
}
\examples{
\dontrun{
library(qdap)
poldat <- with(DATA, polarity(state, person))
write.csv(x = condense(poldat$all), file = "foo.csv")
}
}
\seealso{
  \code{\link[qdap]{mcsv_w}}
}

