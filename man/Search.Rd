\name{Search}
\alias{Search}
\title{Search Columns of a Data Frame}
\usage{
  Search(dataframe, term, column.name = NULL,
    max.distance = 0.02, ...)
}
\arguments{
  \item{dataframe}{A dataframe object to search.}

  \item{term}{A character vector term to search for.}

  \item{column.name}{Optional column of the data frame to
  search (character name or or integer index).}

  \item{max.distance}{Maximum distance allowed for a match.
  Expressed either as integer, or as a fraction of the
  pattern length times the maximal transformation cost
  (will be replaced by the smallest integer not less than
  the corresponding fraction).}

  \item{\ldots}{Other arguments passed to \code{agrep}.}
}
\value{
  Returns the rows of the data frame that match the search
  term.
}
\description{
  Find terms located in columns of a data frame.
}
\examples{
\dontrun{
SampDF <- data.frame("islands"=names(islands)[1:32],mtcars)

Search(SampDF, "Cuba", "islands")
Search(SampDF, "New", "islands")
Search(SampDF, "Ho")
Search(SampDF, "Ho", max.distance = 0)
Search(SampDF, "Axel Heiberg")
Search(SampDF, 19) #too much tolerance in max.distance
Search(SampDF, 19, max.distance = 0)
Search(SampDF, 19, "qsec", max.distance = 0)
}
}
\keyword{search}

