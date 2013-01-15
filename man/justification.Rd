\name{left.just}
\alias{left.just}
\alias{right.just}
\title{Text Justification}
\usage{
  left.just(dataframe, column = NULL, keep.class = FALSE)

  right.just(dataframe)
}
\arguments{
  \item{dataframe}{A data.frame object with the text
  column.}

  \item{column}{The column to be justified.  If NULL all
  columns are justified.}

  \item{keep.class}{logical.  If TRUE will attempt to keep
  the original classes of the dataframe if the
  justification is not altered (i.e. numeric will not be
  honored but factor may be).}
}
\value{
  Returns a dataframe with selected text column left/right
  justified.
}
\description{
  \code{left.just} - Left justifies a text/character
  column.

  \code{right.just} - A means of undoing a left
  justification.
}
\note{
  \code{left.just} inserts spaces to achieve the
  justification.  This could interfere with analysis and
  therefore the output from \code{left.just} should only be
  used for visualization purposes, not analysis.
}
\examples{
\dontrun{
left.just(DATA)
left.just(DATA, "state")
left.just(CO2)
right.just(left.just(CO2))
}
}
\keyword{justification}
\keyword{justify,}

