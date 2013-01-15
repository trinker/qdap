\name{spaste}
\alias{spaste}
\title{Add Leading/Trailing Spaces}
\usage{
  spaste(terms, trailing = TRUE, leading = TRUE)
}
\arguments{
  \item{terms}{A character vector of terms to instert
  trailing and/or leading spaces.}

  \item{leading}{logical.  If TRUE inserts a leading space
  in the terms.}

  \item{trailing}{logical.  If TRUE inserts a trailing
  space in the terms.}
}
\value{
  Returns a character vector with trailing and/or leading
  spaces.
}
\description{
  Adds trailing and/or leading spaces to a vector of terms.
}
\examples{
\dontrun{
spaste(Top25Words)
spaste(Top25Words, FALSE)
spaste(Top25Words, ,FALSE)
}
}

