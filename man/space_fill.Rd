\name{space_fill}
\alias{space_fill}
\title{Replace Spaces}
\usage{
  space_fill(text.var, terms, sep = "~~", rm.extra = TRUE,
    ignore.case = TRUE, fixed = FALSE, ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{terms}{A character vector of grouped word terms to
  insert a new separating/space character.}

  \item{sep}{A character string to separate the terms.}

  \item{rm.extra}{logical.  Should trailing, leading and >
  1 continuous white spaces be removed?}

  \item{ignore.case}{logical.  If FALSE, the pattern
  matching is case sensitive and if TRUE, case is ignored
  during matching.}

  \item{fixed}{logical. If TRUE, pattern is a string to be
  matched as is. Overrides all conflicting arguments.}

  \item{\ldots}{Other arguments passed to
  \code{\link[base]{gsub}}.}
}
\value{
  Returns a character vector with trailing and/or leading
  spaces.
}
\description{
  Replace spaces in words groups that should be grouped
  together.
}
\details{
  \code{link[qdap]{space_fill}} is useful for keeping
  grouped words together.  Many functions in qdap take a
  \code{char.keep} or \code{char2space} argument.  This can
  be used to prepare multi word phrases (e.g., proper
  nouns) as a single unit.
}
\note{
  \code{link[qdap]{strip}} by default does not remove the
  double tilde \code{"~~"} character.
}
\examples{
\dontrun{
x <- c("I want to hear the Dr. Martin Luther King Jr. speech.",
    "I also want to go to the white House to see President Obama speak.")

keeps <- c("Dr. Martin Luther King Jr.", "The White House", "President Obama")
space_fill(x, keeps)
strip(space_fill(x, keeps))
}
}

