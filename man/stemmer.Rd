\name{stemmer}
\alias{stem.words}
\alias{stem2df}
\alias{stemmer}
\title{Stem Text}
\usage{
  stemmer(text.var, rm.bracket = TRUE, capitalize = TRUE,
    warn = TRUE, ...)

  stem.words(...)

  stem2df(dataframe, text.var, stem.name = NULL, ...)
}
\arguments{
  \item{text.var}{The text variable.  In \code{stemmer}
  this is a vector text string. For \code{stem2df} this is
  a character vector of length one naming the text column.}

  \item{rm.bracket}{logical.  If TRUE brackets are removed
  from the text.}

  \item{capitalize}{logical.  If TRUE selected terms are
  capitalized}

  \item{warn}{logical.  If TRUE warns about rows not ending
  with standard qdap punctuation endmarks.}

  \item{\dots}{Various: \cr \emph{\code{stemmer} - Other
  arguments passed to \code{\link[qdap]{capitalizer}}} \cr
  \emph{\code{stem.words} - Words or terms.} \cr
  \emph{\code{stem2df} - Other arguments passed to
  \code{\link[qdap]{stemmer}}}}

  \item{dataframe}{A dataframe object.}

  \item{stem.name}{A character vector of length one for the
  stemmed column.  If \code{NULL} defaults to
  \code{"stem.text"}.}
}
\value{
  \code{stemmer } - returns a character vector with stemmed
  text.

  \code{stem.words} - returns a dataframe with a character
  vector with.

  \code{stem2df} - returns a dataframe with a character
  vector with stemmed text.
}
\description{
  \code{stemmer} - Stems a vector of text strings.

  \code{stem.words} - Wrapper for stemmer that stems a
  vector of words.

  \code{stem2df} - Wrapper for stemmer that stems a vector
  of text strings and returns a dataframe with the vector
  added..
}
\examples{
\dontrun{
#stemmer EXAMPLE:
stemmer(DATA$state)
stemmer(raj$dialogue)

#stem.words EXAMPLE:
stem.words(doggies, jumping, swims)

#stem2df EXAMPLE:
stem2df(DATA, "state", "new")
}
}
\seealso{
  \code{\link[qdap]{capitalizer}}
}
\keyword{stem}

