\name{bag.o.words}
\alias{bag.o.words}
\alias{breaker}
\alias{word.split}
\title{Bag of Words}
\usage{
  bag.o.words(text.var, apostrophe.remove = FALSE, ...)

  breaker(text.var)

  word.split(text.var)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{apostrophe.remove}{logical.  If TRUE removes
  apostrophe's from the output.}

  \item{\ldots}{further arguments passed to strip
  function.}
}
\value{
  Returns a vector of striped words.

  \code{breaker} - returns a vector of striped words and
  qdap recognized endmarks (i.e., \code{".", "!", "?", "*",
  "-"}).
}
\description{
  \code{bag.o.words} - Reduces a text column to a bag of
  words.

  \code{breaker} - Reduces a text column to a bag of words
  and qdap recognized end marks.

  \code{word.split} - Reduces a text column to a list of
  vectors of bag of words and qdap recognized end marks
  (i.e., \code{".", "!", "?", "*", "-"}).
}
\examples{
\dontrun{
bag.o.words("I'm going home!")
bag.o.words("I'm going home!", apostrophe.remove = TRUE)

bag.o.words(DATA$state)
by(DATA$state, DATA$person, bag.o.words)
lapply(DATA$state,  bag.o.words)

breaker(DATA$state)
by(DATA$state, DATA$person, breaker)
lapply(DATA$state,  breaker)

word.split(c(NA, DATA$state))
}
}
\keyword{bag-of-words}

