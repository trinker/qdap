\name{bag_o_words}
\alias{bag_o_words}
\alias{breaker}
\alias{word_split}
\title{Bag of Words}
\usage{
  bag_o_words(text.var, apostrophe.remove = FALSE, ...)

  breaker(text.var)

  word_split(text.var)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{apostrophe.remove}{logical.  If \code{TRUE} removes
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
  \code{bag_o_words} - Reduces a text column to a bag of
  words.

  \code{breaker} - Reduces a text column to a bag of words
  and qdap recognized end marks.

  \code{word_split} - Reduces a text column to a list of
  vectors of bag of words and qdap recognized end marks
  (i.e., \code{".", "!", "?", "*", "-"}).
}
\examples{
\dontrun{
bag_o_words("I'm going home!")
bag_o_words("I'm going home!", apostrophe.remove = TRUE)

bag_o_words(DATA$state)
by(DATA$state, DATA$person, bag_o_words)
lapply(DATA$state,  bag_o_words)

breaker(DATA$state)
by(DATA$state, DATA$person, breaker)
lapply(DATA$state,  breaker)

word_split(c(NA, DATA$state))
}
}
\keyword{bag-of-words}

