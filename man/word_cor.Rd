\name{word_cor}
\alias{word_cor}
\title{Find Correlated Words}
\usage{
  word_cor(text.var, grouping.var = ID(text.var), word,
    cor = 0.7, values = TRUE)
}
\arguments{
  \item{text.var}{The text variable (or an object from
  \code{\link[qdap]{pos}}, \code{\link[qdap]{pos_by}} or
  \code{\link[qdap]{formality}}.  Passing the later three
  object will greatly reduce run time.}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{word}{The word to find associated words for.}

  \item{cor}{The minimum correlation level find associated
  words for.}

  \item{values}{logical.  If True returns the named
  correlates (names are the words).  If \code{FALSE} only
  the associated words are returned.}
}
\value{
  Returns a vector of associated words.
}
\description{
  Find associated words within grouping variable(s).
}
\examples{
x <- factor(with(rajSPLIT, paste(act, pad(TOT(tot)), sep = "|")))
word_cor(rajSPLIT$dialogue, x, "romeo", .45)
word_cor(rajSPLIT$dialogue, x, "love", .5)
with(rajSPLIT, word_cor(dialogue, list(person, act), "hate"))
}
\seealso{
  \code{\link[tm]{findAssocs}},
  \code{\link[qdap]{word_associate}}
}
\keyword{association}
\keyword{correaltion,}

