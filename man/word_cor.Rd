\name{word_cor}
\alias{word_cor}
\title{Find Correlated Words}
\usage{
  word_cor(text.var, grouping.var = ID(text.var), word,
    r = 0.7, values = TRUE, ...)
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

  \item{word}{The word(s) vector to find associated words
  for.}

  \item{r}{The correlation level find associated words for.
  If positive this is the minimum value, if negative this
  is the maximum value.}

  \item{values}{logical.  If \code{TRUE}returns the named
  correlates (names are the words).  If \code{FALSE} only
  the associated words are returned.}

  \item{\dots}{Other arguments passed to
  \code{\link[stats]{cor}}.}
}
\value{
  Returns a vector of associated words or correlation
  matrix if \code{r = NULL}..
}
\description{
  Find associated words within grouping variable(s).
}
\examples{
x <- factor(with(rajSPLIT, paste(act, pad(TOT(tot)), sep = "|")))
word_cor(rajSPLIT$dialogue, x, "romeo", .45)
word_cor(rajSPLIT$dialogue, x, "love", .5)

## Negative correlation
word_cor(rajSPLIT$dialogue, x, "you", -.1)
with(rajSPLIT, word_cor(dialogue, list(person, act), "hate"))

words <- c("hate", "i", "love", "ghost")
with(rajSPLIT, word_cor(dialogue, x, words, r = .5))
with(rajSPLIT, word_cor(dialogue, x, words, r = .4))

## Set `r = NULL` to get matrix between words
with(rajSPLIT, word_cor(dialogue, x, words, r = NULL))
}
\seealso{
  \code{\link[tm]{findAssocs}},
  \code{\link[qdap]{word_associate}},
  \code{\link[stats]{cor}}
}
\keyword{association}
\keyword{correaltion,}

