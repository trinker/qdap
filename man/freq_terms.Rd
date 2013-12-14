\name{freq_terms}
\alias{freq_terms}
\title{Find Frequent Terms}
\usage{
  freq_terms(text.var, top = 20, at.least = 1,
    stopwords = NULL, extend = TRUE, ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{top}{Top number of terms to show.}

  \item{at.least}{An interger indicating at least how many
  letters a word must be to be included in the output.}

  \item{stopwords}{A character vector of words to remove
  from the text.  qdap has a number of data sets that can
  be used as stop words including: \code{Top200Words},
  \code{Top100Words}, \code{Top25Words}.  For the tm
  package's traditional English stop words use
  \code{tm::stopwords("english")}.}

  \item{extend}{logical.  If \code{TRUE} the \code{top}
  argument is extended to any word that has the same
  frequency as the \code{top} word.}

  \item{\ldots}{Other arguments passed to
  \code{\link[qdap]{all_words}}.}
}
\value{
  Returns a dataframe with the top occurring words.
}
\description{
  Find the most frequently occuring terms in a text vector.
}
\examples{
\dontrun{
freq_terms(DATA$state, 5)
freq_terms(DATA$state)
freq_terms(DATA$state, extend = FALSE)
freq_terms(DATA$state, at.least = 4)
(x <- freq_terms(pres_debates2012$dialogue, stopwords = Top200Words))
plot(x)
}
}
\seealso{
  \code{\link[qdap]{word_list}},
  \code{\link[qdap]{all_words}}
}
\keyword{frequent_terms}

