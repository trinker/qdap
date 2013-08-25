\name{stopwords}
\alias{stopwords}
\title{Remove Stopwords}
\usage{
  stopwords(textString,
    stopwords = qdapDictionaries::Top25Words,
    unlist = FALSE, separate = TRUE, strip = FALSE,
    unique = FALSE, char.keep = NULL, names = FALSE,
    ignore.case = TRUE, apostrophe.remove = FALSE, ...)
}
\arguments{
  \item{textString}{A character string of text or a vector
  of character strings.}

  \item{stopwords}{A character vector of words to remove
  from the text.  qdap has a number of data sets that can
  be used as stopwords including: \code{Top200Words},
  \code{Top100Words}, \code{Top25Words}.  For the tm
  package's traditional English stop words use
  \code{tm::stopwords("english")}.}

  \item{unlist}{logical.  If \code{TRUE} unlists into one
  vector.  General use intended for when separate is
  \code{FALSE}.}

  \item{separate}{logical.  If \code{TRUE} separates
  sentences into words. If \code{FALSE} retains sentences.}

  \item{strip}{logical.  IF \code{TRUE} strips the text of
  all punctuation except apostrophes.}

  \item{unique}{logical.  If \code{TRUE} keeps only unique
  words (if unlist is \code{TRUE}) or sentences (if unlist
  is \code{FALSE}).  General use intended for when unlist
  is \code{TRUE}.}

  \item{char.keep}{If strip is \code{TRUE} this argument
  provides a means of retaining supplied character(s).}

  \item{names}{logical.  If \code{TRUE} will name the
  elements of the vector or list with the original
  \code{textString}.}

  \item{ignore.case}{logical.  If \code{TRUE} stop words
  will be removed regardless of case.  Additionally, case
  will be stripped from the text.  If \code{FALSE}
  stopwords removal is contingent upon case.  Additionally,
  case is not stripped.}

  \item{apostrophe.remove}{logical.  If \code{TRUE} removes
  apostrophe's from the output.}

  \item{\ldots}{further arguments passed to
  \code{\link[qdap]{strip}} function.}
}
\value{
  Returns a vector of sentences, vector of words, or
  (default) a list of vectors of words with stop words
  removed.  Output depends on supplied arguments.
}
\description{
  Transcript apply the removal of stopwords.
}
\examples{
\dontrun{
stopwords(DATA$state)
stopwords(DATA$state, tm::stopwords("english"))
stopwords(DATA$state, Top200Words)
stopwords(DATA$state, Top200Words, strip = TRUE)
stopwords(DATA$state, Top200Words, separate = FALSE)
stopwords(DATA$state, Top200Words, separate = FALSE, ignore.case = FALSE)
stopwords(DATA$state, Top200Words, unlist = TRUE)
stopwords(DATA$state, Top200Words, unlist = TRUE, strip=TRUE)
stopwords(DATA$state, Top200Words, unlist = TRUE, unique = TRUE)
}
}
\seealso{
  \code{\link[qdap]{strip}},
  \code{\link[qdap]{bag.o.words}},
  \code{\link[tm]{stopwords}}
}
\keyword{stopwords}

