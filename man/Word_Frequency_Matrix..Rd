\name{Filter.wfm}
\alias{Filter.wfm}
\title{Word Frequency Matrix}
\usage{
\method{Filter}{wfm}(x, min = 1, max = Inf, count.apostrophe = TRUE,
  stopwords = NULL, ...)
}
\arguments{
  \item{x}{A \code{\link[qdap]{wfm}} object.}

  \item{min}{Minimum word length.}

  \item{max}{Maximum word length.}

  \item{count.apostrophe}{logical.  If \code{TRUE}
  apostrophes are counted as characters.}

  \item{stopwords}{An optional vector of stop words to
  remove.}

  \item{\ldots}{ignored. wfm Method for Filter}
}
\description{
\code{Filter.wfm} - Filter words from a wfm that meet
max/min word length criteria.
}

