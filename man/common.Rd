\name{common}
\alias{common}
\alias{common.default}
\alias{common.list}
\title{Find Common Words Between Groups}
\usage{
  common(word.list, ...)

  \method{common}{list} (word.list, ..., overlap = "all",
    equal.or = "more")

  \method{common}{default} (..., overlap = "all",
    equal.or = "equal")
}
\arguments{
  \item{word.list}{A list of names chacter vectors.}

  \item{overlap}{Minimum/exact amount of overlap.}

  \item{equal.or}{A character vector of c(\code{"equal"},
  \code{"greater"}, \code{"more"}, \code{"less"}).}

  \item{\dots}{In liu of word.list the user may input n
  number of character vectors.}
}
\value{
  Returns a dataframe of all words that match the criteria
  set by \code{overlap} and \code{equal.or}.

  \code{NULL}

  \code{NULL}
}
\description{
  Find common words between grouping variables (e.g.
  people).
}
\examples{
\dontrun{
a <- c("a", "cat", "dog", "the", "the")
b <- c("corn", "a", "chicken", "the")
d <- c("house", "feed", "a", "the", "chicken")
common(a, b, d, overlap=2)
common(a, b, d, overlap=3)

r <- list(a, b, d)
common(r)
common(r, overlap=2)

common(word_list(DATA$state, DATA$person)$cwl, overlap = 2)
}
}

