\name{ngrams}
\alias{ngrams}
\title{Generate ngrams}
\usage{
  ngrams(text.var, grouping.var = NULL, n = 2)
}
\arguments{
  \item{text.var}{The text variable}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one output for all text.  Also takes a single
  grouping variable or a list of 1 or more grouping
  variables.}

  \item{n}{The max number of grams calculate.}
}
\value{
  Returns a list of: \item{raw}{A list of pasted single
  vectors of the ngrams per row.} \item{group}{A list of
  pasted vectors of ngrams grouped by grouping.var.}
  \item{unlist1}{A list of a single vector of pasted ngrams
  per grouping.var in the order used.} \item{unlist2}{A
  list of a single vector of pasted ngrams per grouping.var
  in alphabetical order.} \item{group_n}{A list of a list
  of vectors of ngrams per grouping.var & n (not pasted).}
  \item{all}{A single vector of pasted ngrams sorted
  alphabetically.} \item{all_n}{A single vector of ngrams
  sorted alphabetically (not pasted).}
}
\description{
  Transcript apply ngrams.
}
\examples{
\dontrun{
ngrams(DATA$state, DATA$person, 2)
ngrams(DATA$state, DATA$person, 3)
ngrams(DATA$state, , 3)
with(mraja1, ngrams(dialogue, list(sex, fam.aff), 3))
}
}
\keyword{ngram}

