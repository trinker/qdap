\name{word_diff_list}
\alias{word_diff_list}
\title{Differences In Word Use Between Groups}
\usage{
  word_diff_list(text.var, grouping.var, vs.all = FALSE,
    vs.all.cut = 1, stopwords = NULL, alphabetical = FALSE,
    digits = 2)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{group.vars}{The grouping variables.  Takes a single
  grouping variable or a list of 1 or more grouping
  variables.}

  \item{vs.all}{logical. If TRUE looks at each grouping
  variable against all others ("a" vs. all comparison).  If
  FALSE looks at each "a" vs. "b", comparison (e.g. for
  groups "a", "b", and "c"; "a" vs. "b", "a" vs. "c" and
  "b" vs. "c" will be considered).}

  \item{vs.all.cut}{If vs.all.cut = TRUE this argument
  controls the number of other groups that may share a word
  (default is 1).}

  \item{stopwords}{A vector of stop words to remove.}

  \item{alphabetical}{logical. If TRUE orders the word
  lists alphabetized by word.  If FALSE order first by
  frequency and then by word.}

  \item{digits}{the number of digits to be displayed in the
  proportion column (default is 3).}
}
\value{
  An list of word data frames comparing grouping variables
  word use against one another. Eachdata frame contains
  three columns: \item{word}{The words unique to that
  group} \item{freq}{The number of times that group used
  that word} \item{prop}{The proportion of that group's
  over all word use dedicated to that particular word}
}
\description{
  Look at the differences in word uses between grouping
  variable(s).  Look at all possible "a" vs. "b"
  combinations or "a" vs. all others.
}
\examples{
\dontrun{
with(DATA, word_diff_list(text.var = state, grouping.var = list(sex, adult)))
with(DATA, word_diff_list(state, person))
with(DATA, word_diff_list(state, grouping.var = list(sex, adult),
    vs.all=TRUE, vs.all.cut=2))

with(mraja1, word_diff_list(text.var = dialogue,
    grouping.var = list(mraja1$sex, mraja1$fam.aff)))
word_diff_list(mraja1$dialogue, mraja1$person)
word_diff_list(mraja1$dialogue, mraja1$fam.aff, stopwords = Top25Words)
word_diff_list(mraja1$dialogue, mraja1$fam.aff, vs.all=TRUE, vs.all.cut=2)
}
}
\keyword{word-list}

