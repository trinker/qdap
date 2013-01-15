\name{outlier.detect}
\alias{outlier.detect}
\title{Detect Outliers in Text}
\usage{
  outlier.detect(text.var, grouping.var = NULL,
    FUN = word.count, scale.by = "grouping")
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{FUN}{A word function with a numeric vector output
  (e.g. \code{syllable.sum}, \code{character.count} or
  \code{word.count}).}

  \item{scale.by}{A character string indicating which
  dimensions to scale by. One of \code{"all"},
  \code{"grouping"}, or \code{"both"}.  Default NULL scales
  by all.}
}
\value{
  Returns a dataframe with possible outliers.
}
\description{
  Locate possible outliers for text variables given numeric
  word function.
}
\examples{
\dontrun{
with(DATA, outlier.detect(state))
with(DATA, outlier.detect(state, FUN = character.count))
with(DATA, outlier.detect(state, person, FUN = character.count))
with(DATA, outlier.detect(state, list(sex, adult), FUN = character.count))
with(DATA, outlier.detect(state, FUN = syllable.sum))
htruncdf(with(raj, outlier.detect(dialogue, person)), 15, 45)
}
}

