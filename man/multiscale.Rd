\name{multiscale}
\alias{multiscale}
\title{Nested Standardization}
\usage{
  multiscale(numeric.var, grouping.var,
    original_order = TRUE, digits = 2)
}
\arguments{
  \item{numeric.var}{A numeric variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one output for all text.  Also takes a single
  grouping variable or a list of 1 or more grouping
  variables.}

  \item{original_order}{logical.  IF TRUE orders by the
  original order.  If FALSE orders by group.}

  \item{digits}{Integer; number of decimal places to
  round.}
}
\value{
  Returns a list of two: \item{SCALED_OBSERVATIONS}{A
  dataframe of scaled observations at level one and two of
  the nesting with possible outliers.}
  \item{DESCRIPTIVES_BY_GROUP}{A data frame of descriptives
  by group.}
}
\description{
  Standardize within a subgroup and then within a group.
}
\examples{
\dontrun{
dat <- with(mraja1spl, word_stats(dialogue, list(person, sex, fam.aff)))
head(colsplit2df(dat$ts))
with(colsplit2df(dat$ts), multiscale(word.count, person))
with(colsplit2df(dat$ts), multiscale(word.count, list(fam.aff, sex)))
with(colsplit2df(dat$ts), multiscale(word.count, list(fam.aff, sex),
    original_order = FALSE))
}
}
\seealso{
  \code{\link{scale}}
}
\keyword{scale}

