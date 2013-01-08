\name{diversity}
\alias{diversity}
\title{Diversity Statistics}
\usage{
  diversity(text.var, grouping.var = NULL, digits = 3)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one output for all text.  Also takes a single
  grouping variable or a list of 1 or more grouping
  variables.}

  \item{digits}{Number of decimal places to round.}
}
\value{
  Returns a dataframe of various diversity related indices
  for Shannon, collision, Berger Parker and Brillouin.
}
\description{
  Transcript apply diversity statistics.
}
\examples{
\dontrun{
colsplit2df(with(mraja1spl, diversity(dialogue, list(sex, died, fam.aff))))
}
}
\keyword{diversity}

