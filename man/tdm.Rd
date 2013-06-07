\name{tdm}
\alias{tdm}
\title{Convert/Generate Term Document Matrix}
\usage{
  tdm(text.var, grouping.var = NULL, ...)
}
\arguments{
  \item{text.var}{The text variable or a \code{wfm}
  object.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{\ldots}{Other arguments passed to \code{wfm}.}
}
\description{
  Create term document matrices from raw text or \code{wfm}
  for use with other text analysis packages.
}
\details{
  Similar to the \code{tm} package's
  \code{\link[tm]{TermDocumentMatrix}} though not
  identical.
}
\examples{
\dontrun{
x <- wfm(DATA$state, DATA$person)
tdm(x)
library(lsa)
lsa(tdm(x), dims=dimcalc_share())
lsa(tdm(DATA$state, DATA$person), dims=dimcalc_share())
}
}

