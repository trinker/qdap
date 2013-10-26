\name{tdm}
\alias{dtm}
\alias{tdm}
\title{Convert/Generate Term Document Matrix or Document Term Matrix}
\usage{
  tdm(text.var, grouping.var = NULL, ...)

  dtm(text.var, grouping.var = NULL, ...)
}
\arguments{
  \item{text.var}{The text variable or a \code{wfm}
  object.}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{\ldots}{Other arguments passed to \code{wfm}.}
}
\description{
  \code{tdm} - Create term document matrices from raw text
  or \code{wfm} for use with other text analysis packages.

  \code{dtm} - Create document term matrices from raw text
  or \code{wfm} for use with other text analysis packages.
}
\details{
  Identical to the \code{tm} package's
  \code{\link[tm]{TermDocumentMatrix}}/\code{\link[tm]{DocumentTermMatrix}}.
}
\examples{
\dontrun{
dtm(DATA$state, DATA$person)
tdm(DATA$state, DATA$person)

x <- wfm(DATA$state, DATA$person)
tdm(x)
dtm(x)
library(lsa)
lsa(tdm(x), dims=dimcalc_share())
lsa(tdm(DATA$state, DATA$person), dims=dimcalc_share())

## Correspondence Analysis
library(ca)

dat <- pres_debates2012
dat <- dat[dat$person \%in\% qcv(ROMNEY, OBAMA), ]

speech <- stemmer(dat$dialogue)
mytable1 <- with(dat, tdm(speech, list(person, time), stopwords = Top25Words))

fit <- ca(mytable1)
summary(fit)
plot(fit)
plot3d.ca(fit, labels=1)


mytable2 <- with(dat, tdm(speech, list(person, time), stopwords = Top200Words))

fit2 <- ca(mytable2)
summary(fit2)
plot(fit2)
plot3d.ca(fit2, labels=1)
}
}

