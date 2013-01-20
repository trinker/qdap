\name{wfm}
\alias{wf.combine}
\alias{wfdf}
\alias{wfm}
\alias{wfm.expanded}
\title{Word Frequency Matrix}
\usage{
  wfm(text.var = NULL, grouping.var = NULL, wfdf = NULL,
    output = "raw", stopwords = NULL, digits = 2)

  wfdf(text.var, grouping.var = NULL, stopwords = NULL,
    margins = FALSE, output = "raw", digits = 2)

  wfm.expanded(text.var, grouping.var = NULL, ...)

  wf.combine(wf.obj, word.lists, matrix = FALSE)
}
\arguments{
  \item{text.var}{The text variable}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{wfdf}{A word frequency data frame given instead of
  raw text.var and optional grouping.var. Basically
  converts a word frequency dataframe (wfdf) to a word
  frequency matrix (\code{wfm}).  Default is NULL.}

  \item{output}{Output type (either \code{"proportion"} or
  \code{"percent"}).}

  \item{stopwords}{A vector of stop words to remove.}

  \item{digits}{An integer indicating the number of decimal
  places (round) or significant digits (signif) to be used.
  Negative values are allowed}

  \item{margins}{logical. If TRUE provides grouping.var and
  word variable totals.}

  \item{\ldots}{Other arguments supplied to \code{wfm}.}

  \item{wf.obj}{A \code{wfm} or \code{wfdf} object.}

  \item{word.lists}{A list of character vectors of words to
  pass to \code{wf.combine}}

  \item{matrix}{logical.  If TRUE returns the output as a
  \code{wfm} rather than a \code{wfdf} object}
}
\value{
  \code{wfm} - returns a word frequency of the class
  matrix.

  \code{wfdf} - returns a word frequency of the class
  data.frame with a words column and optional margin sums.

  \code{wfm.expanded} - returns a matrix similar to a word
  frequency matrix (\code{wfm}) but the rows are expanded
  to represent the maximum usages of the word and cells are
  dummy coded to indicate that number of uses.

  \code{wf.combine} - returns a word frequency matrix
  (\code{wfm}) or dataframe (\code{wfdf}) with counts for
  the combined word.lists merged and remaining terms(else).
}
\description{
  \code{wfm} - Generate a word frequency matrix by grouping
  variable(s).

  \code{wfdf} - Generate a word frequency data frame by
  grouping variable.

  \code{wfm.expanded} - Expand a word frequency matrix to
  have multiple rows for each word.

  \code{wf.combine} - Combines words (rows) of a word
  frequency data frame (\code{wfdf}) together.
}
\examples{
\dontrun{
#word frequency matrix (wfm) example:
with(DATA, wfm(state, list(sex, adult)))
dat <- with(DATA, wfm(state, person))

#word frequency dataframe (wfdf) example:
with(DATA, wfdf(state, list(sex, adult)))
with(DATA, wfdf(state, person))

#wfm.expanded example:
z <- wfm(DATA$state, DATA$person)
wfm.expanded(z)
wfm.expanded(DATA$state, DATA$person)
wfm.expanded(DATA$state, list(DATA$sex, DATA$adult))

#wf.combine example:
#raw no margins (will work)
x <- wfm(DATA$state, DATA$person)

#raw with margin (will work)
y <- wfdf(DATA$state, DATA$person, margins = TRUE)

#proportion (will not work)
z <- wfdf(DATA$state, DATA$person, output = "proportion")

WL1 <- c(y[, 1])
WL2 <- list(c("read", "the", "a"), c("you", "your", "your're"))
WL3 <- list(bob = c("read", "the", "a"), yous = c("you", "your", "you're"))
WL4 <- list(bob = c("read", "the", "a"), yous = c("a", "you", "your", "your're"))
WL5 <- list(yous = c("you", "your", "your're"))
WL6 <- list(c("you", "your", "your're"))  #no name so will be called words 1
WL7 <- c("you", "your", "your're")

wf.combine(z, WL2) #Won't work not a raw frequency matrix
wf.combine(x, WL2) #Works (raw and no margins)
wf.combine(y, WL2) #Works (raw with margins)
wf.combine(y, c("you", "your", "your're"))
wf.combine(y, WL1)
wf.combine(y, WL3)
wf.combine(y, WL4) #Error b/c there's overlapping words in the word lists
wf.combine(y, WL5)
wf.combine(y, WL6)
wf.combine(y, WL7)

worlis <- c("you", "it", "it's", "no", "not", "we")
y <- wfdf(DATA$state, list(DATA$sex, DATA$adult), margins = TRUE)
z <- wf.combine(y, worlis, matrix = TRUE)

chisq.test(z)
chisq.test(wfm(wfdf = y))
}
}
\keyword{word-frequency-matrix}

