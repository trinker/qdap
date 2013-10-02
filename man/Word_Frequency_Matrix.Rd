\name{wfm}
\alias{wf_combine}
\alias{wfdf}
\alias{wfm}
\alias{wfm_expanded}
\title{Word Frequency Matrix}
\usage{
  wfm(text.var = NULL, grouping.var = NULL, output = "raw",
    stopwords = NULL, char2space = "~~", ...)

  wfdf(text.var, grouping.var = NULL, stopwords = NULL,
    margins = FALSE, output = "raw", digits = 2,
    char2space = "~~", ...)

  wfm_expanded(text.var, grouping.var = NULL, ...)

  wf_combine(wf.obj, word.lists, matrix = FALSE)
}
\arguments{
  \item{text.var}{The text variable}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{output}{Output type (either \code{"proportion"} or
  \code{"percent"}).}

  \item{stopwords}{A vector of stop words to remove.}

  \item{char2space}{A vector of characters to be turned
  into spaces.  If \code{char.keep} is \code{NULL},
  \code{char2space} will activate this argument.}

  \item{\ldots}{Other arguments supplied to
  \code{\link[qdap]{strip}}.}

  \item{digits}{An integer indicating the number of decimal
  places (round) or significant digits (signif) to be used.
  Negative values are allowed.}

  \item{margins}{logical. If \code{TRUE} provides
  grouping.var and word variable totals.}

  \item{word.lists}{A list of character vectors of words to
  pass to \code{wf_combine}}

  \item{matrix}{logical.  If \code{TRUE} returns the output
  as a \code{\link[qdap]{wfm}} rather than a
  \code{\link[qdap]{wfdf}} object.}

  \item{wf.obj}{A \code{wfm} or \code{wfdf} object.}
}
\value{
  \code{wfm} - returns a word frequency of the class
  matrix.

  \code{wfdf} - returns a word frequency of the class
  data.frame with a words column and optional margin sums.

  \code{wfm_expanded} - returns a matrix similar to a word
  frequency matrix (\code{wfm}) but the rows are expanded
  to represent the maximum usages of the word and cells are
  dummy coded to indicate that number of uses.

  \code{wf_combine} - returns a word frequency matrix
  (\code{wfm}) or dataframe (\code{wfdf}) with counts for
  the combined word.lists merged and remaining terms
  (\code{else}).
}
\description{
  \code{wfm} - Generate a word frequency matrix by grouping
  variable(s).

  \code{wfdf} - Generate a word frequency data frame by
  grouping variable.

  \code{wfm_expanded} - Expand a word frequency matrix to
  have multiple rows for each word.

  \code{wf_combine} - Combines words (rows) of a word
  frequency dataframe (\code{wfdf}) together.
}
\note{
  Words can be kept as one by inserting a double tilde
  (\code{"~~"}), or other character strings passed to
  char2space, as a single word/entry. This is useful for
  keeping proper names as a single unit.
}
\examples{
\dontrun{
## word frequency matrix (wfm) example:
with(DATA, wfm(state, list(sex, adult)))[1:15, ]
with(DATA, wfm(state, person))[1:15, ]

## insert double tilde ("~~") to keep phrases(i.e., first last name)
alts <- c(" fun", "I ")
state2 <- mgsub(alts, gsub("\\\\s", "~~", alts), DATA$state)
with(DATA, wfm(state2, list(sex, adult)))[1:18, ]

## word frequency dataframe (wfdf) example:
with(DATA, wfdf(state, list(sex, adult)))[1:15, ]
with(DATA, wfdf(state, person))[1:15, ]

## insert double tilde ("~~") to keep dual words (i.e., first last name)
alts <- c(" fun", "I ")
state2 <- mgsub(alts, gsub("\\\\s", "~~", alts), DATA$state)
with(DATA, wfdf(state2, list(sex, adult)))[1:18, ]

## wfm_expanded example:
z <- wfm(DATA$state, DATA$person)
wfm_expanded(z)[30:45, ] #two "you"s

## wf_combine examples:
#===================
## raw no margins (will work)
x <- wfm(DATA$state, DATA$person)

## raw with margin (will work)
y <- wfdf(DATA$state, DATA$person, margins = TRUE)

## Proportion matrix
z2 <- wfm(DATA$state, DATA$person, output="proportion")

WL1 <- c(y[, 1])
WL2 <- list(c("read", "the", "a"), c("you", "your", "you're"))
WL3 <- list(bob = c("read", "the", "a"), yous = c("you", "your", "you're"))
WL4 <- list(bob = c("read", "the", "a"), yous = c("a", "you", "your", "your're"))
WL5 <- list(yous = c("you", "your", "your're"))
WL6 <- list(c("you", "your", "your're"))  #no name so will be called words 1
WL7 <- c("you", "your", "your're")

wf_combine(z, WL2) #Won't work not a raw frequency matrix
wf_combine(x, WL2) #Works (raw and no margins)
wf_combine(y, WL2) #Works (raw with margins)
wf_combine(y, c("you", "your", "your're"))
wf_combine(y, WL1)
wf_combine(y, WL3)
## wf_combine(y, WL4) #Error
wf_combine(y, WL5)
wf_combine(y, WL6)
wf_combine(y, WL7)

worlis <- c("you", "it", "it's", "no", "not", "we")
y <- wfdf(DATA$state, list(DATA$sex, DATA$adult), margins = TRUE)
z <- wf_combine(y, worlis, matrix = TRUE)

chisq.test(z)
chisq.test(wfm(y))
}
}
\keyword{word-frequency-matrix}

