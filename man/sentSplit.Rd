\name{sentSplit}
\alias{TOT}
\alias{sentCombine}
\alias{sentSplit}
\title{Sentence Splitting}
\usage{
sentSplit(dataframe, text.var, rm.var = NULL, endmarks = c("?", ".", "!",
  "|"), incomplete.sub = TRUE, rm.bracket = TRUE, stem.col = FALSE,
  text.place = "right", ...)

sentCombine(text.var, grouping.var = NULL, as.list = FALSE)

TOT(tot)
}
\arguments{
  \item{dataframe}{A dataframe that contains the person and
  text variable.}

  \item{text.var}{The text variable.}

  \item{rm.var}{An optional character vector of 1 or 2
  naming the variables that are repeated measures (This
  will restart the \strong{"tot"} column).}

  \item{endmarks}{A character vector of endmarks to split
  turns of talk into sentences.}

  \item{incomplete.sub}{logical.  If \code{TRUE} detects
  incomplete sentences and replaces with \code{"|"}.}

  \item{rm.bracket}{logical.  If \code{TRUE} removes
  brackets from the text.}

  \item{stem.col}{logical.  If \code{TRUE} stems the text
  as a new column.}

  \item{text.place}{A character string giving placement
  location of the text column. This must be one of the
  strings \code{"original"}, \code{"right"} or
  \code{"left"}.}

  \item{\ldots}{Additional options passed to
  \code{\link[qdap]{stem2df}}.}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{tot}{A tot column from a
  \code{\link[qdap]{sentSplit}} output.}

  \item{as.list}{logical.  If \code{TRUE} returns the
  output as a list. If \code{FALSE} the output is returned
  as a dataframe.}
}
\value{
\code{sentSplit} - returns a dataframe with turn of talk
broken apart into sentences.  Optionally a stemmed version
of the text variable may be returned as well.

\code{sentCombine} - returns a list of vectors with the
continuous sentences by grouping.var pasted together.
returned as well.

\code{TOT} - returns a numeric vector of the turns of talk
without sentence sub indexing (e.g. 3.2 become 3).
}
\description{
\code{sentSplit} - Splits turns of talk into individual
sentences (provided proper punctuation is used).  This
procedure is usually done as part of the data read in and
cleaning process.

\code{sentCombine} - Combines sentences by the same
grouping variable together.

\code{TOT} - Convert the tot column from
\code{\link[qdap]{sentSplit}} to turn of talk index (no sub
sentence).  Generally, for internal use.
}
\section{Warning}{
  \code{\link[qdap]{sentSplit}} requires the dialogue
  (text) column to be cleaned in a particular way.  The
  data should contain qdap punctuation marks (\code{c("?",
  ".", "!", "|")}) at the end of each sentence.
  Additionally, extraneous punctuation such as
  abbreviations should be removed (see
  \code{\link[qdap]{replace_abbreviation}}). Trailing
  sentences such as \bold{I thought I...} will be treated
  as incomplete and marked with \code{"|"} to denote an
  incomplete/trailing sentence.
}
\examples{
\dontrun{
## `sentSpli`t EXAMPLE:
(out <- sentSplit(DATA, "state"))
sentSplit(DATA, "state", stem.col = TRUE)
sentSplit(DATA, "state", text.place = "left")
sentSplit(DATA, "state", text.place = "original")
sentSplit(raj, "dialogue")[1:20, ]

## plotting
plot(out)
plot(out, grouping.var = "person")

out2 <- sentSplit(DATA2, "state", rm.var = c("class", "day"))
plot(out2)
plot(out2, grouping.var = "person")
plot(out2, grouping.var = "person", rm.var = "day")
plot(out2, grouping.var = "person", rm.var = c("day", "class"))

## `sentCombine` EXAMPLE:
dat <- sentSplit(DATA, "state")
sentCombine(dat$state, dat$person)
truncdf(sentCombine(dat$state, dat$sex), 50)

## `TOT` EXAMPLE:
dat <- sentSplit(DATA, "state")
TOT(dat$tot)
}
}
\author{
Dason Kurkiewicz and Tyler Rinker <tyler.rinker@gmail.com>.
}
\seealso{
\code{\link[qdap]{bracketX}},
\code{\link[qdap]{incomplete_replace}},
\code{\link[qdap]{stem2df}} , \code{\link[qdap]{TOT}}
}
\keyword{sentence,}
\keyword{split,}
\keyword{turn-of-talk}

