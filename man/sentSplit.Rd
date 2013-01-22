\name{sentSplit}
\alias{sentCombine}
\alias{sentSplit}
\alias{TOT}
\title{Sentence Splitting}
\usage{
  sentSplit(dataframe, text.var,
    endmarks = c("?", ".", "!", "|"),
    incomplete.sub = TRUE, rm.bracket = TRUE,
    stem.col = FALSE, text.place = "right", ...)

  sentCombine(text.var, grouping.var = "person")

  TOT(tot)
}
\arguments{
  \item{dataframe}{A dataframe that contains the person and
  text variable.}

  \item{text.var}{The text variable.}

  \item{endmarks}{A character vector of endmarks to split
  turns of talk into sentences.}

  \item{incomplete.sub}{logical.  If TRUE detects
  incomplete sentences and replaces with \code{"|"}.}

  \item{rm.bracket}{logical.  If TRUE removes brackets from
  the text.}

  \item{stem.col}{logical.  If TRUE stems the text as a new
  column.}

  \item{text.place}{A character string giving placement
  location of the text column. This must be one of the
  strings \code{"original"}, \code{"right"} or
  \code{"left"}.}

  \item{\ldots}{Additional options passed to
  \code{stem2df}.}

  \item{grouping.var}{The grouping variable (usually
  \code{"person"}).  Does not take multiple vectors as most
  qdap functions do.}

  \item{tot}{A tot column from a
  \code{\link[qdap]{sentSplit}} output.}
}
\value{
  \code{sentSplit} - returns a dataframe with turn of talk
  broken apart into sentences.  Optionally a stemmed
  version of the text variable may be returned as well.

  \code{sentCombine} - returns a list of vectors with the
  continuous sentences by grouping.var pasted together.
  returned as well.

  \code{TOT} - returns a numeric vector of the turns of
  talk without sentence sub indexing (e.g. 3.2 become 3).
}
\description{
  \code{sentSplit} - Splits turns of talk into individual
  sentences (provided proper punctuation is used).  This
  procedure is usually done as part of the data read in and
  cleaning process.

  \code{sentCombine} - Combines sentences by the same
  grouping variable together.

  \code{TOT} - Convert the tot column from
  \code{\link[qdap]{sentSplit}} to turn of talk index (no
  sub sentence).  Generally, for internal use.
}
\examples{
\dontrun{
#sentSplit EXAMPLE:
sentSplit(DATA, "state")
sentSplit(DATA, "state", stem.col = TRUE)
sentSplit(DATA, "state", text.place = "left")
sentSplit(DATA, "state", text.place = "original")
sentSplit(raj, "dialogue")

#sentCombine EXAMPLE:
dat <- sentSplit(DATA, "state")
sentCombine(dat$state, dat$person)
sentCombine(dat$state, dat$sex)

#TOT EXAMPLE:
dat <- sentSplit(DATA, "state")
TOT(dat$tot)
}
}
\author{
  Dason Kurkiewicz and Tyler Rinker
  <tyler.rinker@gmail.com>.
}
\seealso{
  \code{\link[qdap]{bracketX}},
  \code{\link[qdap]{incomplete.replace}},
  \code{\link[qdap]{stem2df}} , \code{\link[qdap]{TOT}}
}
\keyword{sentence,}
\keyword{split,}
\keyword{turn-of-talk}

