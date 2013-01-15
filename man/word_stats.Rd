\name{word_stats}
\alias{word_stats}
\title{Descriptive Word Statistics}
\usage{
  word_stats(text.var, grouping.var = NULL, tot = NULL,
    parallel = FALSE, rm.incomplete = FALSE,
    digit.remove = FALSE, apostrophe.remove = FALSE,
    digits = 3, ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one output for all text.  Also takes a single
  grouping variable or a list of 1 or more grouping
  variables.}

  \item{tot}{Optional turns of talk variable that yields
  turn of talk measures.}

  \item{parallel}{logical.  If TRUE attempts to run the
  function on multiple cores.  Note that this may not mean
  a spead boost if you have one core or if the data set is
  smaller as the cluster takes time to create (parallel is
  slower until approximately 10,000 rows).}

  \item{rm.incomplete}{logical.  If TRUE incomplete
  statments are removed from calculating the output.}

  \item{digit.remove}{logical.  If TRUE removes digits from
  calculating the output.}

  \item{apostrophe.remove}{logical.  If TRUE removes
  apostophes from calculating the output.}

  \item{digits}{Integer; number of decimal places to
  round.}

  \item{\ldots}{Any other arguments passed to endf.}
}
\value{
  Returns a list of three descriptive word statistics:
  \item{ts}{A data frame of descriptive word statistics by
  row} \item{gts}{A data frame of word statistics per
  grouping variable: \itemize{ \item{n.tot}{ - number of
  turns of talk} \item{n.sent}{ - number of sentences}
  \item{n.words}{ - number of words} \item{n.char}{ -
  number of characters} \item{n.syl}{ - number of
  syllables} \item{n.poly}{ - number of polysyllables}
  \item{sptot}{ - syllables per turn of talk} \item{wptot}{
  - words per turn of talk} \item{wps}{ - words per
  sentence} \item{cps}{ - characters per sentemce}
  \item{sps}{ - syllables per sentence} \item{psps}{ -
  polly syllables per sentence} \item{cpw}{ - characters
  per word} \item{spw}{ - syllables per word}
  \item{n.state}{ - number of statements} \item{n.quest}{ -
  number of questions} \item{n.exclm}{ - vnumber of
  exclamations} \item{n.incom}{ - number of incomplete
  satetments} \item{n.hapax}{ - number of hapax legomenon}
  \item{n.dis}{ - number of dis legomenon}
  \item{grow.rate}{ - proportion of hapax legomenon to
  words} \item{prop.dis}{ - proportion of dis legomenon to
  words} } } \item{mpun}{An account of sentences with
  improper end mark}
}
\description{
  Transcript apply descriptive word statistics.
}
\examples{
\dontrun{
word_stats(mraja1spl$dialogue, mraja1spl$person)
(desc_wrds <- with(mraja1spl, word_stats(dialogue, person, tot = tot)))
names(desc_wrds)
desc_wrds$ts
desc_wrds$gts
desc_wrds$pun
plot(desc_wrds)
with(mraja1spl, word_stats(dialogue, list(sex, died, fam.aff)))
}
}
\keyword{descriptive}
\keyword{statistic}

