\name{word_list}
\alias{word_list}
\title{Raw Word Lists/Frequency Counts}
\usage{
  word_list(text.var, grouping.var = NULL,
    stopwords = NULL, alphabetical = FALSE, cut.n = 20,
    cap = TRUE, cap.list = NULL, cap.I = TRUE,
    rm.bracket = TRUE, char.keep = NULL,
    apostrophe.remove = FALSE, ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{stopwords}{A vector of stop words to remove.}

  \item{alphabetical}{If TRUE the output of frequency lists
  is ordered alphabetically.  If FALSE the list is ordered
  by frequency rank.}

  \item{cut.n}{Cut off point for reduced frequency stop
  word list (rfswl).}

  \item{cap}{logical. If TRUE capitalizes words from the
  cap.list.}

  \item{cap.list}{Vector of words to capitalize.}

  \item{cap.I}{logical. If TRUE capitalizes words
  containing the personal pronoun I.}

  \item{rm.bracket}{logical If TRUE all brackets and
  bracketted text are removed from analysis.}

  \item{char.keep}{A character vector of symbols (i.e.
  punctuation) that \code{word_list} should keep.  THe
  default is to remove every symbol except apostrophes.}

  \item{apostrophe.remove}{logical.  If TRUE removes
  apostrophes from the output.}

  \item{\ldots}{Other arguments passed to
  \code{\link[qdap]{strip}}.}
}
\value{
  An object of class \code{"word.list"} is a list
  containing at the following components:
  \item{cwl}{complete word list; raw words} \item{swl}{stop
  word list; same as rwl with stop words removed}
  \item{fwl}{frequency word list; a data frame of words and
  correspnding frequency counts} \item{fswl}{fequency
  stopword word list; same as fwl but with stopwords
  removed} \item{rfswl}{reduced frequency stopword word
  list; same as fswl but truncated to n rows}
}
\description{
  Transcript Apply Raw Word Lists and Frequency Counts by
  grouping variable(s).
}
\examples{
\dontrun{
XX <-word_list(raj.act.1$dialogue)
names(XX)
XX$cwl
XX$swl
XX$fwl
XX$fswl
XX$rfswl

with(raj, word_list(text.var = dialogue, grouping.var = list(person, act)))
with(DATA, word_list(state, person))
with(DATA, word_list(state, person, stopwords = Top25Words))
with(DATA, word_list(state, person, cap = FALSE, cap.list=c("do", "we")))
}
}
\keyword{word-list}

