\name{end_mark}
\alias{end_mark}
\alias{end_mark_by}
\title{Sentence End Marks}
\usage{
end_mark(text.var, missing.end.mark = "_", missing.text = NA,
  other.endmarks = NULL)

end_mark_by(text.var, grouping.var, digits = 3, percent = FALSE,
  zero.replace = 0, ...)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{missing.end.mark}{A value to use for sentences with
  missing endmarks.}

  \item{missing.text}{A value to use for sentences with
  missing (\code{NA}) text.}

  \item{other.endmarks}{Other 1-2 character endmarks tos
  earch for.}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{percent}{logical.  If \code{TRUE} output given as
  percent.  If \code{FALSE} the output is proportion.}

  \item{zero.replace}{Value to replace 0 values with.}

  \item{digits}{Integer; number of decimal places to round
  when printing.}

  \item{\ldots}{Other arguments passed to \code{end_mark}.}
}
\value{
Returns a character vector of qdap end marks for each
sentence. End marks include: \item{"."}{Declarative
sentence.} \item{"?"}{Question sentence.}
\item{"!"}{Exclamatory sentence.} \item{"|"}{Incomplete
sentence.} \item{"*."}{Imperative-declarative sentence.}
\item{"*?"}{Imperative-question sentence (unlikely to
occur)} \item{"*!"}{Imperative-exclamatory sentence.}
\item{"*|"}{Imperative-incomplete sentence.}
\item{"no.em"}{No end mark.} \item{"blank"}{Empty cell/NA.}
}
\description{
\code{end_mark} - Grab the sentence end marks for a
transcript.  This can be useful to categorize based on
sentence type.

\code{end_mark_by} - Grab the sentence end marks for a
transcriptby grouping variable(s).
}
\examples{
\dontrun{
end_mark(DATA.SPLIT$state)
end_mark(mraja1spl$dialogue)
ques <- mraja1spl[end_mark(mraja1spl$dialogue) == "?", ] #grab questions
htruncdf(ques)
non.ques <- mraja1spl[end_mark(mraja1spl$dialogue) != "?", ] #non questions
htruncdf(non.ques, 20)
ques.per <- mraja1spl[end_mark(mraja1spl$dialogue) \%in\% c(".", "?"), ] #grab ? and .
htruncdf(ques.per, 20)

(x_by <- end_mark_by(DATA.SPLIT$state, DATA.SPLIT$person))
scores(x_by)
counts(x_by)
proportions(x_by)
preprocessed(x_by)
plot(scores(x_by))
plot(counts(x_by))
plot(proportions(x_by))
plot(preprocessed(x_by))
}
}
\keyword{end-mark}

