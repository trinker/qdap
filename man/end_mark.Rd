\name{end_mark}
\alias{end_mark}
\title{Sentence Endmarks}
\usage{
  end_mark(text.var)
}
\arguments{
  \item{text.var}{The text variable.}
}
\value{
  Returns a character vector of qdap endmarks for each
  sentence. Endmarks include: \item{"."}{Declarative
  sentence.} \item{"?"}{Question sentence.}
  \item{"!"}{Exclamatory sentence.} \item{"|"}{Incomplete
  sentence.} \item{"*."}{Imperative-declarative sentence.}
  \item{"*?"}{Imperative-question sentence (unlikely to
  occur)} \item{"*!"}{Imperative-exclamatory sentence.}
  \item{"*|"}{Imperative-incomplete sentence.}
  \item{"no.em"}{No endmark.} \item{"blank"}{Empty
  cell/NA.}
}
\description{
  Grab the sentence endmarks for a transcript.  This can be
  useful to
}
\examples{
\dontrun{
end_mark(DATA$state)
end_mark(mraja1spl$dialogue)
mraja1spl[end_mark(mraja1spl$dialogue) == "?", ] #grab questions
mraja1spl[end_mark(mraja1spl$dialogue) != "?", ] #non questions
mraja1spl[end_mark(mraja1spl$dialogue) \%in\% c(".", "?"), ] #grab ? and .
}
}
\keyword{endmark}

