\name{question_type}
\alias{question_type}
\title{Count of Question Type}
\usage{
  question_type(text.var, grouping.var = NULL,
    neg.cont = FALSE, proportional = FALSE,
    prop.by.row = TRUE, ...)
}
\arguments{
  \item{text.var}{The text variable}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one output for all text.  Also takes a single
  grouping variable or a list of 1 or more grouping
  variables.}

  \item{neg.cont}{logical.  IF TRUE provides separate
  counts for the negative contraction forms of the
  interrogative words.}

  \item{proportional}{logical.  If TRUE outputs the table
  proportionally (see \code{\link[qdap]{prop}}).}

  \item{prop.by.row}{logical.  If TRUE applies proportional
  to the row.  If FALSE applies by column.}

  \item{\ldots}{Other arguments passed to
  \code{\link[qdap]{prop}}.}
}
\value{
  Returns a table of total questions (\code{tot.quest}) and
  counts of question types (initial interrogative word).
}
\description{
  Transcript apply question counts.
}
\details{
  The algorithm searchs for the following interrogative
  words (and optionally, their negative contraction form as
  well):

  1) whose 2) whom 3) who 4) where 5) what 6) which 7) why
  8) when 9) were 10) was 11) does 12) did 13) do 14) is
  15) are 16) will 17) how 18) should 19) could 20) would
  21) shall 22) may 23) might 24) must 25) can 26) has 27)
  have 28) had 29) ok 30) right 31) correct

  The interrogative word that is found first (with the
  exception of "ok", "right" and "correct") in the question
  determines the sentence type. "ok", "right" and "correct"
  sentence types are determined if the sentence is a
  question with no other interogative words found and "ok",
  "right" or "correct" is the last word of the sentence.
  Those with undetermined sentence type are labeled
  unknown.
}
\examples{
\dontrun{
(x <- question_type(DATA$state, DATA$person))
x$raw
x$count
plot(x)
question_type(DATA$state, DATA$person, proportional = TRUE)
DATA[8, 4] <- "Won't I distrust you?"
question_type(DATA$state, DATA$person)
DATA <- qdap::DATA
with(DATA, question_type(state, list(sex, adult)))

with(mraja1spl, question_type(dialogue, person))
with(mraja1spl, question_type(dialogue, list(sex, fam.aff)))
with(mraja1spl, question_type(dialogue, list(sex, fam.aff),
    proportional = TRUE))
}
}
\keyword{question-count}
\keyword{question,}

