\name{question_type}
\alias{question_type}
\title{Count of Question Type}
\usage{
  question_type(text.var, grouping.var = NULL,
    proportional = FALSE, prop.by.row = TRUE, ...)
}
\arguments{
  \item{text.var}{The text variable}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one output for all text.  Also takes a single
  grouping variable or a list of 1 or more grouping
  variables.}

  \item{proportional}{logical.  If TRUE outputs the table
  proportionally (see \code{\link[qdap]{prop}}).}

  \item{prop.by.row}{logical.  If TRUE applies proportional
  to the row.  If FALSE applies by column.}

  \item{\ldots}{Other arguments passed to
  \code{\link[qdap]{prop}}.}
}
\value{
  Returns a table of total questions (\code{tot.quest}) and
  counts of question types (initial interagative word).
}
\description{
  Transcript apply question counts.
}
\details{
  The algorith searchers for the following interrogative
  words:

  1) whose 2) whom 3) who 4) where 5) what 6) which 7) why
  8) when 9) were 10) was 11) does 12) did 13) do 14) is
  15) are 16) will 17) how 18) should 19) could 20) would
  21) shall 22) may 23) might 24) can

  The interrogative word that is found first in the
  question determines the sentence type.
}
\examples{
\dontrun{
question_type(DATA$state, DATA$person)
question_type(DATA$state, DATA$person, proportional = TRUE)
with(DATA, question_type(state, list(sex, adult)))
with(mraja1spl, question_type(dialogue, list(sex, fam.aff)))
with(mraja1spl, question_type(dialogue, person))
}
}
\keyword{question-count}
\keyword{question,}

