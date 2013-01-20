\name{cm_df.transcript}
\alias{cm_df.transcript}
\title{Transcript With Word Number}
\usage{
  cm_df.transcript(text.var, grouping.var, file = NULL,
    indent = 4, width = 70)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{grouping.var}{The grouping variables.  Default NULL
  generates one word list for all text.  Also takes a
  single grouping variable or a list of 1 or more grouping
  variables.}

  \item{file}{A connection, or a character string naming
  the file to print to (e.g. .doc, .txt).}

  \item{indent}{Number of spaces to indent.}

  \item{width}{Width to output the file (defaults to 70;
  this is generally a good width and indent for a .docx
  file).}
}
\value{
  Returns a transcript by grouping variable with word
  number above each word.  This makes use with cm_df2long
  transfer/usage easier because the researcher has coded on
  a transcript with the numeric word index already.
}
\description{
  Output a transcript with word number/index above for easy
  input back into qdap after coding.
}
\note{
  It is recommended that the researcher actually codes on
  the output from this file.  If a file already exists
  \code{cm_df.transcript} will append to that file.
}
\examples{
\dontrun{
with(mraja1spl, cm_df.transcript(dialogue, list(person)))
with(mraja1spl, cm_df.transcript(dialogue, list(sex, fam.aff, died)))
with(mraja1spl, cm_df.transcript(dialogue, list(person), file="foo.doc"))
# delete("foo.doc")   #delete the file just created
}
}
\author{
  DWin, Gavin Simpson and Tyler Rinker
  <tyler.rinker@gmail.com>.
}
\seealso{
  \code{\link[qdap]{cm_df2long}},
  \code{\link[qdap]{cm_df.temp}}
}
\keyword{transcript}

