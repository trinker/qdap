\name{replace_contraction}
\alias{replace_contraction}
\title{Replace Contractions}
\usage{
  replace_contraction(text.var,
    contraction = qdap::contractions, replace = NULL,
    ignore.case = TRUE, sent.cap = TRUE)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{contraction}{A two column key of contractions
  (column 1) and expanded form replacements (column 2) or a
  vector of contractions.  Default is to use qdap's
  contractions data set.}

  \item{replace}{A vector of expanded form replacements if
  a data frame is not supplied to the contraction
  argument.}

  \item{ignore.case}{logical.  If TRUE replaces without
  regard to capitalization.}

  \item{sent.cap}{logical.  If TRUE capitalizes the
  beginning of every sentence.}
}
\value{
  Returns a vector with contractions replaced.
}
\description{
  This function replaces contractions with long form.
}
\examples{
\dontrun{
x <- c("Mr. Jones isn't going.",
    "Check it out what's going on.",
    "He's here but didn't go.",
    "the robot at t.s. wasn't nice",
    "he'd like it if i'd go away")

replace_contraction(x)

#create abbreviation and replacement vectors
abv <- c("isn't", "I'd")
repl <- c("is not", "I would")

replace_abbreviation(x, abv, repl)

KEY <- rbind(abbreviations, data.frame(abv = abv, rep = repl))
replace_abbreviation(x, KEY)
}
}
\seealso{
  \code{\link[qdap]{bracketX}}, \code{\link[qdap]{qprep}},
  \code{\link[qdap]{replace_abbreviation}},
  \code{\link[qdap]{replace_number}},
  \code{\link[qdap]{replace_symbol}}
}
\keyword{contraction}

