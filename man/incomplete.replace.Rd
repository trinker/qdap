\name{incomplete.replace}
\alias{incomp}
\alias{incomplete.replace}
\title{Denote Incomplete End Marks With "|"}
\usage{
  incomplete.replace(text.var, scan.mode = FALSE)

  incomp(text.var, scan.mode = FALSE)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{scan.mode}{logical.  If TRUE only scans and reports
  incomplete sentences.}
}
\value{
  Returns a text variable (character sting) with incomplete
  sentence marks (.., ..., .?, ..?, en \& em dash etc.
  replaced with "|".  If scan mode is TRUE returns a data
  frame with incomplete sentence location.
}
\description{
  Replaces incomplete sentence end marks (.., ..., .?, ..?,
  en \& em dash etc.) with \code{"|"}.
}
\examples{
\dontrun{
x <- c("the...",  "I.?", "you.", "threw..", "we?")
incomplete.replace(x)
incomp(x)
incomp(x, TRUE)
}
}
\keyword{incomplete-sentence}

