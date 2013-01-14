\name{replace_symbol}
\alias{replace_symbol}
\title{Replace Symbols With Word Equivalents}
\usage{
  replace_symbol(text.var, dollar = TRUE, percent = TRUE,
    pound = TRUE, at = TRUE, and = TRUE, with = TRUE)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{dollar}{logical.  If TRUE replaces dollar sign ($)
  with \code{"dollar"}.}

  \item{percent}{logical.  If TRUE replaces percent sign
  (\%) with \code{"percent"}.}

  \item{pound}{logical.  If TRUE replaces pound sign (#)
  with \code{"number"}.}

  \item{at}{logical.  If TRUE replaces at sign (@) with
  \code{"at"}.}

  \item{and}{logical.  If TRUE replaces and sign (&) with
  \code{"and"}.}

  \item{with}{logical.  If TRUE replaces with sign (w/)
  with \code{"with"}.}
}
\value{
  Returns a character vector with symbols replaced..
}
\description{
  This function replaces symbols with word equivalents
  (e'g' \code{@} becomes \code{"at"}.
}
\examples{
\dontrun{
x <- c("I am @ Jon's & Jim's w/ Marry", "I owe $41 for food", "two is 10\%
    of a #")
replace_symbol(x)
}
}
\seealso{
  \code{\link[qdap]{bracketX}},
  \code{\link[qdap]{replace_abbreviation}},
  \code{\link[qdap]{replace_number}},
  \code{\link[qdap]{qprep}}
}
\keyword{symbol-replace}

