\name{replace_number}
\alias{replace_number}
\title{Replace Numbers With Text Representation}
\usage{
  replace_number(text.var, num.paste = "separate")
}
\arguments{
  \item{text.var}{The text variable.}

  \item{num.paste}{A character vector of either
  \code{"separate"} or \code{"combine"}.  Of
  \code{"separate"} is specified the elements of larger
  numbers are separated with spaces.  If \code{"combine"}
  is selected the elements will be joined without spaces.}
}
\value{
  Returns a vector with abbreviations replaced.
}
\description{
  Replaces numeric represented numbers with words (e.g.
  1001 becomes one thousand one).
}
\examples{
\dontrun{
x <- c("I like 346,457 ice cream cones.", "They are 99 percent good")
y <- c("I like 346457 ice cream cones.", "They are 99 percent good")
replace_number(x)
replace_number(y)
replace_number(x, "combine")
}
}
\references{
  Fox, J. (2005). Programmer's niche: How do you spell that
  number? R News. Vol. 5(1), pp. 51-55.
}
\seealso{
  \code{\link[qdap]{bracketX}}, \code{\link[qdap]{qprep}},
  \code{\link[qdap]{replace_abbreviation}},
  \code{\link[qdap]{replace_contraction}},
  \code{\link[qdap]{replace_symbol}}
}
\keyword{number-to-word}

