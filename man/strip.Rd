\name{strip}
\alias{strip}
\title{Strip Text}
\usage{
  strip(x, char.keep = "~~", digit.remove = TRUE,
    apostrophe.remove = TRUE, lower.case = TRUE)
}
\arguments{
  \item{x}{The text variable.}

  \item{char.keep}{A character vector of symbols (i.e.
  punctuation) that \code{strip} should keep.  The default
  is to strip every symbol except apostrophes and a double
  tilde \code{"~~"}.  The double tilde \code{"~~"} is
  included for a convenient means of keeping word groups
  together in functions that split text apart based on
  spaces.}

  \item{digit.remove}{logical.  If TRUE strips digits from
  the text.}

  \item{apostrophe.remove}{logical.  If TRUE removes
  apostrophes from the output.}

  \item{lower.case}{logical.  If TRUE forces all alpha
  characters to lower case.}
}
\value{
  Returns a vector of text that has been stripped of
  unwanted characters.
}
\description{
  Strip text of unwanted characters.
}
\examples{
\dontrun{
strip(DATA$state)
strip(DATA$state, apostrophe.remove=FALSE)
strip(DATA$state, char.keep = c("?", "."))
}
}
\seealso{
  \code{\link[qdap]{stopwords}}
}

