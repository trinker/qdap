\name{strip}
\alias{strip}
\title{Strip Text}
\usage{
  strip(x, char.keep = NULL, digit.remove = TRUE,
    apostrophe.remove = TRUE, lower.case = TRUE)
}
\arguments{
  \item{x}{The text variable.}

  \item{char.keep}{A character vector of symbol character
  (i.e. punctioation) that strip should keep.  The default
  is to strip everything except apostophes.}

  \item{digit.remove}{logical.  If TRUE strips digits from
  the text.}

  \item{apostrophe.remove}{logical.  If TRUE removes
  apostrophe's from the output.}

  \item{lower.case}{logical.  If TRUE forces all alpha
  characters to lower case.}
}
\value{
  Retruns a vector of text that has been stripped of
  unwanted characters.
}
\description{
  Strip text of unwanted charcters.
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

