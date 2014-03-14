\name{Filter.TermDocumentMatrix}
\alias{Filter}
\alias{Filter.DocumentTermMatrix}
\alias{Filter.TermDocumentMatrix}
\alias{Filter.character}
\title{Filter}
\usage{
\method{Filter}{TermDocumentMatrix}(x, min = 1, max = Inf,
  count.apostrophe = TRUE, stopwords = NULL, ignore.case = TRUE, ...)

\method{Filter}{DocumentTermMatrix}(x, min = 1, max = Inf,
  count.apostrophe = TRUE, stopwords = NULL, ignore.case = TRUE, ...)

Filter(x, min = 1, max = Inf, count.apostrophe = TRUE, stopwords = NULL,
  ignore.case = TRUE, ...)

\method{Filter}{character}(x, min = 1, max = Inf, count.apostrophe = TRUE,
  stopwords = NULL, ignore.case = TRUE, ...)
}
\arguments{
  \item{x}{A filterable object (e.g.,
  \code{\link[qdap]{wfm}}, \code{\link[base]{character}}).}

  \item{min}{Minimum word length.}

  \item{max}{Maximum word length.}

  \item{count.apostrophe}{logical.  If \code{TRUE}
  apostrophes are counted as characters.}

  \item{stopwords}{A vector of stop words to remove.}

  \item{ignore.case}{logical.  If \code{TRUE} stopwords
  will be removed regardless of case (ignored if used on a
  \code{\link[qdap]{wfm}}).}

  \item{\ldots}{Other arguments passed to specific Filter
  methods.}
}
\value{
\code{Filter.TermDocumentMatrix} - Returns a matrix of the
class "TermDocumentMatrix".

\code{Filter.DocumentTermMatrix} - Returns a matrix of the
class "DocumentTermMatrix".

\code{Filter.character} - Returns a vector of the class
"character".

\code{Filter.wfm} - Returns a matrix of the class "wfm".
}
\description{
\code{Filter.TermDocumentMatrix} - Filter words from a
TermDocumentMatrix vector that meet max/min word length
criteria.

\code{Filter.DocumentTermMatrix} - Filter words from a
DocumentTermMatrix that meet max/min word length criteria.

\code{Filter} - Filter words from various objects that meet
max/min word length criteria.

\code{Filter.character} - Filter words from a character
vector that meet max/min word length criteria.
}
\details{
TermDocumentMatrix Method for Filter

DocumentTermMatrix Method for Filter

character Method for Filter
}
\note{
The name and idea behind this function is inspired by the
\pkg{dplyr} package's \code{filter} function and has a
similar meaning in that you are grabbing rows (or elements)
meeting a particular criteria.
}
\examples{
\dontrun{
Filter(with(DATA, wfm(state, list(sex, adult))), 5)
with(DATA, wfm(state, list(sex, adult)))

## Filter particular words based on max/min values in wfm
v <- with(DATA, wfm(state, list(sex, adult)))
Filter(v, 5)
Filter(v, 5, count.apostrophe = FALSE)
Filter(v, 5, 7)
Filter(v, 4, 4)
Filter(v, 3, 4)
Filter(v, 3, 4, stopwords = Top25Words)

## Filter works on character strings too...
x <- c("Raptors don't like robots!",  "I'd pay $500.00 to rid them.")
Filter(x, 3)
Filter(x, 4)
Filter(x, 4, count.apostrophe = FALSE)
Filter(x, 4, count.apostrophe = FALSE, stopwords="raptors")
Filter(x, 4, stopwords="raptors")
Filter(x, 4, stopwords="raptors", ignore.case = FALSE)

DATA[, "state"] <- Filter(DATA[, "state"], 4)
DATA <- qdap::DATA
}
}

