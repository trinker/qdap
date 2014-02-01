\name{word_proximity}
\alias{word_proximity}
\title{Proximity Matrix Between Words}
\usage{
word_proximity(text.var, terms, grouping.var = NULL)
}
\arguments{
  \item{text.var}{The text variable.}

  \item{terms}{A vector of quoted terms.}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}
}
\value{
Returns a matrix of proximity measures in the unit of
average sentences between words.
}
\description{
Generate proximity measures to ascertain a mean distance
measure between word uses.
}
\note{
The match.terms is character sensitive.  Spacing is an
important way to grab specific words and requires careful
thought.  Using "read" will find the words "bread", "read"
"reading", and "ready".  If you want to search for just the
word "read" you'd supply a vector of c(" read ", " reads",
" reading", " reader").
}
\examples{
\dontrun{
wrds <- word_list(pres_debates2012$dialogue,
    stopwords = c("it's", "that's", Top200Words))
wrds2 <- tolower(sort(wrds$rfswl[[1]][, 1]))

(x <- with(pres_debates2012, word_proximity(dialogue, wrds2)))
out <- round(scale(x), 2)
diag(out) <- attr(out, "scaled:center")
qheat(scale(x), values=TRUE, digits=2, high="red", low="yellow", grid=NULL)
}
}

