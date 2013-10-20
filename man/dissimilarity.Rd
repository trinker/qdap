\name{dissimilarity}
\alias{dissimilarity}
\title{Dissimilarity Statistics}
\usage{
  dissimilarity(text.var, grouping.var = NULL,
    method = "prop", diag = FALSE, upper = FALSE, p = 2)
}
\arguments{
  \item{text.var}{A text variable or word frequency matrix
  object.}

  \item{grouping.var}{The grouping variables.  Default
  \code{NULL} generates one word list for all text.  Also
  takes a single grouping variable or a list of 1 or more
  grouping variables.}

  \item{method}{Distance methods (see
  \code{\link[stats]{dist}} function). If \code{"prop"}
  (the default) the result is 1 - \code{"binary"}.}

  \item{diag}{logical.  If \code{TRUE} returns the
  diagonals of the matrix.  If \code{method = "prop"}
  diagonals will not be returned.}

  \item{upper}{logical.  If \code{TRUE} returns the upper
  triangle of the matrix.}

  \item{p}{The power of the Minkowski distance}
}
\value{
  Returns a matrix of dissimilarity values (the agreement
  between text).
}
\description{
  Uses the distance function to calculate dissimilarity
  statistics by grouping variables.
}
\examples{
\dontrun{
with(DATA, dissimilarity(state, list(sex, adult)))
with(DATA, dissimilarity(state, person, diag = TRUE))

## Clustering: Dendrogram
(x <- with(pres_debates2012, dissimilarity(dialogue, list(person, time))))
fit <- hclust(x)
plot(fit)
## draw dendogram with red borders around the 3 clusters
rect.hclust(fit, k=3, border=c("red", "purple", "seagreen"))

## Clustering: Dendrogram with p.values
library(pvclust)
wfm.mod <- with(pres_debates2012, wfm(dialogue, list(person, time)))
fit <- pvclust(wfm.mod, method.hclust="ward",
   method.dist="euclidean")
plot(fit)
pvrect(fit, alpha=.95)
}
}
\seealso{
  \code{\link[stats]{dist}}
}
\keyword{dissimilarity}

