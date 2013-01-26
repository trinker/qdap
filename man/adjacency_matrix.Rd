\name{adjacency_matrix}
\alias{adjacency_matrix}
\alias{adjmat}
\title{Takes a Matrix and Generates an Adjacency Matrix}
\usage{
  adjacency_matrix(matrix.obj)

  adjmat(matrix.obj)
}
\arguments{
  \item{matrix.obj}{A matrix object, preferably, of the
  class "termco" generated from \code{termco},
  \code{termco.d} or \code{termco.c}.}
}
\value{
  Returns an adjacency matrix.
}
\description{
  Takes a matrix (wfm) or termco object and generates an
  adjacency matrix for use with the
  \href{http://igraph.sourceforge.net/}{igraph} package.
}
\examples{
\dontrun{
words <- c(" you", " the", "it", "oo")
Terms <- with(DATA, termco(state, list(sex, adult), words))
Terms
adjacency_matrix(Terms)

wordLIST <- c(" montague", " capulet", " court", " marry")
raj.termco <- with(raj.act.1, termco(dialogue, person, wordLIST))
raj.adjmat <- adjmat(raj.termco)
names(raj.adjmat)  #see what's available from the adjacency_matrix object
library(igraph)
g <- graph.adjacency(raj.adjmat$adjacency, weighted=TRUE, mode ="undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
plot(g, layout=layout.auto(g))
}
}
\seealso{
  \code{\link[stats]{dist}}
}
\keyword{adjacency}
\keyword{matrix}

