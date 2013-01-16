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
  class "termco_d" or "termco_c" generated from
  \code{terco.a}, \code{termco.d} or \code{termco.c}.}
}
\value{
  Generates an adjacency matrix
}
\description{
  Takes a matrix (wfm) or termco object (.a, .c or .d) and
  generates an adjacency matrix for use with \code{igraph}.
}
\examples{
\dontrun{
wordLIST <- c(" montague", " capulet", " court", " marry")
(raj.termco <- with(raj.act.1, termco.a(dialogue, person,
    wordLIST, ignore.case = T)))
(raj.adjmat <- adjmat(raj.termco))
names(raj.adjmat)  #see what's available from the adjacency_matrix object
library(igraph)
g <- graph.adjacency(raj.adjmat$adjacency, weighted=TRUE, mode ="undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
layout1 <- layout.auto(g)
plot(g, layout=layout1)
}
}
\seealso{
  \code{\link[stats]{dist}}
}
\keyword{adjacency}
\keyword{matrix}

