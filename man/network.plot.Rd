\name{network.plot}
\alias{network.plot}
\title{
Generate a network plot from an adjaceny matrix
}
\description{
Generate a network plot from an adjaceny matrix
}
\usage{
network.plot(am, mat, nw.label.cex, nw.edge.col, nw.label.cols, nw.layout, title.name = NULL, nw.title.padj, nw.title.location = 3, title.font, title.cex = 0.8, COLTERMSi, log.labels = FALSE, title.color = "blue")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{am}{
%%     ~~Describe \code{am} here~~
}
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{nw.label.cex}{
%%     ~~Describe \code{nw.label.cex} here~~
}
  \item{nw.edge.col}{
%%     ~~Describe \code{nw.edge.col} here~~
}
  \item{nw.label.cols}{
%%     ~~Describe \code{nw.label.cols} here~~
}
  \item{nw.layout}{
%%     ~~Describe \code{nw.layout} here~~
}
  \item{title.name}{
%%     ~~Describe \code{title.name} here~~
}
  \item{nw.title.padj}{
%%     ~~Describe \code{nw.title.padj} here~~
}
  \item{nw.title.location}{
%%     ~~Describe \code{nw.title.location} here~~
}
  \item{title.font}{
%%     ~~Describe \code{title.font} here~~
}
  \item{title.cex}{
%%     ~~Describe \code{title.cex} here~~
}
  \item{COLTERMSi}{
%%     ~~Describe \code{COLTERMSi} here~~
}
  \item{log.labels}{
%%     ~~Describe \code{log.labels} here~~
}
  \item{title.color}{
%%     ~~Describe \code{title.color} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (am, mat, nw.label.cex, nw.edge.col, nw.label.cols, 
    nw.layout, title.name = NULL, nw.title.padj, nw.title.location = 3, 
    title.font, title.cex = 0.8, COLTERMSi, log.labels = FALSE, 
    title.color = "blue") 
{
    require(igraph)
    g <- igraph::graph.adjacency(am, weighted = TRUE, mode = "undirected")
    g <- igraph::simplify(g)
    igraph::V(g)$label <- igraph::V(g)$name
    igraph::V(g)$degree <- igraph::degree(g)
    SUMS <- diag(am)
    if (!log.labels) {
        igraph::V(g)$label.cex <- nw.label.cex
    }
    else {
        igraph::V(g)$label.cex <- (log(SUMS)/max(log(SUMS))) + 
            0.5
    }
    nwc <- length(nw.label.cols)
    COLORS <- text2color(words = V(g)$label, recode.words = COLTERMSi, 
        colors = nw.label.cols)
    V(g)$label.color <- COLORS
    E(g)$color <- nw.edge.col
    if (is.null(nw.layout)) {
        nw.layout <- igraph::layout.fruchterman.reingold(g)
    }
    if (dev.interactive()) 
        dev.new()
    plot(g, layout = nw.layout, vertex.size = 0, vertex.color = "white")
    if (is.null(nw.title.padj)) {
        nw.title.padj = -4.5
    }
    if (is.null(nw.title.location)) {
        nw.title.location = 3
    }
    if (!is.null(title.name)) {
        mtext(text = title.name, side = nw.title.location, padj = nw.title.padj, 
            col = title.color, family = title.font, cex = title.cex)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
