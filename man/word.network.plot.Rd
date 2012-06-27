\name{word.network.plot}
\alias{word.network.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A network plot of words
}
\description{
A network plot of words
}
\usage{
word.network.plot(text.var, grouping.var = NULL, target.words = NULL, stopwords = Top100Words, label.cex = 0.8, label.size = 0.5, edge.curved = FALSE, vertex.shape = "circle", edge.color = "gray70", label.colors = "black", layout = NULL, title.name = NULL, title.padj = -4.5, title.location = 3, title.font = NULL, title.cex = 0.8, log.labels = FALSE, title.color = "black", plot = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{text.var}{
%%     ~~Describe \code{text.var} here~~
}
  \item{grouping.var}{
%%     ~~Describe \code{grouping.var} here~~
}
  \item{target.words}{
%%     ~~Describe \code{target.words} here~~
}
  \item{stopwords}{
%%     ~~Describe \code{stopwords} here~~
}
  \item{label.cex}{
%%     ~~Describe \code{label.cex} here~~
}
  \item{label.size}{
%%     ~~Describe \code{label.size} here~~
}
  \item{edge.curved}{
%%     ~~Describe \code{edge.curved} here~~
}
  \item{vertex.shape}{
%%     ~~Describe \code{vertex.shape} here~~
}
  \item{edge.color}{
%%     ~~Describe \code{edge.color} here~~
}
  \item{label.colors}{
%%     ~~Describe \code{label.colors} here~~
}
  \item{layout}{
%%     ~~Describe \code{layout} here~~
}
  \item{title.name}{
%%     ~~Describe \code{title.name} here~~
}
  \item{title.padj}{
%%     ~~Describe \code{title.padj} here~~
}
  \item{title.location}{
%%     ~~Describe \code{title.location} here~~
}
  \item{title.font}{
%%     ~~Describe \code{title.font} here~~
}
  \item{title.cex}{
%%     ~~Describe \code{title.cex} here~~
}
  \item{log.labels}{
%%     ~~Describe \code{log.labels} here~~
}
  \item{title.color}{
%%     ~~Describe \code{title.color} here~~
}
  \item{plot}{
%%     ~~Describe \code{plot} here~~
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
function (text.var, grouping.var = NULL, target.words = NULL, 
    stopwords = Top100Words, label.cex = 0.8, label.size = 0.5, 
    edge.curved = FALSE, vertex.shape = "circle", edge.color = "gray70", 
    label.colors = "black", layout = NULL, title.name = NULL, 
    title.padj = -4.5, title.location = 3, title.font = NULL, 
    title.cex = 0.8, log.labels = FALSE, title.color = "black", 
    plot = TRUE) 
{
    suppressWarnings(require(igraph))
    if (class(text.var) == "adjacency.matrix") {
        adj.mat.object <- text.var[["adjacency"]]
    }
    else {
        z <- word.freq.matrix(text.var = text.var, grouping.var = grouping.var, 
            stopwords = stopwords)
        adj.mat.object <- adjmat(t(z))[["adjacency"]]
    }
    g <- igraph::graph.adjacency(adj.mat.object, weighted = TRUE, 
        mode = "undirected")
    g <- igraph::simplify(g)
    igraph::V(g)$label <- igraph::V(g)$name
    igraph::V(g)$degree <- igraph::degree(g)
    SUMS <- diag(adj.mat.object)
    if (!log.labels) {
        igraph::V(g)$label.cex <- label.cex
    }
    else {
        igraph::V(g)$label.cex <- (log(SUMS)/max(log(SUMS))) + 
            label.size
    }
    if (!is.null(target.words)) {
        nwc <- length(label.colors)
        COLORS <- text2color(words = V(g)$label, recode.words = target.words, 
            colors = label.colors)
        igraph::V(g)$label.color <- COLORS
    }
    else {
        igraph::V(g)$label.color <- label.colors
    }
    igraph::V(g)$shape <- vertex.shape
    igraph::E(g)$color <- edge.color
    if (is.null(layout)) {
        layout <- igraph::layout.fruchterman.reingold(g)
    }
    if (plot) {
        if (dev.interactive()) 
            dev.new()
        plot.igraph(g, layout = layout, vertex.size = 0, vertex.color = "white", 
            edge.curved = edge.curved)
        if (is.null(title.padj)) {
            title.padj = -4.5
        }
        if (is.null(title.location)) {
            title.location = 3
        }
        if (!is.null(title.name)) {
            mtext(text = title.name, side = title.location, padj = title.padj, 
                col = title.color, family = title.font, cex = title.cex)
        }
    }
    invisible(g)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
