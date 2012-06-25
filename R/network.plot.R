network.plot <-
function(am, mat, nw.label.cex, nw.edge.col, 
    nw.label.cols, nw.layout, title.name = NULL, nw.title.padj, 
    nw.title.location = 3, title.font, title.cex = .8, COLTERMSi,
    log.labels = FALSE, title.color = "blue") {
    require(igraph)
    g <- igraph::graph.adjacency(am, weighted=TRUE, mode ='undirected') 
    g <- igraph::simplify(g)
    igraph::V(g)$label <- igraph::V(g)$name
    igraph::V(g)$degree <- igraph::degree(g)
    SUMS <- diag(am)
    if (!log.labels) {
        igraph::V(g)$label.cex <- nw.label.cex
    } else {
        igraph::V(g)$label.cex <- (log(SUMS)/max(log(SUMS))) + .5
    }
    nwc <- length(nw.label.cols)
    COLORS <- text2color(words = V(g)$label, recode.words = COLTERMSi, 
        colors = nw.label.cols)
    V(g)$label.color <- COLORS
    E(g)$color <- nw.edge.col
    if (is.null(nw.layout)) {
        nw.layout <- igraph::layout.fruchterman.reingold(g)
    }
    if (dev.interactive()) dev.new()
    plot(g, layout=nw.layout, vertex.size=0, vertex.color="white")
    if (is.null(nw.title.padj)){
        nw.title.padj = -4.5
    }
    if (is.null(nw.title.location)){
        nw.title.location = 3
    }
    if (!is.null(title.name)) {
        mtext(text = title.name, side = nw.title.location, padj = nw.title.padj, 
        col = title.color, family = title.font, cex = title.cex)
    }
}
