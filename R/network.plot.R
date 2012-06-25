network.plot <-
function(adj.mat, mat, label.cex, edge.color, 
    label.colors, layout, title.name = NULL, title.padj, 
    title.location = 3, title.font, title.cex = .8, target.words,
    log.labels = FALSE, title.color = "black") {
    suppressWarnings(require(igraph))
    g <- igraph::graph.adjacency(adj.mat, weighted=TRUE, mode ='undirected') 
    g <- igraph::simplify(g)
    igraph::V(g)$label <- igraph::V(g)$name
    igraph::V(g)$degree <- igraph::degree(g)
    SUMS <- diag(adj.mat)
    if (!log.labels) {
        igraph::V(g)$label.cex <- label.cex
    } else {
        igraph::V(g)$label.cex <- (log(SUMS)/max(log(SUMS))) + .5
    }
    nwc <- length(label.colors)
    COLORS <- text2color(words = V(g)$label, recode.words = target.words, 
        colors = label.colors)
    igraph::V(g)$label.color <- COLORS
    igraph::E(g)$color <- edge.color
    if (is.null(layout)) {
        layout <- igraph::layout.fruchterman.reingold(g)
    }
    if (dev.interactive()) dev.new()
    plot(g, layout=layout, vertex.size=0, vertex.color="white")
    if (is.null(title.padj)){
        title.padj = -4.5
    }
    if (is.null(title.location)){
        title.location = 3
    }
    if (!is.null(title.name)) {
        mtext(text = title.name, side = title.location, padj = title.padj, 
        col = title.color, family = title.font, cex = title.cex)
    }
}
