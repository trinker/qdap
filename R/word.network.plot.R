word.network.plot <-
function(text.var, grouping.var = NULL, target.words = NULL, stopwords = Top100Words,
    label.cex = .8, label.size = .5, edge.curved = FALSE, vertex.shape = "circle", 
    edge.color = "gray70", label.colors = "black", layout = NULL, 
    title.name = NULL, title.padj =  -4.5, title.location = 3, title.font = NULL, 
    title.cex = .8, log.labels = FALSE, title.color = "black", 
    legend = NULL, legend.cex = .8, legend.location = c(-1.54, 1.41), plot = TRUE) {
    suppressWarnings(require(igraph))
    if (class(text.var) == "adjacency.matrix") { #actually takes an adjaceny matrix
       adj.mat.object <- text.var[["adjacency"]]
    } else {
        z <- word.freq.matrix(text.var = text.var, grouping.var = grouping.var,
            stopwords = stopwords)
        adj.mat.object <- adjmat(t(z))[["adjacency"]]
    }
    g <- igraph::graph.adjacency(adj.mat.object, weighted=TRUE, mode ='undirected') 
    g <- igraph::simplify(g)
    igraph::V(g)$label <- igraph::V(g)$name
    igraph::V(g)$degree <- igraph::degree(g)
    SUMS <- diag(adj.mat.object)
    if (!log.labels) {
        igraph::V(g)$label.cex <- label.cex
    } else {
        igraph::V(g)$label.cex <- (log(SUMS)/max(log(SUMS))) + label.size
    }
    if (!is.null(target.words)) {
        nwc <- length(label.colors)
        COLORS <- text2color(words = V(g)$label, recode.words = target.words, 
            colors = label.colors)
        igraph::V(g)$label.color <- COLORS
    } else {
        igraph::V(g)$label.color <- label.colors
    }
    igraph::V(g)$shape <- vertex.shape
    igraph::E(g)$color <- edge.color
    if (is.null(layout)) {
        layout <- igraph::layout.fruchterman.reingold(g)
    }
    if (plot) {
        if (dev.interactive()) dev.new()
        plot.igraph(g, layout=layout, vertex.size=0, vertex.color="white",
            edge.curved = edge.curved)
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
        if (!is.null(legend)){
            par(mar = rep(0, 4), xpd = NA)
            legend(x = legend.location[1], y = legend.location[2], 
                cex = legend.cex, legend = legend, 
                fill = label.colors[1:length(legend)])
            par(mar = c(5, 4, 4, 2) + 0.1, xpd = TRUE)
        }
    }
    invisible(g)
}
