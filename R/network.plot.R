network.plot <-
function(adj.mat.object, target.words = NULL, label.cex = .8, 
         edge.color = "gray70", label.colors = c("black", "gray50"), layout = NULL, 
         title.name = NULL, title.padj, title.location = 3, title.font, 
         title.cex = .8, log.labels = FALSE, title.color = "black", plot = TRUE) {
  suppressWarnings(require(igraph))
  if (class(adj.mat.object) == "adjacency.matrix") {
    adj.mat.object <- adj.mat.object[["adjacency"]]
  }
  g <- igraph::graph.adjacency(adj.mat.object, weighted=TRUE, mode ='undirected') 
  g <- igraph::simplify(g)
  igraph::V(g)$label <- igraph::V(g)$name
  igraph::V(g)$degree <- igraph::degree(g)
  SUMS <- diag(adj.mat.object)
  if (!log.labels) {
    igraph::V(g)$label.cex <- label.cex
  } else {
    igraph::V(g)$label.cex <- (log(SUMS)/max(log(SUMS))) + .5
  }
  if (!is.null(target.words)) {
    nwc <- length(label.colors)
    COLORS <- text2color(words = V(g)$label, recode.words = target.words, 
                         colors = label.colors)
    igraph::V(g)$label.color <- COLORS
  }
  igraph::E(g)$color <- edge.color
  if (is.null(layout)) {
    layout <- igraph::layout.fruchterman.reingold(g)
  }
  if (plot) {
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
  invisible(g)
}