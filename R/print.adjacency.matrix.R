print.adjacency.matrix <-
function(adjacency.matrix) {
    cat("Adjacency Matrix:\n\n")
    print(adjacency.matrix$shared, na.print="", quote=FALSE)
    cat("\n\n")
    cat("Summed Overlap:\n\n")
    print(adjacency.matrix$summed_occurrences)
}
