print.adjacency.matrix <-
function(adjacency.matrix) {
    cat("Adjacency Matrix:\n\n")
    print(adjacency.matrix$shared, na.print="", quote=FALSE)
    cat("\n\n")
    cat("Summed occurrences:\n\n")
    print(adjacency.matrix$sum)
}
