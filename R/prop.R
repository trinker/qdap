prop <-
function(mat, digits = 2, percent = FALSE) {
    per <- if (percent) 100 else 1
    daf <- data.frame(mat)
    daf <-sapply(seq_along(daf), function(x) round(per*(mat[, x]/sum(mat)), 
        digits = digits))
    mat2 <- as.matrix(daf)
    colnames(mat2) <- colnames(mat)
    return(mat2)
}
