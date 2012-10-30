v.outer <- 
function(x, FUN, digits = 3, ...){
    FUN <- match.fun(FUN)
    if (is.matrix(x)) {
        x <- as.data.frame(x)
    }
    z <- outer(
      colnames(x), 
      colnames(x), 
      Vectorize(function(i,j) FUN(x[[i]], x[[j]], ...))
    )
    dimnames(z) <- list(colnames(x), colnames(x))
    if (is.numeric(z)) {
        z <- round(z, digits = digits)
    }
    z
}