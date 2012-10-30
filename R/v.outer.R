v.outer <- 
function(x, FUN, digits = 3, ...){
    FUN <- match.fun(FUN)
    if (is.matrix(x)) {
        x <- as.data.frame(x)
    }
    if (is.list(x) & !is.data.frame(x)){
        if (is.null(names(x))) {
            names(x) <- paste0("X", seq_along(x))
        }
        nms <- names(x)   
    } else {
        nms <- colnames(x)
    }
    z <- outer(
      nms, 
      nms, 
      Vectorize(function(i,j) FUN(unlist(x[[i]]), unlist(x[[j]]), ...))
    )
    dimnames(z) <- list(nms, nms)
    if (is.numeric(z)) {
        z <- round(z, digits = digits)
    }
    z
}