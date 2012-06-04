termco.c <-
function(termco.d.object, combined.columns, new.name, 
    zero.replace = 0, elim.old = TRUE){     
    x <- termco.d.object$raw
    y <- termco.d.object$prop
    if (!is.numeric(combined.columns)){
        combined.columns <- which(names(x) %in% combined.columns)
    }
    x <- transform(x, new.name = rowSums(x[, combined.columns]), 
        check.names=FALSE)
    names(x)[length(x)] <- names(y)[length(y)] <- new.name
    y <- transform(y, new.name = rowSums(y[, combined.columns]), 
        check.names=FALSE)
    z <- if (elim.old) {
        seq_along(x)[!seq_along(x) %in% combined.columns] 
    } else {
        seq_along(x)
    }
    x2 <- replacer(x, with = zero.replace)[, z]
    y2 <- replacer(y, with = zero.replace)[, z]
    DF <- replacer(termco.rnp(x, y), "0(0)", with = zero.replace)[, z]
    o <- list(raw = x2, prop = y2, rnp = DF)
    class(o) <- "termco_c"
    return(o)
}
