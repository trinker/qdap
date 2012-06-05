hash <-
function(x) {
    e <- new.env(hash = TRUE, size = nrow(x), 
        parent = emptyenv())
    apply(x, 1, function(col) assign(col[1], 
    as.numeric(col[2]), envir = e))
    return(e)
}
