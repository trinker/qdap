termco.mt <-
function(terms, text.var, return.list=TRUE) {
    y <- stopwords(text.var, stopwords = NULL, 
        unlist=TRUE, strip=TRUE, unique=TRUE)
    x <- lapply(unlist(terms), function(z) {
        v <- term.find(y, mat = z, logic=TRUE)
        y[v]
    })
    names(x) <- unlist(terms)
    if (!return.list){
        x <- sort(unique(unlist(x)))
    }
    return(x)
}
