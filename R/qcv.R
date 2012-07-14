qcv <-
function(terms, sep = ",", fixed = TRUE, ...){
    x <- as.character(substitute(terms))
    z <- Trim(unlist(strsplit(x, split = sep, fixed = fixed, ...)))         
    return(z)
}
