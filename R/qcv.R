qcv <-
function(terms, space.wrap = FALSE, sep = ",", fixed = TRUE, ...){
    x <- as.character(substitute(terms))
    z <- Trim(unlist(strsplit(x, split = sep, fixed = fixed, ...)))   
    if (space.wrap){
        z <- spaste(z)
    }      
    return(z)
}