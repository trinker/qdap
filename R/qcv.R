qcv <- 
function(..., terms = NULL, space.wrap = FALSE, trailing = FALSE, 
    leading = FALSE, split = " "){
    if (!is.null(terms)) {
        x <- unblanker(strsplit(terms, split = split))
    } else {
        x <- substitute(...())
    }
    z <- clean(Trim(unlist(lapply(x, function(y) as.character(y)))))
    if (space.wrap){
        z <- spaste(z)
    }      
    z <- spaste(z, trailing = trailing, leading = leading) 
    z
}