qcv <- 
function(..., terms = NULL, space.wrap = FALSE, trailing = FALSE, 
    leading = FALSE, split = " ", rm.blank = TRUE){
    if (!is.null(terms)) {
        x <- strsplit(terms, split = split)
    } else {
        x <- substitute(...())
    }
    z <- unblanker(scrubber(unlist(lapply(x, function(y) as.character(y)))))
    if (rm.blank) {
        z <- unblanker(z)
    }
    if (space.wrap){
        z <- spaste(z)
    }      
    z <- spaste(z, trailing = trailing, leading = leading) 
    z
}