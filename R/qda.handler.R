#internal function used by trans.cloud and word.comp (not exportd)
qda.handler <-
function(x) {
    if (!is.null(comment(x))) {
        if (comment(x) %in% "freqList") {
            return(freqTab2words(x))
        } else {
            if (!comment(x) %in% "bagOwords") {
                stop("Must be a qdap object or a vector of raw words")
            } else {
                return(x)
            }
        }
    } else {
        if (is.vector(x)) {
            warning ("Not a qdap object.")
            return(x)
        } else {
            stop("Must be a qdap object or a vector of raw words")
        }
    }
}
