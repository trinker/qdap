#internal function used by trans_cloud and word.comp (not exportd)
qda.handler <-
function(x) {
    if (any(class(x) %in% c("freqList", "bagOwords"))) {
        if (methods::is(x, "freqList")) {
            return(freqTab2words(x))
        } else {
            if (methods::is(x, "bagOwords")) {
                return(x)
            }
        }
    } else {
        if (is.vector(x)) {
            warning ("Not a `word_list` object.")
            return(x)
        } else {
            stop("Must be a word_list object or a vector of raw words")
        }
    }
}
