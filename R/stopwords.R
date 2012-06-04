stopwords <-
function(textString, stopwords = Top25Words) {  
    Stopwords <- if (is.null(stopwords)) {
        c(" ")
    } else {
        stopwords
    }
    SW <- function(textString, stopwords) {
        "%w/o%" <- function(x, y) x[!x %in% y]
        unlist(strsplit(tolower(Trim(textString)), 
            " ")) %w/o% tolower(Trim(stopwords))
    }
    sapply(textString, function(x) SW(x, Stopwords))
}
