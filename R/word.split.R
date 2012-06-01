word.split <-
function(x) {
    NAer <- function(x) {
        if (is.na(x)) {
            NA
        } else {
            strsplit(x, " ")
        }
    }
    sapply(x, function(x) as.vector(unlist(NAer(x))))
}
