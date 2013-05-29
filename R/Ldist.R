## Calculates  Levenshtein distance:

Ldist <- function(pattern, strings){
    minD <- function(string, pattern){ 
        for (i in seq(0, 1, .001)) {
            x <- agrep(pattern, string, max.distance = i)
            if (!identical(x, integer(0))) {
                m <- i
                break
            }
        }
        m
    }
    unlist(lapply(strings, minD, pattern = pattern))
}
