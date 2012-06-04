combo_syllable.sum <-
function(text) {
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    counter <- function(x) {
        w <- syllable.count(Trim(x))["syllables"]
        y <- as.data.frame(table(w))
        z <- subset(y, as.numeric(as.character(w)) >= 3)
        j <- sum(z$Freq)
        k <- sum(w)
        return(c(k, j))
    }
    m <- unlist(lapply(as.character(text), function(x) counter(x)))
    n <- as.data.frame(t(matrix(m, 2, length(m)/2)))
    names(n) <- c("syllable.count", "polysyllable.count")
    return(n)
}
