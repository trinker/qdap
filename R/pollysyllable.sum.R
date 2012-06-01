pollysyllable.sum <-
function(text) {
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    counter <- function(x) {
        y <- as.data.frame(table(syllable.count(Trim(x))["syllables"]))
        z <- subset(y, as.numeric(as.character(Var1)) >= 3)
        j <- sum(z$Freq)
        return(j)
    }
    unlist(lapply(as.character(text), function(x) counter(x)))
}
