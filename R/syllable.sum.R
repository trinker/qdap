syllable.sum <-
function(text) {
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    unlist(lapply(as.character(text), function(x) sum(syllable.count(Trim(x))['syllables'])))
}
