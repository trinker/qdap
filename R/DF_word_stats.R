#a helper function used in word_stats (not exported)
DF_word_stats <-
function(text.var, digit.remove = FALSE, apostrophe.remove = FALSE, 
    digits = 3, parallel = FALSE) {
    DF <- na.omit(data.frame(text.var = text.var, 
        stringsAsFactors = FALSE))
    DF$n.sent <- 1:nrow(DF)
    DF$word.count <- word.count(DF$text.var, missing = 0, 
        digit.remove = digit.remove)
    DF$character.count <- character.count(DF$text.var, 
        apostrophe = apostrophe.remove, digit.remove = digit.remove)
    DF <- data.frame(DF, combo_syllable.sum(DF$text.var, parallel = parallel))
    DF <- DF[, c("text.var", "n.sent", "word.count", "character.count",
        "syllable.count",  "polysyllable.count") ]
    DF <- transform(DF, char2word.ratio = 
        round(character.count/word.count, digits=digits),
        syl2word.ratio = round(syllable.count/word.count, digits=digits),
        polysyl2word.ratio = round(polysyllable.count/word.count, digits=digits))
    punctuation <- function(x) substr(x, nchar(x), nchar(x))
    DF$end.mark <- unlist(lapply(DF$text.var, punctuation))
    rownames(DF) <- 1:nrow(DF)
    DF <- DF[order(DF$n.sent),]  
    return(DF)
}
