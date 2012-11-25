#' Capitalizes Select Words
#' 
#' A helper function for word_list that allows the user to supply vectors of words to be capitalized.
#' 
#' @param text A vector of words (generally from bag.o.words or breaker).
#' @param caps.list A list of words to capitalize.
#' @param I.list logical.  If TRUE capitalizes I words and contractions.
#' @param no.apostrophe logical, asking if apostrophes have been removed.  If TRUE will try to insert apostrophe's back into words appropriately.
#' @return Returns a vector of capitalized words based on supplied capitalization arguments.
#' @note Not intended for general use.  Acts as a helper function to several qdap functions.
#' @examples
#' capitalizer(bag.o.words("i like it but i'm not certain"), "like")
#' capitalizer(bag.o.words("i like it but i'm not certain"), "like", FALSE)
capitalizer <-
function(text, caps.list = NULL, I.list = TRUE, apostrophe.remove = FALSE) {
    I_list <- c("I'm", "I'll", "I'd", "I've", "I")
    IDF <- data.frame(from1 = sapply(I_list, function(x) strip(x, 
        apostrophe.remove = FALSE)), from2 = sapply(I_list, strip), 
        to = I_list)
    if (apostrophe.remove) {
        idf <- IDF[-1]
    } else {
        IDF2 <- IDF[-2]
        names(IDF2) <- c("from2", "to")
        idf <- data.frame(rbind(IDF[-2], IDF2))
    }
    names(idf) <- c("from", "to")
    rownames(idf) <- 1:nrow(idf)
    if (!I.list) {
        idf <- NULL
    }
    names <- data.frame(from = tolower(caps.list), to = gsub("(\\w)(\\w*)", 
        "\\U\\1\\L\\2", tolower(caps.list), perl = T))
    names2 <- data.frame(from = paste(names$from, "'s", sep = ""), 
        to = paste(names$to, "'s", sep = ""))
    idf <- rbind(idf, names, names2)
    idf$from <- as.character(idf$from)
    idf$to <- as.character(idf$to)
    subber <- function(x) ifelse(x %in% idf$from, idf[match(x, 
        idf$from), "to"], x)
    unlist(lapply(text, subber))
}
