#' Capitalizes Select Words
#' 
#' A helper function for word_list that allows the user to supply vectors of words to be capitalized.
#' 
#' @param text A vector of words (generally from bag.o.words or breaker).
#' @param caps.list A list of words to capitalize.
#' @param I.list logical.  If TRUE capitalizes I words and contractions.
#' @param no.apostrophe logical.  If TRUE will not insert apostrophe's back into words.
#' @return Returns a vector of capitalized words based on supplied capitalization arguments.
#' @examples
#' capitalizer(bag.o.words("i like it but i'm not certain"), "like")
capitalizer <-
function(text, caps.list = NULL, I.list = TRUE, no.apostrophe = FALSE) {
    I_list <- c("I'm", "I'll", "I'd", "I've", "I")
    IDF <- data.frame(from1 = sapply(I_list, function(x) strip(x, 
        apostrophe.remove = TRUE)), from2 = sapply(I_list, strip), 
        to = I_list)
    
    idf <- if (no.apostrophe == FALSE) {
        IDF[-1]
    } else {
        IDF2 <- IDF[-2]
        names(IDF2) <- c("from2", "to")
        data.frame(rbind(IDF[-1], IDF2))
    }
    names(idf) <- c("from", "to")
    rownames(idf) <- 1:nrow(idf)
    
    idf <- if (I.list) 
        idf else NULL
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
