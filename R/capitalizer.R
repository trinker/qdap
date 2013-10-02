#' Capitalize Select Words
#' 
#' A helper function for \code{\link[qdap]{word_list}} that allows the user to 
#' supply vectors of words to be capitalized.
#' 
#' @param text A vector of words (generally from \code{bag_o_words} or 
#' \code{breaker}).
#' @param caps.list A list of words to capitalize.
#' @param I.list logical.  If \code{TRUE} capitalizes I words and contractions.
#' @param apostrophe.remove logical, asking if apostrophes have been removed.  
#' If \code{TRUE} will try to insert apostrophe's back into words appropriately.
#' @return Returns a vector of capitalized words based on supplied 
#' capitalization arguments.
#' @note Not intended for general use.  Acts as a helper function to several 
#' qdap functions.
#' @export
#' @examples
#' \dontrun{
#' capitalizer(bag_o_words("i like it but i'm not certain"), "like")
#' capitalizer(bag_o_words("i like it but i'm not certain"), "like", FALSE)
#' }
capitalizer <-
function(text, caps.list = NULL, I.list = TRUE, apostrophe.remove = FALSE) {
    I_list <- c("I'm", "I'll", "I'd", "I've", "I")
    idf <- data.frame(from = sapply(I_list, function(x) strip(x, 
        apostrophe.remove = FALSE)), to = I_list)
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

