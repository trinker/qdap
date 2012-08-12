#' Searches Text Column for Words
#' 
#' A convenience function to find words that begin with or contain a letter chunk and returns the frequency counts of the number of occurrences of each word.
#' 
#' @param text.var The text variable
#' @param begins.with This argument takes a word chunk.  Default is NULL. Use this if searching for a word begining with the word chunk.
#' @param contains This argument takes a word chunk.  Default is NULL. Use this if searching for a word containing the word chunk.
#' @param alphabetical logical.  If True orders rows alphabetically, if false orders the rows by frequency.
#' @return Returns a dataframe with frequency counts of words that begin with or containt he provided word chunk.
#' @note Can not provide both begins.with and contains arguments at once.  If both begins.with and contains are NULL all.words returns a frequency count for all words.
#' @seealso 
#' \code{\link[qdap]{term.match}}
#' @examples
#' #' all.words(raj$dialogue, begins.with="re")
#' all.words(raj$dialogue, "q")
#' all.words(raj$dialogue, contains="conc")
#' all.words(raj$dialogue)
#' 
all.words <-
function(text.var, begins.with = NULL, contains = NULL, alphabetical = TRUE){
    if (!is.null(begins.with) & !is.null(contains)) {
        stop("Can not use both 'begins.with' & 'contains' arguments")
    }
    if(!is.null(begins.with)) begins.with <- tolower(begins.with)
    if(!is.null(contains)) contains <- tolower(contains)
    WORDS <- unlist(word.split(reducer(strip(text.var))))
    names(WORDS) <- NULL
    y <- data.frame(table(WORDS), stringsAsFactors = FALSE)
    names(y) <- c("WORD", "FREQ")
    y$WORD <- as.character(y$WORD)
    if (!is.null(begins.with)) {
        y <- y[substring(y[, 1], 1, nchar(begins.with)) %in% begins.with, ]
        if(nrow(y)==0) stop("No words match")
    }
    if (!is.null(contains)) {
        y <- y[grep(contains, y[, 1]), ]
        if(nrow(y)==0) stop("No words match")
    }
    if (!alphabetical) {
        y <- y[order(y$FREQ, y$WORD), ]
    }
    rownames(y) <- NULL
    left.just(y, "WORD")
}
