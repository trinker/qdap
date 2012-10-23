#' Word Counts
#' 
#' Transcript Apply Word Counts
#' 
#' @aliases word.count wc
#' @param text.var The text variable
#' @param by an optional character string giving a method for counting words.  
#' This is one of the strings ``row'' or ``all''.
#' @param missing Value to insert for missing values (empty cells).
#' @param digit.remove logical.  If TRUE removes digits before counting words.
#' @param names logical.  If TRUE the sentences are given as the names of the counts.
#' @return Returns a word count by row or total.
#' @seealso \code[qdap]{\link{character.count}}
#' @keywords word count
#' @examples
#' word.count(DATA$state)
#' word.count(DATA$state, names = TRUE)
#' word.count(DATA$state, by= "all", names = TRUE)
#' sum(word.count(DATA$state))
word.count <- 
function(text.var, by = "row", missing = NA, digit.remove = TRUE, names = FALSE) {
    txt <- sapply(as.character(text.var), function(x) {
            ifelse(is.na(x)|is.null(x), "", x)
        }
    )
    OP <- switch(by, 
         all = {length(unblanker(unlist(word.split(reducer(unlist(strip(
                   paste(as.character(txt), collapse = " "), 
                   digit.remove = digit.remove)))))))}, 
         row = {sapply(txt, function(x) length(unblanker(unlist(word.split(
                   reducer(unlist(strip(as.character(x), digit.remove = 
                   digit.remove))))))))}
    )
    z <- ifelse(OP==0, missing, OP)
    if(!names) {
        names(z) <- NULL
    }
    z
}

wc <- word.count
