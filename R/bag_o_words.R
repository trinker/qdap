#' Bag of Words
#' 
#' \code{bag_o_words} - Reduces a text column to a bag of words.
#' 
#' @param text.var The text variable.
#' @param apostrophe.remove logical.  If \code{TRUE} removes apostrophe's from 
#' the output.
#' @param \ldots Additional arguments passed to strip.
#' @return Returns a vector of stripped words.
#' @keywords bag-of-words
#' @rdname bag_o_words
#' @export
#' @examples
#' \dontrun{
#' bag_o_words("I'm going home!")
#' bag_o_words("I'm going home!", apostrophe.remove = TRUE)
#' unbag(bag_o_words("I'm going home!"))
#' 
#' bag_o_words(DATA$state)
#' by(DATA$state, DATA$person, bag_o_words)
#' lapply(DATA$state,  bag_o_words)
#' 
#' breaker(DATA$state)
#' by(DATA$state, DATA$person, breaker)
#' lapply(DATA$state,  breaker)
#' unbag(breaker(DATA$state))
#' 
#' word_split(c(NA, DATA$state))
#' unbag(word_split(c(NA, DATA$state)))
#' }
bag_o_words <-
function(text.var, apostrophe.remove = FALSE, ...) {
    if (identical(list(), list(...))) {
        bag_o_words1(x = text.var, apostrophe.remove = apostrophe.remove, ...)
    } else {
        bag_o_words2(x = text.var, apostrophe.remove = apostrophe.remove)
    }
}

bag_o_words1 <- 
function(x, apostrophe.remove = FALSE) {
    x <- gsub("\\|", "", x[!is.na(x)])
    x <- paste(x, collapse=" ")
    if(apostrophe.remove) {
        reg <- "[^[:alpha:]]"
        x <- gsub("'", "", x)
    } else {
        reg <- "[^[:alpha:]|\\']"
    }
    x <- strsplit(tolower(gsub(reg, " ", x)), "\\s+")[[1]]
    x[x != ""]
}

bag_o_words2 <-
function(x, apostrophe.remove = FALSE, ...) {
    unblanker(words(strip(clean(x), 
        apostrophe.remove = apostrophe.remove, ...)))
}

#' Bag of Stripped Words and End Marks
#' 
#' \code{unbag} - Wrapper for \code{paste(collapse=" ")} to glue words back into 
#' strings. 
#' 
#' @param na.rm logical.  If \code{TRUE} \code{NA}s are removed before pasting.
#' @return \code{unbag} - Returns a string.
#' @rdname bag_o_words
#' @export
unbag <- function(text.var, na.rm = TRUE) {
    text.var <- unlist(text.var)
    if (na.rm) text.var <- text.var[!is.na(text.var)]
    paste(text.var, collapse=" ")
}


#' Bag of Stripped Words and End Marks
#' 
#' \code{breaker} - Reduces a text column to a bag of words and qdap recognized 
#' end marks.
#' 
#' @return \code{breaker} - Returns a vector of striped words and qdap 
#' recognized endmarks (i.e., \code{".", "!", "?", "*", "-"}).
#' @rdname bag_o_words
#' @export
breaker <-
function(text.var) {
    unblanker(unlist(strsplit(as.character(text.var), 
        "[[:space:]]|(?=[|.!?*-])", perl=TRUE)))
}


#' Bag of Words & Endmarks by Row
#' 
#' \code{word_split} - Reduces a text column to a list of vectors of bag of 
#' words and qdap recognized end marks (i.e., \code{".", "!", "?", "*", "-"}).
#' 
#' @rdname bag_o_words
#' @export
word_split <-
function (text.var) {
    x <- reducer(Trim(clean(text.var)))
    sapply(x, function(x) {
            unblanker(unlist(strsplit(x, "[[:space:]]|(?=[.!?*-])", perl = TRUE)))
        }, simplify = FALSE
    )
}
