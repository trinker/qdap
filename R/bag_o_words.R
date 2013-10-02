#' Bag of Words
#' 
#' \code{bag_o_words} - Reduces a text column to a bag of words.
#' 
#' @param text.var The text variable.
#' @param apostrophe.remove logical.  If \code{TRUE} removes apostrophe's from 
#' the output.
#' @param \ldots further arguments passed to strip function.
#' @return Returns a vector of striped words.
#' @keywords bag-of-words
#' @rdname bag_o_words
#' @export
#' @examples
#' \dontrun{
#' bag_o_words("I'm going home!")
#' bag_o_words("I'm going home!", apostrophe.remove = TRUE)
#' 
#' bag_o_words(DATA$state)
#' by(DATA$state, DATA$person, bag_o_words)
#' lapply(DATA$state,  bag_o_words)
#' 
#' breaker(DATA$state)
#' by(DATA$state, DATA$person, breaker)
#' lapply(DATA$state,  breaker)
#' 
#' word_split(c(NA, DATA$state))
#' }
bag_o_words <-
function(text.var, apostrophe.remove = FALSE, ...) {
    unblanker(words(strip(clean(text.var), 
        apostrophe.remove = apostrophe.remove, ...)))
}

#' Bag of Stripped Words and End Marks
#' 
#' \code{breaker} - Reduces a text column to a bag of words and qdap recognized 
#' end marks.
#' 
#' @return \code{breaker} - returns a vector of striped words and qdap 
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
