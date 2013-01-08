#' Bag of Words
#' 
#' \code{bag.o.words} - Reduces a text column to a bag of words.
#' 
#' @param text.var The text variable.
#' @param apostrophe.remove logical.  If TRUE removes apostrophe's from the output.
#' @param \ldots further arguments passed to strip function.
#' @return Returns a vector of striped words.
#' @keywords bag-of-words
#' @rdname bag.o.words
#' @export
#' @examples
#' \dontrun{
#' bag.o.words(DATA$state)
#' by(DATA$state, DATA$person, bag.o.words)
#' lapply(DATA$state,  bag.o.words)
#' bag.o.words("I'm going home!", apostrophe.remove = FALSE)
#' 
#' DATA 
#' breaker(DATA$state)
#' by(DATA$state, DATA$person, breaker)
#' lapply(DATA$state,  breaker)
#'
#' word.split(c(NA, DATA$state))
#' }
bag.o.words <-
function(text.var, apostrophe.remove = FALSE, ...) {
    unblanker(words(strip(clean(text.var), apostrophe.remove = apostrophe.remove, ...)))
}

#' Bag of Stripped Words and End Marks
#' 
#' \code{breaker} - Reduces a text column to a bag of words and qdap recognized end marks.
#' 
#' @return \code{breaker} - returns a vector of striped words and qdap recognized endmarks (i.e. \code{".", "!", "?", "*", "-"}).
#' @rdname bag.o.words
#' @export
breaker <-
function(text.var) {
  unlist(strsplit(as.character(text.var), 
      "[[:space:+]]|(?=[|.!?*-])", perl=TRUE))
}


#' Bag of Words & Endmarks by Row
#' 
#' \code{word.split} - Reduces a text column to a list of vectors of bag of 
#' words and qda recognized endmarks (i.e. \code{".", "!", "?", "*", "-"}).
#' 
#' @rdname bag.o.words
#' @export
word.split <-
function (text.var) {
    x <- reducer(Trim(clean(text.var)))
    sapply(x, function(x) {
            unlist(strsplit(x, "[[:space:+]]|(?=[.!?*-])", perl = TRUE)) 
        }, simplify = FALSE
    )
}