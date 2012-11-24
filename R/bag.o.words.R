#' Bag of Stripped Words
#' 
#' Reduces a text column to a bag of words.
#' 
#' @param text.var The text variable
#' @param apostrophe.remove logical.  If TRUE removes apostrophe's from the output
#' @param \\ldots further arguments passed to strip function
#' @return Returns a vector of striped words.
#' @seealso \code{\link[qdap]{strip}}, \code{\link[qda]{breaker}}
#' @keywords bag of words
#' @examples
#' bag.o.words(DATA$state)
#' by(DATA$state, DATA$person, bag.o.words)
#' lapply(DATA$state,  bag.o.words)
#' bag.o.words("I'm going home!", apostrophe.remove = FALSE)
bag.o.words <-
function(text.var, apostrophe.remove = FALSE, ...) {
    unblanker(words(strip(clean(text.var), apostrophe.remove = apostrophe.remove, ...)))
}
