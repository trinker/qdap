#' Bag of Stripped Words
#' 
#' Reduces a text column to a bag of words.
#' 
#' @param text.varThe text variable
#' @param \\ldots further arguments passed to strip function
#' @return Returns a vector of striped words
#' @seealso \code{\link[qdap]{strip}}
#' @keywords bag of words
#' @examples
#' DATA 
#' bag.o.words(DATA$state)
#' by(DATA$state, DATA$person, bag.o.words)
#' lapply(DATA$state,  bag.o.words)
#' bag.o.words("I'm going home!", apostrophe.remove = FALSE)
bag.o.words <-
function(text.var, ...) unblanker(words(strip(clean(text.var), ...)))
