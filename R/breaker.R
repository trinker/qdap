#' Bag of Stripped Words and End MArks
#' 
#' Reduces a text column to a bag of words and qdap recognized end marks.
#' 
#' @param text.var text.var
#' @return Returns a vector of striped words and qdap recognized end marks (|.!?*-).
#' @seealso \code{\link[qda]{bag.o.words}}
#' @keywords bag of words
#' @export
#' @examples 
#' DATA 
#' breaker(DATA$state)
#' by(DATA$state, DATA$person, breaker)
#' lapply(DATA$state,  breaker)
breaker <-
function(text.var) {
  unlist(strsplit(as.character(text.var), 
      "[[:space:]]|(?=[|.!?*-])", perl=TRUE))
}
