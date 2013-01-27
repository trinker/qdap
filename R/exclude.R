#' Exclude Elements From a Vector
#' 
#' Quickly exclude words from a word list
#' 
#' @param word.list A list of words/terms to exclude from.
#' @param \dots A vector (character/numeric) if element(s) to be excluded from 
#' the \code{word.list}.
#' @return Returns a vector with the excluded terms removed.
#' @export
#' @examples
#' \dontrun{
#' exclude(1:10, 3, 4)
#' exclude(1:10, 3:4)
#' Top25Words
#' exclude(Top25Words, qcv(the, of, and))
#' exclude(Top25Words, "the", "of", "an")
#' 
#' #Using with term.match and termco 
#' terms <- term.match(DATA$state, qcv(th), FALSE) 
#' exclude(terms, "truth")  
#' #all together
#' termco(DATA$state, DATA$person, exclude(term.match(DATA$state, qcv(th), 
#'     FALSE), "truth"))
#' 
#' MTCH.LST <- exclude(term.match(DATA$state, qcv(th, i)), qcv(truth, stinks))
#' termco(DATA$state, DATA$person, MTCH.LST)
#' }
exclude <-
function(word.list, ...) {
    mes <- try(is.vector(...), TRUE)
    if(substring(mes[[1]], 1, 5) != "Error") {
        excluded <- unlist(...)
    } else {
        mf <- match.call(expand.dots = FALSE)   
        excluded <- as.character(mf[[3]])
    }
    if (!is.list(word.list)) {
      word.list[!word.list %in% excluded]
    } else {
      lapply(word.list, function(x) x[!x %in% excluded])
    }
}
