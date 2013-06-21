#' Add Leading/Trailing Spaces
#' 
#' Adds trailing and/or leading spaces to a vector of terms.
#' 
#' @param terms A character vector of terms to insert trailing and/or leading 
#' spaces.
#' @param leading logical.  If \code{TRUE} inserts a leading space in the terms.
#' @param trailing logical.  If \code{TRUE} inserts a trailing space in the 
#' terms.
#' @return Returns a character vector with trailing and/or leading spaces.
#' @export
#' @examples
#' \dontrun{
#' spaste(Top25Words)
#' spaste(Top25Words, FALSE)
#' spaste(Top25Words, trailing = TRUE, leading = FALSE) #or
#' spaste(Top25Words, , FALSE)
#' }
spaste <- 
function(terms, trailing = TRUE, leading = TRUE){
    if (leading) {
        s1 <- " "
    } else {
        s1 <- ""
    }
    if (trailing) {
        s2 <- " "
    } else {
        s2 <- ""
    }
    pas <- function(x) paste0(s1, x, s2)
    if (is.list(terms)) {
        z <- lapply(terms, pas)
    } else {
        z <- pas(terms)
    }
    return(z)
}
