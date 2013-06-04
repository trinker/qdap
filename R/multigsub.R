#' Multiple gsub
#' 
#' A wrapper for \code{\link[base]{gsub}} that takes a vector of search terms 
#' and a vector or single value of replacements.
#' 
#' @param pattern Character string to be matched in the given character vector. 
#' @param replacement Character string equal in length to pattern or of length 
#' one which are  a replacement for matched pattern. 
#' @param text.var The text variable.
#' @param leadspace logical.  If \code{TRUE} inserts a leading space in the 
#' replacements.
#' @param trailspace logical.  If \code{TRUE}TRUE inserts a trailing space in the 
#' replacements.
#' @param fixed logical. If \code{TRUE}, pattern is a string to be matched as is. 
#' Overrides all conflicting arguments.
#' @param trim logical.  If \code{TRUE} leading and trailing white spaces are 
#' removed.
#' @param \dots Additional arguments passed to \code{\link[base]{gsub}}.
#' @rdname multigsub
#' @return Returns a vector with the pattern replaced.
#' @note The replacements occur sequentially rather than all at once.  This 
#' means a previous (first in pattern string) sub could alter a later sub.
#' @seealso \code{\link[base]{gsub}}
#' @export
#' @examples
#' \dontrun{
#' multigsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
#' mgsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
#' mgsub("[[:punct:]]", "PUNC", DATA$state, fixed = FALSE)
#' }
multigsub <-
function(pattern, replacement = NULL, text.var, leadspace = FALSE, 
    trailspace = FALSE, fixed = TRUE, trim = TRUE, ...){
    if (leadspace | trailspace) {
        replacement <- spaste(replacement, trailing = trailspace, 
            leading = leadspace)
    }
    key <- data.frame(pat=pattern, rep=replacement, 
        stringsAsFactors = FALSE)
    msubs <-function(K, x, trim, ...){
        sapply(seq_len(nrow(K)), function(i){
                x <<- gsub(K[i, 1], K[i, 2], x, fixed = fixed, ...)
            }
        )
        if (trim) x <- gsub(" +", " ", x)
        return(x)
    }
    if (trim) {
        x <- Trim(msubs(K=key, x=text.var, trim = trim, ...))
    } else {    
        x <- msubs(K=key, x=text.var, trim = trim, ...)
    }
    return(x)
}
#' @rdname multigsub
#' @export
mgsub <- multigsub
