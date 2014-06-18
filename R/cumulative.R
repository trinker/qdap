#' Cumulative Scores
#' 
#' \code{cumulative} - Generate rolling/cumulative scores for select \pkg{qdap} 
#' objects.
#' 
#' @param x A qdap object with an accompanying \code{cumulative} method.
#' @param \ldots ignored
#' @rdname cumulative
#' @export
cumulative <- function(x, ...) {

    x
    
    UseMethod("cumulative")
} 