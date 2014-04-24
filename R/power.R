#' Power Score (Sentiment Analysis)
#' 
#' \code{sentiment_frame} - Generate a sentiment lookup environment or data.frame 
#' for use with the \code{xxx.frame} argument of various sentiment functions 
#' function.
#' 
#' @param positives A character vector of positive words.
#' @param negatives A character vector of negative words.
#' @param pos.weights A vector of weights to weight each positive word by.  
#' Length must be equal to length of \code{postives} or length 1 (if 1 weight 
#' will be recycled). 
#' @param neg.weights A vector of weights to weight each negative word by.  
#' Length must be equal to length of \code{negatives} or length 1 (if 1 weight 
#' will be recycled). 
#' @param envir logical.  If \code{TRUE} a lookup table (a dataframe within 
#' an environment) is produced rather than a data.frame.
#' @export
#' @importFrom qdapTools hash 
#' @rdname power
sentiment_frame <- function(positives, negatives, pos.weights = 1, 
    neg.weights = -1, envir = TRUE) {
    plen <- length(positives)
    nlen <- length(negatives)
    if (!length(plen) %in% c(length(positives), 1)) {
        stop("The length of positives and pos.weights must be equal")
    }
    if (!length(nlen) %in% c(length(negatives), 1)) {
        stop("The length of negatives and negative weights must be equal")
    }
    if (length(pos.weights) == 1) {
        pos.weights <- rep(pos.weights, plen)
    }
    if (length(neg.weights) == 1) {
        neg.weights <- rep(neg.weights, nlen)
    }
    dat <- data.frame(words = c(positives, negatives), polarity = c(pos.weights, 
        neg.weights))
    if (envir) {
        hash(dat)
    } else {
        dat
    }
}