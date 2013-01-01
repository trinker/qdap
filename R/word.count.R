#' Word Counts
#' 
#' Transcript Apply Word Counts
#' 
#' @rdname word.count
#' @param text.var The text variable
#' @param byrow logical.  If TRUE counts by row, if FALSE counts all words.
#' @param missing Value to insert for missing values (empty cells).
#' @param digit.remove logical.  If TRUE removes digits before counting words.
#' @param names logical.  If TRUE the sentences are given as the names of the 
#' counts.
#' @return Returns a word count by row or total.
#' @note wc is a convienent short hand for word.count.
#' @seealso \code{\link[qdap]{character.count}},
#' \code{\link[qdap]{syllable.count}}
#' @keywords word count
#' @export 
#' @examples
#' word.count(DATA$state)
#' wc(DATA$state)
#' word.count(DATA$state, names = TRUE)
#' word.count(DATA$state, by= "all", names = TRUE)
#' sum(word.count(DATA$state))
word.count <- 
function(text.var, byrow = TRUE, missing = NA, digit.remove = TRUE, names = FALSE) {
    len2 <- function(x, missing) {
        len <- length(x)
        ifelse((len == 0) | len == 1 && (is.na(x) | is.null(x)), missing, len)
    }
    txt <- stopwords(text.var, strip = TRUE,  digit.remove = digit.remove)
    z <- sapply(txt, len2, missing = missing)
    if (!byrow) {
        z <- sum(z, na.rm = TRUE)   
    }
    if(names) {
        names(z) <- text.var
    }
    z
}

#' @rdname word.count
#' @export
wc <- word.count
