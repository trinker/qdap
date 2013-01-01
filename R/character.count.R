#' Count Number of Characters
#' 
#' Transcript Apply Character Counts
#' 
#' @param text.var The text variable
#' @param byrow logical.  If TRUE counts by row, if FALSE counts all words.
#' @param missing Value to insert for missing values (empty cells).
#' @param apostrophe.remove = TRUE logical.  If TRUE apostrophes will be counted 
#' in the character count.
#' @param digit.remove logical.  If TRUE removes digits before counting words.
#' @param count.space logical.  If TRUE spaces are counted as characters.
#' @return Returns a character count by row or total.
#' @seealso \code[qdap]{\link{word.count}}
#' @keywords character count
#' @export
#' @examples
#' character.count(DATA$state)
#' character.count(DATA$state, by= "all")
#' sum(character.count(DATA$state))
character.count <- 
function(text.var, byrow = TRUE, missing = NA, apostrophe.remove = TRUE,
    digit.remove = TRUE, count.space = FALSE) {
    len2 <- function(x, missing) {
        len <- length(x)
        ifelse((len == 0) | (is.na(x) | is.null(x)), missing, nchar(x))
    }
    txt <- stopwords(text.var, strip = TRUE,  separate =  FALSE,
        digit.remove = digit.remove, stopwords = NULL)
    txt[txt %in% c("", "NA")] <- NA
    if (!count.space) {
        txt <- gsub("\\s+", "", txt)
    }
    z <- unlist(lapply(txt, len2, missing = missing))
    if (!byrow) {
        z <- sum(z, na.rm = TRUE)   
    }
    z
}