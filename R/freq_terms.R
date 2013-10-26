#' Find Frequent Terms
#' 
#' Find the most frequently occuring terms in a text vector.
#' 
#' @param text.var The text variable.
#' @param top Top number of terms to show.
#' @param at.least An interger indicating at least how many letters a word 
#' must be to be included in the output.
#' @param stopwords A character vector of words to remove from the text.  qdap 
#' has a number of data sets that can be used as stop words including: 
#' \code{Top200Words}, \code{Top100Words}, \code{Top25Words}.  For the tm 
#' package's traditional English stop words use \code{tm::stopwords("english")}.
#' @param extend logical.  If \code{TRUE} the \code{top} argument is extended to 
#' any word that has the same frequency as the \code{top} word.
#' @param \ldots Other arguments passed to \code{\link[qdap]{all_words}}.
#' @return Returns a dataframe with the top occurring words.
#' @keywords frequent_terms
#' @export
#' @seealso \code{\link[qdap]{word_list}},
#' \code{\link[qdap]{all_words}}
#' @examples
#' \dontrun{
#' freq_terms(DATA$state, 5)
#' freq_terms(DATA$state)
#' freq_terms(DATA$state, extend = FALSE)
#' freq_terms(DATA$state, at.least = 4)
#' freq_terms(pres_debates2012$dialogue, stopwords = Top200Words)
#' }
freq_terms <- function(text.var, top = 20, at.least = 1, stopwords = NULL, 
    extend = TRUE, ...) {

    out <- all_words(text.var, alphabetical = FALSE, ...)
    if (!is.null(stopwords)) {
        out <- out[!out[, "WORD"] %in% tolower(stopwords), ]
    }
    if (!is.null(at.least)) {
        out <- out[nchar(gsub("'", "", out[, "WORD"])) > (at.least - 1), ]
    }

    if (nrow(out) < top) {
        grab <- seq_len(nrow(out))
    } else {
        if (extend) {
            grab <- out[, "FREQ"] >=  out[top, "FREQ"]
        } else {
            grab <- seq_len(top)
        }
    }

    out[grab, ]
}
