#' Find Correlated Words
#' 
#' Find associated words within grouping variable(s).
#' 
#' @param text.var The text variable (or an object from \code{\link[qdap]{pos}},
#' \code{\link[qdap]{pos_by}} or \code{\link[qdap]{formality}}.  Passing the 
#' later three object will greatly reduce run time.
#' @param grouping.var The grouping variables.  Default \code{NULL} generates 
#' one word list for all text.  Also takes a single grouping variable or a list 
#' of 1 or more grouping variables.
#' @param word The word to find associated words for.
#' @param cor The minimum correlation level find associated words for.
#' @param values logical.  If True returns the named correlates (names are the 
#' words).  If \code{FALSE} only the associated words are returned.
#' @return Returns a vector of associated words.
#' @keywords correaltion, association 
#' @export
#' @seealso \code{\link[tm]{findAssocs}},
#' \code{\link[qdap]{word_associate}}
#' @examples
#' x <- factor(with(rajSPLIT, paste(act, pad(TOT(tot)), sep = "|")))
#' word_cor(rajSPLIT$dialogue, x, "romeo", .45)
#' word_cor(rajSPLIT$dialogue, x, "love", .5)
#' with(rajSPLIT, word_cor(dialogue, list(person, act), "hate"))
word_cor <- function(text.var, grouping.var = ID(text.var), word, cor = .7, values = TRUE) {

    WFM <- t(wfm(text.var = text.var, grouping.var = grouping.var))

    WFM <- data.frame(WFM)
    L <- sapply(WFM[, !colnames(WFM) %in% tolower(word)], function(x) {
        cor(x, WFM[, tolower(word)])
    })

    extr <- L > cor
    if (sum(extr) == 0) stop(sprintf("No words correlate at least %s", cor))
    if (!values) {
        names(L)[extr]
    } else {
        L[extr]
    }

}



