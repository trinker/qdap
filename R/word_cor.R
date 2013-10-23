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
#' @param word The word(s) vector to find associated words for.
#' @param r The correlation level find associated words for.  If positive this
#' is the minimum value, if negative this is the maximum value.
#' @param values logical.  If \code{TRUE}returns the named correlates (names are 
#' the words).  If \code{FALSE} only the associated words are returned.
#' @param \dots Other arguments passed to \code{\link[stats]{cor}}.
#' @return Returns a vector of associated words or correlation matrix if 
#' \code{r = NULL}..
#' @keywords correaltion, association 
#' @export
#' @seealso \code{\link[tm]{findAssocs}},
#' \code{\link[qdap]{word_associate}},
#' \code{\link[stats]{cor}}
#' @examples
#' x <- factor(with(rajSPLIT, paste(act, pad(TOT(tot)), sep = "|")))
#' word_cor(rajSPLIT$dialogue, x, "romeo", .45)
#' word_cor(rajSPLIT$dialogue, x, "love", .5)	
#' 
#' ## Negative correlation
#' word_cor(rajSPLIT$dialogue, x, "you", -.1)
#' with(rajSPLIT, word_cor(dialogue, list(person, act), "hate"))
#' 
#' words <- c("hate", "i", "love", "ghost")
#' with(rajSPLIT, word_cor(dialogue, x, words, r = .5))
#' with(rajSPLIT, word_cor(dialogue, x, words, r = .4))
#' 
#' ## Set `r = NULL` to get matrix between words
#' with(rajSPLIT, word_cor(dialogue, x, words, r = NULL))
word_cor <- function(text.var, grouping.var = ID(text.var), word, r = .7, 
    values = TRUE, ...) {

    WFM <- t(wfm(text.var = text.var, grouping.var = grouping.var))

    WFM <- data.frame(WFM, check.names = F)
    wordlen <- length(word) == 1

    if (!is.null(r)) {
        posit <- r > 0
        L1 <- lapply(word, cor_help1, m = WFM, o = r, sORw = wordlen, 
            vals = values, positive = posit, ...)
        names(L1) <- word
        L1
    } else {
        cor(WFM[, word], ...)
    }
}

cor_help1 <- function(n, m, o, sORw, vals, positive, ...) {
    L <- sapply(m[, !colnames(m) %in% tolower(n)], function(x) {
        cor(x, m[, tolower(n)], ...)
    })

    if (positive) {
        extr <- L > o
        phr <- "at least"
    } else {
        extr <- L < o
        phr <- "less than or equal to"
    }

    if (sum(extr) == 0) {
        if (sORw) {
            stop(sprintf("No words correlate %s %s", phr, o))
        } else {
            return(NULL)
        }
    }
    if (!vals) {
        names(L)[extr]
    } else {
        L[extr]
    }
}







