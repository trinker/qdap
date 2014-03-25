#' Remove Stop Words
#' 
#' Removal of stop words in a variety of contexts
#' .
#' 
#' @param text.var A character string of text or a vector of character strings.
#' @param stopwords A character vector of words to remove from the text.  qdap 
#' has a number of data sets that can be used as stop words including: 
#' \code{Top200Words}, \code{Top100Words}, \code{Top25Words}.  For the tm 
#' package's traditional English stop words use \code{tm::stopwords("english")}.
#' @param unlist logical.  If \code{TRUE} unlists into one vector.  General use 
#' intended for when separate is \code{FALSE}.
#' @param separate logical.  If \code{TRUE} separates sentences into words. If 
#' \code{FALSE} retains sentences.
#' @param strip logical.  IF \code{TRUE} strips the text of all punctuation 
#' except apostrophes.
#' @param unique logical.  If \code{TRUE} keeps only unique words (if unlist is 
#' \code{TRUE}) or sentences (if unlist is \code{FALSE}).  General use intended 
#' for when unlist is \code{TRUE}.
#' @param char.keep If strip is \code{TRUE} this argument provides a means of 
#' retaining supplied character(s).
#' @param names logical.  If \code{TRUE} will name the elements of the vector or 
#' list with the original \code{text.var}.
#' @param ignore.case logical.  If \code{TRUE} stopwords will be removed 
#' regardless of case.  Additionally, case will be stripped from the text.  If 
#' \code{FALSE} stop word removal is contingent upon case.  Additionally, case 
#' is not stripped.
#' @param apostrophe.remove logical.  If \code{TRUE} removes apostrophe's from 
#' the output.
#' @param \ldots further arguments passed to \code{\link[qdap]{strip}} function.
#' @return Returns a vector of sentences, vector of words, or (default) a list 
#' of vectors of words with stop words removed.  Output depends on supplied 
#' arguments.
#' @seealso \code{\link[qdap]{strip}}, 
#' \code{\link[qdap]{bag_o_words}},
#' \code{\link[tm]{stopwords}}
#' @keywords stopwords
#' @export
#' @examples
#' \dontrun{
#' rm_stopwords(DATA$state)
#' rm_stopwords(DATA$state, tm::stopwords("english"))
#' rm_stopwords(DATA$state, Top200Words)
#' rm_stopwords(DATA$state, Top200Words, strip = TRUE)
#' rm_stopwords(DATA$state, Top200Words, separate = FALSE)
#' rm_stopwords(DATA$state, Top200Words, separate = FALSE, ignore.case = FALSE)
#' rm_stopwords(DATA$state, Top200Words, unlist = TRUE)
#' rm_stopwords(DATA$state, Top200Words, unlist = TRUE, strip=TRUE)
#' rm_stop(DATA$state, Top200Words, unlist = TRUE, unique = TRUE)
#' 
#' c("I like it alot", "I like it too") %sw% qdapDictionaries::Top25Words
#' }
rm_stopwords <-
function (text.var, stopwords = qdapDictionaries::Top25Words, unlist = FALSE, 
    separate = TRUE, strip = FALSE, unique = FALSE, char.keep = NULL, 
    names = FALSE, ignore.case = TRUE, apostrophe.remove = FALSE, ...) {
    Stopwords <- if (is.null(stopwords)) {
        c(" ")
    } else {
        stopwords
    }
    SW <- function(text.var, stopwords) {
        "%w/o%" <- function(x, y) x[!x %in% y]
        breaker2 <- function(X) {
            strsplit(X, "[[:space:]]|(?=[!#$%&,-./:;?@_])", perl=TRUE)
        }  
        if (ignore.case) {
            unblanker(unlist(breaker2(tolower(Trim(text.var)))) %w/o% 
                tolower(Trim(stopwords)))
        } else {
            unblanker(unlist(breaker2(Trim(text.var))) %w/o% Trim(stopwords))
        }
    }
    if (strip) {
        text.var <- strip(text.var, char.keep = char.keep, 
            apostrophe.remove = apostrophe.remove, ...)
    }
    x <- lapply(text.var, function(x) SW(x, Stopwords))
    if (unlist) {
        x <- unlist(x)
    }
    if (unique) {
        x <- unique(x)
    }
    if (!separate) {
        x <- sapply(x, paste, collapse = " ", USE.NAMES = FALSE)
        x <- mgsub(c(" .", " ?", " ,", " !"), c(".", "?", ",", "!"), x)
    }
    if (names) {
        names(x) <- text.var
    } else {
        names(x) <- NULL
    }
    return(x)
}

#' @rdname rm_stopwords
#' @export
rm_stop <- rm_stopwords



#' Remove Stop Words
#' 
#' \code{\%sw\%} - Binary operator version of \code{\link[qdap]{rm_stopwords}}  that
#' defaults to \code{separate = FALSE}..
#' 
#' @rdname rm_stopwords
#' @export
`%sw%` <- function(text.var, stopwords = qdapDictionaries::Top25Words) {
    rm_stopwords(text.var = text.var, stopwords = stopwords, separate=FALSE)
}

