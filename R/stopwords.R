#' Remove Stopwords
#' 
#' Transcript apply the removal of stopwords.
#' 
#' @param textString A character string of text or a vector of character strings.
#' @param stopwords A character vector of words to remove from the text.  qdap 
#' has a number of data sets that can be used as stopwords including: 
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
#' list with the original \code{textString}.
#' @param ignore.case logical.  If \code{TRUE} stop words will be removed 
#' regardless of case.  Additionally, case will be stripped from the text.  If 
#' \code{FALSE} stopwords removal is contingent upon case.  Additionally, case 
#' is not stripped.
#' @param apostrophe.remove logical.  If \code{TRUE} removes apostrophe's from 
#' the output.
#' @param \ldots further arguments passed to \code{\link[qdap]{strip}} function.
#' @return Returns a vector of sentences, vector of words, or (default) a list 
#' of vectors of words with stop words removed.  Output depends on supplied 
#' arguments.
#' @seealso \code{\link[qdap]{strip}}, 
#' \code{\link[qdap]{bag.o.words}},
#' \code{\link[tm]{stopwords}}
#' @keywords stopwords
#' @export
#' @examples
#' \dontrun{
#' stopwords(DATA$state)
#' stopwords(DATA$state, tm::stopwords("english"))
#' stopwords(DATA$state, Top200Words)
#' stopwords(DATA$state, Top200Words, strip = TRUE)
#' stopwords(DATA$state, Top200Words, separate = FALSE)
#' stopwords(DATA$state, Top200Words, separate = FALSE, ignore.case = FALSE)
#' stopwords(DATA$state, Top200Words, unlist = TRUE)
#' stopwords(DATA$state, Top200Words, unlist = TRUE, strip=TRUE)
#' stopwords(DATA$state, Top200Words, unlist = TRUE, unique = TRUE)
#' }
stopwords<-
function (textString, stopwords = qdapDictionaries::Top25Words, unlist = FALSE, 
    separate = TRUE, strip = FALSE, unique = FALSE, char.keep = NULL, 
    names = FALSE, ignore.case = TRUE, apostrophe.remove = FALSE, ...) {
    Stopwords <- if (is.null(stopwords)) {
        c(" ")
    } else {
        stopwords
    }
    SW <- function(textString, stopwords) {
        "%w/o%" <- function(x, y) x[!x %in% y]
        breaker2 <- function(X) {
            strsplit(X, "[[:space:]]|(?=[!#$%&,-./:;?@_])", perl=TRUE)
        }  
        if (ignore.case) {
            unblanker(unlist(breaker2(tolower(Trim(textString)))) %w/o% 
                tolower(Trim(stopwords)))
        } else {
            unblanker(unlist(breaker2(Trim(textString))) %w/o% Trim(stopwords))
        }
    }
    if (strip) {
        textString <- strip(textString, char.keep = char.keep, 
            apostrophe.remove = apostrophe.remove, ...)
    }
    x <- lapply(textString, function(x) SW(x, Stopwords))
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
        names(x) <- textString
    } else {
        names(x) <- NULL
    }
    return(x)
}
