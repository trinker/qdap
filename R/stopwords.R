#' Remove Stopwords
#' 
#' Transcript apply the remova of stopwords
#' 
#' @param textString
#' @param stopwords
#' @param unlist
#' @param separate
#' @param strip
#' @param unique
#' @param char.keep
#' @param names logical.  If TRUE will name the elements of the vector or list witht he original textString.
#' @param ignore.case
#' @return Returns a vector o word
#' @seealso 
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' stopwords(DATA$state)
#' stopwords(DATA$state, tm::stopwords("english"))
#' stopwords(DATA$state, Top200Words)
#' stopwords(DATA$state, Top200Words, strip = TRUE)
#' stopwords(DATA$state, Top200Words, separate = FALSE)
#' stopwords(DATA$state, Top200Words, separate = FALSE, ignore.case = FALSE)
#' stopwords(DATA$state, Top200Words, unlist = TRUE)
#' stopwords(DATA$state, Top200Words, unlist = TRUE, unique = TRUE)
stopwords<-
function (textString, stopwords = Top25Words, unlist = FALSE, separate = TRUE, 
    strip = FALSE, unique = FALSE, char.keep = NULL, names = FALSE, 
    ignore.case = TRUE) {
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
      textString <- strip(textString, char.keep = char.keep)
    }
    x <- sapply(textString, function(x) SW(x, Stopwords))
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
    }
    return(x)
}