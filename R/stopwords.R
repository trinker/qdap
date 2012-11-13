#' Reomove Stopwords
#' 
#'  Transcript apply the remova of stopwords
#' 
#' @param textString
#' @param stopwords
#' @param unlist
#' @param separate
#' @param strip
#' @param unique
#' @param names
#' @param char.keep
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' stopwords(DATA$state)
#' stopwords(DATA$state, tm::stopwords("english"))
#' stopwords(DATA$state, Top200Words)
#' stopwords(DATA$state, Top200Words, strip = TRUE)
#' stopwords(DATA$state, Top200Words, bagowords=FALSE)
#' stopwords(DATA$state, Top200Words, unlist = TRUE)
#' stopwords(DATA$state, Top200Words, unlist = TRUE, unique = TRUE)
stopwords<-
function (textString, stopwords = Top25Words, unlist = FALSE, separate = TRUE, 
          strip = FALSE, unique = FALSE, char.keep = NULL) {
    Stopwords <- if (is.null(stopwords)) {
        c(" ")
    } else {
        stopwords
    }
    SW <- function(textString, stopwords) {
        "%w/o%" <- function(x, y) x[!x %in% y]
        unlist(strsplit(tolower(Trim(textString)), " ")) %w/o% 
            tolower(Trim(stopwords))
    }
    if (strip) {
      textString <- strip(textString, char.keep = char.keep)
    }
    x <- sapply(textString, function(x) SW(x, Stopwords), USE.NAMES = names)
    if (unlist) {
        x <- unlist(x)
    }
    if (unique) {
        x <- unique(x)
    }
    if (!separate) {
        x <- sapply(stopwords(dat1$dialogue), paste, 
            collapse = " ", USE.NAMES = FALSE)
    }
    return(x)
}