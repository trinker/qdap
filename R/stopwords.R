#' Transcript Apply Removal of Specified Stopwords
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param textString %% ~~Describe \code{textString} here~~
#' @param stopwords %% ~~Describe \code{stopwords} here~~
#' @param unlist %% ~~Describe \code{stopwords} here~~
#' @param strip %% ~~Describe \code{stopwords} here~~
#' @param unique %% ~~Describe \code{stopwords} here~~
#' @param names %% ~~Describe \code{stopwords} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
#' 
stopwords<-
function (textString, stopwords = Top25Words, unlist = FALSE, 
          strip = FALSE, unique = FALSE, names = FALSE, char.keep = NULL) {
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
    return(x)
}
