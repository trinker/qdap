#' Transcript Apply Syllable and Polysyllable Count
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text %% ~~Describe \code{text} here~~
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
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' ## The function is currently defined as
#' function (text) 
#' {
#'     Trim <- function(x) gsub("^\s+|\s+$", "", x)
#'     counter <- function(x) {
#'         w <- syllable.count(Trim(x))["syllables"]
#'         y <- as.data.frame(table(w))
#'         z <- subset(y, as.numeric(as.character(w)) >= 3)
#'         j <- sum(z$Freq)
#'         k <- sum(w)
#'         return(c(k, j))
#'     }
#'     m <- unlist(lapply(as.character(text), function(x) counter(x)))
#'     n <- as.data.frame(t(matrix(m, 2, length(m)/2)))
#'     names(n) <- c("syllable.count", "polysyllable.count")
#'     return(n)
#'   }
#' 
combo_syllable.sum <-
function(text) {
    fun <- function(x) ifelse(x=="", NA, x)
    text <- fun(as.character(text.var))
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    counter <- function(x) {
        w <- syllable.count(Trim(x))["syllables"]
        y <- as.data.frame(table(w))
        z <- subset(y, as.numeric(as.character(w)) >= 3)
        j <- sum(z$Freq)
        k <- sum(w)
        return(c(k, j))
    }
    m <- unlist(lapply(text, function(x) counter(x)))
    n <- as.data.frame(t(matrix(m, 2, length(m)/2)))
    names(n) <- c("syllable.count", "polysyllable.count")
    return(n)
}
