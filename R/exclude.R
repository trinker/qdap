#' Quickly exclude words from a word list
#' 
#' Quickly exclude words from a word list
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param word.list %% ~~Describe \code{word.list} here~~
#' @param \dots %% ~~Describe \code{\dots} here~~
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
#' function (word.list, ...) 
#' {
#'     mf <- match.call(expand.dots = FALSE)
#'     excluded <- as.character(mf[[3]])
#'     word.list[!word.list %in% excluded]
#'   }
#' 
exclude <-
function(word.list, ...) {
    if(is.vector(...)) {
        excluded <- unlist(...)
    } else {
        mf <- match.call(expand.dots = FALSE)   
        excluded <- as.character(mf[[3]])
    }
    if (!is.list(word.list)) {
      word.list[!word.list %in% excluded]
    } else {
      lapply(word.list, function(x) x[!x %in% excluded])
    }
}
