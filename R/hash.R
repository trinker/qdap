#' %% ~~function to do ... ~~
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
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
#' function (x) 
#' {
#'     e <- new.env(hash = TRUE, size = nrow(x), parent = emptyenv())
#'     apply(x, 1, function(col) assign(col[1], as.numeric(col[2]), 
#'         envir = e))
#'     return(e)
#'   }
#' 
hash <-
function(x) {
    e <- new.env(hash = TRUE, size = nrow(x), 
        parent = emptyenv())
    apply(x, 1, function(col) assign(col[1], 
    as.numeric(col[2]), envir = e))
    return(e)
}
