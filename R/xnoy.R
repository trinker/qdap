#' Finds the items in x not found in y
#' 
#' Finds the items in x not found in y
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param y %% ~~Describe \code{y} here~~
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
#' function (x, y) 
#' {
#'     x[!x %in% y]
#'   }
#' 
xnoy <-
function(x, y) {
    x[!x %in% y]
}
