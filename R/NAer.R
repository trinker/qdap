#' Replace Missing Values (NA)
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param replace %% ~~Describe \code{replace} here~~
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
#' function (x, replace = 0) 
#' {
#'     if (is.vector(x)) {
#'         x[is.na(x)] <- replace
#'         return(x)
#'     }
#'     else {
#'         y <- apply(x, 1, function(x) {
#'             x[is.na(x)] <- replace
#'             return(x)
#'         })
#'         y <- data.frame(t(y))
#'         return(y)
#'     }
#'   }
#' 
NAer <-
function(x, replace=0){
    if (is.vector(x)){
          x[is.na(x)] <- replace
          return(x )
    } else {
          y <- apply(x, 1, function(x) {x[is.na(x)] <- replace; return(x)})
          y <- data.frame(t(y))
          return(y)
    }
}
