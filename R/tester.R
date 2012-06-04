#' Search a Vector of Vectors for Condition (Code)
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param string %% ~~Describe \code{string} here~~
#' @param code %% ~~Describe \code{code} here~~
#' @param type %% ~~Describe \code{type} here~~
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
#' function (string, code, type = "logical") 
#' {
#'     C <- as.character(substitute(code))
#'     test <- function(S, x) any(S == x)
#'     x <- sapply(string, function(x) test(x, C))
#'     if (type == "logical") {
#'         x
#'     }
#'     else {
#'         if (type == "numeric") {
#'             which(x)
#'         }
#'         else {
#'             stop("type must be logical or numeric")
#'         }
#'     }
#'   }
#' 
tester <-
function(string, code, type="logical"){
    C <- as.character(substitute(code))
    test <- function(S, x) any(S==x)
    x <- sapply(string, function(x)test(x, C))
    if (type=="logical") {
        x 
    } else {
        if (type=="numeric") {
            which(x)
        } else {
            stop("type must be logical or numeric")
        }
    }
}
