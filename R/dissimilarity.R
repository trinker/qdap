#' Dissimilarity Statistics
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param wfm.object %% ~~Describe \code{wfm.object} here~~
#' @param method %% ~~Describe \code{method} here~~
#' @param diag %% ~~Describe \code{diag} here~~
#' @param upper %% ~~Describe \code{upper} here~~
#' @param p %% ~~Describe \code{p} here~~
#' @param digits %% ~~Describe \code{digits} here~~
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
#' function (wfm.object, method = "euclidean", diag = FALSE, upper = FALSE, 
#'     p = 2, digits = 3) 
#' {
#'     if (comment(wfm.object) != "true.matrix") {
#'         warning("not a matrix from word.freq.matrix function")
#'     }
#'     if (comment(wfm.object) != "true.matrix") {
#'         wfm.object <- wfm.object[-c(nrow(wfm.object)), -c(1, 
#'             ncol(wfm.object))]
#'         wfm.object <- t(wfm.object)
#'     }
#'     else {
#'         wfm.object <- t(wfm.object)
#'     }
#'     x <- stats::dist(wfm.object, method = method, diag = diag, 
#'         upper = upper, p = p)
#'     return(round(x, digits = digits))
#'   }
#' 
dissimilarity <-
function(wfm.object, method = "euclidean", diag = FALSE, 
    upper = FALSE, p = 2, digits = 3){
    if (comment(wfm.object)!= "true.matrix") {
        warning("not a matrix from word.freq.matrix function")
    }
    if (comment(wfm.object)!= "true.matrix"){
        wfm.object <- wfm.object[-c(nrow(wfm.object)), -c(1, 
            ncol(wfm.object))]
        wfm.object <- t(wfm.object)
    } else {
        wfm.object <- t(wfm.object)
    }
    x <- stats::dist(wfm.object, method = method, diag = diag, upper = upper, p = p) 
    return(round(x, digits = digits))
}
