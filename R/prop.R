#' Convert a Matrix or Data Frame to Proportions
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param mat %% ~~Describe \code{mat} here~~
#' @param digits %% ~~Describe \code{digits} here~~
#' @param percent %% ~~Describe \code{percent} here~~
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
#' function (mat, digits = 2, percent = FALSE) 
#' {
#'     per <- if (percent) 
#'         100
#'     else 1
#'     daf <- data.frame(mat)
#'     daf <- sapply(seq_along(daf), function(x) round(per * (mat[, 
#'         x]/sum(mat)), digits = digits))
#'     mat2 <- as.matrix(daf)
#'     colnames(mat2) <- colnames(mat)
#'     return(mat2)
#'   }
#' 
prop <-
function(mat, digits = 2, percent = FALSE) {
    per <- if (percent) {100} else {1}
    daf <- data.frame(mat)
    daf <-sapply(seq_along(daf), function(x) round(per*(mat[, x]/sum(mat)), 
        digits = digits))
    mat2 <- as.matrix(daf)
    colnames(mat2) <- colnames(mat)
    return(mat2)
}
