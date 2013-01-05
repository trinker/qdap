#' Categorize and Code Outliers in Numeric String
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param x %% ~~Describe \code{standardize} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples

outlier.labeler <- function(x, standardize=TRUE){
    if (standardize) x <- scale(x)
    y <- ifelse(abs(x) >= 3, "3sd", 
        ifelse(abs(x) >= 2 & abs(x) < 3, "2sd", 
        ifelse(abs(x) >= 1.5 & abs(x) < 2, 
        "1.5sd", "-")))
    return(y)
}
