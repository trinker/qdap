#' Head of Truncated Data Frame
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param dataframe %% ~~Describe \code{dataframe} here~~
#' @param n %% ~~Describe \code{n} here~~
#' @param width %% ~~Describe \code{width} here~~
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
#' function (dataframe, n = 10, width = 10) 
#' {
#'     x <- head(truncDF(dataframe, width), n = n)
#'     colnames(x) <- as.character(colnames(dataframe))
#'     return(x)
#'   }
#' 
htruncdf <-
function(dataframe, n=10, width=10) {
    x <- head(truncdf(dataframe, width),n=n)
    colnames(x) <- as.character(colnames(dataframe))
    return(x)
}
