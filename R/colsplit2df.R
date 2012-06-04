#' Wrapper for colSplit that Returns a Data Frame
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param dataframe %% ~~Describe \code{dataframe} here~~
#' @param column %% ~~Describe \code{column} here~~
#' @param orig.keep %% ~~Describe \code{orig.keep} here~~
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
#' function (dataframe, column = 1, orig.keep = FALSE, ...) 
#' {
#'     nc <- if (!is.numeric(column)) {
#'         match(column, names(dataframe))
#'     }
#'     else {
#'         column
#'     }
#'     ncols <- colSplit(dataframe[, column, drop = FALSE], ...)
#'     if (orig.keep) {
#'         cbind(dataframe[, nc, drop = FALSE], ncols, dataframe[, 
#'             -nc])
#'     }
#'     else {
#'         cbind(ncols, dataframe[, -nc])
#'     }
#'   }
#' 
colsplit2df <-
function(dataframe, column=1, orig.keep=FALSE, ...) {
    nc <- if (!is.numeric(column)) {
        match(column, names(dataframe))
    } else {
        column
    }
    ncols <- colSplit(dataframe[, column, drop=FALSE], ...)
    if(orig.keep) {
        cbind(dataframe[, nc, drop=FALSE], ncols, dataframe[, -nc])
    } else {
        cbind(ncols, dataframe[, -nc])
    }
}
