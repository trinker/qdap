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
function(dataframe, splitcol = 1, new.names=NULL, sep=".", 
         orig.keep=FALSE, ...){
    if (!is.data.frame(dataframe)){
        stop("Please supply a data.frame to colsplit2df")
    }
    if (is.numeric(dataframe[, splitcol])) {
        stop("splitcol can not be numeric")
    }
    X <- data.frame(do.call(rbind, strsplit(as.vector(
        dataframe[, splitcol]), split = sep, fixed=TRUE)))
    z <- if (!is.numeric(splitcol)) {
        match(splitcol, names(dataframe)) 
    } else {
        splitcol
    }
    if (!is.null(new.names)) {
        colnames(X) <- new.names
    }
    if (z!=1 & ncol(dataframe) > z) {
        w <- cbind(dataframe[, 1:(z-1), drop=FALSE], X, 
            dataframe[, (z + 1):ncol(dataframe), drop=FALSE])
    } else {
        if (z!=1 & ncol(dataframe) == z) {
            w <- cbind(dataframe[, 1:(z-1), drop=FALSE], X)
        } else {
            if (z==1 & ncol(dataframe) > z) {
                w <- cbind(X, dataframe[, (z + 1):ncol(dataframe), drop=FALSE])
            } else {
                w <- X
            }
        }
    }
    if (is.null(new.names) &"&" %in% unlist(strsplit(names(dataframe[, 
        splitcol, drop=FALSE]), split=NULL))) {
        nams <- unlist(strsplit(names(dataframe[, 
            splitcol, drop=FALSE]), split="&"))
        colnames(w)[1:length(nams)] <- nams
    }
    if(orig.keep) {
        w <- cbind(dataframe[, splitcol, drop=FALSE], w)
    }
    return(w)
}
