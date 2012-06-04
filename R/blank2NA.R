#' Add NA Values to Blank Cells
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param dataframe %% ~~Describe \code{dataframe} here~~
#' @param missing %% ~~Describe \code{missing} here~~
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
#' function (dataframe, missing = NA) 
#' {
#'     cn <- which(sapply(dataframe, is.character))
#'     FUN <- function(da) {
#'         if (is.character(da)) 
#'             da <- Trim(da)
#'         da[da == ""] <- missing
#'         names(da) <- NULL
#'         return(da)
#'     }
#'     DF <- data.frame(apply(dataframe, 2, FUN), check.names = FALSE)
#'     sapply(cn, function(i) {
#'         DF[, i] <<- as.character(DF[, i])
#'     })
#'     return(DF)
#'   }
#' 
blank2NA <-
function(dataframe, missing=NA){
    cn <- which(sapply(dataframe, is.character))
    FUN <- function(da) {
        if (is.character(da)) da <- Trim(da)
        da[da==""] <- missing
        names(da) <- NULL
        return(da)
    }
    DF <- data.frame(apply(dataframe, 2, FUN), check.names=FALSE)
    sapply(cn, function(i) {DF[, i] <<- as.character(DF[, i])})
    return(DF)
}
