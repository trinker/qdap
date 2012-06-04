#' Separate a Column Pasted by paste2
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param column %% ~~Describe \code{column} here~~
#' @param name.sep %% ~~Describe \code{name.sep} here~~
#' @param col.sep %% ~~Describe \code{col.sep} here~~
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
#' function (column, name.sep = "&", col.sep = ".") 
#' {
#'     column <- as.data.frame(column)
#'     svar <- strsplit(as.character(column[, 1]), col.sep, fixed = TRUE)
#'     svar <- data.frame(do.call("rbind", svar))
#'     if (length(unlist(strsplit(names(column), name.sep, fixed = TRUE))) > 
#'         1) {
#'         cn <- strsplit(names(column)[1], name.sep, fixed = TRUE)[[1]]
#'         names(svar) <- cn
#'     }
#'     return(svar)
#'   }
#' 
colSplit <-
function(column, name.sep = "&", col.sep = "."){
    column <- as.data.frame(column)
    svar <- strsplit(as.character(column[, 1]), col.sep, fixed = TRUE)
    svar <- data.frame(do.call('rbind', svar))
    if (length(unlist(strsplit(names(column), 
        name.sep, fixed = TRUE))) > 1){
        cn <- strsplit(names(column)[1], name.sep, fixed = TRUE)[[1]]
        names(svar) <- cn
    } 
    return(svar)
}
