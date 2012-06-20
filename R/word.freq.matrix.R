#' Generate Word Ffrequency Matrix by Person
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases word.freq.matrix wfm
#' @param wfdf %% ~~Describe \code{wfdf} here~~
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param grouping.var %% ~~Describe \code{grouping.var} here~~
#' @param stopwords %% ~~Describe \code{stopwords} here~~
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
#' function (wfdf = NULL, text.var = NULL, grouping.var = NULL, 
#'     stopwords = NULL, digits = 2) 
#' {
#'     if (!is.null(wfdf)) {
#'         if (comment(wfdf) == "t.df") {
#'             wfdf <- wfdf
#'         }
#'         else {
#'             if (comment(wfdf) == "m.df") {
#'                 wfdf <- wfdf[-nrow(wfdf), -ncol(wfdf)]
#'             }
#'             else {
#'                 stop("Object must be a raw word frequency data frame")
#'             }
#'         }
#'         x2 <- wfdf[, -1]
#'         rownames(x2) <- wfdf[, 1]
#'         x2 <- as.matrix(x2)
#'     }
#'     else {
#'         if (!is.null(text.var) & !is.null(grouping.var)) {
#'             wfdf <- word.freq.df(text.var = text.var, grouping.var = grouping.var, 
#'                 stopwords = stopwords)
#'             x2 <- wfdf[, -1]
#'             rownames(x2) <- wfdf[, 1]
#'             x2 <- as.matrix(x2)
#'         }
#'         else {
#'             stop("must specify both text.var & grouping var or wfdf")
#'         }
#'     }
#'     comment(x2) <- "true.matrix"
#'     return(x2)
#'   }
#' 
word.freq.matrix <-
function(wfdf = NULL, text.var = NULL, 
    grouping.var = NULL, stopwords = NULL, digits = 2){
    if (!is.null(wfdf)) {
        if (comment(wfdf) == "t.df") {
            wfdf <- wfdf
        } else {
            if (comment(wfdf) == "m.df") { 
                wfdf <- wfdf[-nrow(wfdf), -ncol(wfdf)]
            } else {
                stop("Object must be a raw word frequency data frame")
            }
        }

        x2 <- wfdf[, -1, drop = FALSE]
        rownames(x2) <- wfdf[, 1]
        x2 <- as.matrix(x2)
    } else {
        if (!is.null(text.var) & ! is.null(grouping.var)) {
            wfdf <- word.freq.df(text.var = text.var, grouping.var = grouping.var, 
                 stopwords = stopwords) 
            x2 <- wfdf[, -1, drop = FALSE]
            rownames(x2) <- wfdf[, 1]
            x2 <- as.matrix(x2)
        } else {
            stop ("must specify both text.var & grouping var or wfdf")
        }
    }
    comment(x2) <- "true.matrix"
    return(x2)
}

wfm <- word.freq.matrix 
