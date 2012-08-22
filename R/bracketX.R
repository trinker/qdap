#' Transcript Apply Removal of Brackets and Encased Text
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text %% ~~Describe \code{text} here~~
#' @param bracket %% ~~Describe \code{bracket} here~~
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
#' function (text, bracket = "all", missing = NULL) 
#' {
#'     X <- switch(bracket, square = sapply(text, function(x) gsub("\[.+?\]", 
#'         "", x)), round = sapply(text, function(x) gsub("\(.+?\)", 
#'         "", x)), curly = sapply(text, function(x) gsub("\{.+?\}", 
#'         "", x)), all = {
#'         P1 <- sapply(text, function(x) gsub("\[.+?\]", "", 
#'             x))
#'         P1 <- sapply(P1, function(x) gsub("\(.+?\)", "", x))
#'         sapply(P1, function(x) gsub("\{.+?\}", "", x))
#'     })
#'     X <- Trim(gsub(" +", " ", X))
#'     if (!is.null(missing)) {
#'         X[X == ""] <- missing
#'     }
#'     return(X)
#'   }
#' 
bracketX <-
function (text, bracket = "all", missing = NULL, names=FALSE) {
    X <- switch(bracket, 
                html = sapply(text, function(x) gsub("<.+?>", "", x)),
                angle = sapply(text, function(x) gsub("<.+?>", "", x)),
                square = sapply(text, function(x) gsub("\\[.+?\\]", "", x)), 
                round = sapply(text, function(x) gsub("\\(.+?\\)", "", x)), 
                curly = sapply(text, function(x) gsub("\\{.+?\\}", "", x)), 
                all = {
                    P1 <- sapply(text, function(x) gsub("\\[.+?\\]", "", x))
                    P1 <- sapply(P1, function(x) gsub("\\(.+?\\)", "", x))
                    P1 <- sapply(P1, function(x) gsub("<.+?>", "", x))
                    sapply(P1, function(x) gsub("\\{.+?\\}", "", x))
                }
    )
    X <- scrubber(gsub(" +", " ", X))
    if (!is.null(missing)) {
        X[X == ""] <- missing
    }
    if (!names) names(X) <- NULL
    return(X)
}
