#' Search Terms for Colulmns of a Data Frame
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param term %% ~~Describe \code{term} here~~
#' @param dataframe %% ~~Describe \code{dataframe} here~~
#' @param column.name %% ~~Describe \code{column.name} here~~
#' @param variation %% ~~Describe \code{variation} here~~
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
#' function (term, dataframe, column.name, variation = 0.02, ...) 
#' {
#'     te <- as.character(substitute(term))
#'     cn <- as.character(substitute(column.name))
#'     HUNT <- agrep(te, dataframe[, cn], ignore.case = TRUE, max.distance = variation, 
#'         ...)
#'     dataframe[c(HUNT), ]
#'   }
#' 
Search <-
function(term, dataframe, column.name, variation = 0.02, ...) {
    te <- as.character(substitute(term))
    cn <- as.character(substitute(column.name))
    HUNT <- agrep(te, dataframe[, cn], ignore.case = TRUE, 
        max.distance = variation, ...)
    dataframe[c(HUNT), ]
}
