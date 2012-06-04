#' Merge Multiple Data Sets
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param frames %% ~~Describe \code{frames} here~~
#' @param by %% ~~Describe \code{by} here~~
#' @param na.replace %% ~~Describe \code{na.replace} here~~
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
#' function (frames, by, na.replace = NA) 
#' {
#'     DF <- Reduce(function(x, y) {
#'         merge(x, y, by = by, all = TRUE)
#'     }, frames)
#'     return(NAer(DF, replace = na.replace))
#'   }
#' 
merge.all <-
function(frames, by, na.replace = NA) {
    DF <- Reduce(function(x, y) {merge(x, y, by = by, all = TRUE)}, frames)
    return(NAer(DF, replace = na.replace))
}
