#' Takes a Word Frequency List and Coverts to Raw Words
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param rfwlDF_LIST %% ~~Describe \code{rfwlDF_LIST} here~~
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
#' function (rfwlDF_LIST) 
#' {
#'     RFL <- function(rfwlDF) {
#'         BB <- rfwlDF
#'         unlist(as.character(BB[rep(seq(dim(BB)[1]), BB$FREQ), 
#'             1]))
#'     }
#'     lapply(rfwlDF_LIST, RFL)
#'   }
#' 
reduceWORDS <-
function(rfwlDF_LIST){
    RFL <- function(rfwlDF){
    BB <- rfwlDF
        unlist(as.character(BB[ rep( seq(dim(BB)[1]), BB$FREQ), 1]))
    }
    lapply(rfwlDF_LIST, RFL)
}
