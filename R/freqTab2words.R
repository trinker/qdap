#' Convert a Word Frequency to Raw Words
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param word.list %% ~~Describe \code{word.list} here~~
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
#' function (word.list) 
#' {
#'     if (is.data.frame(word.list)) {
#'         rep(word.list[, 1], word.list[, 2])
#'     }
#'     else {
#'         lapply(word.list, function(x) rep(x[, 1], x[, 2]))
#'     }
#'   }
#' 
freqTab2words <-
function(word.list){
    if(is.data.frame(word.list)){
        rep(word.list[, 1], word.list[, 2])
    } else {
        lapply(word.list, function(x) rep(x[, 1], x[, 2]))
    }
}
