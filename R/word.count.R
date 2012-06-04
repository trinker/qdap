#' Transcript Apply Word Counts
#' 
#' Transcript Apply Word Counts
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases word.count wc
#' @param text %% ~~Describe \code{text} here~~
#' @param by %% ~~Describe \code{by} here~~
#' @param missing %% ~~Describe \code{missing} here~~
#' @param digit.remove %% ~~Describe \code{digit.remove} here~~
#' @param names %% ~~Describe \code{digit.remove} here~~
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
#' function(text, by = "row", missing = NA, 
#'     digit.remove = TRUE, names = TRUE) {
#'     txt <- sapply(as.character(text), function(x) {
#'             ifelse(is.na(x)|is.null(x), "", x)
#'         }
#'     )
#'     OP <- switch(by, 
#'          all = {length(unblanker(unlist(word.split(
#'                    reducer(unlist(strip(
#'                    paste(as.character(txt), collapse = " "), 
#'                    digit.remove = digit.remove)))))))}, 
#'          row = {sapply(txt, function(x) length(unblanker(
#'                    unlist(word.split(
#'                    reducer(unlist(strip(as.character(x),
#'                    digit.remove = 
#'                    digit.remove))))))))}
#'     )
#'     z <- ifelse(OP==0, missing, OP)
#'     if(!names) names(z) <- NULL
#'     return(z)
#' }
#' 
word.count <- 
function(text, by = "row", missing = NA, 
    digit.remove = TRUE, names = TRUE) {
    txt <- sapply(as.character(text), function(x) {
            ifelse(is.na(x)|is.null(x), "", x)
        }
    )
    OP <- switch(by, 
         all = {length(unblanker(unlist(word.split(reducer(unlist(strip(
                   paste(as.character(txt), collapse = " "), 
                   digit.remove = digit.remove)))))))}, 
         row = {sapply(txt, function(x) length(unblanker(unlist(word.split(
                   reducer(unlist(strip(as.character(x), digit.remove = 
         digit.remove))))))))}
    )
    z <- ifelse(OP==0, missing, OP)
    if(!names) names(z) <- NULL
    return(z)
}

wc <- word.count
