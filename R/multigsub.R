#' Takes a vector of search terms and a vector or single value of replacements
#' 
#' Takes a vector of search terms and a vector or single value of replacements
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases multigsub mgsub
#' @param pattern %% ~~Describe \code{pattern} here~~
#' @param replacement %% ~~Describe \code{replacement} here~~
#' @param x %% ~~Describe \code{x} here~~
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
#' function (pattern, replacement, x, ...) 
#' {
#'     key <- data.frame(pat = pattern, rep = replacement, stringsAsFactors = FALSE)
#'     msubs <- function(K, x, ...) {
#'         sapply(seq_len(nrow(K)), function(i) {
#'             x <<- gsub(K[i, 1], K[i, 2], x, ...)
#'         })
#'         return(x)
#'     }
#'     x <- msubs(K = key, x = x, ...)
#'     return(x)
#'   }
#' 
multigsub <-
function(pattern, replacement = NULL, text.var, leadspace = FALSE, 
    trailspace = FALSE, fixed = TRUE, ...){
    if (leadspace | trailspace) {
        replacement <- spaste(replacement, trailing = trailspace, 
            leading = leadspace)
    }
    key <- data.frame(pat=pattern, rep=replacement, 
        stringsAsFactors = FALSE)
    msubs <-function(K, x, ...){
        sapply(seq_len(nrow(K)), function(i){
                x <<- gsub(K[i, 1], K[i, 2], x, fixed = fixed, ...)
            }
        )
       return(gsub(" +", " ", x))
    }
    x <- Trim(msubs(K=key, x=text.var, ...))
    return(x)
}

mgsub <- multigsub
