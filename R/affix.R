#' Affix Locator
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param char.string %% ~~Describe \code{char.string} here~~
#' @param n.char %% ~~Describe \code{n.char} here~~
#' @param affix %% ~~Describe \code{affix} here~~
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
#' function (char.string, n.char, affix = "suffix") 
#' {
#'     Trim <- function(x) gsub("^\s+|\s+$", "", x)
#'     ender <- function(x, s) {
#'         nc <- nchar(x)
#'         substring(x, (nc - s + 1), nc)
#'     }
#'     beginer <- function(x, s) substring(x, 1, s)
#'     if (affix == "suffix") {
#'         sapply(Trim(as.character(char.string)), function(x) ender(x, 
#'             n.char))
#'     }
#'     else {
#'         if (affix == "prefix") {
#'             sapply(Trim(as.character(char.string)), function(x) beginer(x, 
#'                 n.char))
#'         }
#'         else {
#'             cat("\nWARNING:\nIncorrect affix argument!\n\n")
#'         }
#'     }
#'   }
#' 
affix <-
function(char.string, n.char, affix="suffix") {
    Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
    ender <- function(x, s) {
        nc <- nchar(x)
        substring(x, (nc - s + 1), nc)
    }
    beginer <- function(x, s) substring(x, 1, s)
    if (affix == "suffix") {
        sapply(Trim(as.character(char.string)), function(x) ender(x, 
            n.char))
    } else {
        if (affix == "prefix") {
            sapply(Trim(as.character(char.string)), function(x) beginer(x, 
                n.char))
        } else {
            cat("\nWARNING:\nIncorrect affix argument!\n\n")
        }
    }
}
