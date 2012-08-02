#' Wrapper for strwrap that Writes to the Windows Clipboard
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text %% ~~Describe \code{text} here~~
#' @param width %% ~~Describe \code{width} here~~
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
#' function (text = "clipboard", width = 70) 
#' {
#'     x <- if (text == "clipboard") {
#'         paste(readClipboard(), collapse = " ")
#'     }
#'     else {
#'         text
#'     }
#'     x <- gsub("\s+", " ", gsub("\n|\t", " ", x))
#'     x <- strwrap(x, width = width)
#'     writeClipboard(x, format = 1)
#'     writeLines(strwrap(x, width = width))
#'   }
#' 
strWrap <-
function(text = "clipboard", width = 70) {
    if (text == "clipboard") {
        if (Sys.info()["sysname"] == "Darwin") {
            text <- paste(pipe("pbpaste"), collapse=" ")
        }
        if (Sys.info()["sysname"] == "Windows") {
            text <- paste(readClipboard(), collapse=" ")
        }
    } 
    x <- gsub("\\s+", " ", gsub("\n|\t", " ", text))
    x <- strwrap(x, width = width)
    if (Sys.info()["sysname"] == "Windows") {
        writeClipboard(x, format = 1)
    }
    if (Sys.info()["sysname"] == "Darwin") {
        j <- pipe("pbcopy", "w")
        cat(x, file = j)
        close(j)
    }
    writeLines(x)
}