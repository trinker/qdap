#' Transcript Apply Count Number of Characters Used
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text %% ~~Describe \code{text} here~~
#' @param by %% ~~Describe \code{by} here~~
#' @param missing %% ~~Describe \code{missing} here~~
#' @param apostrophe %% ~~Describe \code{apostrophe} here~~
#' @param digit.remove %% ~~Describe \code{digit.remove} here~~
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
#' function (text, by = "row", missing = NA, apostrophe = TRUE, 
#'     digit.remove = TRUE) 
#' {
#'     text2 <- if (apostrophe == TRUE) {
#'         text
#'     }
#'     else {
#'         gsub("'", "", text, fixed = TRUE)
#'     }
#'     chara <- function(x) {
#'         y <- unlist(strsplit(strip(x, digit.remove = digit.remove), 
#'             NULL))
#'         z <- subset(y, y != " ")
#'         length(z)
#'     }
#'     OP <- switch(by, all = chara(paste(unlist(text2), collapse = "  ")), 
#'         row = unlist(lapply(text2, function(x) chara(x))))
#'     ifelse(OP == 0, missing, OP)
#'   }
#' 
character.count <-
function(text, by = "row", missing = NA, 
    apostrophe = TRUE, digit.remove = TRUE) {
    text2 <- if (apostrophe == TRUE) {
        text
    } else {
        gsub("'", "", text, fixed = TRUE)
    }
    chara <- function(x) {
        y <- unlist(strsplit(strip(x, digit.remove = digit.remove), 
            NULL))
        z <- subset(y, y != " ")
        length(z)
    }
    OP <- switch(by, 
           all = chara(paste(unlist(text2), collapse = "  ")), 
           row = unlist(lapply(text2, function(x) chara(x)))
          )
    ifelse(OP == 0, missing, OP)
}
