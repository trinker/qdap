#' Transcript Apply Conversion of Text to Lower Case and Words Only
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param digit.remove %% ~~Describe \code{digit.remove} here~~
#' @param apostrophe.remove %% ~~Describe \code{apostrophe.remove} here~~
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
#' function (x, digit.remove = TRUE, apostrophe.remove = FALSE) 
#' {
#'     strp <- function(x, digit.remove, apostrophe.remove) {
#'         x2 <- Trim(tolower(gsub(".*?($|'|[^[:punct:]]).*?", "\1", 
#'             as.character(x))))
#'         x2 <- if (apostrophe.remove) 
#'             gsub("'", "", x2)
#'         else x2
#'         ifelse(digit.remove == TRUE, gsub("[[:digit:]]", "", 
#'             x2), x2)
#'     }
#'     unlist(lapply(x, function(x) Trim(strp(x = x, digit.remove = digit.remove, 
#'         apostrophe.remove = apostrophe.remove))))
#'   }
#' 
strip <-
function (x, digit.remove = TRUE, apostrophe.remove = FALSE) {
    strp <- function(x, digit.remove, apostrophe.remove) {
        x2 <- Trim(tolower(gsub(".*?($|'|[^[:punct:]]).*?", "\\1", 
                                as.character(x))))
        x2 <- if (apostrophe.remove) 
            gsub("'", "", x2)
        else x2
        ifelse(digit.remove == TRUE, gsub("[[:digit:]]", "", 
                                          x2), x2)
    }
    x <- gsub("-", " ", x)
    x <- clean(gsub("/", " ", x)) 
    unlist(lapply(x, function(x) Trim(strp(x = x, digit.remove = digit.remove, 
                                           apostrophe.remove = apostrophe.remove))))
}

