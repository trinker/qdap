#' Transcript Apply Conversion of Text to Lower Case and Words Only
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param x %% ~~Describe \code{x} here~~
#' @param digit.remove %% ~~Describe \code{digit.remove} here~~
#' @param apostrophe.remove logical.  If TRUE removes apostrophe's from the output.
#' @param lower.case
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
strip <-
function (x, char.keep = NULL, digit.remove = TRUE, apostrophe.remove = TRUE,
    lower.case = TRUE) {
    strp <- function(x, digit.remove, apostrophe.remove, char.keep, lower.case) {
        if (!is.null(char.keep)) {
            x2 <- Trim(gsub(paste0(".*?($|'|",
            paste(char.keep, collapse = "|"),
            "|[^[:punct:]]).*?"), "\\1", 
                as.character(x)))
        } else {
            x2 <- Trim(gsub(".*?($|'|[^[:punct:]]).*?", "\\1", 
                as.character(x)))
        }
        if (lower.case) {
            x2 <- tolower(x2)
        }
        if (apostrophe.remove) {
            x2 <- gsub("'", "", x2)
        }
        ifelse(digit.remove == TRUE, gsub("[[:digit:]]", "", 
                                          x2), x2)
    }
    x <- clean(gsub("/", " ", gsub("-", " ", x))) 
    unlist(lapply(x, function(x) Trim(strp(x = x, digit.remove = digit.remove, 
       apostrophe.remove = apostrophe.remove, char.keep = char.keep, 
       lower.case = lower.case))))
}
