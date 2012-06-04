#' Searches Text Column for Words
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param begins.with %% ~~Describe \code{begins.with} here~~
#' @param contains %% ~~Describe \code{contains} here~~
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
#' function (text.var, begins.with = NULL, contains = NULL) 
#' {
#'     if (!is.null(begins.with) & !is.null(contains)) {
#'         stop("Can not use both 'begins.with' & 'contains' arguments")
#'     }
#'     if (!is.null(begins.with)) 
#'         begins.with <- tolower(begins.with)
#'     if (!is.null(contains)) 
#'         contains <- tolower(contains)
#'     WORDS <- unlist(word.split(reducer(strip(text.var))))
#'     names(WORDS) <- NULL
#'     y <- data.frame(table(WORDS), stringsAsFactors = FALSE)
#'     names(y) <- c("WORD", "FREQ")
#'     y$WORD <- as.character(y$WORD)
#'     if (!is.null(begins.with)) {
#'         y <- y[substring(y[, 1], 1, nchar(begins.with)) %in% 
#'             begins.with, ]
#'         if (nrow(y) == 0) 
#'             stop("No words match")
#'     }
#'     if (!is.null(contains)) {
#'         y <- y[grep(contains, y[, 1]), ]
#'         if (nrow(y) == 0) 
#'             stop("No words match")
#'     }
#'     left.just(y, "WORD")
#'   }
#' 
all.words <-
function(text.var, begins.with = NULL, contains = NULL){
    if (!is.null(begins.with) & !is.null(contains)) {
        stop("Can not use both 'begins.with' & 'contains' arguments")
    }

    if(!is.null(begins.with)) begins.with <- tolower(begins.with)
    if(!is.null(contains)) contains <- tolower(contains)
    WORDS <- unlist(word.split(reducer(strip(text.var))))
    names(WORDS) <- NULL
    y <- data.frame(table(WORDS), stringsAsFactors = FALSE)
    names(y) <- c("WORD", "FREQ")
    y$WORD <- as.character(y$WORD)
    if (!is.null(begins.with)) {
        y <- y[substring(y[, 1], 1, nchar(begins.with)) %in% begins.with, ]
        if(nrow(y)==0) stop("No words match")
    }

    if (!is.null(contains)) {
        y <- y[grep(contains, y[, 1]), ]
        if(nrow(y)==0) stop("No words match")
    }
    left.just(y, "WORD")
}
