#' Stems a vector of text strings and outputs a data frame
#' 
#' Input a data frame with a text variable and outputs the data frame with the
#' stemmed text variable appended
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param dataframe %% ~~Describe \code{dataframe} here~~
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param stem.name %% ~~Describe \code{stem.name} here~~
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
#' function (dataframe, text.var, stem.name = NULL, ...) 
#' {
#'     if (!is.data.frame(dataframe)) {
#'         stop("Please supply a data.frame to stem2df")
#'     }
#'     if (is.numeric(dataframe[, text.var])) {
#'         stop("text.var can not be numeric")
#'     }
#'     new <- stemmer(dataframe[, text.var])
#'     DF <- data.frame(dataframe, stem.text = new, stringsAsFactors = FALSE)
#'     if (!is.null(stem.name)) {
#'         names(DF)[ncol(DF)] <- stem.name
#'     }
#'     return(DF)
#'   }
#' 
stem2df <-
function (dataframe, text.var, stem.name = NULL, ...) {
    if (!is.data.frame(dataframe)) {
        stop("Please supply a data.frame to stem2df")
    }
    if (is.numeric(dataframe[, text.var])) {
        stop("text.var can not be numeric")
    }
    new <- stemmer(dataframe[, text.var], ...)
    DF <- data.frame(dataframe, stem.text = new, stringsAsFactors = FALSE)
    if (!is.null(stem.name)) {
        names(DF)[ncol(DF)] <- stem.name
    } 
    return(DF)
}
