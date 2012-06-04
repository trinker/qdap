#' Retrieve Rows From a Data Frame
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param dataframe %% ~~Describe \code{dataframe} here~~
#' @param n %% ~~Describe \code{n} here~~
#' @param margin %% ~~Describe \code{margin} here~~
#' @param text.col %% ~~Describe \code{text.col} here~~
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
#' function (dataframe, n, margin = 2, text.col = "all") 
#' {
#'     surround <- function(N, mar, DF) {
#'         x <- c((N - mar:1), N, (N + 1:mar))
#'         x[x > 0 & x < nrow(DF)]
#'     }
#'     z <- if (margin == 0) {
#'         n
#'     }
#'     else {
#'         surround(N = n, mar = margin, DF = dataframe)
#'     }
#'     if (text.col == "all" | is.null(text.col)) {
#'         dataframe[z, ]
#'     }
#'     else {
#'         as.character(dataframe[z, text.col])
#'     }
#'   }
#' 
sentFind <-
function(dataframe, n, margin = 2, text.col = "all") {
    surround <- function(N, mar, DF) {
        x <- c((N - mar:1), N, (N + 1:mar))
        x[x > 0 & x < nrow(DF)]
    }
    z <- if (margin == 0) {
        n
    } else {
        surround(N = n, mar = margin, DF = dataframe)
    }
    if (text.col == "all" | is.null(text.col)) {
        dataframe[z, ]
    } else {
        as.character(dataframe[z, text.col])
    }
}
