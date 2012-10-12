#' Transform a termco Object to Proportions
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param tco %% ~~Describe \code{tco} here~~
#' @param output %% ~~Describe \code{output} here~~
#' @param digits %% ~~Describe \code{digits} here~~
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
#' function (tco, output = "percent", digits = 2) 
#' {
#'     a <- tco[, -c(1:2)]
#'     b <- tco[, 2]
#'     e <- tco[, 1:2]
#'     d <- switch(output, percent = lapply(a, function(x) round(100 * 
#'         (x/b), digits = digits)), proportion = lapply(a, function(x) round(x/b, 
#'         digits = digits)), per = lapply(a, function(x) round(100 * 
#'         (x/b), digits = digits)), prop = lapply(a, function(x) round(x/b, 
#'         digits = digits)))
#'     return(data.frame(e, d, check.names = FALSE))
#'   }
#' 
termco.p <-
function(tco, output = "percent", short.term = FALSE, digits = 2){
    subdf <- function(df, ii) {
        do.call("data.frame", c(as.list(df)[ii, drop=FALSE], check.names=FALSE))
    }
    a <- subdf(tco, -c(1:2))
    b <- tco[, 2]
    e <- tco[, 1:2]
    d <- switch(output, 
           percent = lapply(a, function(x) round(100*(x/b), digits=digits)), 
           proportion = lapply(a, function(x) round(x/b, digits=digits)),
           per = lapply(a, function(x) round(100*(x/b), digits=digits)), 
           prop = lapply(a, function(x) round(x/b, digits=digits))
         )
    o <- data.frame(e, d, check.names = FALSE)
    if (short.term) {
      o <- termco2short.term(o)
    }
    return(o)
}
