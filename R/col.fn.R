#' Generate Color List From venneuler
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param col %% ~~Describe \code{col} here~~
#' @param alpha %% ~~Describe \code{alpha} here~~
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
#' function (col, alpha = 0.3) 
#' {
#'     col <- hcl(col * 360, 130, 60)
#'     col <- col2rgb(col)/255
#'     col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
#'     col
#'   }
#' 
col.fn <-
function(col, alpha=0.3) {
    col<- hcl(col * 360, 130, 60)
    col <- col2rgb(col)/255
    col <- rgb(col[1, ], col[2, ], col[3, ], alpha)
    col
}
