#' Use to clean text variables when importing a new data set.
#' 
#' Use to clean text variables when importing a new data set.  Removes extra
#' white spaces other textual anomalies that may cause errors.
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param num2word %% ~~Describe \code{num2word} here~~
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
#' function (text.var, num2word = FALSE) 
#' {
#'     reducer(Trim(clean(text.var)))
#'   }
#' 
scrubber <-
function(text.var, num2word=FALSE){
    reducer(Trim(clean(text.var)))
}
