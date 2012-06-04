#' Convert the tot Column from sentSplit to Turn of Talk Index
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param tot %% ~~Describe \code{tot} here~~
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
#' function (tot) 
#' {
#'     sapply(tot, function(x) as.numeric(as.character(unlist(strsplit(as.character(x), 
#'         ".", fixed = TRUE))[[1]])))
#'   }
#' 
TOT <-
function(tot){
    sapply(tot, function(x) 
           as.numeric(as.character(unlist(strsplit(as.character(x), ".", 
           fixed=TRUE))[[1]])))
}
