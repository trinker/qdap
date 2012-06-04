#' Merge Demogrphic Information with Person/Text Transcript
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param transcript.df %% ~~Describe \code{transcript.df} here~~
#' @param key.df %% ~~Describe \code{key.df} here~~
#' @param common.column %% ~~Describe \code{common.column} here~~
#' @param defualt.arrange %% ~~Describe \code{defualt.arrange} here~~
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
#' function (transcript.df, key.df, common.column, defualt.arrange = TRUE) 
#' {
#'     DF <- merge(transcript.df, key.df, by = c(common.column, 
#'         common.column), incomparables = NA)
#'     if (defualt.arrange) {
#'         DF[, c(1, 3:ncol(DF), 2)]
#'     }
#'     else {
#'         DF
#'     }
#'   }
#' 
key_merge <-
function(transcript.df, key.df, common.column, 
    defualt.arrange = TRUE) {
    DF <- merge(transcript.df, key.df, by = c(common.column, 
        common.column), incomparables = NA)
    if (defualt.arrange) {
        DF[, c(1, 3:ncol(DF), 2)]
    } else {
        DF
    }
}
