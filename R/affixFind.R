#' Find Affixes
#' 
#' Searches a list of words and returns those with a specific affix
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param affix.string %% ~~Describe \code{affix.string} here~~
#' @param char.string %% ~~Describe \code{char.string} here~~
#' @param affix %% ~~Describe \code{affix} here~~
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
#' function (affix.string, char.string, affix = "suffix") 
#' {
#'     n.char <- nchar(affix.string)
#'     AFF <- affix(char.string = char.string, n.char = n.char, 
#'         affix = affix)
#'     AFF[which(AFF == affix.string)]
#'   }
#' 
affixFind <-
function(affix.string, char.string, affix='suffix'){
    n.char<-nchar(affix.string)
    AFF<-affix(char.string=char.string, n.char=n.char, affix=affix)
    AFF[which(AFF==affix.string)]
}
