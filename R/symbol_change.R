#' Transcript Apply Change Symbols for Word Equivalents
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param dollar %% ~~Describe \code{dollar} here~~
#' @param percent %% ~~Describe \code{percent} here~~
#' @param pound %% ~~Describe \code{pound} here~~
#' @param at %% ~~Describe \code{at} here~~
#' @param and %% ~~Describe \code{and} here~~
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
#' function (text.var, dollar = TRUE, percent = TRUE, pound = TRUE, 
#'     at = TRUE, and = TRUE) 
#' {
#'     sch <- function(text, dollar = TRUE, percent = TRUE, pound = TRUE, 
#'         at = TRUE, and = TRUE) {
#'         x <- ifelse(percent == TRUE, gsub("%", " percent ", text), 
#'             text)
#'         x <- ifelse(dollar == TRUE, gsub("$", " dollars ", x, 
#'             fixed = TRUE), x)
#'         x <- ifelse(pound == TRUE, gsub("#", " number ", x, fixed = TRUE), 
#'             x)
#'         x <- ifelse(and == TRUE, gsub("&", " and ", x, fixed = TRUE), 
#'             x)
#'         x <- ifelse(at == TRUE, gsub("@", " at ", x, fixed = TRUE), 
#'             x)
#'         gsub(" +", " ", x)
#'     }
#'     unlist(lapply(text.var, function(text) sch(text, dollar = dollar, 
#'         percent = percent, pound = pound, at = at, and = and)))
#'   }
#' 
symbol_change <-
function(text.var, dollar = TRUE, percent = TRUE, 
         pound = TRUE, at = TRUE, and = TRUE, with = TRUE) {
  
  sch <- function(text.var, dollar, percent, pound, 
                  at, and, with) {
    if(percent) {
      text.var <- gsub("%", " percent ", text.var)
    }
    if (dollar) {
      text.var <- gsub("$", " dollars ", text.var, fixed = TRUE)
    }
    if (pound) {
      text.var <- gsub("#", " number ", text.var, fixed = TRUE)
    }
    if (and) {
      text.var <- gsub("&", " and ", text.var, fixed = TRUE)
    }
    if (at) {
      text.var <- gsub("@", " at ", text.var, fixed = TRUE)
    }
    if (with) {
      text.var <- gsub("w/o", " without ", text.var, fixed = TRUE)
    }
    if (with) {
      text.var <- gsub("w/", " with ", text.var, fixed = TRUE)
    }
    return(gsub(" +", " ", text.var))
  }
  sch(text.var, dollar = dollar, 
      percent = percent, pound = pound, at = at, and = and, with = with)
}

