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
replace_symbol <-
function(text.var, dollar = TRUE, percent = TRUE, 
         pound = TRUE, at = TRUE, and = TRUE, with = TRUE) {
  x <- c(dollar, percent, pound, at, and, with, with)
  mgsub(pattern = c("%", "$", "#", "&", "@", "w/o", "w/")[x], 
        replacement = c("percent", "dollar", "pound", "and", "at", 
        "without", "with")[x], text.var = text.var, fixed = TRUE,
         leadspace = FALSE, trailspace = FALSE)
}

