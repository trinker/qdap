#' Transcript Apply Summing of Polysyllables
#' 
#' %% ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @param text %% ~~Describe \code{text} here~~
#' @return %% ~Describe the value returned %% If it is a LIST, use %%
#' \item{comp1 }{Description of 'comp1'} %% \item{comp2 }{Description of
#' 'comp2'} %% ...
#' @note %% ~~further notes~~
#' @author %% ~~who you are~~
#' @seealso %% ~~objects to See Also as \code{\link{help}}, ~~~
#' @references %% ~put references to the literature/web site here ~
#' @keywords ~kwd1 ~kwd2
#' @examples
pollysyllable.sum <-
function(text) {
  Trim <- function(x) gsub("^\\s+|\\s+$", "", x)
  counter <- function(x) {
    v <- table(syllable.count(Trim(x))["syllables"])
    if (identical(c(v), integer(0))){
      return(0)
    }
    y <- as.data.frame(v)
    z <- subset(y, as.numeric(as.character(Var1)) >= 3)
    j <- sum(z$Freq)
    return(j)
  }
  unlist(lapply(as.character(text), function(x) counter(x)))
}
