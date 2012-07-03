#' Replaces incomplete sentence marks with a "|"
#' 
#' Replaces incomplete sentence marks (.., ..., .?, ..?, en \& em dash etc.
#' with "|"
#' 
#' %% ~~ If necessary, more details than the description above ~~
#' 
#' @aliases incomplete.replace incomp
#' @param text.var %% ~~Describe \code{text.var} here~~
#' @param en.dash %% ~~Describe \code{en.dash} here~~
#' @param em.dash %% ~~Describe \code{em.dash} here~~
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
#' function (text.var, en.dash = FALSE, em.dash = FALSE) 
#' {
#'     x <- gsub("[.?!]+\.[.?!]*|[.?!]*\.[.?!]+", "|", scrubber(text.var))
#'     if (en.dash) {
#'         x <- gsub("[<96>]", "|", x)
#'     }
#'     if (em.dash) {
#'         x <- gsub("[<97>]", "|", x)
#'     }
#'     return(x)
#'   }
#' 
incomplete.replace <-
function(text.var, scan.mode = FALSE) {
  pat <- "\\?*\\?[.]+|[.?!]*\\? [.][.?!]+|[.?!]*\\. [.?!]+|
        [.?!]+\\. [.?!]*|[.?!]+\\.[.?!]*|[.?!]*\\.[.?!]+"
    if (scan.mode) {
      wid <- options()$width
      options(width = 10000)
      sel <- grepl(pat, scrubber(text.var))
      x <- data.frame(row.num = which(sel), 
                      text = as.character(text.var[sel]))
      print(left.just(x, 2))
      options(width = wid)
    } else {
      x <- gsub(pat, "|", scrubber(text.var))
      return(x)
    }
}
incomp <- incomplete.replace
