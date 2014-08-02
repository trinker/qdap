#' Remove/Replace/Extract Hash Tags
#' 
#' Remove/replace/extract hash tags from a string.
#' 
#' @param text.var The text variable.
#' @param trim logical.  If \code{TRUE} removes leading and trailing white 
#' spaces.
#' @param clean trim logical.  If \code{TRUE} extra white spaces and escaped 
#' character will be removed.
#' @param pattern A character string containing a regular expression (or 
#' character string for \code{fixed = TRUE}) to be matched in the given 
#' character vector.
#' @param replacement Replacement for matched \code{pattern}.
#' @param extract logical.  If \code{TRUE} the hash tags are extracted into a 
#' list of vectors.
#' @param \dots Other arguments passed to \code{\link[base]{gsub}}.
#' @return Returns a character string with hash tags removed.
#' @keywords hash twitter
#' @author \href{http://stackoverflow.com/}{stackoverflow's} hwnd and Tyler Rinker <tyler.rinker@@gmail.com>. 
#' @references The hash tag regular expression was taken from: 
#' \url{http://stackoverflow.com/a/25096474/1000343}
#' @export
#' @seealso \code{\link[base]{gsub}}
#' @examples
#' \dontrun{
#' x <- c("@@hadley I like #rstats for #ggplot2 work.",
#'     "Difference between #magrittr and #pipeR, both implement pipeline operators for #rstats: 
#'         http://renkun.me/r/2014/07/26/difference-between-magrittr-and-pipeR.html @@timelyportfolio",
#'     "Slides from great talk: @@ramnath_vaidya: Interactive slides from Interactive Visualization 
#'         presentation #user2014. http://ramnathv.github.io/user2014-rcharts/#1"
#' )
#' 
#' rm_hash(x)
#' rm_hash(rm_tag(x))
#' rm_hash(x, extract=TRUE)
#' }
rm_hash <- function(text.var, trim = TRUE, clean = TRUE, 
    pattern = "(?<!/)#\\w+", 
    replacement = "", extract = FALSE, ...) {

    if (extract) {
        return(lapply(regmatches(text.var, gregexpr(pattern, text.var, 
            perl = TRUE)), Trim))
    }

    out <- gsub(pattern, replacement, text.var, perl = TRUE, ...)
    if (trim) out <- Trim(out)
    if (clean) out <- clean(out)
    out
}

