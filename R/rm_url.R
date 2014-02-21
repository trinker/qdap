#' Remove/Replace URLs
#' 
#' Remove/Replace URLs from a string.
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
#' @param \dots Other arguments passed to \code{\link[base]{gsub}}.
#' @return Returns a character string with URLs removed.
#' @keywords url www http
#' @export
#' @seealso \code{\link[base]{gsub}}
#' @examples
#' \dontrun{
#' x <- " I like www.talkstats.com and http://stackoverflow.com"
#' rm_url(x)
#' rm_url(x, replacement = '<a href="\\1" target="_blank">\\1</a>')
#' }
rm_url <- function(text.var, trim = TRUE, clean = TRUE, 
    pattern = "(http[^ ]*)|(www\\.[^ ]*)", replacement = "", ...) {

    out <- gsub(pattern, replacement, text.var, ...)
    if (trim) out <- Trim(out)
    if (clean) out <- clean(out)
    out
}
