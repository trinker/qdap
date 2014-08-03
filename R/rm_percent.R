#' Remove/Replace/Extract Percentages
#' 
#' Remove/replace/extract percentages from a string.
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
#' @param extract logical.  If \code{TRUE} the percentages are extracted into a 
#' list of vectors.
#' @param \dots Other arguments passed to \code{\link[base]{gsub}}.
#' @return Returns a character string with percentages removed.
#' @keywords percent
#' @export
#' @seealso \code{\link[base]{gsub}}
#' @examples
#' \dontrun{
#' x <-  c("There is $5.50 for me.", "that's 45.6% of the pizza", 
#'     "14% is $26 or $25.99")
#'
#' rm_percent(x)
#' rm_percent(x, extract=TRUE)
#' }
rm_percent <- function(text.var, trim = TRUE, clean = TRUE, 
    pattern = "\\(?[0-9.]+\\)?%", 
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

