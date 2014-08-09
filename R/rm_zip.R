#' Remove/Replace/Extract Zip Codes
#' 
#' Remove/replace/extract zip codes from a string.
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
#' @param extract logical.  If \code{TRUE} the times are extracted into a 
#' list of vectors.
#' @param \dots Other arguments passed to \code{\link[base]{gsub}}.
#' @return Returns a character string with zip codes removed.
#' @keywords time
#' @export
#' @seealso \code{\link[base]{gsub}}
#' @author \href{http://stackoverflow.com/}{stackoverflow's} hwnd and Tyler Rinker <tyler.rinker@@gmail.com>. 
#' @references The time regular expression was taken from: 
#' \url{http://stackoverflow.com/a/25223890/1000343}
#' @examples
#' \dontrun{
#' x <- c("Mr. Bean bought 2 tickets 2-613-213-4567",
#'   "43 Butter Rd, Brossard QC K0A 3P0 - 613 213 4567", 
#'   "Rat Race, XX, 12345",
#'   "Ignore phone numbers(613)2134567",
#'   "Grab zips with dashes 12345-6789 or no space before12345-6789",  
#'   "Grab zips with spaces 12345 6789 or no space before12345 6789",
#'   "I like 1234567 dogs"
#' )
#'
#' rm_zip(x)
#' rm_zip(x, extract=TRUE)
#' }
rm_zip <- function(text.var, trim = TRUE, clean = TRUE,
    pattern = "(?<!\\d)\\d{5}(?:[ -]\\d{4})?\\b",
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
