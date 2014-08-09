#' Remove/Replace/Extract Phone Numbers
#' 
#' Remove/replace/extract phone numbers from a string.
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
#' @return Returns a character string with phone numbers removed.
#' @keywords time
#' @export
#' @seealso \code{\link[base]{gsub}}
#' @author \href{http://stackoverflow.com/}{stackoverflow's} Marius and Tyler Rinker <tyler.rinker@@gmail.com>. 
#' @references The time regular expression was taken from: 
#' \url{http://stackoverflow.com/a/21008254/1000343}
#' @examples
#' \dontrun{
#' x <- c(" Mr. Bean bought 2 tickets 2-613-213-4567 or 5555555555 call either one",
#'   "43 Butter Rd, Brossard QC K0A 3P0 - 613 213 4567", 
#'   "Please contact Mr. Bean (613)2134567",
#'   "1.575.555.5555 is his #1 number",  
#'   "7164347566",
#'   "I like 1234567 dogs"
#' )
#'
#' rm_phone(x)
#' rm_phone(x, extract=TRUE)
#' }
rm_phone <- function(text.var, trim = TRUE, clean = TRUE,
    pattern = "(?:(?:\\+?1\\s*(?:[.-]\\s*)?)?(?:\\(\\s*([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9])\\s*\\)|([2-9]1[02-9]|[2-9][02-8]1|[2-9][02-8][02-9]))\\s*(?:[.-]\\s*)?)?([2-9]1[02-9]|[2-9][02-9]1|[2-9][02-9]{2})\\s*(?:[.-]\\s*)?([0-9]{4})(?:\\s*(?:#|x\\.?|ext\\.?|extension)\\s*(\\d+))?",
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
