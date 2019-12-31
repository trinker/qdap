#' Clean Imported Text
#' 
#' Use to clean text variables when importing a new data set.  Removes extra
#' white spaces other textual anomalies that may cause errors.
#' 
#' @param text.var The text variable.
#' @param num2word logical If \code{TRUE} replaces a numbers with text 
#' representations.
#' @param fix.comma logical If \code{TRUE} removes any spaces before a comma.
#' @param fix.space logical.  If \code{TRUE} extra spaces before endmarks are 
#' removed.
#' @param rm.quote  logical If \code{TRUE} removes any \code{\"}.
#' @param \ldots Other arguments passed to \code{\link[qdap]{replace_number}}.
#' @return Returns a parsed character vector.
#' @seealso \code{\link[qdap]{strip}}
#' @export
#' @examples
#' \dontrun{
#' x <- c("I like 456 dogs\t  , don't you?", 'The end"')
#' scrubber(x)
#' scrubber(x, TRUE)
#' }
scrubber <-
function(text.var, num2word = FALSE, rm.quote = TRUE, fix.comma = TRUE, 
    fix.space = TRUE, ...){
    x <- reducer(Trim(clean(text.var)))
    if (rm.quote) {
        x  <- gsub('\"', "", x)
    }
    if (fix.comma) {
        x <- gsub(" ,", ",", x)
    }
    ncx <- nchar(x)
    if (fix.space) {
        x <- Trim(gsub("(\\s+)([.?|!,*]+)$", "\\2", x))
    }
    x[is.na(text.var)] <- NA
    if (num2word) {
        x <- replace_number(x, ...)
    }
    x
}
