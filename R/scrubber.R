#' Use to clean text variables when importing a new data set.
#' 
#' Use to clean text variables when importing a new data set.  Removes extra
#' white spaces other textual anomalies that may cause errors.
#' 
#' @param text.var The text variable
#' @param num2word logical If TRUE replaces a numbers with text representations.
#' @param fix.comma logical If TRUE removes any spaces before a comma.
#' @param rm.quote  logical If TRUE removes and \code{\"}.
#' @param \ldots Other arduments passed to \code{replace_number}.
#' @return Returns a parsed character vector.
#' @seealso \code{\link[qdap]{strip}}
#' @keywords parse, clean
#' @export
#' @examples
#' \dontrun{
#' #' x <- c("I like 456 dogs  , don't you?\"")
#' scrubber(x)
#' scrubber(x, TRUE)
#' }
scrubber <-
function(text.var, num2word = FALSE, rm.quote = TRUE, fix.comma = TRUE, ...){
    x <- reducer(Trim(clean(text.var)))
    if (rm.quote) {
        x  <- gsub('\"', "", x)
    }
    if (fix.comma) {
        x <- gsub(" ,", ",", x)
    }
    ncx <- nchar(x)
    x <- paste0(Trim(substring(x, 1, ncx - 1)), substring(x, ncx))
    x[x=="NANA"] <- NA
    if (num2word) {
        x <- replace_number(x, ...)
    }
    x
}
