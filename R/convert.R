#' Convert Seconds to h:m:s
#' 
#' Converts a vector of seconds to h:m:s
#' 
#' @param x A vector of times in seconds.
#' @return Returns a vector of times in h:m:s format.  Generally, this function 
#' is for internal use.
#' @keywords time, coversion
#' @export
#' @import chron
#' @examples 
#' \dontrun{
#' convert(c(256, 3456, 56565))
#' }
convert <-
function(x) {
    l1 <- FALSE
    if (length(x) == 1) {
        x <- c(x, 0)
        l1 <- TRUE
    } 
    h <- floor(x/3600)
    m <- floor((x-h*3600)/60)
    s <- x-(m*60 + h*3600)
    pad <- function(x) sprintf("%02d", as.numeric(x))
    out <- times(paste2(data.frame(apply(data.frame(h=h, m=m, s=s), 
       2, pad)), sep=":"))
    if (l1) {
        out <- out[1]
    }
    out
}
#removed the second argument
#if (length(x) == 1) {
#    x <- c(x, 0)
#    l1 <- TRUE
#}
#op <- times(paste2(data.frame(apply(data.frame(h=h, m=m, s=s), 2, pad)), sep=":"))
#if (element_1) {
#    op[1]
#} else {
#    op
#}
# @param l1 logical.  If TRUE returns only the first element (intended for 
# internal use).
# #' convert(c(256, 3456, 56565), TRUE)
